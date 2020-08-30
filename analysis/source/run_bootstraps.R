source("analysis/source/prelim.R")

wages <- read_csv("analysis/release/wages_with_replacement_rates.csv")
wages_2017 <- read_csv("analysis/release/wages_with_replacement_rates_2017.csv")

#### Histogram of replacement rates ####

#load in the deciles from reweight_income_morg
deciles <- read_rds("analysis/release/deciles.rds")

logit_fit <- read_rds("analysis/release/logit_fit.rds")

wages_logit_weights <- wages %>%
  mutate(inc_decile = cut(weekly_earnings, deciles),
         two_digit_ind = as.factor(two_digit_ind)) %>%
  broom::augment(logit_fit, newdata = .,
                 type.predict = "response") %>%
  mutate(weight = weight * .fitted)


share_of_data_ineligible <- wages_logit_weights %>%
  group_by(cond = benefits_amount > 0) %>%
  summarise(sum = sum(weight)) %>%
  mutate(share = sum/sum(sum)) %>%
  filter(cond) %>%
  pull(share)


wages_logit_weights_with_PUA <- wages_logit_weights %>%
  group_by(state) %>%
  mutate(benefits_amount = if_else(benefits_amount == 0,
                                   0.5 * Hmisc::wtd.mean(benefits_amount, (benefits_amount > 0) * weight),
                                   benefits_amount)) %>%
  ungroup()



wages <- wages %>%
  filter(benefits_amount > 0)

wages_logit_weights <- wages_logit_weights %>%
  filter(benefits_amount > 0)

health_benefits_scaling <- (7.95-2.16)/2.6


library(doParallel)

registerDoParallel(cores=(Sys.getenv("SLURM_NTASKS_PER_NODE")))





sum_stat <- function(quantiles, rep_rate, weight){
  output <- rep(0, length(quantiles))

  for (i in 1:length(quantiles)) {
    if (is.na(quantiles[i])) {
      output[i] <- Hmisc::wtd.mean(rep_rate > 1, weight)
    } else {
      output[i] <- Hmisc::wtd.quantile(rep_rate, weights = weight, quantiles[i])

    }
  }
  return(output)
}



robustness_regular <- wages_logit_weights %>%
  filter(benefits_amount > 0) %>%
  summarise(type = "Statutory replacement rate",
            statistic = c(rep("Quantile", 3), "Share over 1"),
            quantile = c(0.25, 0.5, 0.75, NA),
            value = sum_stat(c(0.25, 0.5, 0.75, NA),
                             replacement_rate_FPUC, weight))

state_estimates <- wages_logit_weights %>%
  filter(benefits_amount > 0) %>%
  group_by(state) %>%
  summarise(main_est_FPUC = Hmisc::wtd.quantile(replacement_rate_FPUC, weight, 0.5),
            main_est_Jan = Hmisc::wtd.quantile(replacement_rate, weight, 0.5))


occupation_estimates <- wages_logit_weights %>%
  group_by(two_digit_occ) %>%
  summarise(main_est = Hmisc::wtd.quantile(replacement_rate_FPUC, weight, 0.5))

#### Merge on replicate weights ####
replicate_weights <- read_csv("analysis/input/replicates_1719.gz") %>%
  select(year = YEAR,
         serial = SERIAL,
         pernum = PERNUM,
         contains("REP"))



logit_data <- read_csv("analysis/release/logit_data.csv")

### Check the effect of removing tipped occupations

occ_2019 <- read_csv("analysis/input/occ_2019.csv") %>%
  mutate(occupation = as.character(occupation))


### Robustness Non Labour income


#want to construct a wages a wages_2017 that has the required replacement rates
#For all stats except tipping and PUA, can then calculate all at once and pivot longer
#for tipping will need to calculate sperately


wages <- wages %>%
  mutate(replacement_rate,
         replacement_rate_FPUC,
         replacement_rate_PUA =  (benefits_amount + 600)/(weekly_earnings),
         replacement_rate_business_income = (benefits_amount + 600)/(weekly_earnings + if_else(business_income > 0,
                                                                                               business_income/52,
                                                                                               0)),
         replacement_rate_payroll_tax = (benefits_amount + 600)/((1 - 0.0765) * weekly_earnings)) %>%
  mutate(occupation_full = as.character(occupation_full)) %>%
  left_join(occ_2019 %>% filter(tip_commission), by = c(occupation_full = "occupation"))




wages_2017 <- wages_2017 %>%
  filter(replacement_rate_FPUC > 0) %>%
  mutate(inc_decile = cut(weekly_earnings, deciles),
         two_digit_ind = as.factor(two_digit_ind)) %>%
  mutate(replacement_rate_imputed = (benefits_amount + 600)/(weekly_earnings + health_benefits_scaling * weekly_employer_contribution),
         replacement_rate_inputed_tax = (benefits_amount + 600)/((1 - 0.0765) * weekly_earnings + health_benefits_scaling * weekly_employer_contribution))




#only need the replicate weights of earners:

replicate_weights <- wages_2017 %>%
  select(year, pernum, serial) %>%
  bind_rows(wages %>% select(year, pernum, serial)) %>%
  inner_join(replicate_weights)







get_sum_stats <- function(x){

  summarise_df <- function(data, column, name){
    data %>%
      group_by(index_group) %>%
      summarise(statistic = c(rep("Quantile", 3), "Share over 1"),
                quantile = c(0.25, 0.5, 0.75, NA),
                value = sum_stat(c(0.25, 0.5, 0.75, NA),
                                 !!sym(column), weight),
                type = name)}

  bootstrap_data <- logit_data %>%
    sample_n(nrow(logit_data),
             replace = TRUE)

  deciles <- bootstrap_data %>%
    filter(job_loser) %>%
    with(Hmisc::wtd.quantile(weekly_earnings,
                             weight,
                             seq(0, 1, length.out = 11)))
  deciles[1] <- 0
  deciles[11] <- Inf


  logit_fit_boot <- bootstrap_data %>%
    mutate(inc_decile = cut(weekly_earnings, deciles),
           two_digit_ind = as.factor(two_digit_ind),
           job_loser = as.numeric(job_loser),
           weight = weight/sum(weight)) %>% # rescale weights to avoid errors
    glm(job_loser ~ inc_decile + two_digit_occ + two_digit_ind + state,
        data = ., family = binomial(),
        weights = weight)


  rm(bootstrap_data)

  reweighted_replicates <- wages %>%
    mutate(inc_decile = cut(weekly_earnings, deciles),
           two_digit_ind = as.factor(two_digit_ind)) %>%
    broom::augment(logit_fit_boot, newdata = .,
                   type.predict = "response") %>%
    left_join(replicate_weights) %>%
    select(-REPWTP) %>%
    select(contains("replacement_rate"), two_digit_ind,
           REPWTP_ORIG = weight,
           two_digit_occ, state, tip_commission,
           benefits_amount,
           contains("REP"), weekly_earnings, .fitted) %>%
    pivot_longer(cols = contains("REPWTP"),
                 names_to = "index_group",
                 values_to = "weight") %>%
    mutate(weight = weight * .fitted)


  PUA_estimates <- reweighted_replicates %>%
    summarise_df("replacement_rate_PUA",
                 "PUA_recipiency")

  reweighted_replicates <- filter(reweighted_replicates, benefits_amount > 0)


  main_estimates <- reweighted_replicates %>%
    summarise_df("replacement_rate_FPUC",
                 "main")

  payroll <- reweighted_replicates %>%
    summarise_df("replacement_rate_payroll_tax",
                 "payroll")

  business_income <- reweighted_replicates  %>%
    summarise_df("replacement_rate_business_income",
                 "business")

  no_tipping <- reweighted_replicates %>%
    filter(is.na(tip_commission)) %>%
    summarise_df("replacement_rate_FPUC",
                 "no tipping")

  state_estimates <- reweighted_replicates %>%
    group_by(index_group, state) %>%
    summarise(value_state_FPUC = Hmisc::wtd.quantile(replacement_rate_FPUC, weight, 0.5),
              value_state_Jan = Hmisc::wtd.quantile(replacement_rate, weight, 0.5)) %>%
    pivot_longer(cols = contains("value_state"),
                 values_to = "value",
                 names_to = "type",
                 names_prefix = "value_")


  occupation_estimates <- reweighted_replicates %>%
    select(index_group, replacement_rate_FPUC, two_digit_occ, weight) %>%
    inner_join(occupation_codes, by = c(two_digit_occ = "code")) %>%
    group_by(index_group, occupation) %>%
    summarise(value = Hmisc::wtd.quantile(replacement_rate_FPUC, weight, 0.5))


  rm(reweighted_replicates)

  reweighted_replicates_2017 <- wages_2017 %>%
    mutate(inc_decile = cut(weekly_earnings, deciles),
           two_digit_ind = as.factor(two_digit_ind)) %>%
    broom::augment(logit_fit_boot, newdata = .,
                   type.predict = "response") %>%
    left_join(replicate_weights) %>%
    select(-REPWTP) %>%
    select(contains("replacement_rate"), two_digit_ind,
           REPWTP_ORIG = weight,
           two_digit_occ, state,
           contains("REP"), weekly_earnings, .fitted) %>%
    pivot_longer(cols = contains("REPWTP"),
                 names_to = "index_group",
                 values_to = "weight") %>%
    mutate(weight = weight * .fitted)

  rm(logit_fit_boot)

  benefits_only <- reweighted_replicates_2017 %>%
    summarise_df("replacement_rate_imputed",
                 "benefits only")

  benefits_and_tax <- reweighted_replicates_2017 %>%
    summarise_df("replacement_rate_inputed_tax",
                 "benefits and tax")



  all_estimates <- bind_rows(main_estimates, PUA_estimates,
                             payroll, business_income,
                             no_tipping, benefits_only,
                             benefits_and_tax,
                             occupation_estimates,
                             state_estimates)

  original_weights <- all_estimates %>% filter(index_group == "REPWTP_ORIG") %>%
    ungroup() %>%
    transmute(statistic, quantile, type, state, occupation, orig_weight = value)

  output <- all_estimates %>%
    filter(index_group != "REPWTP_ORIG") %>%
    left_join(original_weights) %>%
    group_by(statistic, quantile, type, state, occupation) %>%
    summarise(var = 4/160 * sum((value - orig_weight) ^ 2), # Var(X | Y) for E(Var(X | Y))
              mean = mean(value)) #E(X | Y) for Var( E ( X | Y)))

  print(output)

  return(output)
}

set.seed(2020)
bootstrap_output <- foreach(i = 1:300) %dopar%
  get_sum_stats(i)


print(bootstrap_output)

bootstrap_output %>%
  bind_rows() %>%
  write_csv("analysis/input/bootstrap_output.csv")
