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

library(furrr)
plan(multiprocess)



robustness_regular <- wages_logit_weights %>%
  summarise(type = "Main estimate",
            statistic = c(rep("Quantile", 3), "Share over 1"),
            quantile = c(0.25, 0.5, 0.75, NA),
            value = sum_stat(c(0.25, 0.5, 0.75, NA),
                             replacement_rate_FPUC, weight))

state_estimates <- wages_logit_weights %>%
  group_by(state) %>%
  summarise(main_est = Hmisc::wtd.quantile(replacement_rate_FPUC, weight, 0.5))


occupation_estimates <- wages_logit_weights %>%
  group_by(two_digit_occ) %>%
  summarise(main_est = Hmisc::wtd.quantile(replacement_rate_FPUC, weight, 0.5))

#### Merge on replicate weights ####
replicate_weights <- read_csv("analysis/input/covid/ASEC_2019_replicates.gz") %>%
  select(serial = SERIAL,
         pernum = PERNUM,
         contains("REP"))



logit_data <- read_csv("analysis/release/logit_data.csv")

#alternate_logits <- read_rds( "analysis/release/random_deciles.rds")


#Two components in the variance

#Var(Y) = E(Var(Y | X)) + Var(E(Y | X))

#So we average the replicate variance across the bootstrap samples
#And add it to the bootstrapped variance of the estimates


get_sum_stats <- function(x){

  boostrap_data <- logit_data %>%
    sample_n(nrow(logit_data),
             replace = TRUE)

  deciles <- boostrap_data %>%
    filter(job_loser) %>%
    with(Hmisc::wtd.quantile(weekly_earnings,
                             weight,
                             seq(0, 1, length.out = 11)))
  deciles[1] <- 0
  deciles[11] <- Inf


  logit_fit_boot <- boostrap_data %>%
    mutate(inc_decile = cut(weekly_earnings, deciles),
           two_digit_ind = as.factor(two_digit_ind),
           job_loser = as.numeric(job_loser),
           weight = weight/sum(weight)) %>% # rescale weights to avoid errors
    glm(job_loser ~ inc_decile + two_digit_occ + two_digit_ind + state,
        data = ., family = binomial(),
        start = logit_fit$coefficients, #starting from main coefficients marginally speeds up calculation
        weights = weight)


  reweighted_replicates <- wages %>%
    mutate(inc_decile = cut(weekly_earnings, deciles),
           two_digit_ind = as.factor(two_digit_ind)) %>%
    broom::augment(logit_fit_boot, newdata = .,
                   type.predict = "response") %>%
    left_join(replicate_weights) %>%
    select(-REPWTP) %>%
    select(replacement_rate_FPUC, two_digit_ind,
           REPWTP_ORIG = weight,
           two_digit_occ, state,
           contains("REP"), weekly_earnings, .fitted) %>%
    pivot_longer(cols = contains("REPWTP"),
                 names_to = "index_group",
                 values_to = "weight") %>%
    mutate(weight = weight * .fitted)




  main_estimates <- reweighted_replicates %>%
    group_by(index_group) %>%
    summarise(statistic = c(rep("Quantile", 3), "Share over 1"),
              quantile = c(0.25, 0.5, 0.75, NA),
              value = sum_stat(c(0.25, 0.5, 0.75, NA),
                               replacement_rate_FPUC, weight),
              type = "main")

  state_estimates <- reweighted_replicates %>%
    group_by(index_group, state) %>%
    summarise(value = Hmisc::wtd.quantile(replacement_rate_FPUC, weight, 0.5),
              type = "state")

  occupation_estimates <- reweighted_replicates %>%
    group_by(index_group, two_digit_occ) %>%
    summarise(value = Hmisc::wtd.quantile(replacement_rate_FPUC, weight, 0.5))


  all_estimates <- bind_rows(main_estimates, state_estimates, occupation_estimates)

  original_weights <- all_estimates %>% filter(index_group == "REPWTP_ORIG") %>%
    ungroup() %>%
    transmute(statistic, quantile, type, state, two_digit_occ, orig_weight = value)

  all_estimates %>%
    filter(index_group != "REPWTP_ORIG") %>%
    left_join(original_weights) %>%
    group_by(quantile, type, state, two_digit_occ) %>%
    summarise(var = 4/160 * sum((value - orig_weight) ^ 2),
              mean = mean(value)) %>%
    return()
}


if (run_bootstrap) {
  set.seed(2020)
  bootstrap_output <- future_map(1:3, safely(get_sum_stats))


  bootstrap_output_df <- map(bootstrap_output, "result") %>%
    bind_rows()

  confidence_intervals <- bootstrap_output_df %>%
    filter(type == "main") %>%
    group_by(quantile) %>%
    summarise(e_v = mean(var),
              v_e = var(mean)) %>%
    transmute(quantile,
              `Std. Error` = sqrt(e_v + v_e)) %>%
    left_join(select(robustness_regular, -type)) %>%
    mutate(ci_lower  = value - 1.96 * `Std. Error`,
           ci_upper = value + 1.96 * `Std. Error`) %>%
    select(-value) %>%
    pivot_longer(c("Std. Error", "ci_lower", "ci_upper"), names_to = "type", values_to = "value")


  bootstrap_state_df <- bootstrap_output_df %>%
    filter(type == "state") %>%
    bind_rows() %>%
    group_by(state) %>%
    summarise(e_v = mean(var),
              v_e = var(mean)) %>%
    transmute(state,
              `Std. Error` = sqrt(e_v + v_e))

  write_csv(bootstrap_state_df, "analysis/release/state_SEs.csv")

} else {
  confidence_intervals <- tibble(type = "Std. Error",
                                 statistic = c(rep("Quantile", 3), "Share over 1"),
                                 quantile = c(0.25, 0.5, 0.75, NA),
                                 value = NA)

}




### Check the effect of removing tipped occupations

additional_codes <- read_csv("analysis/input/isaac_tipping.csv")$code

occ_2019 <- read_csv("analysis/input/occ_2019.csv") %>%
  mutate(tip_commission = !is.na(tip_commission) | str_pad(occupation, pad = "0",width =  4, side = "left") %in% additional_codes,
         occupation = as.character(occupation))

wages_with_tips <- wages_logit_weights  %>%
  mutate(occupation_full = as.character(occupation_full)) %>%
  left_join(occ_2019 %>% filter(tip_commission), by = c(occupation_full = "occupation")) %>%
  select(weight, tip_commission, contains("replacement_rate"))

stats_for_text <- add_row(stats_for_text,
                          stat_name = "Share in tipped wage",
                          stat_value = with(wages_with_tips,
                                            Hmisc::wtd.mean(!is.na(tip_commission), weight)))


robustness_tips <- wages_with_tips %>%
  filter(is.na(tip_commission)) %>%
  summarise(type = "Drop tipped occupations",
            statistic = c(rep("Quantile", 3), "Share over 1"),
            quantile = c(0.25, 0.5, 0.75, NA),
            value = sum_stat(c(0.25, 0.5, 0.75, NA),
                             replacement_rate_FPUC, weight))



### Robustness Non Labour income

non_wage_income <- wages_logit_weights %>%
  transmute(rep_rate = (benefits_amount + 600)/(weekly_earnings + if_else(business_income > 0,
                                                                          business_income/52,
                                                                          0)),
            weight) %>%
  summarise(type = "Include self-employment income",
            statistic = c(rep("Quantile", 3), "Share over 1"),
            quantile = c(0.25, 0.5, 0.75, NA),
            value = sum_stat(c(0.25, 0.5, 0.75, NA),
                             rep_rate, weight))

#Social Security FICA tax is capped at $137,700 in 2020, but this is so far
#up in the earnings distribution that the cap isn't relevant for the statistics
#that are computed here, which only go as high as the 75th percentile of earnings
payroll_tax <- wages_logit_weights %>%
  transmute(rep_rate = (benefits_amount + 600)/((1 - 0.0765) * weekly_earnings),
            weight) %>%
  summarise(type = "Account for payroll tax",
            statistic = c(rep("Quantile", 3), "Share over 1"),
            quantile = c(0.25, 0.5, 0.75, NA),
            value = sum_stat(c(0.25, 0.5, 0.75, NA),
                             rep_rate, weight))


include_PUA <- wages_logit_weights_with_PUA %>%
  ungroup() %>%
  transmute(rep_rate = (benefits_amount + 600)/(weekly_earnings),
            weight) %>%
  summarise(type = "Include PUA recipients",
            statistic = c(rep("Quantile", 3), "Share over 1"),
            quantile = c(0.25, 0.5, 0.75, NA),
            value = sum_stat(c(0.25, 0.5, 0.75, NA),
                             rep_rate, weight))


#### Check the effect of accounting for benefits = 3 * health insurance

wages_2017 <- wages_2017 %>%
  filter(replacement_rate_FPUC > 0) %>%
  mutate(inc_decile = cut(weekly_earnings, deciles),
         two_digit_ind = as.factor(two_digit_ind)) %>%
  broom::augment(logit_fit, newdata = .,
                 type.predict = "response") %>%
  mutate(weight = weight * .fitted)

robustness_benefits <- wages_2017 %>%
  transmute(rep_rate = (benefits_amount + 600)/(weekly_earnings + health_benefits_scaling * weekly_employer_contribution),
            weight) %>%
  summarise(type = "Include non-wage compensation",
            statistic = c(rep("Quantile", 3), "Share over 1"),
            quantile = c(0.25, 0.5, 0.75, NA),
            value = sum_stat(c(0.25, 0.5, 0.75, NA),
                             rep_rate, weight))


robustness_benefits_payroll <- wages_2017 %>%
  transmute(rep_rate = (benefits_amount + 600)/((1 - 0.0765) * weekly_earnings + health_benefits_scaling * weekly_employer_contribution),
            weight) %>%
  summarise(type = "Account for payroll tax and include non-wage compensation",
            statistic = c(rep("Quantile", 3), "Share over 1"),
            quantile = c(0.25, 0.5, 0.75, NA),
            value = sum_stat(c(0.25, 0.5, 0.75, NA),
                             rep_rate, weight))


bind_rows(robustness_regular, confidence_intervals, non_wage_income,
          robustness_tips, include_PUA,
          robustness_benefits, robustness_benefits_payroll) %>%
  pivot_wider(names_from = "type",
              values_from = "value") %>%
  write_csv("analysis/release/robustness_table.csv")




merge_ses <- function(gt, cols){

  all_cols <- names(gt[["_data"]])

  for (col in cols) {
    gt <- gt %>%
      cols_merge(
        columns = str_detect(all_cols, col),
        hide_columns = str_detect(all_cols, col) & str_detect(all_cols, "se"),
        pattern = "{1} {2} "
      )
  }
  return(gt)
}

relabel <-


ses <- read_csv("analysis/input/bootstrap.csv") %>%
  group_by(quantile, type, state) %>%
  summarise(e_v = mean(var),
            v_e = var(mean)) %>%
  mutate(se = sqrt(e_v + v_e)) %>%
  mutate(type = case_when(type == "main" ~ "Main estimate",
                          type == "benefits and tax" ~ "Account for payroll tax and include non-wage compensation",
                          type == "benefits only" ~ "Include non-wage compensation",
                          type == "business" ~ "Include self-employment income",
                          type == "no tipping" ~ "Drop tipped occupations",
                          type == "payroll" ~ "Account for payroll tax",
                          type == "PUA_recipiency" ~ "Include PUA recipients")) %>%
  filter(is.na(state)) %>%
  select(-state, -e_v, -v_e)


bind_rows(robustness_regular, non_wage_income,
          robustness_tips, payroll_tax, include_PUA,
          robustness_benefits, robustness_benefits_payroll) %>%
  left_join(ses) %>%
  mutate(quantile = if_else(is.na(quantile), "Share over 1", scales::ordinal(quantile*100)),
         type = factor(type, levels = c("Main estimate",
                                        "Include non-wage compensation",
                                        "Account for payroll tax",
                                        "Account for payroll tax and include non-wage compensation",
                                        "Include self-employment income",
                                        "Drop tipped occupations",
                                        "Include PUA recipients"
                                        ))) %>%
  arrange(type) %>%
  select(-statistic) %>%
  pivot_wider(names_from = "quantile",
              values_from = c("value", "se")) %>%
  rename_at(vars(contains("value")),
            ~ str_remove(., "value_")) %>%
  mutate(group = case_when(type %in% c("Main estimate") ~ " ",
                           type %in% c("Drop tipped occupations", "Include PUA recipients", "Include self-employment income") ~ "Statutory replacement rate",
                           TRUE ~ "Comprehensive replacement rate")) %>%
  mutate(type = if_else(type == "Main estimate", as.character(type),
                        str_c("SPACEPLACEHOLDER ", type))) %>%
  gt(groupname_col = "group") %>%
  cols_align(align = "left",
             columns = vars("type")) %>%
  fmt_percent(c("25th", "50th", "75th",
               "Share over 1"), decimals = 0) %>%
  fmt_number(contains("se"),
             scale_by = 100,
             decimals = 1, pattern = "({x})") %>%
  merge_ses(c("25th", "50th", "75th", "Share over 1")) %>%
  tab_spanner("Percentile",
              c("25th", "50th", "75th")) %>%
  cols_label(type = "") %>%
  gt::as_latex() %>%
  str_replace_all("\\} \\\\\\\\ \\n\\\\midrule\\n", "\\} \\\\\\\\") %>%
  str_replace_all("SPACEPLACEHOLDER", "\\\\hspace{0.5cm}") %>%
  str_replace("\\\\multicolumn\\{1\\}\\{l\\}\\{ \\} \\\\\\\\Main estimate", "Main estimate") %>%
  str_replace_all("longtable", "tabular") %>%
  write_lines(str_c("analysis/release/robustness.tex"))
