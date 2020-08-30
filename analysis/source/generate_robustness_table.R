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

logit_data <- read_csv("analysis/release/logit_data.csv")

#alternate_logits <- read_rds( "analysis/release/random_deciles.rds")


#Two components in the variance

#Var(Y) = E(Var(Y | X)) + Var(E(Y | X))

#So we average the replicate variance across the bootstrap samples
#And add it to the bootstrapped variance of the estimates





### Check the effect of removing tipped occupations


occ_2019 <- read_csv("analysis/input/occ_2019.csv") %>%
  mutate(occupation = as.character(occupation))

wages_with_tips <- wages_logit_weights  %>%
  mutate(occupation_full = as.character(occupation_full)) %>%
  left_join(occ_2019 %>% filter(tip_commission), by = c(occupation_full = "occupation")) %>%
  select(weight, tip_commission, contains("replacement_rate"))

tipped_wage <- tibble(stat_name = "Share in tipped wage",
                      stat_value = with(wages_with_tips,
                                            Hmisc::wtd.mean(!is.na(tip_commission), weight))
                      )


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


bind_rows(robustness_regular, non_wage_income,
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



ses <- read_csv("analysis/input/bootstrap_output.csv") %>%
  group_by(quantile, type, state, occupation) %>%
  summarise(e_v = mean(var),
            v_e = var(mean)) %>%
  ungroup() %>%
  mutate(se = sqrt(e_v + v_e))

median_variance <- ses %>% filter(type == "main") %>%
  mutate(share_var_logit = v_e/(e_v + v_e)) %>%
  filter(quantile == 0.5) %>%
  transmute(stat_name = "Share of variance of median coming from logit model",
            stat_value = share_var_logit)

ses_for_robustness <- ses %>%
  mutate(type = case_when(type == "main" ~ "Main estimate",
                          type == "benefits and tax" ~ "Account for payroll tax and include non-wage compensation",
                          type == "benefits only" ~ "Include non-wage compensation",
                          type == "business" ~ "Include self-employment income",
                          type == "no tipping" ~ "Drop tipped occupations",
                          type == "payroll" ~ "Account for payroll tax",
                          type == "PUA_recipiency" ~ "Include PUA recipients")) %>%
  filter(is.na(state), is.na(occupation)) %>%
  select(-state, -occupation, -e_v, -v_e)



bind_rows(robustness_regular, non_wage_income,
          robustness_tips, payroll_tax, include_PUA,
          robustness_benefits, robustness_benefits_payroll) %>%
  left_join(ses_for_robustness) %>%
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

occupational_ses <- ses %>%
  filter(!is.na(occupation)) %>%
  select(occupation, se) %>%
  write_csv("analysis/release/occupation_SEs.csv")

occupational_ses %>%
  pull(se) %>%
  max() %>%
  expect_lt(0.044) %>%
  test_that("Occupational standard errors are all less than 0.044")


