source("analysis/source/prelim.R")


morg <-  read_csv("analysis/input/basic_cps.csv.gz")

CPS_unemployment <- morg %>%
  inner_join(fips_codes) %>%
  filter(CITIZEN != 5) %>%
  transmute(year = YEAR,
            month = MONTH,
            id = CPSIDP,
            dur_unemp = DURUNEMP,
            age = AGE,
            bls_weight = COMPWT,
            male = SEX == 1,
            two_digit_ind = if_else(IND == 0,
                                    NA_real_,
                                    code_industries(IND)),
            two_digit_occ = if_else(OCC != 0,
                                    str_sub(OCC %>% str_pad(4, "left", 0), 1, 2),
                                    NA_character_),
            emp_stat = EMPSTAT,
            unemp_reason = WHYUNEMP,
            state) %>%
  filter(year == 2020)



CPS_wages <- morg %>%
  transmute(year = YEAR + 1,
            id = CPSIDP,
            weekly_earnings = EARNWEEK,
            usual_hours = UHRSWORKORG,
            weight = EARNWT) %>%
  filter(year %in% c(2020),
         weight != 0) %>%
  select(-year)

expect_equal(CPS_wages %>% distinct(id) %>% nrow(),
             CPS_wages %>% nrow())


joined_data <- CPS_unemployment %>%
  inner_join(CPS_wages,
             by = c("id")) %>%
  filter(weekly_earnings > 0,
         weekly_earnings < 9999, #NA code
         emp_stat %in% c(10, 12, 20, 21, 22))


all_deciles <- with(joined_data,
                              Hmisc::wtd.quantile(weekly_earnings,
                                                  weight,
                                                  seq(0, 1, length.out = 11)))

all_deciles[1] <- 0
all_deciles[11] <- Inf

write_rds(all_deciles, "analysis/release/all_deciles.rds")


for_plot <- joined_data %>%
  mutate(job_loser = emp_stat %in% c(21, 22) & unemp_reason %in% c(1,2) & dur_unemp <= 26)

quintiles <- with(for_plot, Hmisc::wtd.quantile(weekly_earnings,
                                                weight,
                                                seq(0, 1, length.out = 6)))

quintiles[1] <- -Inf
quintiles[6] <- Inf

unemp_by_income <- for_plot %>%
  mutate(income_cut = cut(weekly_earnings,
                          quintiles)) %>%
  group_by(income_cut) %>%
  summarise(share_unemp = Hmisc::wtd.mean(job_loser, bls_weight)) %>%
  filter(!is.na(income_cut)) %>%
  ggplot() +
  aes(income_cut, share_unemp) +
  geom_col(fill = RColorBrewer::brewer.pal(6, "Blues")[5]) +
  fte_theme(dark_text = TRUE) +
  scale_x_discrete(name =  "Pre-job loss weekly earnings (quintiles)",
                   labels = c( "Bottom quintile", "Second", "Third", "Fourth", "Fifth quintile")) +
  scale_y_continuous(name = "Unemployment rate",
                     labels = scales::percent_format(accuracy = 1)) +
  theme(panel.grid.major.x = element_blank())


ggsave("analysis/release/unemployment_rate_by_income.png",
       unemp_by_income,
       width = 6, height = 4)


write_rds(quintiles, "analysis/release/basic_quintiles.rds")

#Recalculate using 2020
#Fit model with 2020 data to predict unemployment:

deciles <- for_plot %>%
  filter(job_loser) %>%
  with(Hmisc::wtd.quantile(weekly_earnings,
                           weight,
                           seq(0, 1, length.out = 11)))
deciles[1] <- 0
deciles[11] <- Inf

age_bins <- c(-Inf, 25, 35, 45, 55, 65, Inf)

write_rds(deciles, "analysis/release/deciles.rds")


logit_fit <- for_plot %>%
  mutate(inc_decile = cut(weekly_earnings, deciles),
         two_digit_ind = as.factor(two_digit_ind),
         binned_age = cut(age, age_bins),
         job_loser = as.numeric(job_loser),
         weight = n() * weight/sum(weight)) %>% # rescale weights to avoid errors
  glm(job_loser ~  inc_decile + two_digit_occ + two_digit_ind + state,
      data = ., family = binomial(), weights = weight)

logit_fit %>%
  broom::augment(type.predict = "response") %>%
  ggplot() +
  geom_histogram(aes(.fitted))


logit_fit$data <- NULL
write_rds(logit_fit, "analysis/release/logit_fit.rds")

write_csv(for_plot, "analysis/release/logit_data.csv")


# calculate may unemployment rates by group -------------------------------

#reload in the data

cps <-  read_csv("analysis/input/basic_cps.csv.gz") %>%
  filter(MONTH == 5) %>%
  mutate(unemployed = ifelse(EMPSTAT %in% seq(20, 22), 1 ,0),
         in_lf = ifelse(LABFORCE == 2, 1, 0),
         weight = COMPWT) %>%
  filter(YEAR %in% c(2019, 2020))


prior_earnings <- read_csv("analysis/input/basic_cps.csv.gz") %>%
  filter(MONTH %in% c(5, 6, 7, 8),
         YEAR %in% c(2018, 2019),
         EARNWEEK > 0,
         EARNWEEK < 9999,
         EARNWT != 0) %>%
  transmute(YEAR = YEAR + 1,
            CPSIDP = CPSIDP,
            earnings_cut = cut(EARNWEEK, quintiles))

#check that the may rate is correct before sample restrictions

test_that("may rate is correct before sample restrictions",
          expect_equal(cps %>%
                         filter(YEAR == 2020, MONTH == 5) %>%
                         summarise( sum(weight*unemployed)/sum(weight*in_lf)) %>%
                         pull(),
                       0.13, tol = 0.001))


cps <- cps %>%
  filter(CITIZEN != 5,
         OCC != 0)


test_that("may rate is reduced after sample restrictions",
          expect_equal(cps %>%
                         filter(YEAR == 2020, MONTH == 5) %>%
                         summarise( sum(weight*unemployed)/sum(weight*in_lf)) %>%
                         pull(),
                       0.124, tol = 0.001))


occupation_counts <- cps %>%
  filter(MONTH == 5) %>% #only want may rates
  mutate(
    s_code = str_pad(
      as.character(OCC),
      width = 4,
      side = "left",
      pad = "0"
    ),
    code = substr(s_code, start = 1, stop = 2)
  ) %>%
  left_join(occupation_codes) %>%
  group_by(YEAR, MONTH, occupation) %>%
  summarize(unemp_ct = sum(weight*unemployed),
            in_lab_for = sum(weight*in_lf),
            n = n())


## change into rates and aggregate into 2019-2020 avgs, weighing each month's rate equally
occ_unemp_19_20 <- occupation_counts %>%
  group_by(YEAR, MONTH, occupation) %>%
  transmute(urate = unemp_ct/in_lab_for) %>%
  group_by(YEAR, occupation) %>%
  summarize(year_urate = mean(urate)) %>%
  pivot_wider(id_cols = occupation, names_from = YEAR, values_from = year_urate, names_prefix = "unem_rate_")

write.csv(occ_unemp_19_20, file = "analysis/release/occ_unemp_19_20.csv")


## Major industry codes- in case we need this somewhere.
m_inds <-  tribble(
  ~ind,
  ~icode,
  "Agriculture, forestry,fishing, and hunting",
  1,
  "Mining",
  2,
  "Construction",
  3,
  "Manufacturing",
  4,
  "Wholesale and retail trade",
  5,
  "Transportation and utilities",
  6,
  "Information",
  7,
  "Financial activities",
  8,
  "Professional and business services",
  9,
  "Educational and health services",
  10,
  "Leisure and hospitality",
  11,
  "Other services",
  12,
  "Public administration",
  13,
  "Armed Forces",
  14
)

## Industry unemployment
cps_ind_cts <- cps %>%
  filter(MONTH == 5) %>% #only want may rates
  filter(IND != 0) %>%
  mutate(icode = case_when(
    IND %in% seq(170,290) ~ 1,
    IND %in% seq(370,490) ~ 2,
    IND == 770 ~ 3,
    IND %in% seq(1070,3990) ~ 4,
    IND %in% seq(4070,5790) ~ 5,
    (IND %in% seq(6070,6390) | IND %in% seq(570,690)) ~ 6,
    IND %in% seq(6470,6780) ~ 7,
    IND %in% seq(6870,7190) ~ 8,
    IND %in% seq(7270,7790) ~ 9,
    IND %in% seq(7860,8470) ~ 10,
    IND %in% seq(8560,8690) ~ 11,
    IND %in% 8770:9290 ~ 12,
    IND %in% 9370:9590 ~ 13,
    IND == 9890 ~ 14
  )) %>%
  group_by(YEAR, MONTH, icode) %>%
  summarize(unemp_ct = sum(weight*unemployed), in_lab_for = sum(weight*in_lf))

## change into rates and aggregate into 2019-2020 avgs, weighing each month's rate equally
ind_unemp_19_20 <- cps_ind_cts %>%
  group_by(YEAR, MONTH, icode) %>%
  transmute(urate = unemp_ct/in_lab_for) %>%
  pivot_wider(id_cols = icode, names_from = YEAR, values_from = urate, names_prefix = "unem_rate_")

write.csv(ind_unemp_19_20, file = "analysis/release/ind_unemp_19_20.csv")




quintile_counts <- cps %>%
  filter(MONTH == 5) %>% #only want may rates
  inner_join(prior_earnings) %>%
  group_by(YEAR, MONTH, earnings_cut) %>%
  summarize(unemp_ct = sum(weight*unemployed),
            in_lab_for = sum(weight*in_lf),
            n = n())

inc_unemp_19_20 <- quintile_counts %>%
  group_by(YEAR, MONTH, earnings_cut) %>%
  transmute(urate = unemp_ct/in_lab_for) %>%
  pivot_wider(id_cols = earnings_cut, names_from = YEAR, values_from = urate, names_prefix = "unem_rate_")

write.csv(inc_unemp_19_20, file = "analysis/release/inc_unemp_19_20.csv")


