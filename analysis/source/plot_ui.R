source("analysis/source/prelim.R")


library(gt)

#the benefits calculator is written in python.
#we call the calculator in R using the reticulate package
#the function we use is calc_weekly_state_quarterly()
#which takes 4 quarters of total earnings as the first four arguments
#and state as a 5th argument and returns a weekly benefit amount.
library(reticulate)
#if (Sys.getenv()[["USER"]] == "peterganong") {
#  use_condaenv()
#} else {

if(Sys.getenv()[["USERNAME"]] == "rosha"){
use_condaenv("C:\\Users\\rosha\\anaconda3\\envs\\for_calc", required = TRUE)
} else {
  use_condaenv("C:\\Users\\probert2\\AppData\\local\\Continuum\\anaconda3\\envs\\for_calc",
               required = TRUE)
}
#}
setwd("../ui_calculator/")
source_python("source/ui_calculator.py")
setwd(make_path("./"))

stats_for_text <- tibble(stat_name = character(),
                         stat_value = numeric())

palette <- RColorBrewer::brewer.pal(6, "Blues")



cpi_u <- read_csv("analysis/input/CPIAUCSL.csv") %>%
  filter(month(DATE) == 4,
         year(DATE) %in% c(2016, 2018, 2020)) %>%
  transmute(year = year(DATE) + 1, #asec measures income year prior
            adjustment = last(CPIAUCSL)/CPIAUCSL)



#### Read in data ####


state_pop <- readxl::read_xlsx("analysis/input/population.xlsx", skip = 3) %>%
  select(state = ...1, pop = `2018`) %>%
  filter(str_detect(state, "\\.")) %>%
  mutate(state = str_remove(state, "\\.")) %>%
  left_join(tibble(state = state.name,
                   abb = state.abb)) %>%
  select(state = abb,
         pop = pop)

worker_citizen_instate <-
  read_csv("analysis/input/ASEC_1719.csv.gz") %>%
  filter(INCWAGE < 99999998,
         INCWAGE > 0,
         CITIZEN != 5) %>%
  inner_join(fips_codes)

age_bins <- c(-Inf, 25, 35, 45, 55, 65, Inf)


wages <- worker_citizen_instate %>%
  transmute(state,
            year = YEAR,
            serial = SERIAL,
            pernum = PERNUM,
            fips = STATEFIP,
            weight = ASECWT,
            hh_id = SERIAL,
            person_id = PERNUM,
            hh_weight = ASECWTH,
            wage = INCWAGE,
            business_income = INCBUS,
            employment_status = EMPSTAT,
            unemployment_duration = DURUNEMP,
            weeks_worked = WKSWORK1,
            binned_age = cut(AGE, age_bins),
            male = SEX == 1,
            usual_hours = UHRSWORKLY,
            two_digit_ind = if_else(IND == 0,
                                    NA_real_,
                                    code_industries(IND)),
            two_digit_occ = if_else(OCC != 0,
                                    str_sub(OCC %>% str_pad(4, "left", 0), 1, 2),
                                    NA_character_),
            occupation_full = OCC,
            unemp_reason = WHYUNEMP,
            cutoff = CUTOFF,
            weekly_employer_contribution = EMCONTRB/weeks_worked,
            employer_policy_holder = as.numeric(GRPOWNLY == 2),
            employer_policy_type = PAIDGH,
            family_size = FAMSIZE,
            other_inc = HHINCOME - INCWAGE) %>%
  filter(wage >= (7.25 * usual_hours * weeks_worked))


wages_unadjusted <- wages %>%
  mutate(weekly_earnings = wage/weeks_worked,
         q1_earnings = weeks_worked - 39,
         q2_earnings = weeks_worked - 26,
         q3_earnings = weeks_worked - 13,
         q4_earnings = weeks_worked) %>%
  mutate_at(vars(matches("q[1-4]_earnings" )), ~ case_when(.x > 13 ~ 13*weekly_earnings,
                                                           .x < 0 ~ 0,
                                                           TRUE ~ .x*weekly_earnings))

wages <-   wages %>%
  left_join(cpi_u) %>%
  mutate(across(c("wage", "cutoff", "weekly_employer_contribution", "other_inc"),
                ~ . * adjustment),
         adjustment = NULL) %>%
  mutate(weekly_earnings = wage/weeks_worked,
         q1_earnings = weeks_worked - 39,
         q2_earnings = weeks_worked - 26,
         q3_earnings = weeks_worked - 13,
         q4_earnings = weeks_worked) %>%
  mutate_at(vars(matches("q[1-4]_earnings" )), ~ case_when(.x > 13 ~ 13*weekly_earnings,
                                                           .x < 0 ~ 0,
                                                           TRUE ~ .x*weekly_earnings))




rm(worker_citizen_instate)


#### Add weekly benefits to dataframe ####
#NB: this code is slow and should be expected to take 1-2 mins
wages_with_benefits <- map(list(wages, wages_unadjusted),
                           ~ .x %>%
                             mutate(benefits_amount = calc_weekly_state_quarterly(q1_earnings,
                                                                                  q2_earnings,
                                                                                  q3_earnings,
                                                                                  q4_earnings,
                                                                                  state,
                                                                                  weeks_worked) %>% map_dbl(1),
                                    replacement_rate = benefits_amount/weekly_earnings,
                                    replacement_rate_FPUC = if_else(benefits_amount > 0,
                                                                    (benefits_amount + 600)/weekly_earnings,
                                                                    0)) %>%
                             filter(employment_status %in% c(10, 12, 20, 21, 22),
                                    two_digit_ind != 14))


wages_2017 <- wages_with_benefits %>%
  pluck(1) %>%
  filter(year == 2017)

wages <- wages_with_benefits %>%
  pluck(1) %>%
  filter(year == 2019)

wages_unadjusted <- wages_with_benefits %>%
  pluck(2) %>%
  filter(year == 2019)

#These include filter statements to avoid issues with back compatability
write_csv(wages, "analysis/release/wages_with_replacement_rates.csv")
write_csv(wages_2017, "analysis/release/wages_with_replacement_rates_2017.csv")
#can read in the CSV on disk if trying to check something quickly
wages <- read_csv("analysis/release/wages_with_replacement_rates.csv")
wages_2017 <- read_csv( "analysis/release/wages_with_replacement_rates_2017.csv")

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


write_csv(wages_logit_weights, "analysis/release/wages_logit_weights_filtered.csv")

##### Calculate some aggregate statistics ####


aggregate_stats <- wages_logit_weights %>%
  summarise(`Median replacment rate (no FPUC)` = Hmisc::wtd.quantile(replacement_rate, weight, 0.5),
            `RR2 (no FPUC)` = Hmisc::wtd.mean(benefits_amount, weight)/Hmisc::wtd.mean(weekly_earnings, weight)) %>%
  pivot_longer(cols = everything(),
               names_to = "stat_name",
               values_to = "stat_value")


share_high_rr <- wages_logit_weights %>%
  summarise(sum(weight*(replacement_rate_FPUC > 1))/sum(weight)) %>%
  pull()

share_very_high_rr <- wages_logit_weights %>%
  summarise(sum(weight*(replacement_rate_FPUC > 2))/sum(weight)) %>%
  pull()


medians_and_shares_over <- wages_logit_weights  %>%
  select(weight, contains("replacement_rate")) %>%
  pivot_longer(contains("rep"),
               names_to = "type",
               values_to = "replacement_rate") %>%
  group_by(type) %>%
  summarise(share_over_one = sum(weight*(replacement_rate > 1))/sum(weight),
            share_over_two = sum(weight*(replacement_rate > 2))/sum(weight),
            median = Hmisc::wtd.quantile(replacement_rate, weights = weight, probs = 0.5))


wages_unadjusted %>%
  filter(wage >= 25000,
         wage < 35000) %>%
  summarise(Hmisc::wtd.mean(wage == 30000, weight))

### Get summary stats from main spec for robustness table


#https://www.bls.gov/news.release/pdf/ecec.pdf, Table A, private industry workers, 50th wage percentile
#Rescaling of health benefits to capture total benefits
health_benefits_scaling <- (7.95-2.16)/2.6




#####
#### Comparison by industry table
m_inds <-  tribble(
  ~ind, ~icode,
  "Agriculture & forestry",  1,
  "Mining",  2,
  "Construction", 3,
  "Manufacturing", 4,
  "Wholesale & retail trade", 5,
  "Transportation & utilities",  6,
  "Information",   7,
  "Financial activities",  8,
  "Professional services",  9,
  "Educational & health services", 10,
  "Leisure & hospitality", 11,
  "Other services",   12,
  "Public administration",  13) %>%
  mutate(icode = as.factor(icode))


wages_2017 %>%
  mutate(inc_decile = cut(weekly_earnings, deciles),
         two_digit_ind = as.factor(two_digit_ind)) %>%
  left_join(m_inds, by = c(two_digit_ind = "icode")) %>%
  group_by(ind) %>%
  summarise(ave_wage = Hmisc::wtd.mean(weekly_earnings, weight),
            ave_nw_comp = (9.25/3.05) * Hmisc::wtd.mean(weekly_employer_contribution , weight),
            ave_UI = Hmisc::wtd.mean(if_else(benefits_amount > 0,
                                             benefits_amount + 600,
                                             0, weight), weight),
            median_RR_inc_benefits = Hmisc::wtd.quantile((benefits_amount + 600)/
                                                           (weekly_earnings + (9.25/3.05) * weekly_employer_contribution),
                                                         weight, 0.5),
            total_workers = sum(weight),
            n = n()) %>%
  mutate(total_comp = ave_wage + ave_nw_comp,
         aggregate_RR = ave_UI/total_comp,
         share_table = total_workers/sum(total_workers)) %>%
  write_csv("analysis/release/industry_comparison.csv")


#what share of workers work in industries where the median replacement rate is greater than 100%
#What share of workers work in industries where the

wages_2017 %>%
  mutate(inc_decile = cut(weekly_earnings, deciles),
         two_digit_ind = as.factor(two_digit_ind)) %>%
  broom::augment(logit_fit, newdata = .,
                 type.predict = "response") %>%
  filter(weekly_earnings < Hmisc::wtd.quantile(weekly_earnings, weight, 0.99)) %>%
  left_join(m_inds, by = c(two_digit_ind = "icode")) %>%
  group_by(ind) %>%
  summarise(rr_median = Hmisc::wtd.quantile((benefits_amount > 0 ) * (benefits_amount + 600) /
                                              (weekly_earnings + health_benefits_scaling * weekly_employer_contribution),
                                            weight, 0.5),
            ave_benefits =  Hmisc::wtd.mean((benefits_amount > 0 ) * (benefits_amount), weight),
            rr_means = Hmisc::wtd.mean((benefits_amount > 0 ) * (benefits_amount + 600), weight) /
              Hmisc::wtd.mean(weekly_earnings + health_benefits_scaling * weekly_employer_contribution, weight),
            total_workers = sum(weight * .fitted))  %>%
  pivot_longer(contains("rr"),
               names_to =  "type",
               values_to = "rr") %>%
  group_by(type, industry_greater_than_1 = rr > 1) %>%
  summarise(sum =  sum(total_workers))  %>%
  mutate(share = sum/sum(sum)) %>%
  write_csv("analysis/release/shares_industries.csv")


source("analysis/source/generate_robustness_table.R")


####  Histogram of incomes ####

quantiles <- map_dfr(c(0.1, 0.25, 0.5, 0.75, 0.9),
                     ~ wages_logit_weights %>%
                       summarise(q = Hmisc::wtd.quantile(weekly_earnings,
                                                         weights = weight,
                                                         probs = .x)) %>%
                       mutate(p = .x,
                              ymin = 0,
                              ymax = 1800))

plot_range <- quantiles %>%
  filter(p %in% c(0.1, 0.9)) %>%
  pull(q)

quantiles <- quantiles %>%
  filter(!p %in% c(0.1, 0.9))

earnings_summary_stats <- wages_logit_weights %>%
  summarise(
    "pandemic median earnings" = Hmisc::wtd.quantile(weekly_earnings,
                                            weights = weight,
                                            probs = 0.5),
    "pandemic mean earnings" = Hmisc::wtd.mean(weekly_earnings,
                                   weights = weight)) %>%
  pivot_longer(everything(),
               names_to = "stat_name",
               values_to = "stat_value") %>%
  rbind(wages %>%
  filter(employment_status == 21, unemployment_duration <= 26, unemp_reason %in% c(1, 2)) %>%
  summarise(stat_value = Hmisc::wtd.mean(weekly_earnings, weight)) %>%
  mutate(stat_name = "pre-pandemic mean earnings")
)

histogram <- wages_logit_weights %>%
  mutate(weights = weight/sum(weight)) %>% #reweight before truncation
  filter(weekly_earnings > 0, #plot axis includes 0 so we should go down to 0
         weekly_earnings < plot_range[2] + 200) %>%
  ggplot()  +
  geom_histogram(aes(weekly_earnings, weight = weights),
                 bins = 13,
                 colour = "white",
                 fill = palette[5]) +
  geom_vline(data = earnings_summary_stats,
             aes(xintercept = stat_value),
             colour = brewer.pal("Greys", n = 9)[8]) +
  geom_text(data = earnings_summary_stats,
            aes(x = stat_value + 25,
                label = str_remove(stat_name, " earnings"),
                y = c(0.18, 0.1, 0.1)),
            hjust = 0,
            family = "serif",
            colour = brewer.pal("Greys", n = 9)[8],
            angle = 90) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.3)) +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "Pre-job loss weekly earnings",
       y = "Percent in bin") +
  fte_theme(dark_text = TRUE, legend_title_on = TRUE) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(colour = "black"),
        axis.ticks.length.x = unit(0.1, "inches"))


ggsave("analysis/release/income_distribution.png",
       histogram,
       width = 8, height = 4.5)


#### Plot of Nevada benefits #####

calculate_by_state <- function(wages, state){
  wages %>%
    mutate(state = state,
           weeks_worked = 52,
           benefits_amount = calc_weekly_state_quarterly(median_earnings*13,
                                                         median_earnings*13,
                                                         median_earnings*13,
                                                         median_earnings*13,
                                                         state,
                                                         weeks_worked) %>% map_dbl(1),
           replacement_rate_Jan = benefits_amount/median_earnings,
           replacement_rate_FPUC = if_else(benefits_amount > 0,
                                           (benefits_amount + 600)/median_earnings,
                                           0))
}

plot_nevada_benefits <- tibble(weekly_earnings = seq(200, plot_range[2] + 200),
                               state = "NV", weeks_worked = 52) %>%
  mutate(benefits_original = calc_weekly_state_quarterly(weekly_earnings * 13,
                                                         weekly_earnings * 13,
                                                         weekly_earnings * 13,
                                                         weekly_earnings * 13,
                                                         state,
                                                         weeks_worked) %>% map_dbl(1),
         benefits_FPUC = if_else(benefits_original > 0, benefits_original + 600, 0)) %>%
  pivot_longer(cols = contains("benefits"),
               names_prefix = "benefits_",
               names_to = "type",
               values_to = "benefits")


plot_labels <- tribble(
  ~x, ~y, ~type, ~label, ~angle,
  1500, 520, "original", "Nevada", 0,
  1270, 1110, "FPUC", "Nevada under CARES", 0,
  1250, 1350, "above", "above 100% replacement", 45,
  1300, 1250, "below", "below 100% replacement", 45
  # 1450, 1550, "unity", "benefits = earnings", 26.9
)

min_plot_ben <- min(plot_nevada_benefits$benefits)
above_below_rep_shading <- tibble(weekly_earnings = seq(200, plot_range[2] + 200)) %>%
  mutate("below 100% replacement" = map(weekly_earnings, ~ tibble(min = 200 + 0.01, max = .x)),
         "above 100% replacement" = map(weekly_earnings, ~ tibble(min = .x, max = plot_range[2] + 200))
  ) %>%
  pivot_longer(cols = !c("weekly_earnings"), names_to = "above_below", values_to = "nested") %>%
  unnest(cols = "nested")

plot_nevada_benefits  %>%
  filter(type != "proportional") %>%
  ggplot() +
  geom_ribbon(data = above_below_rep_shading,
              aes(x = weekly_earnings, ymin = min, ymax = max, fill = above_below)) +
  geom_line(aes(weekly_earnings, benefits, colour = type), show.legend = FALSE) +
  geom_text(data = plot_labels %>%
              filter(type != "proportional"),
            aes(x, y, colour = type, label = label, angle = angle),
            hjust = 0,
            size = 3,
            show.legend = FALSE) +
  fte_theme(dark_text = TRUE, legend_title_on = TRUE) +
  scale_x_continuous(labels = scales::dollar, expand = c(0.05, 0.05, 0, 0),
                     limits = c(200, plot_range[2] + 200)) +
  scale_y_continuous(labels = scales::dollar, expand = c(0.05, 0.05, 0, 0),
                     limits = c(200, plot_range[2] + 200)) +
  scale_colour_manual(values = c(original = palette[5],
                                 FPUC = colortools::adjacent(palette[5])[3],
                                 below = "black",
                                 above = "black")) +
  scale_fill_manual(name = "",
                    values = c(brewer.pal(9, "Greys")[3],
                               brewer.pal(9, "Greys")[2])
  ) +
  labs(x = "Weekly earnings",
       y = "Weekly unemployment benefits") +
  theme() +
  coord_fixed()

ggsave("analysis/release/nevada_benefits.png",
       width = 5, height = 5)

benefits_by_state <- map_dfr(fips_codes$state,
                             ~ calculate_by_state(tibble(median_earnings = seq(350, 1600, by = 150)) , .x)) %>%
  left_join(state_pop) %>%
  group_by(median_earnings) %>%
  summarise(benefits = Hmisc::wtd.mean(benefits_amount , pop),
            benefits_FPUC = Hmisc::wtd.mean(benefits_amount + (benefits_amount > 0) * 600, pop))



thresh = c(0.4, 0.8, 1, 1.4, Inf)


area_shading <- tibble(median_earnings = seq(0, 1700, by = 1),
                       thresh = list(thresh)) %>%
  unnest() %>%
  mutate(y_max = thresh * median_earnings,
         y_max = if_else(y_max > 1400 | is.na(y_max),
                         1400,
                         y_max)) %>%
  group_by(median_earnings) %>%
  mutate(height = if_else(row_number() != 1,
                          y_max - lag(y_max),
                          y_max),
         thresh = factor(thresh) %>%
           fct_inorder() %>%
           fct_rev())


labels <- tibble(label = cut(thresh * 0.99, c(-Inf, thresh)) %>% label_cuts(type = "percent")(),
                 x = c(1450, 1450, 1450, 1100, 700),
                 y = c(300, 800, 1320, 1320, 1320))

schedule_plot_points <- benefits_by_state %>%
  pivot_longer(cols = contains("ben"),
               names_to = "type",
               values_to = "benefits")
schedule_plot <- ggplot() +
  geom_area(aes(median_earnings, height, fill = thresh),
            data = area_shading,
            show.legend = FALSE) +
  geom_point(aes(median_earnings, benefits, shape = type),
             data = schedule_plot_points,
             size = 2, fill = "white", colour = "black", stroke = 1) +
  geom_label(aes(x = x, y = y, label = label),
                 family = "serif",
                 colour = brewer.pal("Greys", n = 9)[6],
                 label.r = unit(0, "lines"),
                 data = labels) +
  fte_theme(dark_text = TRUE, legend_title_on = TRUE) +
  scale_x_continuous(breaks = c(400, 800, 1200, 1600),
                     name = "Weekly earnings",
                     labels = scales::dollar_format(), expand = c(0,0)) +
  scale_y_continuous(name = "Average weekly benefits",
                     labels = scales::dollar_format(),
                     expand = c(0, 0)) +
  scale_fill_manual(values = c(`0.4` = brewer.pal(9, "Blues")[2],
                               `0.8` = brewer.pal(9, "Blues")[3],
                               `1` = brewer.pal(9, "Blues")[4],
                               `1.4` = brewer.pal(9, "Purples")[5],
                               `Inf` = brewer.pal(9, "Purples")[6])) +
  scale_shape_discrete(name = "",
                       labels = c("without $600 supplement", "with $600 supplement")) +
  theme(panel.grid.major =  element_blank(),
        axis.line = element_line(),
        panel.border = element_blank(),
        axis.ticks = element_line(color = brewer.pal("Greys", n = 9)[6]),
        legend.position = "right")



ggsave("analysis/release/national_benefits.png",
       schedule_plot,
       width = 8, height = 4.5)


#### Weekly earnings quantile plot ####

elig_deciles <- wages_logit_weights %>%
  with(Hmisc::wtd.quantile(weekly_earnings,
                           weight,
                           seq(0, 1, length.out = 11)))
elig_deciles[1] <- -Inf
elig_deciles[11] <- Inf


replacement_rate_by_decile <- wages_logit_weights  %>%
  mutate(eligible_decile = cut(weekly_earnings, elig_deciles)) %>%
  group_by(eligible_decile) %>%
  summarise_at(vars(contains("replacement_rate")),
               ~ Hmisc::wtd.quantile(.x, weights = weight, probs = 0.5)) %>%
  mutate(replacement_rate_FPUC = replacement_rate_FPUC - replacement_rate,
         replacement_rate_Jan = replacement_rate) %>%
  select(-replacement_rate) %>%
  pivot_longer(cols = contains("replacement"), names_to = "type",
               values_to = "replacement_rate", names_prefix = "replacement_rate_")

replacement_rate_by_decile %>%
  write_csv(str_c(out_table_path, "rep_rate_prior_earnings.csv"))

decile_plot_FPUC <- replacement_rate_by_decile %>%
  filter(type %in% c("FPUC", "Jan")) %>%
  ggplot() +
  aes(eligible_decile, replacement_rate, fill = type) +
  geom_col(position = position_stack()) +
  geom_hline(yintercept = 1) +
  scale_x_discrete(labels = label_cuts("dollar")) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Median replacement rate",
       x = "Pre-job loss weekly earnings (deciles)") +
  fte_theme(dark_text = TRUE, legend_title_on = FALSE) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = c(0.7, 0.82),
        axis.text.x = element_text(angle = 30, hjust = .8)) +
  scale_fill_manual(name = NULL,
                    values = c(Jan = palette[5],
                               FPUC = colortools::adjacent(palette[5])[3]),
                    labels = c(Jan = "Without $600 supplement",
                               FPUC = "With $600 supplement"))


ggsave("analysis/release/quantile_plot_FPUC.png",
       decile_plot_FPUC,
       width = 8, height = 4.5)


#### Now  plot by occupation ####
median_occupation_with_benefits <- wages_logit_weights %>%
  mutate(two_digit_occ = as.character(two_digit_occ)) %>%
  inner_join(occupation_codes, by = c(two_digit_occ = "code")) %>%
  group_by(occupation) %>%
  summarise_at(vars(contains("replacement_rate")),
               ~ Hmisc::wtd.quantile(.x, weights = weight, probs = 0.5)) %>%
  rename(replacement_rate_Jan = replacement_rate)


median_occupation_with_benefits %>%
  write_csv(str_c(out_table_path, "rep_rate_occ.csv"))

median_occupation_with_benefits_for_graph <-
  median_occupation_with_benefits %>%
  mutate(replacement_rate_FPUC = replacement_rate_FPUC - replacement_rate_Jan,
         occupation = factor(occupation) %>% fct_reorder(replacement_rate_FPUC, .desc = TRUE)) %>%
  pivot_longer(cols = contains("replacement_rate"),
               names_to = "benefits_type",
               values_to = "rep_rate",
               names_prefix = "replacement_rate_")

median_occupations <- median_occupation_with_benefits_for_graph %>%
  ggplot() +
  aes(occupation, rep_rate, fill = benefits_type) +
  geom_col() +
  geom_hline(yintercept = 1) +
  scale_fill_manual(values = c(Jan = palette[5],
                               FPUC = colortools::adjacent(palette[5])[3]),
                    labels = c(Jan = "Without $600 supplement",
                               FPUC = "With $600 supplement")) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Median replacement rate",
       x = NULL) +
  fte_theme(dark_text = TRUE) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = c(0.8, 0.82))

ggsave("analysis/release/occupation_at_median.png",
       median_occupations,
       width = 8, height = 4.5)

median_industries_with_benefits_for_graph <-
  wages_logit_weights %>%
  inner_join(m_inds, by = c(two_digit_ind = "icode")) %>%
  filter(year == 2019) %>%
  group_by(ind) %>%
  summarise_at(vars(contains("replacement_rate")),
               ~ Hmisc::wtd.quantile(.x, weights = weight, probs = 0.5)) %>%
  rename(replacement_rate_Jan = replacement_rate) %>%
  mutate(replacement_rate_FPUC = replacement_rate_FPUC - replacement_rate_Jan,
         ind = factor(ind) %>% fct_reorder(replacement_rate_FPUC, .desc = TRUE)) %>%
  pivot_longer(cols = contains("replacement_rate"),
               names_to = "benefits_type",
               values_to = "rep_rate",
               names_prefix = "replacement_rate_")


median_industries <- median_industries_with_benefits_for_graph %>%
  ggplot() +
  aes(ind, rep_rate, fill = benefits_type) +
  geom_col() +
  geom_hline(yintercept = 1) +
  scale_fill_manual(values = c(Jan = palette[5],
                               FPUC = colortools::adjacent(palette[5])[3]),
                    labels = c(Jan = "Without $600 supplement",
                               FPUC = "With $600 supplement")) +
  scale_y_continuous(labels = scales::percent) +
  labs(y = "Median replacement rate",
       x = NULL) +
  fte_theme(dark_text = TRUE) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1, size = rel(0.8)),
        legend.position = c(0.8, 0.82))

ggsave("analysis/release/industry_at_median.png",
       median_industries,
       width = 8, height = 4.5)

#### Area plots #####
calc_replacement_shares <- function(wages, supplement_type, supplement_size){
  wages %>%
    transmute(weight,
              supplement_type = supplement_type,
              supplement = if_else(supplement_type == "proportional",
                                   if_else(benefits_amount + weekly_earnings*supplement_size > 1200,
                                           if_else(1200 - benefits_amount > 0,
                                                   1200 - benefits_amount,
                                                   0),
                                           weekly_earnings*supplement_size),
                                   supplement_size),
              new_benefits = benefits_amount + supplement,
              replacement_rate = new_benefits/weekly_earnings,
              supplement_size = supplement_size)
}



bin_replacement_rates <- function(replacement_rate_data, bins){
  replacement_rate_data %>%
    mutate(bin = cut(replacement_rate, bins)) %>%
    group_by(supplement_size,
             supplement_type,
             bin, expansion = replacement_rate > 1) %>%
    summarise(count = sum(weight)) %>%
    group_by(supplement_size,
             supplement_type) %>%
    mutate(share = count/sum(count)) %>%
    select(share,
           expansion,
           supplement_type,
           supplement_size,
           bin) %>%
    ungroup()
}






label_bins <- function(cut_label){
  output <- cut_label %>%
    str_remove_all("\\(|\\]|\\[") %>%
    str_split(",") %>%
    map(~ as.numeric(.x) %>%
          scales::percent(accuracy = 1) %>%
          str_c(collapse = "-")) %>%
    map_chr(1) %>%
    str_replace("-Inf", "+")



  output[output == "NA%"] <- NA

  return(output)
}


area_plot_data <- map2(c("fixed", "proportional"),
                       list(seq(0, 800, 25), seq(0, 1, 0.02)),
                       function(supplement_type, supplement_sizes) map_dfr(supplement_sizes,
                                                                           ~calc_replacement_shares(wages_logit_weights, supplement_type, .x)) %>%
                         bin_replacement_rates(c(0, seq(0.4, 1.4, 0.2), Inf)) %>%
                         mutate(bin = as.factor(bin) %>% fct_rev()) %>%
                         complete(supplement_size, bin, fill = list(supplement_type = supplement_type,
                                                                    share = 0)))

base_plots <- map(area_plot_data,
                  ~ ggplot(.x) +
                    aes(supplement_size, share) +
                    geom_area(aes(fill = bin)) +
                    geom_line(data = .x %>%
                                filter(!expansion) %>%
                                group_by(supplement_size) %>%
                                summarise(share = sum(share))) +
                    scale_y_continuous(name = "Share of unemployed",
                                       breaks = seq(0, 1, 0.1),
                                       labels = scales::percent_format(accuracy = 1)) +
                    scale_fill_manual(name = "Replacement rate",
                                      values = c(brewer.pal(9, "Purples")[9:7],
                                                 brewer.pal(9, "Blues")[6:1]),
                                      labels = label_bins) +
                    fte_theme(dark_text = TRUE, legend_title_on = TRUE) +
                    theme(panel.ontop = TRUE,
                          panel.grid.major  = element_line(colour = "white"),
                          panel.background = element_rect(fill = NA),
                          legend.position = "right") )


lump_sum_area <- base_plots[[1]] + scale_x_continuous(name = "Size of fixed supplement",
                                                      labels = function(x) x %>% scales::dollar() %>%
                                                        str_replace("$600", "sd)"),
                                                      breaks = seq(0, 800, 100))


ggsave("analysis/release/lump_sum_area.png",
       lump_sum_area,
       width = 8, height = 4.5)




fixed_stats <- calc_replacement_shares(wages_logit_weights, "fixed", 300) %>%
  group_by(supplement_size) %>%
  summarise(share_over_100 = sum(weight*(replacement_rate > 1))/sum(weight),
            share_under_60 =sum(weight*(replacement_rate < 0.6))/sum(weight))


####Fixed supplement plots#####
fixed_supp_hist <- wages_logit_weights %>%
  mutate(total_weight = sum(weight)) %>%
  crossing(tibble(supp_size = seq(0, 600, 100))) %>%
  mutate(supp_rep_rate = (benefits_amount + supp_size) / weekly_earnings,
         supp_rep_bin = cut(supp_rep_rate, c(0, 0.4, 0.6, 0.8, 1, 1.2, 1.4, Inf)),
         supp_name = str_c("$", supp_size, " supplement")
  ) %>%
  group_by(supp_name, supp_rep_bin) %>%
  summarise(share = (sum(weight) / total_weight) %>% round(2)) %>%
  distinct() %>%
  mutate(label = scales::percent(share),
         label = if_else(label %in% c("0%", "100%"), "", label %>% as.character())
         ) %>%
  ggplot(aes(x = supp_rep_bin, y = share)) +
  geom_col(fill = brewer.pal(9, "Blues")[6]) +
  facet_wrap(~supp_name) +
  fte_theme(dark_text = TRUE) +

  geom_text(aes(x = supp_rep_bin, y = share, label = label),
            vjust = -0.2, size = 4.3, family = "serif",
            color = brewer.pal("Greys", n = 9)[7]) +
  scale_x_discrete(name = "Replacement rate",
                   labels = c("< 40%", "40-60%", "60-80%",
                              "80% - 100%", "100% - 120%", "100 - 140%", "140%+")) +
  scale_y_continuous(name = "Share of benefit eligibles in group",
                     labels = scales::percent_format(),
                     limits = c(0, 0.9)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major.x = element_blank())

ggsave("analysis/release/fixed_supp_hist.png", fixed_supp_hist, width = 8, height = 4.5)


#### Benefits Map ####
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)

state_values <- wages_logit_weights %>%
  bind_rows(wages_logit_weights %>%
              mutate(state = "US")) %>%
  group_by(state) %>%
  summarise(across(contains("replacement_rate"),
                   ~ Hmisc::wtd.quantile(.x, weights = weight, probs = 0.5)),
            benefits_level = Hmisc::wtd.mean(benefits_amount, weight)) %>%
  rename(replacement_rate_Jan = replacement_rate) %>%
  select(state,
         benefits_level,
         rr_fpuc = replacement_rate_FPUC,
         rr_no_fpuc = replacement_rate_Jan)


standard_errors <- read_csv("analysis/input/bootstrap_output.csv") %>%
  filter(!is.na(state)) %>%
  select(-quantile) %>%
  group_by(type, state) %>%
  summarise(e_v = mean(var),
            v_e = var(mean)) %>%
  mutate(se = sqrt(e_v + v_e)) %>%
  ungroup() %>%
  transmute(state,
            type = if_else(type == "state_FPUC",
                           "se_rr_fpuc",
                           "se_rr_no_fpuc"),
            se) %>%
  pivot_wider(names_from = "type",
              values_from = "se")
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

state_values %>%
  filter(state != "US") %>%
  select(-benefits_level) %>%
  left_join(standard_errors) %>%
  mutate(index = rep(1:25, 2),
         col_number = rep(1:2, each = 25)) %>%
  left_join(tibble(state = state.abb, name = state.name)) %>%
  mutate(state = name,
         name = NULL) %>%
  pivot_wider(names_from = "col_number",
              values_from = c("state", "rr_fpuc", "se_rr_fpuc",
                              "rr_no_fpuc", "se_rr_no_fpuc")) %>%
  select(c("state_1",
           "rr_fpuc_1",
           "se_rr_fpuc_1",
           "rr_no_fpuc_1",
           "se_rr_no_fpuc_1",
           "state_2",
           "rr_fpuc_2",
           "se_rr_fpuc_2",
           "rr_no_fpuc_2",
           "se_rr_no_fpuc_2")) %>%
  gt() %>%
  cols_label(`state_1` = "State",
             state_2 = "State",
             rr_fpuc_1  = "with FPUC",
             rr_fpuc_2  = "with FPUC",
             rr_no_fpuc_1 = "Without FPUC",
             rr_no_fpuc_2 = "Without FPUC") %>%
  cols_align("center", contains("rr")) %>%
  fmt_percent(columns = contains("rr"),
              decimals = 0) %>%
  fmt_number(columns = contains("se"), scale_by = 100,
             decimals = 1, pattern = "({x})") %>%
  fmt_missing(columns = everything(),
              missing_text  = "")  %>%
  merge_ses(c("rr_fpuc_1", "rr_fpuc_2",
              "rr_no_fpuc_1", "rr_no_fpuc_2")) %>%
  tab_spanner("Replacement rate (SE)",
              contains("c_1")) %>%
  tab_spanner("Replacement rate (SE)",
              contains("c_2")) %>%
  gt::as_latex() %>%
  str_replace_all("longtable", "tabular") %>%
  str_replace_all("\\} &", "\\} & &") %>%
  str_remove(".*\\n") %>% #remove extraneous first line
  write_lines(str_c("analysis/release/state_table.tex"))

state_values %>%
  write_csv(str_c(out_table_path, "rep_rate_state.csv"))


spdf <- geojsonio::geojson_read("analysis/input/us_states_hexgrid.geojson",  what = "sp")

spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- broom::tidy(spdf, region = "google_name") %>%
  left_join(tibble(state = state.abb,
                   id = state.name)) %>%
  left_join(state_values) %>%
  filter(state != "DC") %>%
  mutate(rr_fpuc_binned = label_bins(cut_number(rr_fpuc, 4) %>%
                                       factor() %>%
                                       fct_reorder(rr_fpuc, .desc = FALSE)))


# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE), id=spdf@data$iso3166_2)) %>%
  filter(id != "DC")


benefits_map <- ggplot() +
  geom_polygon(data = spdf_fortified, aes(x = long, y = lat,
                                          group = group,
                                          fill = factor(rr_fpuc_binned) %>%
                                            fct_rev()),
               color= "white" ) +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  scale_fill_manual(name = "Replacment rate\nunder CARES",
                    values = RColorBrewer::brewer.pal(6, "Purples")[c(6,5,4,3)],
                    na.translate = FALSE) +
  theme_void(base_family = "serif") +
  coord_map()

ggsave("analysis/release/states_map_discrete.png",
       benefits_map,
       width = 8, height = 4.5)


#### Benchmark ####
### Benchmark Payments ####
add_prob_unemp <- function(wages_df){
  with_eligible <- wages_df %>%
    mutate(job_loser  = (employment_status == 21 & unemployment_duration <= 26 & unemp_reason %in% c(1, 2)))

  eligible_deciles <- with_eligible %>%
    filter(job_loser) %>%
    with(Hmisc::wtd.quantile(weekly_earnings,
                             weight,
                             seq(0, 1, length.out = 11)))
  eligible_deciles[1] <- 0
  eligible_deciles[11] <- Inf


  for_fit <- with_eligible %>%
    ungroup() %>%
    mutate(inc_decile = cut(weekly_earnings, eligible_deciles),
           weight = n() * weight/sum(weight)) %>%
    filter(benefits_amount > 0)


  model <- for_fit %>%
    glm(job_loser  ~  inc_decile + state + two_digit_occ + two_digit_ind,
        weights = weight, family = binomial(), data = . )

  print(model)

  model %>%
    broom::augment_columns(data = for_fit,
                           type.predict = "response")
}


double_create_US_state <- function(x) {
  x %>%
    bind_rows(x %>%
                mutate(state = "US"))
}

CPS_values <-
  wages_unadjusted %>%
  filter(weekly_earnings < Hmisc::wtd.quantile(weekly_earnings, weight, 0.99)) %>%
  add_prob_unemp() %>%
  mutate(weight = weight * .fitted) %>%
  filter(benefits_amount > 0) %>%
  double_create_US_state() %>%
  group_by(state) %>%
  summarise(aww = Hmisc::wtd.mean(wage/weeks_worked,
                                  weights = weight),
            rr_fpuc = Hmisc::wtd.quantile((benefits_amount + 600)/weekly_earnings,
                                          weights = weight,
                                          0.5),
            awba  = Hmisc::wtd.mean(benefits_amount,
                                    weights = weight),
            rr1 = Hmisc::wtd.mean(benefits_amount/weekly_earnings,
                                  weights = weight),
            source = "CPS")

#State benchmarks are from the Benefits Accuracy Measurement survey:
# https://oui.doleta.gov/unemploy/ui_replacement_rates.asp
#Last accessed August 2020

benchmarks <- read_csv("analysis/input/BAM_Q2_2019.csv") %>%
  mutate_at(c("wba", "earnings"), ~ str_remove_all(., "\\$|,") %>%
              as.numeric()) %>%
  transmute(aww = earnings,
            awba = wba,
            rr1 = rr1,
            state = State,
            source = "BAM")


benchmarks_for_plot <- benchmarks %>%
  bind_rows(CPS_values %>%
              select(-rr_fpuc)) %>%
  pivot_longer(cols = c("aww", "awba", "rr1"),
               names_to = "type",
               values_to = "amount") %>%
  pivot_wider(names_from = source,
              values_from = amount)


benchmarks <- benchmarks_for_plot  %>%
  filter(type %in% c("aww")) %>%
  ggplot() +
  aes(BAM, CPS) +
  geom_text(aes(label = state)) +
  geom_abline() +
  geom_abline(slope = 0.85,
              colour = "red",
              alpha = 0.8) +
  geom_abline(slope = 1.15,
              colour = "red",
              alpha = 0.8) +
  labs(x = "Benchmarks from Department of Labor",
       y = "Our calculations from Current Population Survey") +
  facet_wrap(~type, labeller = labeller(type = c(aww = "Average weekly wage",
                                                 awba = "Average benefit amount")),
             scales = "free") +
  scale_x_continuous(labels = scales::dollar, limits = c(600, 1200)) +
  scale_y_continuous(labels = scales::dollar) +
  fte_theme(dark_text = TRUE, legend_title_on = TRUE)


print(benchmarks)
ggsave("analysis/release/benchmarks.png",
       benchmarks,
       width = 4.5, height = 4.5)


prepandemic <- read_csv("analysis/input/ar5159.csv") %>%
  transmute(state = st,
            date = floor_date(mdy(rptdate), "month"),
            weeks_paid = c38,
            amount_paid = c45,
            weeks_claimed = c21 + c22 + c24) %>%
  mutate(average_payment = amount_paid/weeks_paid) %>%
  filter(!(state %in% c("PR", "VI", "DC")),
         year(date) %in% c(2019),
         month(date) %in% c(4, 5, 6)) %>%
  double_create_US_state() %>%
  group_by(state) %>%
  summarise(average_payment = sum(amount_paid)/sum(weeks_paid)) %>%
  left_join(CPS_values %>%
              select(state,
                     benefits_level = awba)) %>%
  mutate(period = "April-June 2019")


full_claims_history <- read_csv("analysis/input/ar5159.csv") %>%
  transmute(state = st,
            date = floor_date(mdy(rptdate), "month"),
            weeks_paid = c38,
            amount_paid = c45,
            weeks_claimed = c21 + c22 + c24) %>%
  mutate(average_payment = amount_paid/weeks_paid) %>%
  filter(!(state %in% c("PR", "VI", "DC")),
         year(date) %in% c(2020, 2019),
         month(date) %in% c(4, 5, 6, 7))

eta_benchmarks_data <- full_claims_history %>%
  filter(year(date) == 2020) %>%
  bind_rows(full_claims_history %>%
              filter(year(date) == 2020) %>%
              mutate(state = "US")) %>%
  group_by(state) %>%
  summarise(average_payment = sum(amount_paid)/sum(weeks_paid)) %>%
  left_join(state_values) %>%
  mutate(period = "April-July 2020") %>%
  bind_rows(prepandemic) %>%
  mutate(period = factor(period, levels = c("April-June 2019", "April-July 2020")))

eta_plots <- c("2019", "2020") %>%
  map(~eta_benchmarks_data %>%
                            filter(str_detect(period, .x)) %>%
                            ggplot(aes(average_payment, benefits_level)) +
                            geom_text(aes(label = state,
                                          colour = state == "US")) +
                            geom_abline() +
                            geom_abline(slope = 0.80,
                                        colour = "red",
                                        alpha = 0.8) +
                            geom_abline(slope = 1.20,
                                        colour = "red",
                                        alpha = 0.8) +
                            labs(x = "Benchmarks from Department of Labor",
                                 y = "Our calculations from Current Population Survey") +
                            scale_x_continuous(labels = scales::dollar) +
                            scale_y_continuous(labels = scales::dollar) +
                            scale_colour_manual(values = c(`TRUE` = "Red",
                                                           `FALSE` = "Black")) +
                            fte_theme(dark_text = TRUE, legend_title_on = TRUE) +
                            facet_wrap(~period)
               )


eta_plot_names <- c("2019", "2020") %>%
  map(~str_c("analysis/release/benchmarks_eta_", .x, ".png"))

walk2(eta_plots, eta_plot_names,
      .f = ~ggsave(filename = .y, plot = .x, width = 4.5, height = 4.5)
      )

benefits_benchmarks <- eta_benchmarks_data %>%
  filter(state == "US", str_detect(period, "2020")) %>%
  select(`Our estimate of average payment April-July 2020` = benefits_level,
         `DOL number of average payment April-July 2020` = average_payment) %>%
  pivot_longer(cols = everything(), names_to = "stat_name", values_to = "stat_value")

rep_rate_stats<- medians_and_shares_over %>%
  filter(type == "replacement_rate_FPUC") %>%
  select(-type) %>%
  pivot_longer(everything(),
               names_to = "type",
               values_to = "value") %>%
  transmute(stat_name = str_replace_all(type, "_", " ") %>%
              str_c(" - $600"),
            stat_value = value)

occupation_rep_rates <- median_occupation_with_benefits_for_graph %>%
  group_by(occupation) %>%
  summarise(rep_rate = sum(rep_rate)) %>%
  filter(occupation %in% c("Janitors",
                           "Sales & retail")) %>%
  transmute(stat_value = rep_rate,
            stat_name = str_c(occupation, "- median - $600")
  )

sample_num <- wages_logit_weights %>%
  summarise(stat_value = n()) %>%
  mutate(stat_name = "Number of respondents in sample")

morg_num <- tibble(stat_name = "Number of people in MORG reweighting sample",
                   stat_value = logit_fit$residuals %>% length())

state_rep_rate_ranking <- wages_logit_weights %>%
  group_by(state) %>%
  summarise(med_rate = Hmisc::wtd.quantile(replacement_rate_FPUC, weight, probs = 0.5)
  ) %>%
  arrange(med_rate) %>%
  filter(state %in% state[1 : 3] | state %in% state[48 : 50]) %>%
  mutate(type = c("lowest", "second lowest", "third lowest",
                  "third highest", "fourth highest", "highest"),
         `median rep rate` = med_rate %>% as.character()
  ) %>%
  pivot_longer(cols = c("state", "median rep rate"),
               names_to = "suffix",
               values_to = "stat_value") %>%
  mutate(stat_name = str_c(type, " ", suffix)) %>%
  select(stat_name, stat_value)

supp_400_stats <- wages_logit_weights %>%
  mutate(rr_400 = (benefits_amount + 400) / weekly_earnings,
         above_rep = (rr_400 >= 1),
         below_60 = (rr_400 <= 0.5)) %>%
  summarise(`$400 supplement share above replacement` = weighted.mean(above_rep, weight),
            `$400 supplement share below 60% replacement` = weighted.mean(below_60, weight)) %>%
  pivot_longer(cols = everything(), names_to = "stat_name", values_to = "stat_value")

CA_stats <- wages_logit_weights %>%
  filter(two_digit_ind %in% c("5", "11"),
         state == "CA") %>%
  group_by(two_digit_ind) %>%
  summarise(stat_value = Hmisc::wtd.quantile(replacement_rate_FPUC, weights = weight, probs = 0.5)) %>%
  transmute(stat_name = if_else(two_digit_ind == "11",
                                "CA rep rate: Leisure + Hospitality",
                                "CA rep rate: retail + wholesale trade"),
            stat_value) %>%
  rbind(state_values %>%
          filter(state == "CA") %>%
          select(stat_value = "rr_fpuc") %>%
          mutate(stat_name = "CA median rep rate")) %>%
  rbind(standard_errors %>%
          filter(state == "CA") %>%
          select(stat_value = "se_rr_fpuc") %>%
          mutate(stat_name = "CA rep rate std. error"))

self_income <- wages_logit_weights %>%
  mutate(greater = business_income > wage %>% as.numeric()
         ) %>%
  left_join(tibble(basket = c("all", "all", "greater"),
                   greater = c(1, 0, 1))
            ) %>%
  group_by(basket) %>%
  summarise(stat_name = n()) %>%
  pivot_wider(names_from = basket, values_from = stat_name) %>%
  mutate(`percent of respondents with self-employment > wage` = greater / all) %>%
  rename(`number of respondents with self-employment > wage` = greater) %>%
  select(contains("respondents")) %>%
  pivot_longer(cols = everything(), names_to = "stat_name", values_to = "stat_value")

stats_for_text <- list(rep_rate_stats, occupation_rep_rates,
                           sample_num, morg_num,
                           CA_stats, supp_400_stats,
                       benefits_benchmarks, earnings_summary_stats,
                       self_income, aggregate_stats) %>%
  bind_rows() %>%
  mutate(stat_value = stat_value %>% as.character()) %>%
  bind_rows(state_rep_rate_ranking)

#### Occupations table ####

occupations <- read_csv("analysis/release/occ_unemp_19_20.csv")
industries <- read_csv("analysis/release/ind_unemp_19_20.csv")
income_groups <- read_csv("analysis/release/inc_unemp_19_20.csv")

quintiles <- read_rds("analysis/release/basic_quintiles.rds")

FPUC_recipiency_rates <- tibble(
  date = ymd(c("20190301", "20190401", "20190501",
               "20200301", "20200401", "20200501")),
  fpuc_rr = c(0, 0, 0, 1,  1, 1))


#for 2019
##https://fred.stlouisfed.org/series/UNRATENSA
#for 2020
##https://www.bls.gov/news.release/archives/empsit_06052020.pdf (estimates correcting for the misclassifcation error)

FRED_unemp <- tibble(
  year = c(2019, 2019, 2019, 2020, 2020, 2020),
  month = c("March", "April", "May", "March", "April", "May"),
  unemp_rate_fred = c(0.039, 0.033, 0.034, 0.054, 0.197, 0.163 )
) %>%
  filter(month == "May") #only use May




wages_with_occupations <- occupations %>%
  select(-X1) %>%
  pivot_longer(cols = starts_with("unem_rate_"),
               values_to = "unemp_rate",
               names_to = "year",
               names_prefix = "unem_rate_") %>%
  mutate(year = as.numeric(year)) %>%
  right_join(wages %>%
               filter(year == 2019) %>%
               select(-year)  %>%
               left_join(occupation_codes, by = c( two_digit_occ = "code")))


wages_with_occupations %>%
  group_by(year) %>%
  summarise(Hmisc::wtd.mean(unemp_rate, weight)) %>%
  pull(2) %>%
  pluck(2) %>%
  expect_equal(0.123, tol = 0.001) %>%
  test_that("May rate is reduced further by compositional changes")




wages_with_industries <- industries %>%
  select(-X1) %>%
  mutate(icode = factor(icode)) %>%
  pivot_longer(cols = starts_with("unem_rate_"),
               values_to = "unemp_rate",
               names_to = "year",
               names_prefix = "unem_rate_") %>%
  mutate(year = as.numeric(year)) %>%
  right_join(wages %>%
               filter(year == 2019) %>%
               mutate(two_digit_ind = factor(two_digit_ind)) %>%
               select(-year), by = c(icode = "two_digit_ind")) %>%
  left_join(m_inds %>%
              mutate(icode = factor(icode)))


wages_with_incomes <- income_groups %>%
  select(-X1) %>%
  pivot_longer(cols = starts_with("unem_rate_"),
               values_to = "unemp_rate",
               names_to = "year",
               names_prefix = "unem_rate_") %>%
  mutate(year = as.numeric(year)) %>%
  right_join(wages %>%
               filter(year == 2019) %>%
               select(-year) %>%
               mutate(earnings_cut = cut(weekly_earnings, quintiles))) %>%
  mutate(`Earnings quintile` = label_cuts("dollar")(earnings_cut))


#want to expand the

unemployment_rates <- map_dbl(list(wages_with_occupations,
    wages_with_industries, wages_with_incomes), ~ .x %>% filter(year == 2020) %>%
      with(Hmisc::wtd.mean(unemp_rate, weight)))


stats_for_text <- tibble(stat_name = "Unemployment rate for incidence (unadjusted)",
                         stat_value = unemployment_rates[1] %>% as.character()) %>%
  bind_rows(stats_for_text)

recipiency_stats <- tibble(stat_name = character(), stat_value = character())
incidence_by_group <- function(data, group_name) {



  data <- data %>%
    mutate(date = ymd(str_c(year, 05, 1, sep = "-"))) %>%
    filter(!is.na(unemp_rate))


  ue_est <- data %>%
    group_by(year) %>%
    arrange(year) %>%
    summarise(ue = Hmisc::wtd.mean(unemp_rate, weight)) %>%
    left_join(FRED_unemp) %>%
    transmute(year, rescaling = unemp_rate_fred/ue)

  total_payout_all_reciept <- data %>%
    left_join(ue_est) %>%
    group_by(year, date) %>%
    summarise(full_claim_amount = sum(4.3 * unemp_rate  * rescaling * benefits_amount * weight),
              full_weeks_claimed = sum(4.3 * unemp_rate  * rescaling * weight))


  actual_payout <- full_claims_history %>%
    group_by(date) %>%
    summarise(actual_amount = sum(amount_paid),
              weeks_claimed = sum(weeks_claimed),
              weeks_paid = sum(weeks_paid))

  recipiency_rates <- left_join(total_payout_all_reciept, actual_payout) %>%
    transmute(date,
              recipiency_rate = actual_amount/full_claim_amount,
              recipiency_rate_other = weeks_paid/full_weeks_claimed)

  if (group_name == "occupation") {
   recipiency_stats <<- recipiency_rates %>%
     ungroup() %>%
     transmute(stat_name = str_c("recipiency rate: ", year(date)),
             stat_value = recipiency_rate %>% as.character())

  }

  data <- data %>%
    mutate(recipiency_type = "All reciept") %>%
    bind_rows(data %>%
                mutate(recipiency_type = "Estimated receipt")) %>%
    left_join(recipiency_rates %>%
                mutate(recipiency_type = "Estimated receipt") %>%
                bind_rows(recipiency_rates %>%
                            mutate(recipiency_type = "All reciept",
                                   recipiency_rate = 1)))

  wages_with_changes <- data %>%
    group_by(!!sym(group_name)) %>%
    left_join(FPUC_recipiency_rates) %>%
    group_by(date) %>%
    mutate(weekly_earnings_FPUC = (1 - unemp_rate) * weekly_earnings +
             unemp_rate  * recipiency_rate *
             (benefits_amount + fpuc_rr*if_else(benefits_amount > 0,
                                                600,
                                                0)),
           weekly_earnings_no_FPUC = (1 - unemp_rate) * weekly_earnings +
             unemp_rate  * recipiency_rate * benefits_amount,
           loss_labour = unemp_rate * weekly_earnings) %>%
    ungroup() %>%
    transmute(person_id, hh_id,
              weight,
              recipiency_type,
              !!sym(group_name),
              unemp_rate,
              recipiency_rate,
              fpuc_rr,
              benefits_amount,
              weekly_when_working = weekly_earnings,
              person_id, year, month = month(date), weekly_earnings_FPUC,
              weekly_earnings_no_FPUC)


  incidence_df <- wages_with_changes %>%
    filter(benefits_amount > 0) %>%
    group_by(recipiency_type, person_id, hh_id, year) %>%
    summarise(weekly_earnings_FPUC = sum(weekly_earnings_FPUC),
              weekly_earnings_no_FPUC = sum(weekly_earnings_no_FPUC),
              weekly_when_working = sum(weekly_when_working),
              unemp_rate = mean(unemp_rate),
              weight = first(weight),
              !!sym(group_name) := first(!!sym(group_name))) %>%
    group_by(recipiency_type, person_id, hh_id) %>%
    mutate(indiv_p_change_FPUC = if_else(year == 2020,
                                         weekly_earnings_FPUC/lag(weekly_earnings_FPUC), 0),
           indiv_p_change_no_FPUC = if_else(year == 2020,
                                          weekly_earnings_no_FPUC/lag(weekly_earnings_no_FPUC), 0)) %>%
    group_by(recipiency_type, year, !!sym(group_name)) %>%
    summarise(indiv_p_change_FPUC = Hmisc::wtd.quantile(indiv_p_change_FPUC, weight, 0.5) - 1,
              indiv_p_change_no_FPUC = Hmisc::wtd.quantile(indiv_p_change_no_FPUC, weight, 0.5) - 1,
              unemp = Hmisc::wtd.mean(unemp_rate, weight),
              weekly_when_working = Hmisc::wtd.quantile(weekly_when_working, weight, 0.5)) %>%
    pivot_wider(names_from = "year",
                values_from = c("indiv_p_change_FPUC", "indiv_p_change_no_FPUC",
                                "unemp")) %>%
    transmute(recipiency_type,
              !!sym(group_name),
              `Median wage when working` = weekly_when_working,
              `Unemployment rate (2020)` = unemp_2020,
              `Unemployment rate (2019)` = unemp_2019,
              `rel without FPUC` = indiv_p_change_no_FPUC_2020,
              `rel with FPUC` = indiv_p_change_FPUC_2020) %>%
    arrange(`Median wage when working`) %>%
    filter(!is.na(!!sym(group_name))) %>%
    group_by(recipiency_type) %>%
    nest()

  incidence_list <- incidence_df$data
  names(incidence_list) <- str_c(incidence_df$recipiency_type, "-", group_name)

  return(incidence_list)
}


incidence_tables <- map2(list(wages_with_occupations,
          wages_with_industries, wages_with_incomes),
     list("occupation", "ind", "Earnings quintile"),
     incidence_by_group) %>%
  flatten()

incidence_tables %>%
  writexl::write_xlsx("analysis/release/incidence_table.xlsx")


### format table for export
map2(incidence_tables,
     names(incidence_tables),
     ~ .x %>%
       rename_at(vars(dplyr::matches("occupation|ind")),
                 ~ str_replace(., "ind", "Industry") %>%
                   str_to_title()) %>%
       mutate_at(vars(dplyr::matches("Earnings quintile")),
                 ~ str_c(c("Bottom quintile (", "Second quintile (",
                           "Third quintile (", "Fourth quintile (",
                           "Top quintile ("),
                           ., rep(")", 5))) %>%
       gt() %>%
       fmt_currency(c("Median wage when working"), decimals = 0) %>%
       fmt_percent(c("Unemployment rate (2020)",
                     "Unemployment rate (2019)",
                     "rel without FPUC",
                     "rel with FPUC"), decimals = 1) %>%
       tab_spanner("Median change in income",
                   contains("rel")) %>%
       tab_spanner("Unemployment rate",
                   contains("Unemployment rate")) %>%
       cols_label(`Median wage when working` = "Weekly earnings",
                  `Unemployment rate (2019)` = "2019",
                  `Unemployment rate (2020)` = "2020",
                  `rel without FPUC` = "without FPUC",
                  `rel with FPUC` = "with FPUC") %>%
       cols_move("Unemployment rate (2020)",
                 "Unemployment rate (2019)") %>%
       gt::as_latex() %>%
       str_replace_all("longtable", "tabular") %>%
       write_lines(str_c("analysis/release/", .y, ".tex")))

stats_for_text <- bind_rows(list(stats_for_text, recipiency_stats,
                                 mutate_all(median_variance, as.character),
                                 mutate_all(tipped_wage, as.character)
                                 )
                            )
write_csv(stats_for_text, "analysis/release/stats_for_text.csv")
