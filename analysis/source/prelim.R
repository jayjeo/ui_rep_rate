library(tidyverse)
library(lubridate)
library(yaml)
library(rprojroot)
library("RColorBrewer")

matches <- dplyr::matches

make_path <- is_git_root$make_fix_file(".") #nolint


out_path <- str_c(make_path(), "/analysis/release/")
out_table_path <- str_c(out_path, "stats_in_figures/")

setwd(make_path("./"))
config <- yaml.load_file(make_path("analysis/config.yml"))

source("analysis/source/graph_prelim.R")

occupation_codes <- tribble(
  ~code, ~occupation,
  "00", "Managers",
  "01", "Managers",
  "02", "Managers",
  "03", "Managers",
  "04", "Managers",
  "47", "Sales & retail",
  "23", "Teachers",
  "36", "Medical assistants",
  "91", "Transport",
  "40", "Food service",
  "41", "Food service",
  "32", "Nurses & therapists",
  "10", "IT",
  "42", "Janitors",
  "62", "Construction",
  "54", "Receptionist"
)



code_industries <- function(IND) {case_when(
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
  IND == 9890 ~ 14,
  TRUE ~ 15 #Add other code
)}


fips_codes <- maps::state.fips %>%
  select(fips,
         state = abb) %>%
  select(STATEFIP = fips, state) %>%
  distinct() %>%
  bind_rows(tibble(state = c("HI", "AK"),
                   STATEFIP = c(15, 02))) %>%
  filter(state != "DC")
