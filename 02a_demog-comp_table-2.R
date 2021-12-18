library(tidyverse)
library(patchwork)


educ_acs <- read_csv("data/ACS-demographics/acs-2019_edu-national.csv")
race_acs <- read_csv("data/ACS-demographics/acs-2019_race-national.csv")

race_stats <- read_csv("data/demographics-cleaned/svy-subgroups_by-race.csv")
educ_stats <- read_csv("data/demographics-cleaned/svy-subgroups_by-educ.csv")


# Select date
sub_date <- function(tbl, ax_date = "2021-03-22", hp_date = "2021-03-29", fb_date = "2021-03-27") {
  bind_rows(
    filter(tbl, mode == "ax", date == ax_date),
    filter(tbl, mode == "hp", date == hp_date),
    filter(tbl, mode == "fb", date == fb_date)
  )
}

# compare Composition ----
## EDUCATION
educ_compare <- sub_date(educ_stats) %>% 
  select(mode, educ, pct_insamp_wgt, pct_insamp_raw) %>% 
  pivot_wider(id_cols = educ, names_from = mode, values_from = c(pct_insamp_raw, pct_insamp_wgt)) %>% 
  relocate(educ, matches("_ax"), matches("_hp"), matches("_fb")) %>% 
  left_join(educ_acs, by = c("educ"))

race_compare <- sub_date(race_stats) %>% 
  select(mode, race, pct_insamp_wgt, pct_insamp_raw) %>% 
  pivot_wider(id_cols = race, names_from = mode, values_from = c(pct_insamp_raw, pct_insamp_wgt)) %>% 
  relocate(race, matches("_ax"), matches("_hp"), matches("_fb")) %>% 
  left_join(select(race_acs, -count_pop), by = c("race"))



cat_for_tab <- function(tbl, r = NULL, e = NULL, u = "", file, dir = "tables/composition") {
  
  if ("race" %in% colnames(tbl))
    tbl_i <- filter(tbl, race == r) %>% select(-race)
  if ("educ" %in% colnames(tbl))
    tbl_i <- filter(tbl, educ == e) %>% select(-educ)
  
  pct_wide <- tbl_i %>% 
    mutate_all(scales::percent_format(accuracy = 1, suffix = u))
  
  pct_wide[1, ] %>% 
    replace(is.na(.), "") %>% 
    str_c(collapse = " & ") %>% 
    write_lines(file = fs::path(dir, file))
}

educ_compare %>% cat_for_tab(e = "HighSchoolorLess", file = "educ_highschool.tex", u = "\\%")
educ_compare %>% cat_for_tab(e = "FourYearDegree", file = "educ_4year.tex")
educ_compare %>% cat_for_tab(e = "PostGraduate", file = "educ_postgrad.tex")
educ_compare %>% cat_for_tab(e = "SomeCollege", file = "educ_somecoll.tex")

race_compare %>% cat_for_tab(r = "NHWhite", file = "race_white.tex", u = "\\%")
race_compare %>% cat_for_tab(r = "Hispanic", file = "race_hisp.tex")
race_compare %>% cat_for_tab(r = "NHBlack", file = "race_black.tex")
race_compare %>% cat_for_tab(r = "NHAsian", file = "race_asian.tex")
