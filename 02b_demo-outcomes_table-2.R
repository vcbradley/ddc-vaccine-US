library(patchwork)
library(tidyverse)
library(scales)

# Axios ---
ax_raw <- read_rds("data/axios-ipsos/axios-ipsos_stacked.rds")
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


# Table version e.g. compare FB and HP ----

outcome_then_mode <- function(tbl, r = NULL, e = NULL, u = "") {
  if ("race" %in% colnames(tbl))
    tbl_i <- filter(tbl, race == r) %>% select(-race)
  if ("educ" %in% colnames(tbl))
    tbl_i <- filter(tbl, educ == e) %>% select(-educ)

  if (! "ax" %in% tbl_i$mode)
    tbl_i <- add_row(tbl_i, mode = "ax")

  pct_wide <- tbl_i %>%
    mutate(mode = fct_relevel(factor(mode), "ax", "hp", "fb")) %>%
    arrange(mode) %>%
    select(-date, -matches("pct_insamp"), -n_raw) %>%
    pivot_wider(names_from = c(mode), values_from = c(pct_vaccinated, pct_willing, pct_hesitant)) %>%
    mutate_all(percent_format(accuracy = 1, suffix = u))

  pct_wide[1, ] %>%
    replace(is.na(.), "") %>%
    str_c(collapse = " & ")
}


sub_date(race_stats) %>% outcome_then_mode(r = "NHWhite", u = "\\%") %>% write_lines("tables/vax_by_demo/NHWhite.tex")
sub_date(race_stats) %>% outcome_then_mode(r = "NHBlack") %>% write_lines("tables/vax_by_demo/NHBlack.tex")
sub_date(race_stats) %>% outcome_then_mode(r = "Hispanic") %>% write_lines("tables/vax_by_demo/Hispanic.tex")
sub_date(race_stats) %>% outcome_then_mode(r = "NHAsian") %>% write_lines("tables/vax_by_demo/NHAsian.tex")

sub_date(educ_stats) %>% outcome_then_mode(e = "HighSchoolorLess", u = "\\%") %>% write_lines("tables/vax_by_demo/HighSchoolorLess.tex")
sub_date(educ_stats) %>% outcome_then_mode(e = "SomeCollege") %>% write_lines("tables/vax_by_demo/SomeCollege.tex")
sub_date(educ_stats) %>% outcome_then_mode(e = "FourYearDegree") %>% write_lines("tables/vax_by_demo/FourYearDegree.tex")
sub_date(educ_stats) %>% outcome_then_mode(e = "PostGraduate") %>% write_lines("tables/vax_by_demo/PostGraduate.tex")
