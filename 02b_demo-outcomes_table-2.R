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



# Axios JOINT microdata by edu x race ----
ax_grp <- ax_raw %>%
  filter(WAVE %in% c("43", "44", "45"), ppethm != "2+ Races, Non-Hispanic") %>%
  mutate(race = str_remove_all(ppethm, ", Non-Hispanic"),
         educ = str_replace_all(educ, "’", "'")) %>%
  arrange(ppethm) %>%
  mutate(race = fct_inorder(race)) %>%
  mutate(educ = recode_factor(educ,
                              `HS or Less` = "High School or Less",
                              `Some college or Associate degree` = "2-Year Degree",
                              `Bachelor's degree` = "4-Year Degree",
                              `Master's degree or above` = "Post-Graduate")) %>%
  mutate(educ = fct_rev(educ)) %>%
  group_by(educ, race) %>%
  summarize(willing = weighted.mean(willing, wt_final, na.rm = TRUE),
            hesitant = weighted.mean(hesitant, wt_final, na.rm = TRUE),
            n = n()) %>%
  mutate(pct_willing = scales::percent(willing, accuracy = 1),
         pct_hesitant = scales::percent(hesitant, accuracy = 1))

gg_will <- ax_grp %>%
  ggplot(aes(x = race, y = educ)) +
  geom_tile(aes(fill = willing), color = "white", size = 2) +
  geom_text(aes(label = pct_willing), color = "white") +
  scale_fill_viridis_c(end = 0.9) +
  theme_classic() +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = "Not Vaccinated\nBut Willing") +
  theme(axis.text = element_text(color = "black"))

gg_hes <- ax_grp %>%
  ggplot(aes(x = race, y = educ)) +
  geom_tile(aes(fill = hesitant), color = "white", size = 2) +
  geom_text(aes(label = pct_hesitant), color = "white") +
  scale_fill_viridis_c(end = 0.9, option = "B") +
  theme_classic() +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = "Not Vaccinated\nHesitant") +
  theme(axis.text = element_text(color = "black"))

gg_will + gg_hes
ggsave("plots/by-educ-race_hes_willingness.pdf", w = 10, h = 4)





