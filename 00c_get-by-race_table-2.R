library(tidyverse)
library(tidycensus)
library(ccesMRPprep)
library(glue)
library(lubridate)
library(fs)

hp_raw = read_csv('data/USCB_household_pulse/hp_combined_weeks22to27.csv')
ax_raw <- read_rds("data/axios_ipsos/axios-ipsos_stacked.rds")

# ACS ------
acs_race_raw <- get_acs("state", variables = ccesMRPprep::acscodes_age_sex_race, 
                        year = 2019, survey = "acs1") %>% 
  left_join(acscodes_df)

acs_race_pop <- acs_race_raw %>% 
  rename(state = NAME) %>% 
  filter(!state %in% c("District of Columbia", "Puerto Rico")) %>% 
  mutate(
    race = case_when(
      race == 1 ~ "NHWhite",
      race == 2 ~ "NHBlack",
      race == 3 ~ "Hispanic",
      race == 4 ~ "NHAsian",
      TRUE ~ "All Other"
    )) %>% 
  count(race, wt = estimate, name = "count_pop") %>% 
  mutate(pct_pop = count_pop / sum(count_pop)) %>% 
  ungroup()

write_csv(acs_race_pop, "data/ACS-demographics/acs-2019_race-national.csv")



# Facebook download -------

fb_domain <- "https://www.cmu.edu/delphi-web/surveys/weekly"
file_suffix <- "_weekly_nation_hispanic_race.csv"
fb_weeks <- dir_ls("data/facebook_cmu/raw") %>% 
  path_file() %>% 
  str_sub(1, 17) %>% 
  unique()

fb_vars = c('region', 'hispanic', 'race',
            'period_start', 'period_end', 
            'val_pct_vaccinated',
            'sample_size_pct_vaccinated', 'represented_pct_vaccinated', 
            'val_pct_hesitant_vaccine', 'represented_pct_hesitant_vaccine', 
            'val_pct_vaccinated_or_accept',  'represented_pct_vaccinated_or_accept')

# download from online
fb_natrace_raw <- map_dfr(
  .x = fb_weeks, 
  .f = ~ read_csv(paste0(fb_domain, "/", .x, file_suffix), 
                  col_types = cols()) %>% 
    select(fb_vars) %>% 
    relocate(hispanic, race))

# create week-level data
fb_race_fmt <- fb_natrace_raw %>% 
  mutate(date = ymd(period_end),
         year_month = paste(year(ymd(period_end)),  month(ymd(period_end)), sep = "-"),
         period_start = ymd(period_start),
         period_end = ymd(period_end),
         pct_vaccinated = val_pct_vaccinated / 100,
         pct_hesitant = val_pct_hesitant_vaccine / 100) %>% 
  rename(race_fb = race,
         hispanic_fb = hispanic) %>% 
  mutate(race = case_when(
    hispanic_fb ~ "Hispanic",
    !hispanic_fb & race_fb == "White" ~ "NHWhite",
    !hispanic_fb & race_fb == "Asian" ~ "NHAsian",
    !hispanic_fb & race_fb == "BlackAfricanAmerican" ~ "NHBlack",
    !hispanic_fb & race_fb == "AmericanIndianAlaskaNative" ~ "NHAIAN",
    !hispanic_fb & race_fb == "NativeHawaiianPacificIslander" ~ "NHNHOPI",
    TRUE ~ NA_character_
  )) %>% 
  filter(!(is.na(race_fb) & is.na(hispanic_fb))) %>%
  group_by(date, race) %>% 
  summarize(
    pct_vaccinated = sum(pct_vaccinated*represented_pct_vaccinated, na.rm = TRUE)   /
      sum(represented_pct_vaccinated, na.rm = TRUE),
    pct_haveorwillgetvax = sum(0.01*val_pct_vaccinated_or_accept*represented_pct_vaccinated_or_accept) / 
      sum(represented_pct_vaccinated_or_accept),
    n_weighted  = sum(represented_pct_vaccinated, na.rm = TRUE),
    n_raw  = sum(sample_size_pct_vaccinated, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(pct_hesitant = 1 - pct_haveorwillgetvax) %>% 
  group_by(date) %>% 
  mutate(
    pct_wgt = n_weighted / sum(n_weighted),
    pct_raw = n_raw / sum(n_raw)
  ) %>% 
  ungroup()


# Create Household Pulse wave-demo level data ----
hp_race_fmt <-  hp_raw %>% 
  mutate(
    year_month = paste(year(end_date),  month(end_date), sep = "-"),
    date = ymd(end_date),
    race = case_when(
      RHISPANIC == 2 ~ "Hispanic",
      RHISPANIC == 1 & RRACE == 1 ~ "NHWhite",
      RHISPANIC == 1 & RRACE == 2 ~ "NHBlack",
      RHISPANIC == 1 & RRACE == 3 ~ "NHAsian",
      TRUE ~ NA_character_
    )
  ) %>% 
  group_by(date, year_month, race) %>% 
  summarize(n_raw = n(),
            n_weighted = sum(PWEIGHT),
            pct_haveorwillgetvax = sum(as.numeric(RECVDVACC == 1 | GETVACC %in% c(1,2)) * PWEIGHT) / 
              sum(PWEIGHT),
            pct_vaccinated = sum((RECVDVACC == 1)*PWEIGHT) / sum(PWEIGHT),
            .groups = "drop") %>% 
  mutate(pct_hesitant = 1 -  pct_haveorwillgetvax) %>% 
  group_by(date, year_month) %>% 
  mutate(
    pct_wgt = n_weighted / sum(n_weighted),
    pct_raw = n_raw /  sum(n_raw)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(race))



# Axios ----
ax_race_fmt <- ax_raw %>% 
  mutate(race  = recode(ppethm,
                       `White, Non-Hispanic` = "NHWhite",
                       `Black, Non-Hispanic` = "NHBlack",
                       `Hispanic` = "Hispanic", 
                       .default = "Other")) %>% 
  group_by(date, WAVE, race) %>% 
  summarize(
    pct_vaccinated = weighted.mean(vaccinated, wt_final, na.rm = TRUE),
    pct_willing = weighted.mean(willing, wt_final, na.rm = TRUE),
    pct_hesitant = weighted.mean(hesitant, wt_final, na.rm = TRUE),
    n_raw = n(), 
    n_weighted = sum(wt_final, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  group_by(date, WAVE) %>% 
  mutate(
    pct_raw = n_raw /  sum(n_raw),
    pct_wgt = n_weighted / sum(n_weighted)
  ) %>% 
  ungroup()



# Stack ----
out_race <- bind_rows(
  hp_race_fmt %>% mutate(mode = "hp"), 
  fb_race_fmt %>% mutate(mode = "fb"),
  ax_race_fmt %>% mutate(mode = "ax")
) %>% 
  transmute(
    date, mode, race, 
    pct_vaccinated, 
    pct_willing = coalesce(pct_willing, pct_haveorwillgetvax - pct_vaccinated), 
    pct_hesitant, 
    pct_insamp_wgt = pct_wgt, 
    pct_insamp_raw = pct_raw, 
    n_raw
  )


# Save ---
write_csv(out_race, "data/demographics-cleaned/svy-subgroups_by-race.csv")

