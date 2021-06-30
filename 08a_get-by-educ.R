library(tidyverse)
library(tidycensus)
library(ccesMRPprep)
library(glue)
library(fs)
library(lubridate)

# Data ---
hp_raw <- read_csv('data/USCB_household_pulse/hp_combined_weeks22to27.csv')
ax_raw <- read_rds("data/axios_ipsos/axios-ipsos_stacked.rds")

# ACS ---
acs_edu_raw <- get_acs("state", variables = ccesMRPprep::acscodes_age_sex_educ, 
                       year = 2019, survey = "acs1") %>% 
  left_join(acscodes_df)


# Clean / Tabulate with ACS ------
acs_wteduc_us <- acs_edu_raw %>% 
  rename(state = NAME) %>% 
  filter(!state %in% c("District of Columbia", "Puerto Rico")) %>% 
  mutate(
    educ = case_when(
      educ == 1 ~ "HighSchoolorLess",
      educ == 2 ~ "SomeCollege",
      educ == 3 ~ "FourYearDegree",
      educ == 4 ~ "PostGraduate"
    )) %>% 
  count(educ, wt = estimate, name = "count_pop") %>% 
  transmute(educ, pct_pop = count_pop / sum(count_pop))

write_csv(acs_wteduc_us, "data/ACS-demographics/acs-2019_edu-national.csv")


# Facebook download -------
fb_domain <- "https://www.cmu.edu/delphi-web/surveys/weekly"
file_suffix <- "weekly_nation_edulevel.csv"
fb_weeks <- dir_ls("data/facebook_cmu/raw") %>% 
  path_file() %>% 
  str_sub(1, 17) %>% 
  unique()


fb_vars = c('region', 'edulevel',
            'period_start', 'period_end', 
            'val_pct_vaccinated',
            'sample_size_pct_vaccinated', 'represented_pct_vaccinated', 
            'val_pct_hesitant_vaccine', 'represented_pct_hesitant_vaccine', 
            'val_pct_vaccinated_or_accept',  'represented_pct_vaccinated_or_accept')


# download from online
fb_educ_raw <- map_dfr(
  .x = fb_weeks, 
  .f = ~ read_csv(paste0(fb_domain, "/", .x, "_", file_suffix), col_types = cols()) %>% 
    select(!!fb_vars)
)

fb_educ_fmt <- fb_educ_raw %>% 
  mutate(edu_fb = recode(edulevel, 
                         HighSchool = "HighSchoolorLess", 
                         LessThanHighSchool = "HighSchoolorLess"),
         end_date = ymd(period_end)) %>% 
  group_by(end_date, edu_fb) %>% 
  summarize(
    n_fb = sum(represented_pct_vaccinated),
    n_raw = sum(sample_size_pct_vaccinated),
    pct_vaccinated = sum(0.01*val_pct_vaccinated*represented_pct_vaccinated) / 
      sum(represented_pct_vaccinated),
    # folow 06a
    pct_haveorwillgetvax = sum(0.01*val_pct_vaccinated_or_accept*represented_pct_vaccinated_or_accept) / 
      sum(represented_pct_vaccinated_or_accept),
    # my way
    pct_hesitant_direct = sum(0.01*val_pct_hesitant_vaccine * represented_pct_hesitant_vaccine) / 
      sum(represented_pct_hesitant_vaccine), 
    .groups = "drop") %>% 
  mutate(pct_hesitant = 1 -  pct_haveorwillgetvax,
         pct_willing =  pct_haveorwillgetvax - pct_vaccinated) %>% 
  filter(!is.na(edu_fb)) %>% 
  rename(date = end_date) %>% 
  group_by(date) %>% 
  mutate(
    pct_wgt = n_fb / sum(n_fb),
    pct_raw = n_raw / sum(n_raw)
  ) %>% 
  ungroup()


# HP ------
hp_educ_fmt <- hp_raw %>% 
  mutate(
    edu_fb = case_when(
      EEDUC %in% 1:3 ~ "HighSchoolorLess",
      EEDUC %in% 4:5 ~ "SomeCollege",
      EEDUC %in% 6 ~ "FourYearDegree",
      EEDUC %in% 7 ~ "PostGraduate",
      TRUE ~ NA_character_)
  ) %>% 
  group_by(WEEK, end_date, edu_fb) %>% 
  summarize(n_hp = sum(PWEIGHT),
            n_raw = n(),
            pct_haveorwillgetvax = sum(as.numeric(RECVDVACC == 1 | GETVACC %in% c(1,2)) * PWEIGHT) / 
              sum(PWEIGHT),
            pct_vaccinated = sum((RECVDVACC == 1)*PWEIGHT) / sum(PWEIGHT),
            .groups = "drop") %>% 
  mutate(pct_hesitant = 1 -  pct_haveorwillgetvax,
         pct_willing =  pct_haveorwillgetvax - pct_vaccinated) %>% 
  rename(date = end_date) %>% 
  group_by(date) %>% 
  mutate(
    pct_wgt = n_hp / sum(n_hp),
    pct_raw = n_raw / sum(n_raw)
  ) %>% 
  ungroup()



# Axios ----
ax_educ_fmt <- ax_raw %>% 
  mutate(edu_fb  = recode(educ,
                        `HS or Less` = "HighSchoolorLess",
                        `Some college or Associate degree` = "SomeCollege",
                        `Bachelor’s degree` = "FourYearDegree", 
                        `Master’s degree or above` = "PostGraduate")) %>% 
  group_by(date, WAVE, edu_fb) %>% 
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
out_educ <- bind_rows(
  hp_educ_fmt %>% mutate(mode = "hp"), 
  fb_educ_fmt %>% mutate(mode = "fb"),
  ax_educ_fmt %>% mutate(mode = "ax")
) %>% 
  transmute(
    date, mode, 
    educ = edu_fb, 
    pct_vaccinated, 
    pct_willing = coalesce(pct_willing, pct_haveorwillgetvax - pct_vaccinated), 
    pct_hesitant, 
    pct_insamp_wgt = pct_wgt, 
    pct_insamp_raw = pct_raw, 
    n_raw
  )


# Save ---
write_csv(out_educ, "data/demographics-cleaned/svy-subgroups_by-educ.csv")

