source('_setup.R')
library(tidyverse)
library(fs)
library(glue)
library(ddi)
library(ggpubr)
library(patchwork)

download_fb = F



# CDC data prep ----------------
cdc_demos <- fread('data/raw/CDC/demographic_trends_of_people_receiving_covid19_vaccinations_in_the_united_states_2021-05-26.csv')
cdc_total <- fread('data/raw/CDC/trends_in_number_of_covid19_vaccinations_in_the_us_2021-05-26.csv')
pop_total <- fread('data/raw/CDC/SCPRC-EST2019-18+POP-RES.csv') %>% filter(STATE == 0) %>% select(pop_total = POPEST18PLUS2019)

# from here https://www.census.gov/data/tables/time-series/demo/popest/2010s-national-detail.html
pop_total_age <- data.frame(age_group_2way = c('Under65','Over65'),  pop_total = c(201142110, 54058263))


cdc_total_subset = cdc_total %>%
  filter(`Date Type` == 'Admin', `Program` == 'US') %>%
  mutate(n_vaccinated_total = as.numeric(`People with at least One Dose Cumulative`)) %>%
  select(date = Date, n_vaccinated_total)%>%
  left_join(pop_total, by = character())


cdc_age <- cdc_demos %>%
  filter(grepl('Age', `Demographic Group`)
         , !`Demographic Group` %in% c('Age_known', 'Ages_<12yrs', 'Ages_12-15_yrs', 'Ages_16-17_yrs', 'Age_unknown')) %>%
  rename(date = Date
         , age_group = `Demographic Group`
         , n_vaccinated_withage = `People with at least one dose`
         , pop_total_withage = `Census`
         ) %>%
  mutate(pct_vaccinated_withage = n_vaccinated_withage / pop_total_withage
    , age_group = str_remove_all(age_group, "(Ages_|_yrs)")
    , age_group_2way = case_when(age_group %in% c('75+', '65-74') ~ 'Over65'
                              , age_group == 'unknown' ~ 'Unknown'
                              , TRUE ~ 'Under65')
    , age_group_fb = case_when(age_group %in% c('75+', '65-74') ~ '65+'
                                  , age_group == 'unknown' ~ 'Unknown'
                                  , TRUE ~ '18-65')
    ) %>%
      group_by(date, age_group_2way) %>%
      summarize(n_vaccinated = sum(n_vaccinated_withage)
                , pop_total_withage = sum(pop_total_withage)
                , .groups = 'drop') %>%
      left_join(pop_total_age, by = 'age_group_2way')  %>%
  mutate(pop_total_missingage = pop_total - pop_total_withage)


cdc_age_imp <- cdc_age %>%
  pivot_wider(id_cols = date, names_from = age_group_2way, values_from = c(n_vaccinated, pop_total))  %>%
  left_join(., cdc_total_subset, by = 'date') %>%
  mutate(n_vaccinated_noage = n_vaccinated_total - (n_vaccinated_Over65 + n_vaccinated_Under65)
         ) %>%
  mutate(allold_Over65 = ifelse(n_vaccinated_Over65 + n_vaccinated_noage > pop_total_Over65, pop_total_Over65, n_vaccinated_Over65 + n_vaccinated_noage)
         , allold_Under65 = ifelse(n_vaccinated_Over65 + n_vaccinated_noage > pop_total_Over65, n_vaccinated_Under65 + (n_vaccinated_Over65 + n_vaccinated_noage - pop_total_Over65), n_vaccinated_Under65)
         , allyoung_Over65 = n_vaccinated_Over65
         , allyoung_Under65 = n_vaccinated_Under65 + n_vaccinated_noage
         , allold_Over65_pctvax = allold_Over65 / pop_total_Over65
         , allold_Under65_pctvax = allold_Under65 / pop_total_Under65
         , allyoung_Over65_pctvax = allyoung_Over65 / pop_total_Over65
         , allyoung_Under65_pctvax = allyoung_Under65 / pop_total_Under65
         )

cdc_age_long <- cdc_age_imp %>%
  pivot_longer(cols = c(allyoung_Over65_pctvax, allyoung_Under65_pctvax, allold_Over65_pctvax, allold_Under65_pctvax)
               , values_to = 'pct_vaccinated'
               , names_to = c('assump', 'age_group_2way', 'vax')
               , names_sep = '_') %>%
  pivot_wider(id_cols = c('date', 'age_group_2way'), names_from = 'assump', names_prefix = 'pct_vaccinated_', values_from = 'pct_vaccinated') %>%
  mutate(age_group_fb = case_when(age_group_2way == 'Over65' ~ '65+', TRUE ~ '18-64')) %>%
  left_join(pop_total_age, by = 'age_group_2way')





# Facebook download -------

fb_vars = c( 'region', 'survey_geo'
             , 'period_start', 'period_end'
             , 'val_pct_vaccinated', 'se_pct_vaccinated'
             , 'sample_size_pct_vaccinated', 'represented_pct_vaccinated'
) %>%
  c('age')

file_suffix <- "weekly_nation_age.csv"
fb_weeks <- dir_ls("data/raw/facebook-delphi", regexp = '\\d+_\\d+') %>%
  path_file() %>%
  str_sub(1, 17) %>%
  unique()

if(download_fb){
  # download from online
  fb_domain <- "https://www.cmu.edu/delphi-web/surveys/weekly"
  fb_raw <- map_dfr(
    .x = fb_weeks,
    .f = ~ read_csv(paste0(fb_domain, "/", .x, "_", file_suffix), col_types = cols()) %>%
      select(!!fb_vars) %>%
      relocate(age))
  write.csv(fb_raw, 'data/raw/facebook-delphi/facebook_age_data.csv', row.names = F)
}else{
  fb_raw <- read_csv('data/raw/facebook-delphi/facebook_age_data.csv')
}



# create week-level data
fb_age_fmt <- fb_raw %>%
  mutate(date = ymd(period_end),
         period_start = ymd(period_start),
         period_end = ymd(period_end),
         pct_vaccinated = val_pct_vaccinated / 100) %>%
  filter(date <= max(cdc_age$date)) %>%
  mutate(age_group_fb = case_when(
    age %in% c("18-24", "25-44", "45-64") ~ "18-64",
    age %in% c("65plus") ~ "65+"
  )) %>%
  group_by(date, age_group_fb, period_end, period_start) %>%
  # collapse at 18-64 and 65+
  summarize(
    pct_vaccinated = sum(pct_vaccinated*sample_size_pct_vaccinated)   /
      sum(sample_size_pct_vaccinated),
    sample_size_pct_vaccinated  = sum(sample_size_pct_vaccinated),
    .groups = "drop"
  ) %>%
  mutate(study_name = 'Delphi-Facebook')

# Create Household Pulse week level data ----
hpdata = read_csv(file = glue("data/raw/census-household-pulse/microdata/chp_microdata_cleaned_waves22to29.csv"))

hpdata_grp <- hpdata %>%
  mutate(age_group = case_when(age %in% 18:29 ~ "18-29",
                               age %in% 30:39 ~ "30-39",
                               age %in% 40:49 ~ "40-49",
                               age %in% 50:64 ~ "50-64",
                               age %in% 65:74 ~ "65-74",
                               age >= 75 ~ "75+"),
         age_group_fb = case_when(age >= 65 ~ "65+",
                                  age %in% 18:64 ~ "18-64")) %>%
  as.data.table()

#' Format week level data
fmt_dt <-  function(tbl) {
  as_tibble(tbl) %>%
    mutate(date = ymd(end_date))
}

# create week-level data
hpdata_age = hpdata_grp[
  , .(n = .N
      , N_weighted = sum(PWEIGHT)
      , cv_weights = sd(PWEIGHT)/mean(PWEIGHT)
      , pct_vaccinated_raw = mean(vaccinated)
      , pct_vaccinated = sum(vaccinated * PWEIGHT)/sum(PWEIGHT)
      , pct_hesitant = sum((hesitant_prob + hesitant_def + unsure) * PWEIGHT)/sum(PWEIGHT)
  ), .(wave = WEEK, start_date, end_date, age_group)] %>%
  fmt_dt() %>%
  filter(!is.na(age_group))

hpdata_age_fb = hpdata_grp[
  , .(n = .N
      , N_weighted = sum(PWEIGHT)
      , cv_weights = sd(PWEIGHT)/mean(PWEIGHT)
      , pct_vaccinated_raw = mean(vaccinated)
      , pct_vaccinated = sum(vaccinated * PWEIGHT)/sum(PWEIGHT)
      , pct_hesitant = sum((hesitant_prob + hesitant_def + unsure) * PWEIGHT)/sum(PWEIGHT)
  ), .(wave = WEEK, start_date, end_date, age_group_fb)] %>%
  fmt_dt() %>%
  filter(!is.na(age_group_fb)) %>%
  mutate(study_name = 'Census Household Pulse')






# get ddc ----------------------------
# overall trends
all_polls <- read_csv('data/final/all_polls.csv')
all_polls_fmt <- all_polls %>%
  as_tibble() %>%
  mutate(date = ymd(end_date))


fb_ddc_agegroup_fb <- left_join(
  filter(fb_age_fmt, !is.na(age_group_fb)) %>% mutate(date = as.Date(date)),
  filter(cdc_age_long, !is.na(age_group_fb))  %>% mutate(date = as.Date(date)),
  by = c("date", "age_group_fb")) %>%
  group_by(date, age_group_fb) %>%
  mutate(ddc_allyoung = ddc(pct_vaccinated_allyoung,
                      pct_vaccinated,
                      N = pop_total,
                      n = sample_size_pct_vaccinated)
         , ddc_allold = ddc(pct_vaccinated_allold,
                                 pct_vaccinated,
                                 N = pop_total,
                                 n = sample_size_pct_vaccinated)
         ) %>%
  ungroup()

hp_ddc_agegroup_fb <- left_join(
  filter(hpdata_age_fb, !is.na(age_group_fb)) %>% mutate(date = as.Date(date)),
  filter(cdc_age_long, !is.na(age_group_fb)) %>% mutate(date = as.Date(date)),
  by = c("date", "age_group_fb")) %>%
  group_by(date, age_group_fb) %>%
  mutate(ddc_allyoung = ddc(pct_vaccinated_allyoung,
                      pct_vaccinated,
                      N = pop_total,
                      n = n)
         , ddc_allold = ddc(pct_vaccinated_allold,
                                 pct_vaccinated,
                                 N = pop_total,
                                 n = n)
         ) %>%
  ungroup()


all_ddc <- bind_rows(
  hp_ddc_agegroup_fb %>% mutate(mode = 'household_pulse', study_name = 'Census Household Pulse'),
  fb_ddc_agegroup_fb %>% mutate(mode = 'facebook', study_name = 'Delphi-Facebook'))







######### PLOTS #############

# Age 65 + vs. All Ages, by Mode -----

plot_trend_by_age <-
  ggplot() +
  facet_rep_wrap(~ age_group_fb,
                 labeller = labeller(age_group_fb = c(`18-64` = "Among 18-64 year olds", `65+` = "Among 65+ year olds"))) +
  geom_pointline(data = filter(fb_age_fmt, !is.na(age_group_fb)), aes(x = date, y = pct_vaccinated, color = study_name)) +
  geom_ribbon(data = cdc_age_long %>% mutate(study_name = 'CDC (benchmark)')
              , aes(x = date, ymin = pct_vaccinated_allold, ymax = pct_vaccinated_allyoung, fill = study_name)
              , alpha = 0.3) +
  geom_pointline(data = filter(hpdata_age_fb, !is.na(age_group_fb)), aes(x = date, y = pct_vaccinated, color = study_name)) +
  geom_text(data = tibble(
    age_group_fb = "65+",
    date = c(ymd("2021-05-01"), ymd("2021-04-30"), ymd('2021-04-23')),
    pct_vaccinated = c(0.945, 0.795, 0.65),
    study_name = c("Delphi-Facebook", "Census Household Pulse", "CDC (benchmark)"),
    label = c("Delphi-Facebook", "Census\nHousehold Pulse", "CDC (benchmark)")
  ), aes(x = date, y = pct_vaccinated, label = label, color = study_name),
  size = 2.5) +
  geom_text(data = tibble(
    age_group_fb = "18-64",
    date = c(ymd("2021-04-25"), ymd("2021-05-12"), ymd('2021-05-10')),
    pct_vaccinated = c(0.83, 0.58, 0.35),
    study_name = c("Delphi-Facebook", "Census Household Pulse", "CDC (benchmark)"),
    label = c("Delphi-Facebook", "Census\nHousehold Pulse", "CDC (benchmark)")
  ), aes(x = date, y = pct_vaccinated, label = label, color = study_name),
  size = 2.5) +
  scale_color_manual(values = scale_values) +
  scale_fill_manual(values = scale_values) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(),
                     limits = c(0, 1)) +
  theme_pubr() +
  theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
        strip.background = element_rect(color = NA, fill = "white"),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = 'none') +
  expand_limits(x = ymd("2021-05-30")) +
  labs(x = '2021',
       y = "% Vaccinated, at least one Dose")
plot_trend_by_age



# Plot ddcc -----------------
plot_ddc_byage <- ggplot() +
  geom_ribbon(data = all_ddc %>% filter(age_group_fb == '18-64', date <= as.Date('2021-05-26'))
              , aes(x = date, ymin = ddc_allyoung, ymax = ddc_allold, fill = study_name), alpha = 0.35) +
  #geom_pointline(color = "#425b25") +
  # geom_pointline(data = all_polls %>% filter(mode %in% c('household_pulse', 'facebook'), pop == 'US', pct_error == 0, end_date <= as.Date('2021-05-26')),
  #                aes(x = end_date, y = ddc_weighted, color = study_name),
  #                alpha = 0.5) +
  #geom_hline(yintercept = 0, lty = 2) +
  expand_limits(y = 0) +
  #facet_rep_wrap(~study_name, ) +
  geom_text(data = tibble(study_name = c("Delphi-Facebook", 'Census Household Pulse'),
                          date = c( ymd("2021-05-5"), ymd('2021-05-13')),
                          ddc = c( 0.0155, 0.0035),
                          #label = c('All ages', '18-64', 'All ages', '18-64')
                          ),
            aes(x = date, y = ddc, label = study_name, color = study_name),
            size = 3) +
  scale_color_manual(values = scale_values) +
  scale_fill_manual(values = scale_values) +
  scale_y_continuous(expand = expansion()) +
  theme_pubr() +
  theme(panel.grid.major.y = element_line(colour = "gray", linetype = "dotted"),
        strip.background = element_rect(color = NA, fill = "white"),
        strip.text = element_text(face = "bold", size = 12),
        legend.position = 'none') +
  expand_limits(x = ymd("2021-05-30")) +
  labs(x = '2021',
       y = "ddc",
       title = "unweighted ddc for 18-64 year olds")
plot_ddc_byage

plot_trend_by_age + plot_ddc_byage + plot_layout(nrow = 2, heights = c(1,1.5))

ggsave(filename = 'plots/fig_ddc_by_age.pdf', width= 7, height = 7)

