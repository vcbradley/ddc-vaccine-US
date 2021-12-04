

# plot benchmark comparison -------------------------
benchmark_dates <- c("2021-04-21", "2021-05-05", "2021-05-26", '2021-12-03')

all_cdc = map_dfr(.x = benchmark_dates
        , .f = ~ read_csv(glue("data/CDC/cdc_cleaned_{.x}.csv"))
        , .id = 'download_num') %>%
  left_join(., data.frame(download_num = as.character(1:length(benchmark_dates)), download_date = benchmark_dates))


all_cdc


# new CDC trends data --------------------
# from https://data.cdc.gov/Vaccinations/COVID-19-Vaccination-Demographics-in-the-United-St/km4m-vcsb
cdc_demos_raw <- read_csv('~/Downloads/COVID-19_Vaccination_Demographics_in_the_United_States_National.csv')

cdc_demos <- cdc_demos_raw %>%
  filter(grepl('Age', Demographic_category)) %>%
  mutate(date = as.Date(Date, tryFormats = c('%m/%d/%Y'))
         , under_18_flag = Demographic_category %in% c('Ages_12-17_yrs', 'Ages_5-11_yrs')
         , over_18_flag = !Demographic_category %in% c('Ages_12-17_yrs', 'Ages_5-11_yrs', 'Age_unknown', 'Age_known')
         , unknown_flag = Demographic_category == 'Age_unknown'
         , known_flag = Demographic_category == 'Age_known'
  ) %>%
  group_by(date) %>%
  summarize(n_vaxxed_under18 = sum(under_18_flag * Administered_Dose1)
            , n_vaxxed_over18 = sum(over_18_flag * Administered_Dose1)
            , n_vaxxed_unknown = sum(unknown_flag * Administered_Dose1)
            , n_vaxxed = sum(Administered_Dose1 * (known_flag))
  ) %>%
  mutate(pct_vaxxed_over18 = (n_vaxxed_over18 + n_vaxxed_unknown * (n_vaxxed_over18 / (n_vaxxed_over18 + n_vaxxed_under18)))/255200373)
cdc_demos

all_cdc_types <- bind_rows(all_cdc %>% mutate(download_type = 'natl') %>% select(-state, -download_num)
          , cdc_demos %>%
            rename(pct_pop_vaccinated = pct_vaxxed_over18
                   , n_pop_vaccinated = n_vaxxed_over18
                   ) %>%
            mutate(pop_total = 255200373, download_date = '2021-12-03', download_type = 'demos') %>%
            select(download_date, date, n_pop_vaccinated, pop_total, pct_pop_vaccinated, download_date, download_type)
          )



plt_annot <- all_cdc_types %>%
  filter(date %in% seq(as.Date('2021-01-01'), as.Date('2021-07-01'), by = 15)
         , download_date %in% c("2021-05-26", '2021-12-03'))

all_cdc_types %>% filter(date < '2021-07-01', download_date %in% c("2021-05-26", '2021-12-03')) %>%
ggplot(aes(x = date, y = pct_pop_vaccinated, color = paste0(download_date, download_type))) +
  geom_line() +
  #geom_vline(xintercept = as.Date('2021-05-26'), lty = 2, color = 'gray40') +
  theme_pubr() +
  theme(panel.grid.major.x = element_line(color = "grey80")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = '1 month', date_labels = "%B") +
  labs(x='', y = '% first dose') +
  geom_text_repel(data = plt_annot
                  , aes(label = paste(round(pct_pop_vaccinated * 100, 1), '%')), size = 3)





# calculate error --------------------------------

all_polls <- read_csv('data/final/all_polls.csv') %>% filter(pct_error == 0, pop == 'US') %>% distinct()
all_polls_slim <- all_polls %>% select(mode, pop, start_date, end_date, pct_vaccinated)

all_polls_comp <- all_polls_slim %>%
  left_join(., all_cdc_types %>% filter(download_date %in% c("2021-05-26", '2021-12-03'))
            , by = c("end_date" = 'date'))


error_tab <- all_polls_comp %>%
  mutate(error = pct_vaccinated - pct_pop_vaccinated, study_pct_vaccinated = pct_vaccinated) %>%
  pivot_wider(id_cols = c('end_date', 'mode', 'study_pct_vaccinated')
              , names_from = c('download_date', 'download_type')
              , values_from = c('error')
              , names_prefix = 'error_CDC_')

write_csv(error_tab, 'data/error_by_CDC_date.csv')


annotate_dat <- error_tab %>%
  group_by(mode) %>%
  filter(`error_CDC_2021-05-26` == max(`error_CDC_2021-05-26`, na.rm = T))

ggplot(error_tab) +
  geom_pointline(aes(x = end_date, y = `error_CDC_2021-05-26_natl`, color = 'May data')) +
  geom_pointline(aes(x = end_date, y = `error_CDC_2021-12-03_natl`, color = 'Dec data')) +
  geom_pointline(aes(x = end_date, y = `error_CDC_2021-12-03_demos`, color = 'Dec data, demos')) +
  facet_wrap(~mode) +
  theme_pubr() +
  scale_y_continuous(labels = scales::percent) +
  geom_text_repel(data = annotate_dat, aes(x = end_date, y = `error_CDC_2021-05-26`, color = 'May data'
                      , label = paste0(round(`error_CDC_2021-05-26` * 100, 0), '%'))) +
  geom_text_repel(data = annotate_dat, aes(x = end_date, y = `error_CDC_2021-12-03`, color = 'Dec data'
                                           , label = paste0(end_date, ": ", round(`error_CDC_2021-12-03` * 100, 0), '%')))


error_tab %>% group_by(mode) %>% filter(`error_CDC_2021-12-03_demos` == max(`error_CDC_2021-12-03_demos`))

# by age ----------------------
cdc_by_age <- read_csv('data/CDC/cdc_by_age_2021-12-03')

ggplot(cdcdata_age %>% filter(date <= '2021-06-1'), aes(x = date, y = 1-pct_vax_under18)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  theme_pubr() +
  labs(x = '2021', y='% of Vaxxed Over 18') +
  geom_text_repel(data = cdcdata_age %>% filter(date == floor_date(date, 'month'), date <= '2021-06-01')
                  , aes(label = paste0(round(100 * (1-pct_vax_under18), 1), '%'))
                  , min.segment.length = 0)
ggsave('plots/fig_pct_vaxxed_over18.pdf', width = 6, height = 4)




