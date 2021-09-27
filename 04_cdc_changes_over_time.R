source('_setup.R')

# get names of all benchmark files
files = list.files('data/final', pattern = 'benchmark_', full.names = T)

# read in and combine
cdc_snapshots = rbindlist(lapply(files, function(f){
  df <- fread(f) %>%
    filter(source == 'CDC_historical') %>%
    mutate(download_date = max(date))
}), fill = T)

# subset most recent snapshot
cdc_most_recent <- cdc_snapshots %>%
  filter(download_date == max(download_date)) %>%
  select(date, most_recent_est = pct_pop_vaccinated)

# subset first snapshot
cdc_first <- cdc_snapshots %>%
  filter(download_date == min(download_date)) %>%
  select(date, first_est = pct_pop_vaccinated)

# join most recent and first onto snapshots
cdc_snapshots <- left_join(cdc_snapshots, cdc_most_recent
            , by = 'date') %>%
  left_join(., cdc_first, by = 'date') %>%
  mutate(pct_of_most_recent = pct_pop_vaccinated / most_recent_est
         , pct_of_first = pct_pop_vaccinated / first_est
         , date = as.Date(date)
         , download_date = as.Date(download_date)
         )


# quick comparison plot
ggplot(cdc_snapshots, aes(x = as.Date(date), y = pct_pop_vaccinated, color = as.character(download_date))) +
  geom_point() + labs(x = 'Date', title = 'Comparison of CDC historical benchmark data') +
  theme_pubclean()

ggplot(cdc_snapshots, aes(x = as.Date(date), y = pct_of_first, color = as.character(download_date))) +
  geom_point() + labs(x = 'Date', title = 'Comparison of CDC historical benchmark data') +
  theme_pubclean()


#################

# grab the 10 days leading up to 4/12 for each download date
cdc_snapshots_subset <- cdc_snapshots %>%
  filter(as.Date(date) > min(as.Date(download_date)) - 10, as.Date(date) <= min(as.Date(download_date))) %>%
  mutate(days_of_data = as.numeric(min(download_date) - as.Date(date)) + 1
         , days_since_first_report = as.numeric(as.Datedownload_date - min(download_date))
         , pct_increase = pct_of_first - 1
         )



all_cdc_benchmark_subset = all_cdc_benchmark[as.Date(date) > min(download_date) - 10 & as.Date(date) <= min(download_date)]
all_cdc_benchmark_subset[, days_of_data :=  as.numeric(min(download_date) - as.Date(date)) + 1]
all_cdc_benchmark_subset[, days_since_first_report := as.numeric(download_date - min(download_date)) ]
all_cdc_benchmark_subset[, pct_increase := pct_of_first - 1]

annotate = all_cdc_benchmark_subset[days_since_first_report == max(days_since_first_report)]

all_cdc_benchmark_subset %>%
  as_tibble() %>%
  ggplot(aes(x = days_since_first_report, y = pct_increase)) +
  geom_pointline(aes(group = days_of_data), alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  geom_text_repel(
    data = as_tibble(annotate) %>% slice(c(1, 5, 8, 9, 10)),
    aes(x = days_since_first_report,
        y = pct_increase,
        label = glue("{format.Date(date, '%b %d')} ({days_of_data} days of data)")),
    segment.alpha = 0.5,
    nudge_x = 8,
    size = 2.5
  ) +
  theme_pubr() +
  # theme(legend.position = 'right') +
  expand_limits(x = 60) +
  labs(
    x = "Days since first report",
    y = "Increase\nin reported vaccine uptake",
    color = 'Days of data (as of April 12)'
  )

ggsave('plots/plt_benchmark_change.png',
       width = 5, height = 2.5)




