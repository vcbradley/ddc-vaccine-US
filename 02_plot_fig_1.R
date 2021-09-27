source('_setup.R')


benchmark_dates <- c("2021-04-21", "2021-05-05", "2021-05-26")


######### BENCHMARK DATA ###########
# read in benchmark data
bench_path <- path('data', 'final', glue('benchmark_{benchmark_dates}.csv'))
benchmark <- map_dfr(bench_path, ~ fread(.x)) %>%
  filter(state != 'US', !is.na(pct_pop_vaccinated)) %>%
  group_by(date) %>%
  mutate(rank = frank(-pct_pop_vaccinated))


######### POLL DATA ###########
# read in poll data
all_polls_sel <- fread(file.path('data', 'final', 'all_polls_all_vars.csv.gz')) %>%
  filter(pct_error == 0, pop != 'US') %>%
  # choose the polls we want to use -- lastest overlapping interval
  filter((mode == 'facebook' & end_date == '2021-03-27')
         | (mode == 'household_pulse' & end_date == '2021-03-29')
         | (mode == 'ipsos_axios' & end_date == '2021-03-22')
  )


# pivot and join on benchmark data
# for benchmark, use 3/27 (FB wave end date + 5 days of lag)
bench_sel <- benchmark %>%
  filter(date == as.Date('2021-03-27') + 5)  %>%
  select(pop = state,
         pct_vaccinated_pop = pct_pop_vaccinated,
         pop_rank = rank)

all_polls_plt <- all_polls_sel %>%
  pivot_wider(id_cols = c('pop'),
              values_from = c('pct_vaccinated', 'pct_hesitant', 'pct_willing'),
              names_from = c('mode')
  ) %>%
  left_join(bench_sel, by = c('pop')) %>%
  mutate(pop_fac = fct_reorder(pop, pop_rank, .desc = TRUE))





######## CALC DIFFS ##########
outcomes = c('vaccinated', 'hesitant', 'willing')

# calc differences
biggest_diffs = list()
biggest_rank_diffs = list()

for (m in outcomes) {

  all_polls_plt_m <- all_polls_plt %>%
    mutate('diff_{m}' := get(glue('pct_{m}_facebook')) - get(glue('pct_{m}_household_pulse'))) %>%
    arrange(get(glue('pct_{m}_facebook')), decreasing = TRUE) %>%
    mutate('fb_rank_{m}' := 1:n()) %>%
    arrange(get(glue('pct_{m}_household_pulse')), decreasing = TRUE) %>%
    mutate('hp_rank_{m}' := 1:n()) %>%
    mutate('rank_diff_{m}' := get(glue('fb_rank_{m}')) - get(glue('hp_rank_{m}')))

  biggest_diffs[[m]] <- all_polls_plt_m %>% arrange(abs(get(glue('diff_{m}'))), decreasing = T) %>% slice(1:5)
  biggest_rank_diffs[[m]] <- all_polls_plt_m %>% arrange(abs(get(glue('rank_diff_{m}'))), decreasing = T) %>% slice(1:5)
}
