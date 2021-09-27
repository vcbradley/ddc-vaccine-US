source('_setup.R')
source('functions/functions_plots.R')


######### BENCHMARK DATA ###########

# which version of the benchmark to use
benchmark_date = '2021-05-26'

# read in benchmark data
benchmark <- fread(file.path('data', 'final', glue('benchmark_{benchmark_date}.csv'))) %>%
  filter(state != 'US', !is.na(pct_pop_vaccinated)) %>%
  group_by(date) %>%
  mutate(pop_rank_vaccinated = frank(-pct_pop_vaccinated))

# ggplot(benchmark %>% filter(date >= '2021-03-22' & date < '2021-04-01')) +
#   geom_line(aes(x = as.Date(date), y = rank, color = state)) +
#   annotate('text', x = as.Date('2021-03-22'),
#            y = benchmark %>% filter(date == '2021-03-22') %>% pull(rank),
#            label =  benchmark %>% filter(date == '2021-03-22') %>% pull(state)) +
#   guides(color = FALSE)



######### POLL DATA ###########
# read in poll data
all_polls_plt <- fread(file.path('data', 'final', 'all_polls_all_vars.csv')) %>%
  filter(pct_error == 0, pop != 'US') %>%
  # choose the polls we want to use -- lastest overlapping interval
  filter((mode == 'facebook' & end_date == '2021-03-27')
         | (mode == 'household_pulse' & end_date == '2021-03-29')
         | (mode == 'ipsos_axios' & end_date == '2021-03-22')
  )


# pivot and join on benchmark data
all_polls_plt <- all_polls_plt %>%
  pivot_wider(id_cols = c('pop')
              , values_from = c('pct_vaccinated', 'pct_hesitant', 'pct_willing')
              , names_from = c('mode')
  ) %>%
  left_join(.
            , benchmark %>%
              # for benchmark, use 3/27 (FB wave end date + 5 days of lag)
              filter(date == as.Date('2021-03-27') + 5)  %>%
              select(pop = state, pct_vaccinated_pop = pct_pop_vaccinated, pop_rank_vaccinated)
            , by = c('pop'))

all_polls_plt <- all_polls_plt %>%
  mutate(pop_fac = factor(pop
                          , levels = all_polls_plt %>% arrange(pop_rank_vaccinated, decreasing = T) %>% pull(pop)
                          )
         )



######## CALC DIFFS ##########
outcomes = c('vaccinated', 'hesitant', 'willing')

# calc differences
biggest_diffs = list()
biggest_rank_diffs = list()

for (m in outcomes) {
  all_polls_plt <- all_polls_plt %>%
    mutate('diff_{m}' := get(glue('pct_{m}_facebook')) - get(glue('pct_{m}_household_pulse'))) %>%
    arrange(desc(get(glue('pct_{m}_facebook')))) %>%
    mutate('fb_rank_{m}' := 1:n()) %>%
    arrange(desc(get(glue('pct_{m}_household_pulse')))) %>%
    mutate('hp_rank_{m}' := 1:n()) %>%
    mutate('rank_diff_{m}' := get(glue('fb_rank_{m}')) - get(glue('hp_rank_{m}')))

  biggest_diffs[[m]] <- all_polls_plt %>% arrange(abs(get(glue('diff_{m}'))), decreasing = T) %>% slice(1:5)
  biggest_rank_diffs[[m]] <- all_polls_plt %>% arrange(abs(get(glue('rank_diff_{m}'))), decreasing = T) %>% slice(1:5)
}


ggplot(all_polls_plt) +
  geom_point(aes(x= pct_vaccinated_facebook, y = fb_rank_vaccinated)) +
  geom_point(aes(x= pct_vaccinated_household_pulse, y = hp_rank_vaccinated)) +
  geom_point(aes(x= pct_vaccinated_pop, y = pop_rank_vaccinated))


############# MAKE PLOTS ###########

# which states to annotate in each panel
show_states = c('MO', 'IN', 'MA')

#labels
labels = list(fb = 'Delphi-Facebook', hp = 'Census Household Pulse', pop = 'CDC')

# make plot
plot_comp = makeCompPlot(df = all_polls_plt, show_states = show_states, labels = labels)

# save
ggsave(plot_comp, filename = 'plots/fig_which_to_trust.png', height = 12, width = 10)

