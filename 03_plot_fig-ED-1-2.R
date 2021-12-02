source('_setup.R')
source('functions/functions_plots.R')


# which version of the benchmark to use
benchmark_version = '2021-05-26'

# survey dates
which_waves <- list(facebook = '2021-03-27'
                    , household_pulse = '2021-03-29'
                    , ipsos_axios = '2021-03-22'
                    , benchmark = '2021-03-31'   # end of max wave plus 5 days for reporting lag
)

# which_waves <- list(facebook = '2021-05-08'
#                     , household_pulse = '2021-05-10'
#                     , ipsos_axios = '2021-05-10'
#                     , benchmark = '2021-05-15'   # end of max wave plus 5 days for reporting lag
# )


######### BENCHMARK DATA ###########
# read in benchmark data
bench_path <- path('data', 'final', glue('benchmark_{benchmark_version}.csv'))
benchmark <- map_dfr(bench_path, ~ fread(.x)) %>%
  filter(state != 'US', !is.na(pct_pop_vaccinated)) %>%
  group_by(date) %>%
  mutate(rank_vaccinated_pop = frank(-pct_pop_vaccinated))




######### POLL DATA ###########
# read in poll data
all_polls <- fread(file.path('data', 'final', 'all_polls_all_vars.csv.gz'))

all_polls_sel <- all_polls %>%
  filter(pct_error == 0, pop != 'US') %>%
  # choose the polls we want to use -- latest overlapping interval
  filter((mode == 'facebook' & end_date == which_waves$facebook)
         | (mode == 'household_pulse' & end_date == which_waves$household_pulse)
        # | (mode == 'ipsos_axios' & end_date == which_waves$ipsos_axios)
         )




# pivot and join on benchmark data
# for benchmark, use 3/27 (FB wave end date + 5 days of lag)
bench_sel <- benchmark %>%
  filter(date == as.Date(which_waves$benchmark))  %>%
  select(pop = state
         , pct_vaccinated_pop = pct_pop_vaccinated
         , rank_vaccinated_pop)

all_polls_plt <- all_polls_sel %>%
  filter(pct_error == 0, pop != 'US') %>%
  group_by(end_date, mode) %>%
  mutate(rank_vaccinated = frank(-pct_vaccinated)
         , rank_hesitant = frank(-pct_hesitant)
         , rank_willing = frank(-pct_willing)
  )  %>% pivot_wider(id_cols = c('pop'),
                values_from = c('pct_vaccinated', 'pct_hesitant', 'pct_willing', 'rank_vaccinated', 'rank_hesitant', 'rank_willing'),
                names_from = c('mode')
  ) %>%
  left_join(bench_sel, by = c('pop')) %>%
  mutate(pop_fac = fct_reorder(pop, rank_vaccinated_pop, .desc = TRUE))



# check ranking is working correctly
ggplot(all_polls_plt) +
  geom_point(aes(x= pct_vaccinated_facebook, y = rank_vaccinated_facebook), color = 'blue') +
  geom_point(aes(x= pct_vaccinated_household_pulse, y = rank_vaccinated_household_pulse), color = 'green') +
  geom_point(aes(x= pct_vaccinated_pop, y = rank_vaccinated_pop), color = 'gray')


############# MAKE PLOTS ###########

# which states to annotate in each panel
show_states = c('MO', 'IN', 'MA')
all_polls_plt %>%
  filter(pop %in% show_states) %>%
  select(pop, date, rank_vaccinated_pop, rank_vaccinated_household_pulse, rank_vaccinated_facebook)

#labels
labels = list(fb = 'Delphi-Facebook', hp = 'Census Household Pulse', pop = 'CDC')

# make plot
plot_comp = makeCompPlot(df = all_polls_plt, show_states = show_states, labels = labels)

fig.lab = glue("Waves used: CDC {format(as.Date(which_waves$benchmark), format =  '%m/%d/%Y')}, Facebook-Delphi {format(as.Date(which_waves$facebook), format =  '%m/%d/%Y')}, Census Household Pulse {format(as.Date(which_waves$household_pulse), format =  '%m/%d/%Y')}")
plot_comp_annotated <- plot_comp + plot_annotation(caption = fig.lab)

# save
ggsave(plot_comp_annotated
       , filename = glue('plots/fig_which_to_trust_{as.Date(which_waves$benchmark)}.jpeg')
       , height = 24 * 1.25
       , width = 18 * 1.25
       , units = 'cm')


## rank correlations
all_polls_plt %>%
  summarise(cor_rank_vaccinated = cor(pct_vaccinated_facebook, pct_vaccinated_household_pulse, method = 'kendall')
          , cor_rank_hesitant = cor(pct_hesitant_facebook, pct_hesitant_household_pulse, method = 'kendall')
          , cor_rank_willing = cor(pct_willing_facebook, pct_willing_household_pulse, method = 'kendall')
          , cor_rank_vaccinated_fb_pop = cor(pct_vaccinated_facebook, pct_vaccinated_pop, method = 'kendall')
          , cor_rank_vaccinated_hp_pop = cor(pct_vaccinated_household_pulse, pct_vaccinated_pop, method = 'kendall')
            )
