### compare all_polls from new and old project repos

new = fread(file = file.path('data', 'final', 'all_polls_all_vars.csv.gz'))
old = fread(file  = '~/github/ddi-covid-studies/all_polls_with_ddc_benchunc.csv')


new
old


comp = full_join(new %>% select(end_date, pop, mode, pct_error
                         , n, pct_vaccinated, pct_hesitant, pct_willing
                         , pct_vaccinated_se
                         , deff, MoE, pct_pop_vaccinated, sd_G
                         , ddc,ddc_weighted, n_eff, n_eff_star, pct_reduction_n_eff
                         ) %>% pivot_longer(cols = n:pct_reduction_n_eff, values_to = 'new')
          , old %>% select(end_date, pop, mode, pct_error
                           , n, pct_vaccinated, pct_hesitant, pct_willing
                           , pct_vaccinated_se
                           , deff, MoE, pct_pop_vaccinated, sd_G
                           , ddc,ddc_weighted, n_eff, n_eff_star, pct_reduction_n_eff
          ) %>% pivot_longer(cols = n:pct_reduction_n_eff, values_to = 'old')
          , by = c('end_date', 'pop', 'mode', 'pct_error', 'name'))



comp %>%
  filter(mode %in% c('facebook', 'household_pulse', 'ipsos_axios'), pop == 'US') %>%
  ggplot(aes(x = new, y = old, color = mode)) +
  geom_point() +
  geom_abline(slope= 1, intercept = 0) +
  facet_wrap(~name, scales = 'free')


comp %>%
  filter(is.na(new) & !is.na(old), mode %in% c('facebook', 'household_pulse', 'ipsos_axios')) %>%
  group_by(mode, pop, end_date) %>%
  count()

