

source('_setup.R')

# source helper functions for cleaning data
source('functions/functions_clean_CDC.R')
source('functions/functions_clean_CHP.R')
source('functions/functions_clean_FB.R')
source('functions/functions_clean_others.R')


######### PREP BENCHMARK DATA #########
### Loop through all dates with benchmark data in the repo
benchmark_dates = c( '2021-04-21'
                     , '2021-05-05'
                     , '2021-05-26')

for(b in benchmark_dates){
  # have to download CDC locally and specify paths
  cdc_path = file.path('data', 'raw', 'CDC'
                       , paste0('trends_in_number_of_covid19_vaccinations_in_the_us_',b,'.csv'))
  cdc_age_path = file.path('data', 'raw', 'CDC'
                           , paste0('demographic_trends_of_people_receiving_covid19_vaccinations_in_the_united_states_',b,'.csv'))

  getBenchmark(b, cdc_path = cdc_path, cdc_age_path = cdc_age_path)
}

######### PULL IN BENCHMARK DATA #########
#pull in the one we're going to use
which_benchmark = '2021-05-26'
benchmark <- fread(file.path('data','final', glue('benchmark_{which_benchmark}.csv')))


#### RUN CLEAN CHP DATA #####
chp_waves = 22:29
prepCHPcombined(chp_waves = chp_waves)


#### RUN CLEAN FACEBOOK ######
cleanFBdata()


#### RUN CLEAN IPSOS-AXIOS ######
cleanIPSOSdata()


######### PULL IN POLL DATA #########
poll_vars = c('mode', 'wave', 'pop', 'start_date', 'end_date', 'n'
              , 'pct_haveorwillgetvax', 'pct_vaccinated', 'pct_hesitant', 'pct_willing'
              , 'pct_vaccinated_se', 'pct_hesitant_se', 'pct_willing_se'
              , 'MoE', 'deff', 'source')

chpdata <- fread(file.path('data','final', glue('chp_cleaned_waves{min(chp_waves)}to{max(chp_waves)}.csv')), select = poll_vars) %>%
  mutate(study_name = 'Census Household Pulse')
fbdata <- fread(file.path('data','final', 'fb_cleaned.csv'), select = poll_vars) %>%
  mutate(study_name = 'Delphi-Facebook')
ipsos <- fread(file.path('data','final', 'axios_ipsos_cleaned.csv'), select = poll_vars) %>%
  mutate(study_name = 'Axios-Ipsos')


# stack polls
all_polls = bind_rows(fbdata, chpdata, ipsos)


# merge in benchmark
all_polls <- left_join(all_polls %>% rename(source_poll = source)
                        , benchmark %>% filter(!(state == 'US' & source == 'OWID')) %>%  #drop national-level OWID data
                          select(-n_pop_vaccinated_imputedflag)
                        , by = c('end_date' = 'date', 'pop' = 'state'))







