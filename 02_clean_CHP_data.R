
source('_setup.R')


prepCHPmicrodata = function(chp_waves = 22:29
                    , overwrite_chp = F
){
  dest_dir = file.path('data', 'raw', 'census-household-pulse', 'microdata')

  # download data if overwrite_chp or if missing
  for(w in chp_waves){
    print(w)
    filename = paste0('HPS_Week',w,'_PUF_CSV')
    hp_url = paste0('https://www2.census.gov/programs-surveys/demo/datasets/hhp/2021/wk',w,'/',filename, '.zip')
    file_path = file.path(dest_dir, filename)

    if(overwrite_chp | !file.exists(file_path)){
      download.file(url = hp_url, destfile = paste0(file_path, '.zip'))
      unzip(paste0(file_path, '.zip'), exdir = file_path)
      file.remove(paste0(file_path, '.zip'))
    }
  }

  # read in data and stack
  hpvars = c('SCRAM', 'WEEK','EST_ST','RECVDVACC', 'DOSES', 'GETVACC', 'GETVACRV', 'PWEIGHT',
             'TBIRTH_YEAR', 'RRACE', 'RHISPANIC', 'EEDUC')
  hpdata = purrr::map_dfr(hp_weeks, function(w) {
    filename = file.path(dest_dir, paste0('HPS_Week',w,'_PUF_CSV/pulse2021_puf_',w,'.csv'))
    temp = fread(filename, select = hpvars)
    temp
  })

  # translate EST_ST to state name
  estcode_to_state = fread('data/raw/census-household-pulse/estcode_to_state.csv')
  hpdata <- left_join(hpdata, estcode_to_state, by = 'EST_ST')

  # calculate age
  hpdata <- hpdata %>% mutate(age = 2021 - TBIRTH_YEAR)

  # get start and end dates for each wave
  chp_wave_dates = fread('data/raw/census-household-pulse/chp_wave_dates.csv')
  hpdata <- left_join(hpdata, chp_wave_dates, by = c('WEEK' = 'wave_num')) %>%
    mutate(start_date = as.Date(as.character(start_date), format = '%Y%m%d')
           , end_date = as.Date(as.character(end_date), format = '%Y%m%d'))

  # get state abbrvs
  hpdata <- left_join(hpdata
                 , tibble(state_name = c(state.name, 'District of Columbia'), state = c(state.abb, 'DC'))
                 , by = 'state_name'
                 )

  # create some indicators
  hpdata <- hpdata %>%
    mutate(
      vaccinated = as.numeric(RECVDVACC == 1)
      , willing_def = as.numeric(GETVACC == 1 | GETVACRV == 1)
      , willing_prob = as.numeric(GETVACC == 2 | GETVACRV == 2)
      , hesitant_prob = as.numeric(GETVACC == 3 | GETVACRV == 4)   # question code switched from GETVACC to GETVACRV on wave 28 when they added 3 = unsure
      , hesitant_def = as.numeric(GETVACC == 4 | GETVACRV == 5)
      , unsure = as.numeric(RECVDVACC != 1 & (GETVACRV == 3 | GETVACC < 0 | GETVACRV < 0))  # refusals and explicit unsures coded as unsure
    ) %>%
    mutate_at(vars(vaccinated:unsure),~replace(., is.na(.), 0))
  #mutate(rowsum = vaccinated + willing_def + willing_prob + hesitant_prob + hesitant_def + unsure) %>%  # check that all respondents have a code
  #group_by(rowsum) %>% count()

  # write cleaned microdata to file
  readr::write_csv(hpdata,
                   file = glue('data/raw/census-household-pulse/microdata/chp_microdata_cleaned_waves{min(hpdata$WEEK)}to{max(hpdata$WEEK)}.csv'))

  # combine replicate weights and write to file
  hpdata_repwt = purrr::map_dfr(hp_weeks, function(w){
    filename = glue('data/raw/census-household-pulse/microdata/HPS_Week{w}_PUF_CSV/pulse2021_repwgt_puf_{w}.csv')
    fread(filename)
  })
  readr::write_csv(hpdata_repwt,
                   file = glue('data/raw/census-household-pulse/microdata/chp_rpwgts_cleaned_waves{min(hpdata_repwt$WEEK)}to{max(hpdata_repwt$WEEK)}_repwts.csv'))

  return(hpdata)
}



getHPTabColnames = function(cname, which = 'est'){
  cname_new = case_when(cname == "Select characteristics" ~ 'subgroup'
                        , cname == "Total" ~ 'pop_total'
                        , cname == "Received a COVID-19 vaccine_Yes_Total" ~ 'n_vaccinated'
                        , cname == "Received a COVID-19 vaccine_Yes_Received or plan to receive all required doses" ~ 'n_vaccinated_willgetalldoses'
                        , cname == "Received a COVID-19 vaccine_Yes_Have not received/do not plan to receive all required doses" ~ 'n_vaccinated_wontgettalldoses'
                        , cname == "Received a COVID-19 vaccine_Yes_Did not report" ~ 'n_vaccinated_DNKdoses'
                        , cname == "Received a COVID-19 vaccine_No_Total" ~ 'n_not_vaccinated'
                        , cname == "Received a COVID-19 vaccine_No_Will definitely get a vaccine" ~ 'n_willing_definitely'
                        , cname == "Received a COVID-19 vaccine_No_Will probably get a vaccine" ~ 'n_willing_probably'
                        , cname == "Received a COVID-19 vaccine_No_Unsure about getting a vaccine" ~ 'n_vaxunsure'
                        , cname == "Received a COVID-19 vaccine_No_Will probably not get a vaccine"  ~ 'n_hesitant_probably'
                        , cname == "Received a COVID-19 vaccine_No_Will definitely not get a vaccine" ~ 'n_hesitant_definitely'
                        , cname == "Received a COVID-19 vaccine_No_Did not report" ~ 'n_hesitant_DK'
                        , cname == "Received a COVID-19 vaccine_Did not report" ~ 'n_vaccinated_DK'
                        , TRUE ~ cname
  )

  if(which == 'SE'){
    cname_new = paste0(cname_new, ifelse(grepl('n_',cname_new) | cname_new == 'pop_total', '_SE', ''))
  }

  return(cname_new)
}




prepCHPtables = function(chp_waves = 22:29
                         , overwrite_chp = F
){

  dest_dir = file.path('data', 'raw', 'census-household-pulse', 'tables')

  ####### download census tables
  for(w in chp_waves){
    # toplines
    filename = paste0('health5_week', w,'.xlsx')
    file_path = file.path(dest_dir, filename)
    if(!file.exists(file_path) | overwrite_chp){
      hp_tab_url = paste0('https://www2.census.gov/programs-surveys/demo/tables/hhp/2021/wk',w,'/',filename)
      download.file(url = hp_tab_url, destfile = file_path)
    }

    # and standard errors
    filename_se = paste0('health5_se_week', w,'.xlsx')
    file_path_se = file.path(dest_dir, filename_se)
    if(!file.exists(file_path_se) | overwrite_chp){
      hp_tab_url_se = paste0('https://www2.census.gov/programs-surveys/demo/tables/hhp/2021/wk',w,'/',filename_se)
      download.file(url = hp_tab_url_se, destfile = file_path_se)
    }
  }


  ######## extract vax data from correct sheets and stack
  census_tables = list.files(dest_dir, pattern = 'health5_week')

  # for all tables
  all_tables_raw = list()
  all_tables_se_raw = list()
  for(t in census_tables){

    #get wave name
    wave_num = as.numeric(gsub('health5_week|[.]xlsx', '', t))

    # get sheetnames (each is a state / 'US')
    sheets = excel_sheets(path = file.path(dest_dir, t))
    sheets = sheets[!grepl('Metro_Area', sheets)]

    # iterate through sheets
    for(s in sheets){
      for(type in c('est', 'se')){
        # read in data
        if(type == 'est'){
          path = file.path(dest_dir, t)
        }else{
          path = file.path(dest_dir, gsub('health5','health5_se',t))
        }
        tmp = suppressMessages(read_excel(path, skip = 3, sheet = s, na = ''))

        # get colnames -- same in each file, pull from estimates
        colnames = as.vector(apply(tmp, 2, function(c){
          c = c[1:3]  #make col names from first 3 rows
          c = c[!(is.na(c) | c == 'NA')] #drop NAs
          paste(c, collapse = '_')
        }))

        # setcolnames and drop extra rows
        setnames(tmp, colnames)

        # do remaining cleaning
        tmp <- tmp %>%
          slice(5:n()) %>%
          mutate(demo = ifelse(is.na(Total), `Select characteristics`, NA), .before = `Select characteristics`) %>%
          fill(demo) %>%
          mutate(demo = ifelse(is.na(demo), 'Total', demo), pop = s, wave = wave_num) %>%
          filter(!(is.na(Total) | grepl('^[*]',`Select characteristics`)))

        if(type == 'est'){
          all_tables_raw[[paste0(t,'_', s)]] <- tmp
        }else{
          all_tables_se_raw[[paste0(t,'_', s)]] <- tmp
        }
      }
    }
  }
  # bind together
  all_tables = rbindlist(all_tables_raw, fill = T)
  all_tables_se = rbindlist(all_tables_se_raw, fill = T)

  setnames(all_tables, old = names(all_tables), new = getHPTabColnames(names(all_tables), which = 'est'))
  setnames(all_tables_se, old = names(all_tables_se), new = getHPTabColnames(names(all_tables_se), which = 'SE'))

  # merge
  all_tables_full <- full_join(all_tables, all_tables_se, by = c('demo', 'subgroup', 'pop','wave')) %>%
    relocate(pop, wave, .before = demo) %>%
    mutate_at(vars(pop_total:n_vaxunsure_SE), as.numeric)


  # get start and end dates for each wave
  chp_wave_dates = fread('data/raw/census-household-pulse/chp_wave_dates.csv')
  all_tables_full <- left_join(all_tables_full, chp_wave_dates
                               , by = c('wave' = 'wave_num')) %>%
    mutate(start_date = as.Date(as.character(start_date), format = '%Y%m%d')
           , end_date = as.Date(as.character(end_date), format = '%Y%m%d'))


  # calc percentages and clean
  all_tables_full <- all_tables_full %>% mutate(
    wave = as.numeric(wave)

    # calc percentages
    , pct_vaccinated = n_vaccinated / pop_total
    , pct_willing_definitely = n_willing_definitely / pop_total
    , pct_willing_probably = n_willing_probably / pop_total
    , pct_hesitant_definitely = n_hesitant_definitely / pop_total
    , pct_hesitant_probably = n_hesitant_probably / pop_total
    , pct_vaxunsure = n_vaxunsure / pop_total

    #option not offered in earlier waves, so replace NAs with 0
    , pct_vaxunsure = ifelse(is.na(pct_vaxunsure), 0, pct_vaxunsure)
    , n_vaxunsure_SE = ifelse(is.na(n_vaxunsure_SE), 0, n_vaxunsure_SE)

    # calc net willing / hesitant
    , pct_willing = pct_willing_definitely + pct_willing_probably
    , pct_hesitant = pct_hesitant_definitely + pct_hesitant_probably + pct_vaxunsure

    , pct_haveorwillgetvax = pct_willing + pct_vaccinated   #unsure not uncluded in have or will get vax
  )

  write.csv(all_tables_full, file = file.path('data','raw','census-household-pulse', 'tables', paste0('chp_tables_cleaned_waves',min(chp_waves), 'to', max(chp_waves),'.csv')))

  return(all_tables_full)

}



prepCHPcombined = function(chp_waves = 22:29, overwrite_chp = F){

  # prep microdata and tables
  chp_microdata = prepCHPmicrodata(chp_waves = chp_waves, overwrite_chp = overwrite_chp)
  chp_tables = prepCHPtables(chp_waves = chp_waves, overwrite_chp = overwrite_chp)


  # aggregate microdata
  chp_microdata_agg <- bind_rows(chp_microdata %>% mutate(pop = 'US')
            , chp_microdata %>% mutate(pop = state)
            ) %>%
    group_by(pop, wave = WEEK, start_date, end_date) %>%
    summarize(n = n()
           , n_weighted = sum(PWEIGHT)
           , weight_mean = mean(PWEIGHT)
           , weight_var = var(PWEIGHT)
           , CV = sqrt(var(PWEIGHT/mean(PWEIGHT)))
           , deff = 1 + CV^2

           , pct_vaccinated_raw = mean(vaccinated)
           , pct_vaccinated = sum(vaccinated * PWEIGHT)/sum(PWEIGHT)

           , pct_willing_definitely_raw = mean(willing_def)
           , pct_willing_probably_raw = mean(willing_prob)
           , pct_hesitant_probably_raw = mean(hesitant_prob)
           , pct_hesitant_definitely_raw = mean(hesitant_def)
           , pct_vaxunsure_raw = mean(unsure)

           , pct_willing_raw = pct_willing_definitely_raw + pct_willing_probably_raw
           , pct_hesitant_raw = pct_hesitant_definitely_raw + pct_hesitant_probably_raw

           , pct_willing_definitely = sum(willing_def * PWEIGHT)/sum(PWEIGHT)
           , pct_willing_probably = sum(willing_prob * PWEIGHT)/sum(PWEIGHT)
           , pct_hesitant_probably = sum(hesitant_prob * PWEIGHT)/sum(PWEIGHT)
           , pct_hesitant_definitely = sum(hesitant_def * PWEIGHT)/sum(PWEIGHT)
           , pct_vaxunsure = sum(unsure * PWEIGHT)/sum(PWEIGHT)

           , pct_willing = pct_willing_definitely + pct_willing_probably
           , pct_hesitant = pct_hesitant_definitely + pct_hesitant_probably + pct_vaxunsure

           , pct_haveorwillgetvax = pct_vaccinated + pct_willing

           # calc standard errors
           , pct_vaccinated_raw_se = sqrt(pct_vaccinated_raw * (1 - pct_vaccinated_raw) / n)
           , pct_vaccinated_se = sqrt(pct_vaccinated * (1 - pct_vaccinated) * deff / n)
           , pct_hesitant_se = sqrt(pct_hesitant * (1 - pct_hesitant) * deff / n)
           , pct_willing_se = sqrt(pct_willing * (1 - pct_willing) * deff / n)
           , .groups = 'drop')

  # check all rows are there
  # chp_microdata_agg %>% group_by(pop) %>% summarize(n(), sum(n)) %>% print(n=100)

  chp_microdata_agg <- chp_microdata_agg %>%
    # add these columns for merging with data from tables
    mutate(source = 'microdata'
           , demo = 'Total'
           , subgroup = 'Total'
           , mode = 'household_pulse')

  # check that we're not miscategorizing
  # chp_microdata_agg %>%
  #   summarize(sum = pct_vaccinated + pct_willing_definitely + pct_willing_probably + pct_hesitant_probably + pct_hesitant_definitely + pct_vaxunsure) %>%
  #   group_by(sum)%>% count()
  # chp_microdata_agg %>%
  #   summarize(sum = pct_vaccinated_raw + pct_willing_definitely_raw + pct_willing_probably_raw + pct_hesitant_probably_raw + pct_hesitant_definitely_raw + pct_vaxunsure_raw) %>%
  #   group_by(sum)%>% count()

  #### prep tables -- grab n, CV, and deff from microdata (they're not included)
  chp_tables <- chp_tables %>%
    mutate(source = 'tables', mode = 'household_pulse') %>%
    left_join(., chp_microdata_agg %>% select(pop, wave, subgroup, n, CV, deff), by = c('pop', 'wave', 'subgroup')) %>%
    mutate(
      MoE = 2 * sqrt(deff) * pct_vaccinated_se
      , pct_vaccinated_se = n_vaccinated_SE/pop_total
      , pct_hesitant_se = sqrt(n_hesitant_probably_SE^2 + n_hesitant_definitely_SE^2 + n_vaxunsure_SE^2)/pop_total
      , pct_willing_se = sqrt(n_willing_probably_SE^2 + n_willing_definitely_SE^2)/pop_total
    )


  # compare tables and microdata ests -- check that numbers match to 3 decimals
  full_join(
    chp_tables %>% select(wave, pop, pct_vaccinated_tab = pct_vaccinated, pct_willing_tab = pct_willing, pct_hesitant_tab = pct_hesitant)
    , chp_microdata_agg %>% select(wave, pop, pct_vaccinated_micro = pct_vaccinated, pct_willing_micro = pct_willing, pct_hesitant_micro = pct_hesitant)
  , by = c('wave', 'pop')) %>%
    mutate(pct_vaccinated = trunc(pct_vaccinated_tab, 3) == trunc(pct_vaccinated_micro, 3)
           , pct_willing = trunc(pct_willing_tab, 3) == trunc(pct_willing_micro, 3)
           , pct_hesitant = trunc(pct_hesitant_tab, 3) == trunc(pct_hesitant_micro, 3)
           ) %>%
    summarize(mean(pct_vaccinated), mean(pct_willing), mean(pct_hesitant))


  # write out cleaned, final CHP data
  write.csv(chp_tables, glue('data/final/chp_cleaned_waves{min(chp_waves)}to{max(chp_waves)}.csv'))

}


######### RUN CLEAN CHP DATA #########
prepCHPcombined(chp_waves = 22:29)

