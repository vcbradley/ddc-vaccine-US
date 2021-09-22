

cleanIPSOSdata = function(){
  ipsos = fread(file.path('data/raw/axios-ipsos/axios_ipsos_full.csv'))

  ipsos <- ipsos %>%
    mutate(source = 'toplines'
           , pct_hesitant = 1 - pct_haveorwillgetvax
           , pct_willing = pct_haveorwillgetvax - pct_vaccinated
           , pct_vaccinated_se = sqrt(deff * pct_vaccinated * (1 - pct_vaccinated)/n)
           , pct_hesitant_se = sqrt(deff * pct_hesitant * (1 - pct_hesitant)/n)
           , pct_willing_se = sqrt(deff * pct_willing * (1 - pct_willing)/n)
           )

  write.csv(ipsos, file = file.path('data/final/axios_ipsos_cleaned.csv'))
}


