## set up
pkg.list = c('data.table', 'ggplot2', 'gridExtra'
             , 'ggpubr', 'grid', 'scales'
             , 'knitr', 'kableExtra'
             , 'curl', 'dplyr', 'R.utils'
             , 'tidyr', 'lemon', 'readxl'
             , 'survey', 'lubridate'
             , 'glue', 'patchwork', 'ggrepel'
             , 'rjson',  'imputeTS')


for (p in pkg.list){
  loaded = require(p, character.only = T)
  if(!loaded){
    cat(paste0('Installing package: ', p))
    install.packages(p)
    library(p, character.only = T)
  }
}


# set color scales
scale_values = c('CDC (benchmark)' = 'gray'#'#946eb7' #purple
                 , 'Delphi-Facebook'= '#4891dc' #blue
                 , 'Census Household Pulse' = '#69913b' #green
                 , 'Axios-Ipsos' = '#cf7a30' #orange
                 #, '#A51C30' #red
)


hpwaves = list(data.frame(wave_num = 22, start_date = '20210106', end_date = '20210118')
               , data.frame(wave_num = 23, start_date = '20210120', end_date = '20210201')
               , data.frame(wave_num = 24, start_date = '20210203', end_date = '20210215')
               , data.frame(wave_num = 25, start_date = '20210217', end_date = '20210301')
               , data.frame(wave_num = 26, start_date = '20210303', end_date = '20210315')
               , data.frame(wave_num = 27, start_date = '20210317', end_date = '20210329')
               , data.frame(wave_num = 28, start_date = '20210414', end_date = '20210426')
               , data.frame(wave_num = 29, start_date = '20210428', end_date = '20210510')
               , data.frame(wave_num = 30, start_date = '20210512', end_date = '20210524')
)
hpwaves = rbindlist(hpwaves)
hpwaves[, start_date := as.Date(start_date, format = '%Y%m%d')]
hpwaves[, end_date := as.Date(end_date, format = '%Y%m%d')]

