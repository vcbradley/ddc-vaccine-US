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


