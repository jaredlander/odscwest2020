# make sure all packages are installed
install.packages(c(
    'tidyverse', 'tidymodels', 'ggplot2', 'vip',
    'coefplot', 'ggridges',
    'ggthemes',
    'feasts', 'tsibble', 'fable', 'fpp3', 'dygraphs', 'timetk',
    'rmarkdown', 'flexdashboard',
    'glmnet', 'xgboost', 'DiagrammeR',
    'RSQLite', 'DBI', 'dbplyr',
    'leaflet', 'DT', 'threejs', 'crosstalk', 
    'here',
    'devtools',
    'future', 
    'piggyback',
    'parallel', 'doFuture', 'tictoc'
))

# download all data
piggyback::pb_download(repo='jaredlander/coursedata')
