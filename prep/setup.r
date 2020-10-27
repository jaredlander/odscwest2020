# make sure all packages are installed
install.packages(c(
    'tidyverse', 'tidymodels', 'ggplot2', 'vip', 'themis',
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
    'plumber',
    'parallel', 'doFuture', 'tictoc',
    'skimr', 'animation'
))

# download all data
piggyback::pb_download(repo='jaredlander/coursedata')
