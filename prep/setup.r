# make sure all packages are installed
install.packages(c(
    'tidyverse', 'tidymodels',
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
    'piggyback'
))

# download all data
piggyback::pb_download(repo='jaredlander/coursedata')
