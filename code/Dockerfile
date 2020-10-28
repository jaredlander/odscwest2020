FROM rstudio/plumber:next

LABEL maintainer="The ODSC R Crew"

RUN Rscript -e "install.packages(c('here', 'workflows', 'recipes', 'readr', 'jsonlite', 'xgboost'), repos='https://packagemanager.rstudio.com/all/__linux__/focal/latest', Ncpus=4)"

COPY mod0.rds .
COPY plumber.r .

EXPOSE 8000

CMD ["plumber.r"]
