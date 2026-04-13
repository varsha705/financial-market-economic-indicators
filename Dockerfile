FROM --platform=linux/amd64 rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shiny','shinydashboard','shinyWidgets','dplyr','readr','ggplot2','lubridate','randomForest','forecast','zoo'), repos='https://cran.r-project.org')"

RUN R -e "install.packages('xgboost', repos='https://cran.r-project.org')"

COPY app.R /srv/shiny-server/inflation-app/app.R
COPY data/processed/ /srv/shiny-server/inflation-app/data/processed/

RUN chmod -R 755 /srv/shiny-server/inflation-app/

EXPOSE 3838