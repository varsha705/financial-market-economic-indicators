FROM openanalytics/r-shiny

RUN R -e "install.packages(c('shinydashboard','shinyWidgets','dplyr','readr','ggplot2','lubridate','randomForest','xgboost','forecast','zoo'), repos='https://cran.r-project.org')"

COPY app.R /srv/shiny-server/app.R
COPY data/processed/ /srv/shiny-server/data/processed/

EXPOSE 3838