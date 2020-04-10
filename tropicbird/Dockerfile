FROM rocker/tidyverse:3.6.2
# System dependencies for rgdal and data base files
RUN apt-get update && \
  apt-get install -y --no-install-recommends libjpeg-dev
# Needed R packages
RUN R -e "install.packages(c('Hmisc', 'geosphere', 'foreach', 'doMC', 'maptools'))"
# RUN R -e "install.packages(c('foreach', 'grImport', 'smatr', 'ggfortify', 'oce', 'Hmisc', 'rgdal', 'geosphere', 'unmarked', 'inline', 'checkmate', 'cowplot', 'drake'), repos = c(CRAN = 'https://mran.revolutionanalytics.com/snapshot/2019-03-10'))"
