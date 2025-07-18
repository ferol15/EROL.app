FROM rocker/verse:latest

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libxml2-dev \
    libssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

RUN echo "options(repos = c(CRAN = 'https://packagemanager.rstudio.com/cran/__linux__/focal/latest'))" >> /usr/local/lib/R/etc/Rprofile.site

RUN Rscript -e 'install.packages(c(\
    "shiny", \
    "shinydashboard", \
    "DT", \
    "plotly", \
    "haven", \
    "foreign", \
    "dplyr", \
    "ggplot2", \
    "corrplot", \
    "broom", \
    "knitr", \
    "kableExtra", \
    "fresh", \
    "later", \
    "psych", \
    "GPArotation", \
    "lavaan", \
    "semPlot", \
    "viridis", \
    "networkD3", \
    "igraph", \
    "reshape2", \
    "RColorBrewer", \
    "lavaanPlot", \
    "scales", \
    "magrittr", \
    "sandwich", \
    "lmtest", \
    "performance", \
    "htmltools" \
), dependencies = TRUE)'

COPY . /app

WORKDIR /app

EXPOSE 7860

CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 7860)"]