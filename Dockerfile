FROM rocker/shiny-verse:4.5

RUN apt-get update && apt-get install -y \
      ca-certificates \
      curl \
      gnupg \
      build-essential \
      gfortran \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libpng-dev \
      libjpeg-dev \
      libtiff-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libicu-dev \
      && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('shinyjs', 'DT', 'ggiraph'))"

COPY . /srv/shiny-server/

EXPOSE 3838