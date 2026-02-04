FROM rocker/shiny-verse:4.5

# ---- Core OS deps + CA certs + build toolchain ----
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

# ---- Install R packages ----
RUN R -e "install.packages(c('shinyjs','DT', 'ggiraph'), repos='https://cloud.r-project.org/')"

# ---- App ----
WORKDIR /srv/shinyapp
COPY . /srv/shinyapp/

EXPOSE 7008
CMD ["R", "-q", "-e", "shiny::runApp('/srv/shinyapp', host='0.0.0.0', port=7008)"]
