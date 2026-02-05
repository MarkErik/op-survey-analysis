# For x86-based systems
FROM rocker/shiny-verse:4.5

# For Apple Silicon
#FROM --platform=linux/amd64 rocker/shiny-verse:4.5


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
RUN R -e "install.packages(c('shinyjs', 'DT', 'ggiraph'))"


# ---- App ----
COPY . /srv/shiny-server/

# Expose port 8080 (ShinyProxy default)
EXPOSE 8080

# Configure Shiny Server to listen on port 8080
RUN sed -i 's/listen 3838/listen 8080/g' /etc/shiny-server/shiny-server.conf