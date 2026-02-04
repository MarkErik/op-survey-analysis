FROM r-base:4.3.3

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

# ---- System libs needed by tidyverse / ggiraph / shiny ----
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
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

# ---- Use Posit Package Manager for binary packages ----
ENV RSPM=https://packagemanager.posit.co/cran/__linux__/bookworm/latest

# ---- Install R packages (FAST: binaries, not source) ----
RUN R -q -e "install.packages( \
    c('shiny','shinyjs','DT','tidyverse','ggiraph'), \
    repos=Sys.getenv('RSPM') \
  )" \
 && R -q -e "stopifnot(requireNamespace('shiny', quietly=TRUE))" \
 && R -q -e "stopifnot(requireNamespace('tidyverse', quietly=TRUE))"

# ---- App ----
WORKDIR /srv/shinyapp
COPY . /srv/shinyapp/

EXPOSE 7008
CMD ["R", "-q", "-e", "shiny::runApp('/srv/shinyapp', host='0.0.0.0', port=7008)"]
