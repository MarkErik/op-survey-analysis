FROM debian:bookworm-slim

ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=UTC

# ---- Core OS deps + CA certs + build toolchain ----
RUN apt-get update && apt-get install -y --no-install-recommends \
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

# ---- Add CRAN Debian repo and install R ----
RUN apt-get update && apt-get install -y --no-install-recommends \
            dirmngr \
         && rm -rf /var/lib/apt/lists/*
        
# Import CRAN Debian signing key (Johannes Ranke) and trust it for apt
RUN gpg --batch --keyserver keyserver.ubuntu.com \
              --recv-key '95C0FAF38DB3CCAD0C080A7BDC78B2DDEABC47B7' \
         && gpg --batch --armor --export '95C0FAF38DB3CCAD0C080A7BDC78B2DDEABC47B7' \
            > /etc/apt/trusted.gpg.d/cran_debian_key.asc
        
# Add CRAN repo for Debian bookworm and install R
RUN echo "deb http://cloud.r-project.org/bin/linux/debian bookworm-cran40/" \
            > /etc/apt/sources.list.d/cran.list \
         && apt-get update \
         && apt-get install -y --no-install-recommends r-base \
         && rm -rf /var/lib/apt/lists/*
        

# ---- Install R packages and fail if missing ----
RUN R -q -e "install.packages(c('shiny','shinyjs','DT','tidyverse','ggiraph'), repos='https://cloud.r-project.org/')" \
 && R -q -e "stopifnot(requireNamespace('shiny', quietly=TRUE))" \
 && R -q -e "stopifnot(requireNamespace('tidyverse', quietly=TRUE))"

# ---- App ----
WORKDIR /srv/shinyapp
COPY . /srv/shinyapp/

EXPOSE 7008
CMD ["R", "-q", "-e", "shiny::runApp('/srv/shinyapp', host='0.0.0.0', port=7008)"]
