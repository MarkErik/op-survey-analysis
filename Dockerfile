# Use a base image that supports multi-architecture builds
FROM r-base:4.3.3

RUN apt-get update && apt-get install -y --no-install-recommends \
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

# Install R packages needed for the app
RUN R -e "install.packages(c('shiny', 'shinyjs', 'DT', 'tidyverse', 'ggiraph'), repos='https://cloud.r-project.org/')"

# Create app directory
WORKDIR /srv/shinyapp

# Copy app files
COPY . /srv/shinyapp/

# Expose port
EXPOSE 7008

# Run the Shiny app as root
CMD ["R", "-e", "shiny::runApp('/srv/shinyapp', host='0.0.0.0', port=7008)"]