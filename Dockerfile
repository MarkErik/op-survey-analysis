FROM rocker/shiny-verse:4.5

# ---- Install R packages ----
RUN R -e "install.packages(c('DT', 'ggiraph'), repos='https://cloud.r-project.org/')"

# ---- App ----
WORKDIR /srv/shinyapp
COPY . /srv/shinyapp/

EXPOSE 7008
CMD ["R", "-q", "-e", "shiny::runApp('/srv/shinyapp', host='0.0.0.0', port=7008)"]
