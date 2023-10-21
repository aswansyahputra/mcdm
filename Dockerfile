FROM rocker/r2u:22.04
WORKDIR /code
RUN install2.r --error --skipinstalled --ncpus -1 dplyr DT formattable markdown purrr RankAggreg readr shiny shinycssloaders shinyjs shinythemes shinyWidgets
RUN installGithub.r rstudio/httpuv cran/MCDM
COPY . .
CMD R -e 'shiny::runApp(host = "0.0.0.0", port = 7860)'
