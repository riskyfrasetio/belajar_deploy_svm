FROM rocker/r-ver:3.5.0
RUN apt-get update -qq && apt-get install -y \
      libssl-dev \
      libcurl4-gnutls-dev
RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('e1071')"
RUN R -e "install.packages('jsonlite')"
RUN R -e "install.packages('data.table')"
# copy everything from the current directory into the container
COPY / /
# open port 80 to traffic
EXPOSE 3000
# when the container starts, start the main.R script
ENTRYPOINT ["Rscript", "main.R"]