FROM rocker/r-ver:3.5.0
RUN R -e "install.packages('plumber')"
# copy everything from the current directory into the container
COPY / /
# open port 80 to traffic
EXPOSE 80
# when the container starts, start the main.R script
ENTRYPOINT ["Rscript", "main.R"]