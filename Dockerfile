FROM rocker/r-ver:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libmariadb-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev

# Install required R packages individually
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('bslib')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('pool')"
RUN R -e "install.packages('DBI')"
RUN R -e "install.packages('dplyr')"
RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('scales')"
RUN R -e "install.packages('reactable')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('shinycssloaders')"
RUN R -e "install.packages('psych')"
RUN R -e "install.packages('mirt')"
RUN R -e "install.packages('reshape2')"
RUN R -e "install.packages('RMySQL')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('readr')"
RUN R -e "install.packages('purrr')"
RUN R -e "install.packages('tibble')"
RUN R -e "install.packages('tidyr')"
RUN R -e "install.packages('stringr')"
RUN R -e "install.packages('ggthemes')"
RUN R -e "install.packages('forcats')"

# Copy your app files to the Docker container
COPY . /srv/shiny-server/myapp/
WORKDIR /srv/shiny-server/myapp

# Set up environment variables for database connection
ENV DB_NAME=avp36 \
    DB_HOST=database \
    DB_USER=shiny_user \
    DB_PASS=shiny_password \
    DB_PORT=3306

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/myapp', host='0.0.0.0', port=3838)"]