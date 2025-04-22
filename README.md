# Research Dashboard for Longitudinal Survey and Assessment Data - Docker Setup

## Overview

This repository contains a dockerized version of the Research Dashboard for Longitudinal Survey and Assessment Data, a Shiny application for analyzing longitudinal survey data with database integration. The database schema showcases an organizational framework for storing complex research data (simulated for demonstration purposes). The application provides tools for exploring psychometric properties, visualizing missing data patterns, and analyzing participant responses across time.

## Prerequisites

-   [Docker Desktop](https://www.docker.com/products/docker-desktop/) installed on your computer
-   No other software required - R, R packages, and the database are all included in the Docker containers

## Getting Started

1.  Clone or download this repository to your local machine
2.  Open a terminal/command prompt in the project directory
3.  Run the following command to start the application:

``` bash
docker-compose up
```

4.  Open your web browser and navigate to <http://localhost:3838>
5.  To stop the application, press `Ctrl+C` in the terminal or run:

``` bash
docker-compose down
```

## Features

-   **Database-driven**: All data is stored in a MariaDB database, automatically initialized with the project schema
-   **Missing data visualization and summaries**: Track completion patterns across time points
-   **Psychometric analysis**: Examine reliability and validity of measures
-   **Participant management**: Add or remove study participants

## Architecture

This application runs in two Docker containers:

1.  **app**: R Shiny application with all required packages
2.  **database**: MariaDB database server with initialized schema

Docker Compose manages the connection between these containers, allowing them to communicate seamlessly.

## Modifying the Application or Data

If you need to make changes to the application:

1.  Edit the R code files or SQL script as needed
2.  Run `docker-compose down` to stop the containers
3.  Run `docker-compose up --build` to rebuild and restart with your changes

## Adding R Packages

If your modifications require additional R packages:

1.  Edit the `Dockerfile` and add the new package to the installation list:

```         
RUN R -e "install.packages('new_package_name')"
```

2.  Rebuild the containers with `docker-compose up --build`

## Troubleshooting

-   **Database connection issues**: Check that the environment variables in the Dockerfile match the database credentials in docker-compose.yml
-   **Missing packages**: Add any missing R packages to the Dockerfile
-   **Port conflicts**: If ports 3838 or 3306 are already in use on your machine, modify the `ports` section in docker-compose.yml

## Project Structure

-   `R/`: Application code modules
-   `database_init/`: SQL scripts for database initialization
-   `www/`: Static assets for the Shiny app
-   `Dockerfile`: Instructions for building the R application container
-   `docker-compose.yml`: Configuration for the multi-container setup
