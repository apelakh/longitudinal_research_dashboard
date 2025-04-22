# global.R
library(shiny)
library(bslib)
library(shinydashboard)
library(pool)
library(DBI)
library(lubridate)
# library(hrbrthemes)
library(scales)
library(reactable)
library(DT)
library(shinyjs)
library(shinycssloaders)
library(psych)
library(mirt)
library(reshape2)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)
library(forcats)

jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

# Source the database connection
source("R/db/connection.R")

# The pool object is now available globally

# Source all module files
source("R/modules/database_design.R")
source("R/modules/connection_test.R")
source("R/modules/missing_data.R")
source("R/modules/psychometrics.R") 
source("R/modules/descriptives.R")
source("R/modules/manage_participants.R")
source("R/modules/models.R")

# Source utility functions
source("R/utils/plotting.R")
source("R/utils/stats.R")
source("R/utils/helpers.R")

# Source database functions
source("R/db/queries.R")
source("R/db/utils.R")

# Set app-wide options
options(
  shiny.maxRequestSize = 30 * 1024^2, # 30MB upload limit
  shiny.sanitize.errors = FALSE        # Show full error messages during development
)

# Global theme settings for plots
theme_longitudinal <- function(base_size = 15) {
  
  # theme_ipsum(
  #   base_size = base_size,
  #   plot_title_size = base_size*1.56,
  #   subtitle_size = base_size*1.05,
  #   strip_text_size = base_size*1.05,
  #   strip_text_face = "bold",
  #   caption_size = base_size*.78,
  #   axis_title_size = base_size*1.05,
  #   axis_title_just = "mc",
  #   axis_title_face = "bold"
  # ) +
  #   theme(
  #     legend.position = "bottom"
  #   )
  
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
}

# Define app-wide colors for consistent styling
app_colors <- list(
  primary = "#3c8dbc",
  success = "#00a65a",
  info = "#00c0ef",
  warning = "#f39c12",
  danger = "#f56954"
)

pal_cat_default <- ggthemes::ggthemes_data$gdocs$colors$value # default colors for categorical variables
col_default <- app_colors$primary # default color for elements without aesthetic mappings

# Make sure to clean up the pool when the app exits
shiny::onStop(function() {
  if (exists("pool")) {
    safe_pool_close(pool)
  }
})