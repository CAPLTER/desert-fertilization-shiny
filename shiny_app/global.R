# libraries ---------------------------------------------------------------

library(shiny)
library(DBI)
library(RPostgreSQL)
library(pool)
library(DT)
library(R6)

library(dplyr)


# options -----------------------------------------------------------------

options(shiny.maxRequestSize = 30*1024^2) # increase max file upload size
options(shiny.reactlog = FALSE)
Sys.setenv(TZ = "America/Phoenix")


# supporting modules, functions, and configurations -----------------------

# configuration from config.yml
this_configuration <- config::get(config = "default")

# database connection
this_pool <- pool::dbPool(
  drv      = RPostgreSQL::PostgreSQL(),
  dbname   = this_configuration$dbname,
  host     = this_configuration$host,
  user     = this_configuration$user,
  password = this_configuration$password
)

shiny::onStop(function() {
  pool::poolClose(this_pool)
})


# modules and functions --------------------------------------------------------

source("R/helper_sql_execution.R") # ensure that this is loaded first


# R6 ---------------------------------------------------------------------------

# ResinViewer1 <- ResinViewer$new(id = "resin_display")


# selectors --------------------------------------------------------------------

# initials of annuals survey crew
annuals_comp_surveyors <- c(
  "Arely Castillo",
  "Michelle Dao",
  "Chad Hauck",
  "Raisa Mahmud",
  "Carina Rodriguez",
  "Ella Schulte",
  "Sydney Shaw",
  "Quincy Stewart"
)
