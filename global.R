
# libraries ---------------------------------------------------------------

library(shiny)
library(pool)
library(readxl)
library(DT)
library(tools)
library(tidyverse)
library(DBI)
library(RPostgreSQL)
library(lubridate)


# options -----------------------------------------------------------------

# increase max file upload size
options(shiny.maxRequestSize = 30*1024^2)


# supporting modules, functions, and configurations -----------------------

# config
source('config.R')

# functions and helpers
source('config.R')
source('helper_query_resin_samples.R')
source('helper_format_lachat.R')
source('helper_upload_resin.R')
source('helper_sql_execution.R')
# source('import_metadata.R') 

# modules
source('module_fertilizer.R')