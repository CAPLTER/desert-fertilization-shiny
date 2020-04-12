
# libraries ---------------------------------------------------------------

library(shiny)
library(pool)
library(readxl)
library(DT)
library(tools)
library(tidyverse)
library(DBI)
library(RPostgreSQL)


# options -----------------------------------------------------------------

# increase max file upload size
options(shiny.maxRequestSize = 30*1024^2)


# supporting modules, functions, and configurations -----------------------

# config
source('config.R')

# functions
source('resin_samples.R')
source('import_metadata.R') 
source('format_lachat.R')
source('config.R')
source('data_upload.R')
source('helper_sql_execution.R')

# modules
source('module_fertilizer.R')