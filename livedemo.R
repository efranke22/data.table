# libraries 
library(data.table)
library(dtplyr)
library(tidyverse)
library(tidytable)

# loading in the data
system.time(summer <- fread("demo_data/divvy_summer2022.csv")) # data.table
system.time(summer <- read_csv("demo_data/divvy_summer2022.csv")) # readr






