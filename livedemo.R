# libraries 
library(data.table)
library(dtplyr)
library(tidyverse)
library(tidytable)

# live demo using House prices in Ames, Iowa data from the CMU S&DS Data Repository

# originally from: D De Cock (2011). Ames, Iowa: Alternative to the Boston Housing
# Data as an End of Semester Regression Project. Journal of Statistics Education 19 (3).
# https://doi.org/10.1080/10691898.2011.11889627

# loading in the data
system.time(house_prices <- fread("demo_data/ames-housing.csv")) # data.table
system.time(house_prices2 <- read_csv("demo_data/ames-housing.csv")) # readr
system.time(house_prices3 <- read.csv("demo_data/ames-housing.csv")) # base r

house_prices <- data.table(house_prices)

# %like% and %between% logical operators

# let's say we want to subset to only houses with warranty deeds
house_prices[Sale.Type %like% 'WD',]
# note that %like% is case sensitive 
house_prices[Sale.Type %like% 'wd',]
# if we want to ingore case, we would need to use tolower()
house_prices[tolower(Sale.Type) %like% 'wd', ]


# now, let's say we want to just look at houses with prices in the IQR 
quantile(house_prices$SalePrice, c(0.25, .75))
IQR_houses = house_prices[SalePrice %between% c(129500, 213500),]

# rolling joins


# grouped modeling




# loading in the data
system.time(summer <- fread("demo_data/divvy_summer2022.csv")) # data.table
system.time(summer2 <- read_csv("demo_data/divvy_summer2022.csv")) # readr
system.time(summer <- read.csv("demo_data/divvy_summer2022.csv")) # base r

summer <- data.table(summer)


