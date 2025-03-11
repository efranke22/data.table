# libraries 
library(data.table)
library(dtplyr)
library(tidyverse)
library(microbenchmark)

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
# let's say we want to subset to only houses with warranty deeds (WD in Sale.Type column)

wd_houses <- house_prices[Sale.Type %like% 'WD',]

# note that %like% is case sensitive 
house_prices[Sale.Type %like% 'wd',]
# if we want to ignore case, we would need to use tolower()
house_prices[tolower(Sale.Type) %like% 'wd', ]

# now, let's say we want to just look at houses with prices in the IQR 

quantile(house_prices$SalePrice, c(0.25, .75))
IQR_houses = house_prices[SalePrice %between% c(129500, 213500),]

# there are a lot of columns in the house_prices data.table
# to select columns in data.table we can do the following:

house_prices[, .(Mo.Sold, Yr.Sold, SalePrice)]

# grouping
# we can use a similar syntax to above to count the number of houses sold each year

house_prices[, .N, by = .(Mo.Sold, Yr.Sold)] #.N is a special symbols which generates the count for each group

# there are multiple of these special symbols used for convenience

house_prices[, .I, by = .(Mo.Sold, Yr.Sold)] # .I returns the the row indices for each group

# we can also use .SD or .SDcols on group by variables to apply functions across multiple 
# both of these work, but the first one might be quicker if we are trying to do lots of columns at once
house_prices[, .SD[, lapply(.(avg_SalePrice = SalePrice, avg_Lot.Area = Lot.Area), mean)], by = .(Mo.Sold, Yr.Sold)]
house_prices[, .(avg_SalePrice = mean(SalePrice), avg_Lot.Area = mean(Lot.Area)), by = .(Mo.Sold, Yr.Sold)]

# notice too that we can use pipes if we want with data.table (base pipe or magrittr pipe)

house_prices %>%
  .[, .(avg_SalePrice = mean(SalePrice), avg_Lot.Area = mean(Lot.Area)),by = .(Mo.Sold, Yr.Sold)]

house_prices |>
  _[, .(avg_SalePrice = mean(SalePrice), avg_Lot.Area = mean(Lot.Area)),by = .(Mo.Sold, Yr.Sold)]

# we can mutate the data.table like we would in dplyr as follows...

modified_subset_house_prices = copy(subset_house_prices)[, Date :=  as.Date(paste(Yr.Sold, Mo.Sold, 15, sep = "-"))]

subset_house_prices
modified_subset_house_prices

# rolling joins
# maybe we are interested in avg neighborhood house price (we create fake data for this)
# cross join to create grid of all combinations of neighborhoods, years, and months in data.table 
neighborhood_dates = CJ(Neighborhood = unique(house_prices$Neighborhood),
                        Yr.Sold = unique(house_prices$Yr.Sold),
                        Mo.Sold = unique(house_prices$Mo.Sold))
           
# we create pseudo-average prices using the IQR we calculated earlier
avg_house_prices = neighborhood_dates[, `:=` (
  Date = as.Date(paste(Yr.Sold, Mo.Sold, 1, sep = "-")),
  Avg.Price = sample(seq(129500, 213500, by = 500), size = .N, replace = TRUE))]
   

avg_house_prices = avg_house_prices[, .(Neighborhood, Date, Avg.Price)]

# Some data wrangling on the house_prices data.table...

subset_house_prices = house_prices[, .(Mo.Sold, Yr.Sold, Neighborhood, SalePrice)]
subset_house_prices[, SoldDate := as.Date(paste(Yr.Sold, Mo.Sold, 15, sep = "-"))] # just putting exact sale date as 15th of the month as example
# notice we modified the house_prices data.table in place!
subset_house_prices

# next we select only the columns of interest...
subset_house_prices = subset_house_prices[,.(Neighborhood, SalePrice, SoldDate)]

# finally, we can create the rolling join!
avg_house_prices[subset_house_prices, on = c('Neighborhood', 'Date' = 'SoldDate'), roll = TRUE]

# we can also keep og columns so it is clearer how the joining is happening...
rolling_join = avg_house_prices[subset_house_prices, on = c('Neighborhood', 'Date' = 'SoldDate'),
                 roll = TRUE,
                 j = .(Neighborhood,
                       Date,
                       Avg.Date = x.Date,
                       Avg.Price,
                       SalePrice)]

rolling_join

# grouped visualization

to_visualize = house_prices[Neighborhood %in% c('NAmes', 'OldTown', 'CollgCr'), ]

to_visualize[,
             print(
               ggplot(.SD, aes(x = Yr.Sold, y = SalePrice,
                               fill = as.factor(Yr.Sold),
                               color = as.factor(Yr.Sold))) +
                 geom_boxplot(alpha = 0.5) +
                 theme_classic()+
                 labs(title = paste(Neighborhood))+
                 theme(legend.position = 'none', plot.title.position = "plot")),
             by = .(Neighborhood)]

# grouped modeling

grouped_models <- house_prices[, .(mods = list(
  lm(SalePrice ~ Lot.Area + Bedroom.AbvGr + Full.Bath + Year.Built*Yr.Sold, data = .SD) #can put any type of model in here
  )),
  by = Neighborhood ]

OldTown_lm_summary <- summary(grouped_models$mods[[15]])
NridgHt_lm_summary <- summary(grouped_models$mods[[8]])

OldTown_lm_summary
NridgHt_lm_summary

# dtplyr 
# suppose we are interested in filtering for all houses built in 2001. 
# we can use dtplyr to write this in dplyr syntax but run as data.table expressions

house_prices2 <- lazy_dt(house_prices) # captures the intent of dplyr verbs, only actually performing computation when requested

# to compare with microbenchmark, 
dtplyr_way <- function() { # captures the intent of dplyr verbs, only actually performing computation when requested
  house_prices2 %>%
    dplyr::group_by(Neighborhood, Yr.Sold) %>% 
    dplyr::count() %>%
    as_tibble()
}

data_table_way <- function() {
  house_prices[, (.value = .N), by = .(Neighborhood, Yr.Sold)]
}


dplyr_way <- function() {
  house_prices %>%
    group_by(Neighborhood, Yr.Sold) %>% 
    count() 
}

microbenchmark(
  dtplyr_way = dtplyr_way(),
  data_table_way = data_table_way(),
  dplyr_way = dplyr_way(),
  times = 50L
)

# tidytable!
library(tidytable)
large_houses <- house_prices %>%
  mutate(big_house = case_when(Full.Bath %in% c(3,4) & Gr.Liv.Area >= 2500 ~ 1, 
                               TRUE ~ 0))

grouped_avg_price <- house_prices %>%
  summarize(avg_price = mean(SalePrice),
            .by = c(Street, Neighborhood))

# a note on indexes
# because we wanted to give a sense of the basic data.table syntax in this live demo,
# we did not implement secondary indexes or keys at the start. the syntax differs a bit.
# a cool new feature is automatic indexing! this is implemented for binary operators == and %in%. 
# An index is automatically created and saved as an attribute.

house_prices[Year.Built == 2001]
indices(house_prices)
house_prices[.(2001), on = "Year.Built"]
# note: this is different than the type data.table syntax: 
house_prices[Year.Built == 2001] # does not reap binary search benefits

# for more examples regarding secondary indexes and automatic indexing, see
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-secondary-indices-and-auto-indexing.html