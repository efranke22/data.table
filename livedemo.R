#### LIBRARIES ####
library(data.table)
library(dtplyr)
library(tidyverse)
library(microbenchmark)

#### READING THE DATA AND LOGICAL OPERATORS ####

# live demo using House prices in Ames, Iowa data from the CMU S&DS Data Repository

# originally from: D De Cock (2011). Ames, Iowa: Alternative to the Boston Housing
# Data as an End of Semester Regression Project. Journal of Statistics Education 19 (3).
# https://doi.org/10.1080/10691898.2011.11889627

# loading in the data
house_prices <- fread("demo_data/ames-housing.csv") # data.table

# %like% and %between% logical operators
# let's say we want to subset to only houses with warranty deeds (WD in Sale.Type column)
wd_houses <- house_prices[Sale.Type %like% 'WD',]

# note that %like% is case sensitive 
#house_prices[Sale.Type %like% 'wd',]
# if we want to ignore case, we would need to use tolower() [from Base R]
#house_prices[tolower(Sale.Type) %like% 'wd', ]

# now, let's say we want to just look at houses with prices within the IQR 
quantile(house_prices$SalePrice, c(0.25, .75))
IQR_houses = house_prices[SalePrice %between% c(129500, 213500),]

# there are a lot of columns in the house_prices data.table
# to select columns in data.table we can do the following:
house_prices[, .(Mo.Sold, Yr.Sold, SalePrice)]

#### GROUPING ####
# we can count the number of houses sold each year using .N
#.N is a special symbols which generates the count for each group
house_prices[, .N, by = .(Mo.Sold, Yr.Sold)] 

# there are multiple of these special symbols used for convenience
# for instance, there are two other special symbols are .SD and .SDcols
# .SD gives us the data rows associated with each group#
# house_prices[, .SD[, lapply(.(avg_SalePrice = SalePrice, avg_Lot.Area = Lot.Area), mean)], by = .(Mo.Sold, Yr.Sold)]
# house_prices[, .(avg_SalePrice = mean(SalePrice), avg_Lot.Area = mean(Lot.Area)), by = .(Mo.Sold, Yr.Sold)]


#### MODIFY BY REFERENCE ####
# one of the things that makes data.table so efficient is that it modifies by reference by default 
# modeled off of: https://tysonbarrett.com/jekyll/update/2019/07/12/datatable/
# for more infromation, see: vignette("datatable-reference-semantics")

subset_house_prices = house_prices[, .(Mo.Sold, Yr.Sold, Neighborhood, SalePrice)]
tracemem(subset_house_prices)
subset_house_prices[, SoldDate := as.Date(paste(Yr.Sold, Mo.Sold, 15, sep = "-"))]
subset_house_prices
tracemem(subset_house_prices)

# So, if we make a new pointer (in essence a new name that points to the same data),
# then changes to one makes changes to the other.
subset_house_prices2 = subset_house_prices
subset_house_prices2
tracemem(subset_house_prices2)
subset_house_prices[,SoldDate := NULL] #removes SoldDate
subset_house_prices2

# notice we can even use case_when [which is a dplyr function that works on a vector] and pipes with data.table(!!)
tracemem(subset_house_prices)
subset_house_prices[, SoldDate := as.Date(paste(Yr.Sold, Mo.Sold, 15, sep = "-"))] %>% 
  .[, SalePrice_quartile := case_when(SalePrice < 129500 ~ 1,
                                      SalePrice < 160000 ~ 2,
                                      SalePrice < 213500 ~ 3,
                                      SalePrice < 755000 ~ 4,
                            TRUE ~ NA)] %>% 
  .[, Neighbhoorhood_lower := tolower(Neighborhood)]

# what is very cool about this, is that it also modifies by reference (even though we are using pipes AND a dplyr function)!
subset_house_prices
tracemem(subset_house_prices) #reference stays the same! No internal copies made of the object

# As explained by Barrett, "this is hugely beneficial in large data sets, where RAM can get used up with all the copies"

# compare to this:
subset_house_prices3 <- subset_house_prices
subset_house_prices3$row_number <- seq(1,2930, 1) # a copy was made

"tracemem[0x10f9d5708 -> 0x116688808]: 
tracemem[0x116688808 -> 0x116688878]: copy $<-.data.table $<- "

subset_house_prices
subset_house_prices3

#### A NOTE OF INDICES ####
# because we wanted to give a sense of the basic data.table syntax in this live demo,
# we did not implement secondary indexes or keys at the start. the syntax differs a bit.
# a cool new feature is automatic indexing! this is implemented for binary operators == and %in%. 
# An index is automatically created and saved as an attribute.

setindexv(house_prices, NULL)
house_prices[Year.Built == 2001]
indices(house_prices)
house_prices[.(2001), on = "Year.Built"]
# note: this is different than the type data.table syntax: 
house_prices[Year.Built == 2001] # does not reap binary search benefits

# for more examples regarding secondary indexes and automatic indexing, see
# https://cran.r-project.org/web/packages/data.table/vignettes/datatable-secondary-indices-and-auto-indexing.html

#### ROLLING JOINS ####
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
subset_house_prices[, SoldDate := as.Date(paste(Yr.Sold, Mo.Sold, 15, sep = "-"))]
subset_house_prices = subset_house_prices[,.(Neighborhood, SalePrice, SoldDate)]

# finally, we can create the rolling join!
rolling_join = avg_house_prices[subset_house_prices, on = c('Neighborhood', 'Date' = 'SoldDate'),
                 roll = Inf,
                 j = .(Neighborhood,
                       Date,
                       Avg.Date = x.Date,
                       Avg.Price,
                       SalePrice)]

rolling_join

#### GROUPED VISUALIZATION ####

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

#### GROUPED MODELING ####

grouped_models <- house_prices[, .(mods = list(
  lm(SalePrice ~ Lot.Area + Bedroom.AbvGr + Full.Bath + Year.Built*Yr.Sold, data = .SD) #can put any type of model in here
  )),
  by = Neighborhood ]

OldTown_lm_summary <- summary(grouped_models$mods[[15]])
NridgHt_lm_summary <- summary(grouped_models$mods[[8]])

OldTown_lm_summary
NridgHt_lm_summary

#### dtplyr ####
# suppose we are interested in filtering for all houses built in 2001. CHANGE
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

#### tidytable ####
library(tidytable)
large_houses <- house_prices %>%
  mutate(big_house = case_when(Full.Bath %in% c(3,4) & Gr.Liv.Area >= 2500 ~ 1, 
                               TRUE ~ 0))

grouped_avg_price <- house_prices %>%
  summarize(avg_price = mean(SalePrice),
            .by = c(Street, Neighborhood))