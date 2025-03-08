library(data.table)
library(dtplyr)
library(tidyverse)
library(microbenchmark)

# LOAD data ----------------------------------------------------------------------------
system.time(crime <- fread("demo_data/crimes.csv")) # data.table 14.2
system.time(crime <- read_csv("demo_data/crimes.csv")) # readr 158.4

# Create graph comparing speed of fread() and read_csv() ------------------------------
set.seed(312)
a <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/crimes.csv", n_max = 4000000),
  read_data_table = fread("demo_data/crimes.csv", nrows = 4000000),
  times = 5L
)) %>%
  group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 4000000)

b <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/crimes.csv", n_max = 1000000),
  read_data_table = fread("demo_data/crimes.csv", nrows = 1000000),
  times = 5L
)) %>% group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 1000000)

c <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/crimes.csv", n_max = 100000),
  read_data_table = fread("demo_data/crimes.csv", nrows = 100000),
  times = 5L
)) %>% group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 100000)

d <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/crimes.csv", n_max = 10000),
  read_data_table = fread("demo_data/crimes.csv", nrows = 10000),
  times = 5L
)) %>% group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 10000)

e <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/crimes.csv", n_max = 1000),
  read_data_table = fread("demo_data/crimes.csv", nrows = 1000),
  times = 5L
)) %>% group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 1000)

f <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/crimes.csv", n_max = 100),
  read_data_table = fread("demo_data/crimes.csv", nrows = 100),
  times = 5L
)) %>% group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 100)

bind_rows(a, b, c, d, e, f) %>%
  mutate(expr = ifelse(expr == "read_dplyr", "read_csv", "fread")) %>%
  ggplot(aes(x=nrow, y=avg_time, color = expr))+
  geom_point()+
  geom_line()+
  scale_x_log10(labels = scales::label_comma(), breaks = c(10, 100, 1000, 10000, 100000, 1000000, 4000000))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Rows", y="Avg load time (seconds)", color = "")+
  scale_color_manual(values = c("#CC79A7", "#0072B2"))


# Keys and Indices -------------------------------------------------------------------
setkeyv(crime, NULL)
setkeyv(crime, "Beat")
key(crime)
data.frame(microbenchmark(
  dplyr_way = crime %>% filter(Beat == 511), 
  dt_way = crime[.(511)], times = 10L)) %>% 
  group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) 

setindexv(crime, NULL)
setindexv(crime, c("Year", "Primary Type"))
indices(crime)
crime[.(2023, "AUTOMOBILE"), on = c("Year", "Description")] 
data.frame(microbenchmark(
  dplyr_way = crime %>% filter(Year == 2023 & Description == "AUTOMOBILE"), 
  dt_way = crime[.(2023, "AUTOMOBILE"), on=c("Year", "Description")], times = 10L)) %>% 
  group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) 
indices(crime)

crime[`Zip Codes` == 22618]
indices(crime)

# Visualize/Model by grouping variables---------------------------------------------
crime_sub <- crime %>% filter(`Primary Type` %in% c("ARSON", "GAMBLING", "LIQUOR LAW VIOLATION")) %>%
  select(`Primary Type`, Latitude, Longitude) %>% na.omit() 

crime_sub[,.(mods = list(lm(n ~ Year + District, data = .SD[, .(n, Year, District)]))), 
                    by = `Primary Type`]


crime_sub[, print(
     ggplot(.SD, aes(x = Latitude, y = Longitude)) +
       geom_point(alpha = 0.5) +
       theme_classic()), by = .(`Primary Type`)]


library(dtplyr)
crime2 <- lazy_dt(crime)
crime_sub <- crime2 %>% filter(`Primary Type` %in% c("ARSON", "GAMBLING", "LIQUOR LAW VIOLATION")) %>%
  select(`Primary Type`, Latitude, Longitude) %>% na.omit() %>%
  as_tibble()

data.frame(microbenchmark(dtplyr_way = crime2 %>% filter(`Primary Type` %in% c("ARSON", "GAMBLING", "LIQUOR LAW VIOLATION")) %>%
                 select(`Primary Type`, Latitude, Longitude) %>% na.omit() %>%
                 as_tibble(), 
               dplyr_way = crime %>% filter(`Primary Type` %in% c("ARSON", "GAMBLING", "LIQUOR LAW VIOLATION")) %>%
                 select(`Primary Type`, Latitude, Longitude) %>% na.omit(), 
               times=10L)) %>% 
  group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) 
