library(data.table)
library(dtplyr)
library(tidyverse)
library(tidytable)


# Create computational efficiency plot ----------------------------------------
set.seed(312)
a <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/divvy_summer2022.csv"),
  read_data_table = fread("demo_data/divvy_summer2022.csv"),
  times = 10L
)) %>%
  group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 2378624)

b <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/divvy_summer2022.csv", n_max = 1000000),
  read_data_table = fread("demo_data/divvy_summer2022.csv", nrows = 1000000),
  times = 10L
)) %>% group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 1000000)

c <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/divvy_summer2022.csv", n_max = 100000),
  read_data_table = fread("demo_data/divvy_summer2022.csv", nrows = 100000),
  times = 10L
)) %>% group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 100000)

d <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/divvy_summer2022.csv", n_max = 10000),
  read_data_table = fread("demo_data/divvy_summer2022.csv", nrows = 10000),
  times = 10L
)) %>% group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 10000)

e <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/divvy_summer2022.csv", n_max = 1000),
  read_data_table = fread("demo_data/divvy_summer2022.csv", nrows = 1000),
  times = 10L
)) %>% group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 1000)

f <- data.frame(microbenchmark(
  read_dplyr = read_csv("demo_data/divvy_summer2022.csv", n_max = 100),
  read_data_table = fread("demo_data/divvy_summer2022.csv", nrows = 100),
  times = 10L
)) %>% group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) %>%
  mutate(nrow = 100)

bind_rows(a, b, c, d, e, f) %>%
  ggplot(aes(x=nrow, y=avg_time, color = expr))+
  geom_point()+
  geom_line()+
  scale_x_log10(labels = scales::label_comma(), breaks = c(10, 100, 1000, 10000, 100000, 1000000))+
  theme_bw()+
  theme(legend.position = "bottom")