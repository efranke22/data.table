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
  mutate(expr = ifelse(expr == "read_dplyr", "read_csv", "fread")) %>%
  ggplot(aes(x=nrow, y=avg_time, color = expr))+
  geom_point()+
  geom_line()+
  scale_x_log10(labels = scales::label_comma(), breaks = c(10, 100, 1000, 10000, 100000, 1000000))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Rows", y="Avg load time (seconds)", color = "")+
  scale_color_manual(values = c("#CC79A7", "#0072B2"))

# Reading in a 50 million row dataset ----------------------------------------
set.seed(312)
test = data.table(x=sample(1e4, 5e7, TRUE), y=sample(letters, 5e7, TRUE))
test_df = as.data.frame(test)
fwrite(test, "demo_data/test.csv") # efficient! 

system.time(test <- fread("demo_data/test.csv")) # data.table
system.time(test <- read_csv("demo_data/test.csv")) # readr
system.time(test <- read.csv("demo_data/test.csv")) # base r


# Automatic Indexing ------------------------------------

set.seed(312)
big_data = data.table(x=sample(1e4, 5e7, TRUE), y=sample(letters, 5e7, TRUE), z=sample(letters, 5e7, TRUE))

t3 <- system.time(way3 <- big_data %>% filter(y == "E" & x== 312))
t1 <- system.time(way1 <- big_data[y == "E" & x == 312])
setkeyv(big_data, c("x", "y"))
key(big_data)
t2 <- system.time(way2 <- big_data[.(312, "E")])

require(rbenchmark)
benchmark(big_data %>% filter(x== 312 & y == "E"), big_data[.(312, "E")], 
          replications = 50)
