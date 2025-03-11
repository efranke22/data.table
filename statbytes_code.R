library(data.table)
library(dtplyr)
library(tidyverse)
library(microbenchmark)
library(rbenchmark)

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

fread_v_read_csv <- bind_rows(a, b, c, d, e, f) %>%
  mutate(expr = ifelse(expr == "read_dplyr", "read_csv", "fread")) %>%
  ggplot(aes(x=nrow, y=avg_time, color = expr))+
  geom_point()+
  geom_line()+
  scale_x_log10(labels = scales::label_comma(), breaks = c(10, 100, 1000, 10000, 100000, 1000000, 4000000))+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(x="Rows", y="Avg load time (seconds)", color = "")+
  scale_color_manual(values = c("#CC79A7", "#0072B2"))

ggsave('speed_plots/fread_v_read_csv.png', bg = 'white', width = 4.5, height = 4, dpi = 1000)

# Keys and Indices -------------------------------------------------------------------
data.frame(microbenchmark(
  dplyr_way = crime %>% dplyr::filter(Beat == 511), 
  dt_way = crime[Beat == 511], times = 10L)) %>%
  group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) 

setkeyv(crime, NULL)
setkeyv(crime, "Beat")
key(crime)
crime[.(511)]

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

# Create computational efficiency plot (pivots) ---------------------------------
data = fread("crimes.csv")

all_combinations = CJ(Year = unique(data$Year),
                      Ward = unique(data$`Ward`),
                      `Zip Codes` = unique(data$`Zip Codes`),
                      `Community Area` = unique(data$`Community Area`))

all_combinations = na.omit(all_combinations)
all_combinations[,id := paste(`Zip Codes`, Ward, `Community Area`, sep = "_")]
all_combinations = all_combinations[,.(Year, id)]

subset_data = data[, .(Year, Ward, `Zip Codes`, `Community Area`)]
subset_data = na.omit(subset_data)
subset_data[,id := paste(`Zip Codes`, Ward, `Community Area`, sep = "_")]
subset_data[,value := .N, by = .(Year, id)]
subset_data = unique(subset_data[,.(Year, id, value)])

count_data = merge(all_combinations, subset_data, by = c("Year", "id"), all.x = TRUE)
count_data[is.na(value), value := 0] #setting all combos that don't appear in the data to value = 0

count_data_tbl = dplyr::as_tibble(count_data)

wider_results <- data.frame(microbenchmark(
  data.table = data.table::dcast.data.table(count_data, Year ~ id),
  tidyverse = tidyr::pivot_wider(count_data_tbl, names_from = id, values_from = value),
  times = 100)) %>%
  group_by(expr) %>% summarize(avg_time_ms = mean(time)/1000000) 


wider_results %>%
  mutate(expr = ifelse(expr == "tidyverse", "pivot_wider", "dcast")) %>%
  ggplot(aes(x = expr, y = avg_time_ms))+
  geom_col(aes(fill = expr))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="", y="Avg run time (milliseconds)", fill = "")+
  scale_fill_manual(values = c("#CC79A7", "#0072B2"))+
  coord_flip()

ggsave('speed_plots/pivot_wider.png', bg = 'white', width = 4.5, height = 4, dpi = 1000)


longer_dt = data.table::dcast.data.table(count_data, Year ~ id)
longer_tbl =  dplyr::as_tibble(longer_dt)

longer_results <- data.frame(microbenchmark(
  data.table = data.table::melt.data.table(longer_dt, id.vars = 1,
                                           variable.name = "name",
                                           variable.factor = FALSE),
  tidyverse = tidyr::pivot_longer(longer_tbl, cols = -Year),
  times = 100)) %>%
  group_by(expr) %>% summarize(avg_time_ms = mean(time)/1000000) 

longer_results %>%
  mutate(expr = ifelse(expr == "tidyverse", "pivot_longer", "melt")) %>%
  ggplot(aes(x = expr, y = avg_time_ms))+
  geom_col(aes(fill = expr))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="", y="Avg run time (millseconds)", fill = "")+
  scale_fill_manual(values = c("#CC79A7", "#0072B2"))+
  coord_flip()

ggsave('speed_plots/pivot_longer.png', bg = 'white', width = 4.5, height = 4, dpi = 1000)

# Create computational efficiency plot (joins) ---------------------------------
# specifically inner join and left join

# creating fake join data ...
right_dt = count_data %>%
  distinct(id) %>%
  mutate(responding_officer = sample(1:500, size = length(unique(count_data$id)), replace = TRUE)) %>%
  data.table()

right_tbl = dplyr::as_tibble(right_dt)

left_join_results <- data.frame(microbenchmark(
  data.table = right_dt[count_data, on = 'id', allow.cartesian = TRUE],
  tidyverse = dplyr::left_join(count_data_tbl, right_tbl, by = "id"),
  times = 100
)) %>%
  group_by(expr) %>% summarize(avg_time_ms = mean(time)/1000000) 


left_join_results %>%
  ggplot(aes(x = expr, y = avg_time_ms))+
  geom_col(aes(fill = expr))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="", y="Avg run time (milliseconds)", fill = "")+
  scale_fill_manual(values = c("#CC79A7", "#0072B2"))+
  coord_flip()

ggsave('speed_plots/left_join.png', bg = 'white', width = 4.5, height = 4, dpi = 1000)

right_dt_subset = data.table(sample_frac(right_dt, 0.3))
right_tbl_subset = dplyr::as_tibble(right_dt_subset)


inner_join_results <- data.frame(microbenchmark(
  data.table = count_data[right_dt_subset, on = "id", allow.cartesian = TRUE, nomatch = NULL],
  tidyverse = dplyr::inner_join(count_data_tbl, right_tbl_subset, by = "id"),
  times = 100
)) %>%
  group_by(expr) %>% summarize(avg_time_ms = mean(time)/1000000) 

inner_join_results %>%
  ggplot(aes(x = expr, y = avg_time_ms))+
  geom_col(aes(fill = expr))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="", y="Avg run time (milliseconds)", fill = "")+
  scale_fill_manual(values = c("#CC79A7", "#0072B2"))+
  coord_flip()

ggsave('speed_plots/inner_join.png', bg = 'white', width = 4.5, height = 4, dpi = 1000)

# Visualize/Model by grouping variables & dtplyr---------------------------------------------
crime2 <- lazy_dt(crime)
crime_sub <- crime2 %>% filter(`Primary Type` %in% c("ARSON", "GAMBLING", "LIQUOR LAW VIOLATION")) %>%
  select(`Primary Type`, Latitude, Longitude) %>% na.omit() %>%
  as_tibble()

crime_sub[, print(
  ggplot(.SD, aes(x = Latitude, y = Longitude)) +
    geom_point(alpha = 0.5) +
    theme_classic()), by = .(`Primary Type`)]


data.frame(microbenchmark(dtplyr_way = crime2 %>% filter(`Primary Type` %in% c("ARSON", "GAMBLING", "LIQUOR LAW VIOLATION")) %>%
                            select(`Primary Type`, Latitude, Longitude) %>% na.omit() %>%
                            as_tibble(), 
                          dplyr_way = crime %>% filter(`Primary Type` %in% c("ARSON", "GAMBLING", "LIQUOR LAW VIOLATION")) %>%
                            select(`Primary Type`, Latitude, Longitude) %>% na.omit(), 
                          times=10L)) %>% 
  group_by(expr) %>% summarize(avg_time = mean(time)/1000000000) 