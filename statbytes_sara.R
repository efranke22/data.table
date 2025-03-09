library(data.table)
library(dtplyr)
library(tidyverse)
library(tidytable)
library(microbenchmark)
library(rbenchmark)

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
