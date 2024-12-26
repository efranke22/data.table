
## Computational stuff, this would be for statbytes 
# data.table fread is faster than read_csv (0.069 average elapsed time versus 0.111)
system.time(read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-07-02/tt_datasets.csv'))
system.time(fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-07-02/tt_datasets.csv'))
# https://adv-r.hadley.nz/perf-measure.html for more potential computational measurement tools


# USCOTS 
# Regardless of direction of presentation, first we should motivate the problem
## - the benefit of computational efficiency of data.table. 
## - In context of USCOTS theme, we always have to do data cleaning for model preparation
# Ideas of where to go from here.....
# 1) Create a tutorial showing how to think about "the six main verbs" (select, filter, mutate, arrange, group_by, summarize) with data.table
# and provide exercises to practice these
# https://ggplot-dplyr-intro.netlify.app/#Wrangling_data_with_dplyr_functions could model off the data wrangling section of this
# 2) 