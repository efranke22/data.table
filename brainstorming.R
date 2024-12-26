
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
# 1.5) Similar to idea 1. But we could have a presentation that really focuses on how to teach data wrangling visually for students totally new to it
##  - see cheatsheet in this repo which is a basic version of doing this.
# 2) Data.table for computer scientists. Might be harder because we are not a computer scientists and think like statisticians so not sure 
# if we could replicate how they think about data wrangling. But because data.table is more similar to "computer programming" we think that 
# it could be easier for students with a comp sci background to learn that rather than tidyverse?