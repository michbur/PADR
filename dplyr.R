library(dplyr)
dat <- read.csv("./data/data1.csv")

dat[["strain"]]

mutate(dat, strain = as.character(strain))

dat <- read.csv("./data/data1.csv") %>% 
  mutate(strain = as.character(strain))

select(dat, M63_1, M63_2, M63_3)

melt(dat, variable.name = "medium") %>% 
  mutate(medium = sapply(strsplit(as.character(medium), "_"), first))

median_dat <- melt(dat, variable.name = "medium") %>% 
  mutate(medium = sapply(strsplit(as.character(medium), "_"), first)) %>% 
  group_by(active, strain, medium) %>% 
  summarise(value = median(value))


filter(median_dat, strain == "5160")

filter(median_dat, strain == "5160", active == "W2")

group_by(median_dat, active, medium) %>% 
  summarise(max_value = max(value))

group_by(median_dat, active, medium) %>% 
  filter(value == max(value))

pathotype <- read.csv("./data/data2.csv") %>% 
  mutate(strain = as.character(strain))

final_dat <- inner_join(median_dat, pathotype)

# 1. Create a data.frame mean_dat of mean values only for replicates 
# 1-2 for each strain and active substance.
# 2. In mean_dat find the minimum value for each medium and pathotype. 
# 3. Compute the median value for all pathotypes regardless of the active
