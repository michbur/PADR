# https://github.com/michbur/PADR/blob/master/dplyr.R

library(dplyr)
library(reshape2)
dat <- read.csv("https://raw.githubusercontent.com/michbur/PADR/master/data/data1.csv")

dat[["strain"]]
class(dat[["strain"]])

dat[["LB_1"]]
dat[["strain"]] <- as.character(dat[["strain"]])

mutate(dat, strain = as.character(strain))

dat <- read.csv("https://raw.githubusercontent.com/michbur/PADR/master/data/data1.csv") %>% 
  mutate(strain = as.character(strain))

dat[["strain"]]

select(dat, M63_1, M63_2, M63_3)
select(dat, M63_1) %>% head
select_(dat, "M63_1")

select(dat, paste0("M63_", 1L:3))

slice(dat, 1L:5)

melt(dat, variable.name = "medium") 

melt(dat, variable.name = "medium")[["medium"]]

melt(dat, variable.name = "medium")[["medium"]] %>% 
  as.character

melt(dat, variable.name = "medium")[["medium"]] %>% 
  as.character %>% 
  strsplit(split = "_")

melt(dat, variable.name = "medium")[["medium"]] %>% 
  as.character %>% 
  strsplit("_") %>% 
  sapply(first)

melt(dat, variable.name = "medium") %>% 
  mutate(medium2 = sapply(strsplit(as.character(medium), "_"), first)) 

melt(dat, variable.name = "medium") %>% 
  mutate(medium = sapply(strsplit(as.character(medium), "_"), first),
         value = ifelse(value < 0, 0, value))

melt(dat, variable.name = "medium") %>% 
  mutate(medium = sapply(strsplit(as.character(medium), "_"), first),
         value = ifelse(value < 0, 0, value)) %>% 
  group_by(medium) %>% 
  mutate(value = (max(value) - min(value))/max(value))

median_dat <- melt(dat, variable.name = "medium") %>% 
  mutate(medium = sapply(strsplit(as.character(medium), "_"), first),
         value = ifelse(value < 0, 0, value)) %>% 
  group_by(medium) %>% 
  #mutate(value = (max(value) - min(value))/max(value)) %>% 
  group_by(active, strain, medium) %>% 
  summarise(value = median(value))

filter(median_dat, strain == "5160")

filter(median_dat, strain == "5160", active %in% c("W1", "W2"))

filter(median_dat, strain != "5160")

filter(median_dat, strain == "5160", !(active %in% c("W1", "W2")))
filter(median_dat, strain == "5160", active == "W3")

group_by(median_dat, active, medium) %>% 
  summarise(max_value = max(value))

group_by(median_dat, active, medium) %>% 
  filter(value == max(value))

dat_np <- select(dat, -pathotype)
pathotype <- read.csv("https://raw.githubusercontent.com/michbur/PADR/master/data/data2.csv") %>% 
  mutate(strain = as.character(strain))

final_dat <- inner_join(median_dat, pathotype, by = c("strain" = "strain"))

# 1. Create a data.frame mean_dat of mean values only for replicates 
# 1-2 for each strain and active substance.
# 2. In mean_dat find the minimum value for each medium and pathotype. 
# 3. Compute the median value for all pathotypes regardless of the active

melt(dat, variable.name = "medium") %>% 
  mutate(medium2 = sapply(strsplit(as.character(medium), "_"), first),
         value = ifelse(value < 0, 0, value),
         rep_id = sapply(strsplit(as.character(medium), "_"), function(x) x[length(x)])) %>% 
  filter(rep_id != "3") %>% 
  group_by(active, strain, medium) %>% 
  summarise(value = mean(value))
