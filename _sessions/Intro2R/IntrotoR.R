# load packages
library(tidyverse)

# load data
baselers <- read_csv('data/baselers.csv')

# analyze
summary(baselers)

# regression
lm(weight ~ sex,
   data = baselers)