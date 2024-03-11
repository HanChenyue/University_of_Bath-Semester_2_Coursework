library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidyverse)

# Load the data
food_categories <- read.csv("food_categories.csv") %>% mutate(category = as.factor(category))
# Check the type of food categories available
levels(food_categories$category)

