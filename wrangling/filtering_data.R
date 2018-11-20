# cleaning the data for analysis

library(dplyr)
library(tidyverse)

# load data
nfl_data <- read_csv("/Users/eddieowens/Desktop/USF/FallMod2/ML/project/NFL_Project/nfl_cleaned.csv")

# 1. filter out any rows where the home and away scores do not match (cols E,F and L,M)
nfl_data <- nfl_data %>% filter(`Home Score` == homesc,`Away Score` == awaysc)


# 2. filter out games that went to ot (see ot column)
nfl_data <- nfl_data %>% filter(ot==0)

# 3. convert the spread column to be relative to the home team
nfl_data <- nfl_data %>% mutate(`Spread Favorite` = ifelse(`Favorite Team ID` == home,`Spread Favorite`,-`Spread Favorite`))

# 4. if you want to explore the classification problem, create a column where 1 is a home win and 0 is an away win
nfl_data <- nfl_data %>% mutate(home_wn = as.integer(`Home Score`>`Away Score`))
glimpse(nfl_data)

write.csv(nfl_data,file = "nfl_filtered.csv")
