load("/Users/eddieowens/Desktop/USF/FallMod2/ML/project/wrangling/withdata.RData")
library(tidyverse)

nfl <- read.csv('../nfl_refined.csv', stringsAsFactors = F)

sample <- nfl %>% filter(HomeTeam=='DET', AwayTeam=='GB', posteam=='DET',
                         Season==2017, PassAttempt==1, PlayType!='No Play') %>% 
  select(desc, Yards.Gained, AirYards, YardsAfterCatch, Passer, PlayType)
View(sample)

test <- sample %>% group_by(Rusher) %>% summarise(totyds = sum(Yards.Gained))
View(test)

fums <- nfl %>% filter(Fumble==1) %>% select(desc, posteam, RecFumbTeam)
View(fums)
