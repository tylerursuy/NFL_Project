library(PerformanceAnalytics)
library(tidyverse)

game_data <- read.csv('../nfl_cleaned.csv')

otgames <- game_data %>% filter(ot == 1)
game_data <- game_data %>% filter(ot == 0)
hmpredpts <- game_data$Over.Under/2 - game_data$homespread/2
awpredpts <- game_data$Over.Under - hmpredpts
predscores <- c(hmpredpts, awpredpts)
scores <- c(game_data$Home.Score, game_data$Away.Score)
hmhtscores <- game_data[,c('hmhalfsc','awhalfsc')]
awhtscores <- game_data[,c('awhalfsc','hmhalfsc')]
colnames(hmhtscores) <- c('tmhalfsc', 'opphalfsc')
colnames(awhtscores) <- c('tmhalfsc', 'opphalfsc')
htscores <- rbind(hmhtscores, awhtscores)
hstats <- game_data[,c('hpatt','hypa','hcomppct','hint','hratt','hypr','hsacks','hsackyds',
                       'hfum','hfuml')]
astats <- game_data[,c('apatt','aypa','acomppct','aint','aratt','aypr','asacks','asackyds',
                       'afum','afuml')]
colnames(hstats) <- c('patt','ypa','comppct','int','ratt','ypr','sacks','sackyds','fum','fuml')
colnames(astats) <- c('patt','ypa','comppct','int','ratt','ypr','sacks','sackyds','fum','fuml')
combined <- rbind(hstats, astats)

reduced <- data.frame(scores, predscores, htscores, combined)
#reduced$scores[reduced$scores != 0] <- log(reduced$scores[reduced$scores != 0])
reduced$scores <- sqrt(reduced$scores)
View(reduced)

write.csv(reduced, "../reduced.csv")

train_sample <- sample.int(nrow(reduced), size = round(.75*nrow(reduced)))
train <- reduced[train_sample,]
test <- reduced[-train_sample,]

scorelm <- lm(scores~., data=train)
summary(scorelm)

null = lm(scores~1, data = train)
stepregr = step(null, scope = list(upper = scorelm), data=train, direction="both", trace=0)
#summary(stepregr)

manualregr <- lm(scores~predscores+tmhalfsc+patt+ypa+ratt+ypr+sackyds, data = train)
manualregr$coefficients

y_pred <- predict(scorelm, newdata = test)
sqrt(sum((test$scores-y_pred)^2))
y_pred <- predict(stepregr, newdata = test)
sqrt(sum((test$scores-y_pred)^2))
y_pred <- predict(manualregr, newdata = test)
sqrt(sum((test$scores-y_pred)^2))

manualregr <- lm(scores~predscores+tmhalfsc+patt+ypa+ratt+ypr+sackyds, data = reduced)
hist(manualregr$residuals)
skewness(manualregr$residuals)
kurtosis(manualregr$residuals)

#dal@atl
newdata <- data.frame('predscores'=c(50/2+1.75, 50/2-1.75), 'tmhalfsc'=c(6,3), 'patt'=c(20,14), 
                      'ypa'=c(136/20,73/14), 'ratt'=c(9,10),'ypr'=c(24/9,48/10), 
                      'sackyds'=c(-17,-11))
#tb@nyg
newdata <- data.frame('predscores'=c(54/2+1.5, 54/2-1.5), 'tmhalfsc'=c(14,7), 'patt'=c(12,14), 
                      'ypa'=c(112/12,103/14), 'ratt'=c(16,15),'ypr'=c(5.4,4.5), 
                      'sackyds'=c(-17,-7))
#pit@jax
newdata <- data.frame('predscores'=c(47/2-2, 47/2+2), 'tmhalfsc'=c(9,0), 'patt'=c(13,19), 
                      'ypa'=c(28/13,53/19), 'ratt'=c(26,6),'ypr'=c(141/26,13/6), 
                      'sackyds'=c(-13,0))
#kc@la
newdata <- data.frame('predscores'=c(64/2+1.5, 64/2-1.5), 'tmhalfsc'=c(23,23), 'patt'=c(21,20), 
                      'ypa'=c(163/21,213/20), 'ratt'=c(10,10),'ypr'=c(3.1,5.5), 
                      'sackyds'=c(-9,-11))

final_scores = predict(manualregr, newdata = newdata)^2
sum(final_scores)
diff(final_scores)

la = rnorm(10000, sqrt(final_scores[1]), sd(manualregr$residuals))^2
kc = rnorm(10000, sqrt(final_scores[2]), sd(manualregr$residuals))^2
1-sum(la>(kc+1.5))/10000
