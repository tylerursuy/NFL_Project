library(PerformanceAnalytics)
library(tidyverse)
library(car)

game_data <- read.csv('../nfl_cleaned.csv', stringsAsFactors = F)

game_data$Favorite.Team.ID[game_data$Favorite.Team.ID=='LAR'] <- 'LA'
game_data$home[game_data$home=='JAC'] <- 'JAX'
game_data$away[game_data$away=='JAC'] <- 'JAX'
game_data$home[game_data$home=='SD'] <- 'LAC'
game_data$away[game_data$away=='SD'] <- 'LAC'
game_data$home[game_data$home=='STL'] <- 'LA'
game_data$away[game_data$away=='STL'] <- 'LA'

spread_home <- game_data$Spread.Favorite
spread_home[game_data$Favorite.Team.ID != game_data$home] <- 
  -spread_home[game_data$Favorite.Team.ID != game_data$home]

hmpredpts <- game_data$Over.Under/2 - spread_home/2
awpredpts <- game_data$Over.Under - hmpredpts
predscores <- c(hmpredpts, awpredpts)

scores <- c(game_data$Home.Score, game_data$Away.Score)

hmintpct <- game_data$hint / game_data$hpatt
awintpct <- game_data$aint / game_data$apatt
int_pct <- c(hmintpct, awintpct)

hmsackpct <- game_data$hsacks / (game_data$hsacks + game_data$hpatt)
awsackpct <- game_data$asacks / (game_data$asacks + game_data$apatt)
sack_pct <- c(hmsackpct, awsackpct)

hmsackyds <- game_data$hsackyds / (game_data$hsacks + game_data$hpatt)
awsackyds <- game_data$asackyds / (game_data$asacks + game_data$apatt)
sack_yd_pct <- c(hmsackyds, awsackyds)

hmfumpct <- game_data$hfum / (game_data$hpatt + game_data$hratt + game_data$hsacks)
awfumpct <- game_data$afum / (game_data$apatt + game_data$aratt + game_data$asacks)
fum_pct <- c(hmfumpct, awfumpct)

hmfumlost <- game_data$hfuml / game_data$hfum
awfumlost <- game_data$afuml / game_data$afum
fum_lost_pct <- c(hmfumlost, awfumlost)

# add transformation of pass, rush yards
# add pass, rush yards/att
hairya <- game_data$hairyd/game_data$hpatt; aairya <- game_data$aairyd/game_data$apatt
hyaca <- game_data$hyac/game_data$hpatt; ayaca <- game_data$ayac/game_data$hpatt
hypr <- game_data$hryd/game_data$hratt; aypr <- game_data$aryd/game_data$aratt
airya <- c(hairya, aairya); yaca <- c(hyaca, ayaca); ypr <- c(hypr, aypr)

hmhtscores <- game_data[,c('hmhalfsc','awhalfsc')]
awhtscores <- game_data[,c('awhalfsc','hmhalfsc')]
colnames(hmhtscores) <- c('tmhalfsc', 'opphalfsc')
colnames(awhtscores) <- c('tmhalfsc', 'opphalfsc')
htscores <- rbind(hmhtscores, awhtscores)
hstats <- game_data[,c('hairyd','hyac','hpassd','hpatt','hcomppct','hryd','hratt','hrushsd')]
astats <- game_data[,c('aairyd','ayac','apassd','apatt','acomppct','aryd','aratt','arushsd')]
colnames(hstats) <- c('airyd','yac','psd','patt','comppct','ryd','ratt','rushsd')
colnames(astats) <- colnames(hstats)
combined <- rbind(hstats, astats)

reduced <- data.frame(date=rep(game_data$Date,2), team=c(game_data$Home.Team, game_data$Away.Team),
                      ot=rep(game_data$ot, 2),
                      scores, predscores, htscores, airya, yaca, combined, ypr, int_pct, 
                      sack_pct, sack_yd_pct, fum_pct)
reduced <- reduced %>% filter(ot==0)
reduced_noot <- reduced[,-c(1:3)]

View(reduced_noot)
View(reduced)

write.csv(reduced, "../reduced_wdate.csv")
write.csv(reduced_noot, "../reduced_noot.csv")

reduced$scores <- sqrt(reduced$scores)

train_sample <- sample.int(nrow(reduced), size = round(.75*nrow(reduced)))
train <- reduced[train_sample,]
test <- reduced[-train_sample,]

scorelm <- lm(scores~., data=train)
#summary(scorelm)
#vif(scorelm)

null = lm(scores~1, data = train)
stepregr = step(null, scope = list(upper = scorelm), data=train, direction="both", trace=0)
#summary(stepregr)

manualregr <- lm(scores~predscores+tmhalfsc+opphalfsc+sack_yd_pct+int_pct+airya+yaca+
                   psd+rushsd, data = train)
#summary(manualregr)
#vif(manualregr)

y_pred <- predict(scorelm, newdata = test)
sqrt(sum((test$scores-y_pred)^2) / nrow(test))
y_pred_step <- predict(stepregr, newdata = test)
sqrt(sum((test$scores-y_pred_step)^2) / nrow(test))
y_pred_man <- predict(manualregr, newdata = test)
sqrt(sum((test$scores-y_pred_man)^2) / nrow(test))

skewness(manualregr$residuals)
kurtosis(manualregr$residuals)
hist(manualregr$residuals)
plot(manualregr)

split <- length(y_pred_man) / 2
hm_final_pred <- round((y_pred_man[0:split])^2)
aw_final_pred <-round((y_pred_man[(split+1):length(y_pred_man)])^2)
real_hm <- round(test$scores[0:split]^2)
real_aw <- round(test$scores[(split+1):length(test$scores)]^2)
pred_df <- data.frame(hm_final_pred, aw_final_pred, real_hm, real_aw)
pred_df <- pred_df %>% 
  mutate(pred_home_win = ifelse(hm_final_pred > aw_final_pred, 1, 0)) %>% 
  mutate(real_home_win = ifelse(real_hm > real_aw, 1, 0)) %>% 
  mutate(correct = ifelse(pred_home_win == real_home_win, 1, 0))

correct <- pred_df$correct
right <- sum(correct)
wrong <- length(correct) - right
right / (right + wrong)

manualregr_log <- glm(scores~predscores+tmhalfsc+sack_yd_pct+int_pct+sack_pct+ypa,
                      data = train, family = "binomial")
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
