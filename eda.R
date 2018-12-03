# perform EDA
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gridExtra)

# load data
nfl_data <- read_csv('/Users/brianwright/classes/msds621-ML/NFL_Project/reduced_noot.csv')
head(nfl_data)

#max(nfl_data$scores)^2

# add indicator column for whether team is home or away
nfl_data_with_flag = nfl_data %>% mutate(team = ifelse(X1 < 1342,"H","A"),net_diff = scores^2-predscores,ypa=airya+yaca)
head(nfl_data_with_flag)

# exploring some columns
pass_hist <- ggplot(nfl_data_with_flag) + geom_histogram(aes(patt))
rush_hist <- ggplot(nfl_data_with_flag) + geom_histogram(aes(ratt))
grid.arrange(pass_hist,rush_hist,ncol=2)


# plot of predicted score vs halftime score 
pred_vs_half <- ggplot(data = nfl_data_with_flag)  + geom_smooth(formula = y ~ x,aes(x = (predscores),y=(tmhalfsc),color=team),se=FALSE) + labs(x='Predicted Score',y='Halftime Score')+scale_color_discrete(name='Team')+theme_light() #+geom_abline(aes(intercept=0,slope=1/2))
pred_vs_end <- ggplot(data = nfl_data_with_flag)  + geom_smooth(formula = y ~ x,aes(x = (predscores),y=(scores^2),color=team),se=FALSE) +theme_light()#+ geom_point(aes(x=sqrt(predscores),y=sqrt(tmhalfsc),color=flag))
half_vs_end <-  ggplot(data = nfl_data_with_flag)  +theme_light()+ geom_smooth(formula = y ~ x,aes(x = (tmhalfsc),y=(scores^2),color=team),se=FALSE)
grid.arrange(pred_vs_half,pred_vs_end,half_vs_end, ncol=3)



#comparing density of scores, and predscores for home vs away
p1 <- ggplot(data = nfl_data_with_flag)  + geom_density(aes(predscores,fill=team),alpha=.5)
p2 <- ggplot(data = nfl_data_with_flag) + geom_density(aes(tmhalfsc,fill=team),alpha=.5)
p3 <- ggplot(data = nfl_data_with_flag) + geom_density(aes(scores^2,fill=team),alpha=.5)
grid.arrange(p1,p2,p3,ncol=3)
grid.arrange(p1,p3,ncol=2)


# plotting dist of scores for each amount of fumbles, int, and sacks
int_score_density <- ggplot(data = nfl_data_with_flag)  + geom_density(aes(scores^2,fill=as.factor(int)),alpha=.5)
sacks_score_density <- ggplot(data = nfl_data_with_flag)  + geom_density(aes(scores^2,fill=as.factor(sacks)),alpha=.5)
fumble_score_density <- ggplot(data = nfl_data_with_flag)  + geom_density(aes(scores^2,fill=as.factor(fuml)),alpha=.5)
grid.arrange(int_score_density,fumble_score_density,sacks_score_density,nrow=3)



# compare scatter of pred vs final, half vs final, and pred vs half, with respective AB line added
sctr_pre_final <- ggplot(nfl_data_with_flag) + geom_jitter(aes(x=predscores,y=scores^2,color=as.factor(team)),alpha=.5)+geom_abline(aes(intercept=0,slope=1))
sctr_pre_half <- ggplot(nfl_data_with_flag) + geom_jitter(aes(x=predscores,y=tmhalfsc,color=as.factor(team)),alpha=.5)+geom_abline(aes(intercept=0,slope=1/2))
sctr_half_final <- ggplot(nfl_data_with_flag) + geom_jitter(aes(x=tmhalfsc,y=scores^2,color=as.factor(team)),alpha=.5)+geom_abline(aes(intercept=0,slope=2))
grid.arrange(sctr_pre_final,sctr_pre_half,sctr_half_final,nrow=3)



# explore how passing influences score amongst other relationships
ggplot(data = nfl_data_with_flag) + geom_smooth(formula = y~x,aes(x=comppct,y=scores^2,color=team),se=FALSE)
ggplot(data=nfl_data_with_flag) + geom_smooth(formula = y~x,aes(x=patt,y=comppct,color=team),se=F)

ggplot(data=nfl_data_with_flag) + geom_jitter(formula = y~x,aes(x=patt,y=ypa,color=team),alpha=.6)

# rush att vs pass att, not very helpful
ggplot(data=nfl_data_with_flag) + geom_jitter(formula = y~x,aes(x=patt,y=ratt,color=team))

# rush att vs score, not very helpful
ggplot(data=nfl_data_with_flag) + geom_jitter(formula = y~x,aes(x=ratt,y=scores^2,color=team),alpha=.4)+geom_smooth(formula = y~x,aes(x=ratt,y=scores^2,color=team))


# hist of net
ggplot(data=nfl_data_with_flag) + geom_histogram(aes(net_diff,color=team))






# exploring new variables, like int_pct, sack_yd_pct, etc
ggplot(data = nfl_data_with_flag) + geom_smooth(aes(x=int_pct,y=scores,color=team))
ggplot(data = nfl_data_with_flag) + geom_smooth(aes(x=sack_pct,y=scores,color=team))
ggplot(data = nfl_data_with_flag) + geom_smooth(aes(x=sack_yd_pct,y=scores,color=team))
ggplot(data = nfl_data_with_flag) + geom_smooth(aes(x=fum_pct,y=scores,color=team))


