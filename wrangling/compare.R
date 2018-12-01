library(rvest)
library(tidyverse)

all_scores <- NULL
for(yr in 2009:2017) {
  url <- paste("https://www.pro-football-reference.com/years/",yr,"/games.htm",sep="")
  page <- read_html(url)
  scores <- html_table(page)[[1]]
  scores <- scores[!scores$Date %in% c('Date', 'Playoffs'),]
  dates <- sapply(scores$Date, FUN = convertdate)
  home <- scores$`Winner/tie`
  home[scores[,6]=='@'] <- scores$`Loser/tie`[scores[,6]=='@']
  hmpts <- scores$PtsW; awpts <- scores$PtsL
  hmpts[scores[,6]=='@'] <- scores$PtsL[scores[,6]=='@']
  awpts[scores[,6]=='@'] <- scores$PtsW[scores[,6]=='@']
  hmyds <- scores$YdsW; awyds <- scores$YdsL
  hmyds[scores[,6]=='@'] <- scores$YdsL[scores[,6]=='@']
  awyds[scores[,6]=='@'] <- scores$YdsW[scores[,6]=='@']
  hmto <- scores$TOW; awto <- scores$TOL
  hmto[scores[,6]=='@'] <- scores$TOL[scores[,6]=='@']
  awto[scores[,6]=='@'] <- scores$TOW[scores[,6]=='@']
  stats <- data.frame('date'=dates, home, 'hmpts'=as.numeric(hmpts), 'awpts'=as.numeric(awpts), 
                      'hmyds'=as.numeric(hmyds), 'awyds'=as.numeric(awyds), 'hmto'=as.numeric(hmto),
                      'awto'=as.numeric(awto), stringsAsFactors = F)
  all_scores <- rbind(all_scores, stats)
}
#View(all_scores)

teams <- unique(all_scores$home)
teams <- teams[order(teams)]
teams <- teams[!teams %in% c("", "Loser/tie")]

wrangled <- read.csv('../nfl_cleaned_full.csv')
teams2 <- unique(wrangled$home)
teams2 <- as.character(teams2[order(teams2)])

date <- str_sub(wrangled$gameid, end = -3)
#home <- sapply(wrangled$home, FUN = convertteam)
home <- wrangled$Home.Team
relevant <- data.frame(date, home, wrangled[,c('gameid','ot','homesc','awaysc','htotyds',
                                               'atotyds','htos','atos')], stringsAsFactors = F)

merged <- merge(relevant, all_scores, all.x = TRUE, all.y = FALSE)
merged <- merged[!is.na(merged$homesc),]
merged <- merged[!is.na(merged$hmpts),]
otgames <- merged[merged$ot==1,]
merged <- merged[merged$ot == 0,]

sum(merged$homesc==merged$hmpts & merged$awaysc==merged$awpts)
sum(merged$htotyds >= merged$hmyds-8 & merged$htotyds <= merged$hmyds+8 & 
      merged$atotyds >= merged$awyds-8 & merged$atotyds <= merged$awyds+8)
sum(merged$htos==merged$hmto & merged$atos==merged$awto)

valid_strict <- merged$homesc==merged$hmpts & merged$awaysc==merged$awpts &
  abs(merged$htotyds-merged$hmyds)<=8 & abs(merged$atotyds-merged$awyds)<=8 &
  merged$htos==merged$hmto & merged$atos==merged$awto
sum(valid_strict)

valid_loose <- abs(merged$homesc-merged$hmpts)<=1 & abs(merged$awaysc-merged$awpts)<=1 & 
  abs(merged$htotyds-merged$hmyds)<=8 & abs(merged$atotyds-merged$awyds)<=8 &
  abs(merged$htos-merged$hmto)<=1 & abs(merged$atos-merged$awto)<=1
sum(valid_loose)

valid_ot_ids = otgames$gameid
valid_gameids <- merged$gameid[valid_loose]
valid_gameids <- c(valid_gameids, valid_ot_ids)
valid_gameids <- data.frame(valid_gameids, stringsAsFactors = F)
write.csv(valid_gameids, '../valid_gameids.csv')

View(merged[which(merged$homesc != merged$hmpts),
            c('date','home','homesc','hmpts','awaysc','awpts')])
View(merged[which(merged$htos != merged$hmto),c('date','home','htos','hmto','atos','awto')])

convertteam <- function(team) {
  index <- which(teams2==team)
  if(index <= 15 || index == 30 || index == 18) return(teams[index])
  else {
    if(index == 31 || index == 19) return(teams[index-2])
    else return(teams[index-1])
  }
}
convertdate <- function(date, year=yr) {
  split <- str_split(date, ' ')[[1]]
  if(split[1] == 'September') {
    if(str_length(split[2])==1) return(paste(year, '090', split[2], sep = ''))
    else return(paste(year, '09', split[2], sep = ''))
  }
  else if(split[1] == 'October') {
    if(str_length(split[2])==1) return(paste(year, '100', split[2], sep = ''))
    else return(paste(year, '10', split[2], sep = ''))
  }
  else if(split[1] == 'November') {
    if(str_length(split[2])==1) return(paste(year, '110', split[2], sep = ''))
    else return(paste(year, '11', split[2], sep = ''))
  }
  else if(split[1] == 'December') {
    if(str_length(split[2])==1) return(paste(year, '120', split[2], sep = ''))
    else return(paste(year, '12', split[2], sep = ''))
  }
  else if(split[1] == 'January') {
    if(str_length(split[2])==1) return(paste(year+1, '010', split[2], sep = ''))
    else return(paste(year+1, '01', split[2], sep = ''))
  }
  else if(split[1] == 'February') {
    if(str_length(split[2])==1) return(paste(year+1, '020', split[2], sep = ''))
    else return(paste(year+1, '02', split[2], sep = ''))
  }
}
