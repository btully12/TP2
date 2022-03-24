install.packages("tidyverse")
install.packages("ggplot2")
library("lattice") 
library("tidyselect")
library(ggplot2)
library(dplyr)
install.packages("pivottabler")
library(pivottabler)
df <- read.csv('database.csv')


summary(df)


df = unique(df)

head(df)

dim(df)

object.size(df)




df<-subset(df, df$X2010_ATHLETES != '-99')
df<-subset(df, df$X2009_ATHLETES != '-99')
df<-subset(df, df$X2008_ATHLETES != '-99')
df<-subset(df, df$X2007_ATHLETES != '-99')
df<-subset(df, df$X2006_ATHLETES != '-99')
df<-subset(df, df$X2005_ATHLETES != '-99')
df<-subset(df, df$X2004_ATHLETES != '-99')
df<-subset(df, df$X2013_RETENTION != '-99')
df<-subset(df, df$NCAA_SUBDIVISION != '-99')
 


keep <- c('FOURYEAR_ATHLETES','2014_ATHLETES', '2013_ATHLETES', '2012_ATHLETES',
           '2011_ATHLETES', '2010_ATHLETES', '2009_ATHLETES', '2008_ATHLETES',
           '2007_ATHLETES', '2006_ATHLETES', '2005_ATHLETES', '2004_ATHLETES')

new <- (mean(c(15,19,23,27,31,35,39,43,47,51,55)))

athlete <- df[, colnames(df)[mean(c(15,19,23,27,31,35,39,43,47,51,55))]]

athlete <- df[[mean((df$X2014_SCORE X2013_SCORE '2012_SCORE','2011_SCORE','2010_SCORE','2009_SCORE','2008_SCORE','2007_SCORE','2006_SCORE','2005_SCORE','2004_SCORE'))]]

df['avg_score'] <- athlete



piv <- df %>% 
  group_by(NCAA_CONFERENCE,avg_score) %>% 
  select(NCAA_CONFERENCE,SPORT_NAME,avg_score)

pt <- PivotTable$new() 
pt$addData(piv)
pt$renderPivot()
pt$addColumnDataGroups("NCAA_CONFERENCE")
pt$addRowDataGroups()
pt


df



athlete <- df[, colnames(df)[mean(c('2014_SCORE','2013_SCORE','2012_SCORE','2011_SCORE','2010_SCORE','2009_SCORE','2008_SCORE','2007_SCORE','2006_SCORE','2005_SCORE','2004_SCORE'))]]

hist(athlete)
xx <-(table(athlete))
xx

hist(xx,freq = TRUE,
     ylim=c(1,98))
ath_mean <- colMeans(athlete)


t = cor(athlete)

heatmap(t)

hist(xx,main="Average Scores of Athletes (2004-14)",
     xlim(0,100))




t = cor(athlete)






t<-data.matrix(df, rownames.force = NA)



conference <- data.frame.groupby(df,'NCAA_CONFERENCE')




DDD























df %>%
  group_by(SPORT_NAME,NCAA_DIVISION) 





