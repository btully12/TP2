install.packages("tidyverse")
install.packages("ggplot2")
install.packages("data.table")
library(data.table)
library("lattice") 
library("tidyselect")
library(ggplot2)
library(dplyr)
install.packages("pivottabler")
library(pivottabler) 
install.packages("tidyer")
install.packages("tibble")
install.packages("hrbrthemes")
library(tidyr)
library(tibble)
library(hrbrthemes)



df <- read.csv('database.csv')







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


 
summary(df)




#######
df$avg_score = ((df$X2014_SCORE+df$X2013_SCORE+df$X2012_SCORE+df$X2011_SCORE+df$X2010_SCORE+df$X2009_SCORE+df$X2008_SCORE+df$X2007_SCORE+df$X2006_SCORE+df$X2005_SCORE+df$X2004_SCORE)/11)

df$avg_score = round(df$avg_score,digit=2)

df$avg_score = as.numeric(df$avg_score)

histogram(df$avg_score, main="Average APR Scores (2004-2014)",col = 'yellow', xlab = "Scores",ylab = "")






####
names(df)


dfcor <- dplyr::select(df, -c("ACADEMIC_YEAR","NCAA_DIVISION",
                              "SCHOOL_ID","SCHOOL_NAME",
                              "SCHOOL_TYPE","SPORT_CODE",
                              "SPORT_NAME","NCAA_SUBDIVISION",
                              "NCAA_CONFERENCE","FOURYEAR_RETENTION",
                              "X2014_ATHLETES","X2014_RETENTION", 
                              "X2013_ATHLETES","X2013_RETENTION",
                              "X2012_ATHLETES", "X2012_RETENTION",
                              "X2011_ATHLETES", "X2011_RETENTION" ,
                              "X2010_ATHLETES", "X2010_RETENTION",
                              "X2009_ATHLETES","X2009_RETENTION" ,
                              "X2008_ATHLETES",  "X2008_RETENTION",
                              "X2007_ATHLETES",
                              "X2007_RETENTION","X2006_ATHLETES", 
                              "X2006_RETENTION",
                              "X2005_ATHLETES",
                              "X2005_RETENTION",
                              "X2004_ATHLETES","X2004_RETENTION",
                              "avg_score","FOURYEAR_ATHLETES","FOURYEAR_SCORE"))

score_elig = cor(dfcor)

heatmap(score_elig)
########

p<-df %>% 
  group_by(SPORT_NAME, NCAA_CONFERENCE,avg_score) %>%
  select(NCAA_CONFERENCE,SPORT_NAME,avg_score)



aggregate(avg_score ~ SPORT_NAME + NCAA_DIVISION, data=df, FUN = mean)


p
sport_avg = tapply(p$avg_score,p$SPORT_NAME,mean)
sport_avg



piv <- df %>% 
  group_by(NCAA_CONFERENCE,SPORT_NAME,avg_score,NCAA_DIVISION) %>% 
  select(NCAA_CONFERENCE,SPORT_NAME,avg_score,NCAA_DIVISION)
  
con_sport <- df %>% 
  group_by(NCAA_CONFERENCE,avg_score) %>% 
  select(NCAA_CONFERENCE,avg_score)

t <- dplyr::select(con_sport$NCAA_CONFERENCE,
-c('America East Conference', 'American Athletic Conference',
   'Atlantic 10 Conference',
'Big Sky Conference', 'Big South Conference',
 'Big West Conference', 'Centennial Conference',
'East Coast Conference', 'Empire 8',
'Great Lakes Intercollegiate Athletic Conference',
'Great Northwest Athletic Conference', 'Gulf South Conference',
'Horizon League', 'Liberty League',
'Metro Atlantic Athletic Conference', 'Mid-American Conference',
'Mid-Eastern Athletic Conference', 'Missouri Valley Conference',
'Mountain West Conference', 'Northeast Conference',
'Northeast-10 Conference', 'Northern Sun Intercollegiate Conference',
'Ohio Valley Conference',  'Patriot League',
'Peach Belt Conference', 'Pennsylvania State Athletic Conference',
'Southern Collegiate Athletic Conference',
'Southern Conference', 'Southland Conference',
'Southwestern Athletic Conference', 'Sun Belt Conference',
'The Summit League', 'West Coast Conference',
'Western Athletic Conference'))

####




sport_avg <- data.frame(aggregate(as.matrix(piv$avg_score), by=list(Sport=piv$SPORT_NAME), FUN=mean))
sport_avg



conf_avg <- data.frame(aggregate(as.matrix(piv$avg_score), by=list(Conference=piv$NCAA_CONFERENCE), FUN=mean))
conf_avg <- conf_avg[c(4,5,6,7,10,22,29),]
conf_avg

ggplot(conf_avg) + 
  geom_point(aes(x = V1, y = reorder(Conference, V1)))
######




# pairs 


dfpair<- dplyr::select(df, -c(
                              "SCHOOL_ID","SCHOOL_NAME",
                              "SCHOOL_TYPE","SPORT_CODE",
                              "SPORT_NAME","NCAA_SUBDIVISION",
                              "NCAA_CONFERENCE"))

graphics.off() 
par("mar") 
par(mar=c(1,1,1,1))
pairs(FOURYEAR_RETENTION ~ ., data = dfpair)






####
Men <- sport_avg[grep("Men's", sport_avg$Sport), ] 
Men=Men[-5,]

Men=Men[-3,]
Men=Men[-6,]
Men=Men[-9,]
Men=Men[-2,] 
Men=Men[-9,]
Men=Men[-9,]


Women <- sport_avg[grep("Women's", sport_avg$Sport), ]
#####


piv %>% 
  as_tibble() %>% 
  rowid_to_column(var = "avg_score") %>% 
  gather(key="SPORT_NAME", value="NCAA_DIVISION",) %>% 
  mutate(avg_score=as.numeric(gsub("V","",V1))) %>% 
  ggplot(aes(Sport,V1,fill=V1)) + 
  geom_tile() +
  theme_ipsum() + 
  theme(legend.position="none")




####

Men_Avg = mean(Men$V1)
Women_Avg = mean(Women$V1)
compare = cbind(Men_Avg,Women_Avg)
hist(sports_avg$avg_score)
#####

barplot(compare, beside=T,ylim = c(0,1000), col = c("red","green"),label = ) 

ggplot(piv, aes(x = SPORT_NAME, y = avg_score)) + 
  geom_col(colour = "blue") + 
  scale_fill_brewer(palette = "Pastel1")




table(Men) %>% 
  as.data.frame() %>% 
  ggplot() + 
  aes(x=Sport, y=V1, fill=Freq) %>% 
  geom_tile()




heatmap(
)
pt <- PivotTable$new() %>%
  pt$addData(pew) %>%
  pt$defineCalculation(calculationName = "avg_score") %>%
  pt$addColumnDataGroups("SPORT_NAME") %>%
  pt$addRowDataGroups("NCAA_DIVISION") %>%
  pt$addRowDataGroups("NCAA_CONFERENCE")

qpvt(pew,"Sport","V1")

pew








qpvt(pew, rows = "Sport",calculations = "V1")

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





