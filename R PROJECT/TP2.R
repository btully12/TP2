# File Name: TP2_BJT_JRH
## Author: Brett Tully & Jack Hall
### R 
#### Class: DS-160-01
##### Semester: Spring '22







install.packages("tidyverse")
install.packages("ggplot2")
install.packages("data.table")
install.packages("ggridges")
install.packages("pivottabler")
install.packages("tidyer")
install.packages("tibble")
install.packages("hrbrthemes")
install.packages("devtools")
install_github("easyGgplot2", "kassambara")
install.packages("reshape") 
library(pivottabler) 
library(tidyr)
library(tibble)
library(hrbrthemes)
library(devtools) 
library(easyGgplot2)
library(ggridges)
library(reshape)
library(vioplot)
library(data.table)
library("lattice") 
library("tidyselect")
library(ggplot2)
library(dplyr)




df <- read.csv('database.csv')





summary(df)

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

jpeg(file="histogram.jpeg")
histogram(df$avg_score, main="Average APR Scores (2004-2014)",col = 'yellow', xlab = "Scores",ylab = "")
dev.off()





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


jpeg(file="correlation-matrix.jpeg")
heatmap(score_elig)
dev.off()
########

p<-df %>% 
  group_by(SPORT_NAME, NCAA_CONFERENCE,avg_score) %>%
  select(NCAA_CONFERENCE,SPORT_NAME,avg_score)
p


k = aggregate(avg_score ~ NCAA_CONFERENCE+SPORT_NAME, data=df, FUN = mean)
k


p
sport_avg = tapply(p$avg_score,p$SPORT_NAME,mean)
sport_avg




  

con_sport <- df %>% 
  group_by(NCAA_CONFERENCE,avg_score) %>% 
  select(NCAA_CONFERENCE,avg_score)



####




sport_avg <- data.frame(aggregate(as.matrix(piv$avg_score), by=list(Sport=piv$SPORT_NAME), FUN=mean))
sport_avg



conf_avg <- data.frame(aggregate(as.matrix(piv$avg_score), by=list(Conference=piv$NCAA_CONFERENCE), FUN=mean))
conf_avg <- conf_avg[c(4,5,6,7,10,22,29),]
conf_avg
dev.off()

jpeg(file="main-Confrence-scores.jpeg")
ggplot(conf_avg) + 
  geom_point(aes(x = V1, y = reorder(Conference, V1)))
dev.off()
######




# pairs 


dfpair<- dplyr::select(df, -c(
                              "SCHOOL_ID","SCHOOL_NAME",
                              "SCHOOL_TYPE","SPORT_CODE",
                              "SPORT_NAME","NCAA_SUBDIVISION",
                              "NCAA_CONFERENCE"))
####
pairs(data = dfpair)
####JACK





####
Women <- sport_avg[grep("Women's", sport_avg$Sport), ]
Men <- sport_avg[grep("Men's", sport_avg$Sport), ] 

Men$V1
Women

men_av= c(945,968, 980, 971, 979,
 982, 974, 962, 964, 972,
969, 960, 961, 973, 975, 956)

women_av = c(968, 954, 976,988,
986, 982, 987, 987,
987, 982, 980, 979,
974, 982, 977, 972)

mw_avg  = data.frame(men_av,women_av)
mw_avg 





#####



####
#men average and women average graphed
Men_Avg = mean(Men$V1)
Women_Avg = mean(Women$V1)
compare = cbind.data.frame(Men_Avg,Women_Avg)
compare
barplot(data = compare)
#######JACK



jpeg(file="men-sports-scores.jpeg")
ggplot(Men) + 
  geom_point(aes(x = V1, y = reorder(Sport, V1))) 
dev.off()

jpeg(file="womens-sports-scores.jpeg")
ggplot(Women) + 
  geom_point(aes(x = V1, y = reorder(Sport, V1)))
dev.off()

#####
#Men's heatmap


ggplot(p)



jpeg(file="Men's-heatmap.jpeg")
table(Men) %>% 
  as.data.frame() %>% 
  ggplot() + 
  aes(x=Sport, y=V1, fill=Freq) %>% 
  geom_tile()
dev.off()






























