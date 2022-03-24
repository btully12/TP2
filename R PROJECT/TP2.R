install.packages("tidyverse")
install.packages("ggplot2")
library("lattice") 
library("tidyselect")
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

athlete <- df[, colnames(df)[c(15,19,23,27,31,35,39,43,47,51,55)]]

t<-data.matrix(df, rownames.force = NA)
ath_mean <- colMeans(athlete)


p <- ggplot(athlete, aes(x=ath_mean)) + 
  geom_histogram()
geom_histogram(athlete)

