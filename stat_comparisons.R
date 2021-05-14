library(tidyverse)
library(ROCR)
library(MASS)
library(dplyr)
library(caret)
library(coin)
####### Project 2 PGA Tour Analysis #######
data = read.csv('pgaTourData.csv')
data = as.data.frame(data)

# The bottom half of the data set contains many empty observations 
df = data[-c(1679:2312),]

#Create Atleast 1 win binary variable
df$Win_Binary <- ifelse(df$Wins >=1 , 1,0)

#For the NA observations in the column adjust them to 0
df$Win_Binary[is.na(df$Win_Binary)] <- 0

#make win binary a character variable
df$Win_Binary = factor(df$Win_Binary)

#Select only year 2017 for training set
df18 = df[c(1:193),]
#Select only year 2018 for test set
df17 = df[c(194:383),]


df17$Win_Binary = relevel(df17$Win_Binary, ref = "0")
df18$Win_Binary = relevel(df18$Win_Binary, ref = "0")

#game metrics

shapiro.test(df17$Average.Score[df17$Win_Binary==1])
shapiro.test(df17$Average.Score[df17$Win_Binary==0])
median_test(Average.Score~Win_Binary, data = df17)

shapiro.test(df17$Average.SG.Total[df17$Win_Binary==1])
shapiro.test(df17$Average.SG.Total[df17$Win_Binary==0])
median_test(Average.SG.Total~Win_Binary, data = df17)

shapiro.test(df17$Money[df17$Win_Binary==1])
shapiro.test(df17$Money[df17$Win_Binary==0])
median_test(Money~Win_Binary, data = df17)

shapiro.test(df17$Points[df17$Win_Binary==1])
shapiro.test(df17$Points[df17$Win_Binary==0])
median_test(Points~Win_Binary, data = df17)

shapiro.test(df17$Top.10[df17$Win_Binary==1])
shapiro.test(df17$Top.10[df17$Win_Binary==0])
median_test(Top.10~Win_Binary, data = df17)

shapiro.test(df17$Rounds[df17$Win_Binary==1])
shapiro.test(df17$Rounds[df17$Win_Binary==0])
median_test(Rounds~Win_Binary, data = df17)


#long game

shapiro.test(df17$Fairway.Percentage[df17$Win_Binary==1])
shapiro.test(df17$Fairway.Percentage[df17$Win_Binary==0])
t.test(df17$Fairway.Percentage ~ df17$Win_Binary)

shapiro.test(df17$Avg.Distance[df17$Win_Binary==1])
shapiro.test(df17$Avg.Distance[df17$Win_Binary==0])
t.test(df17$Avg.Distance ~ df17$Win_Binary)

shapiro.test(df17$SG.OTT[df17$Win_Binary==1])
shapiro.test(df17$SG.OTT[df17$Win_Binary==0])
median_test(SG.OTT~Win_Binary, data = df17)




#medium game

shapiro.test(df17$gir[df17$Win_Binary==1])
shapiro.test(df17$gir[df17$Win_Binary==0])
median_test(gir~Win_Binary, data = df17)

shapiro.test(df17$SG.APR[df17$Win_Binary==1])
shapiro.test(df17$SG.APR[df17$Win_Binary==0])
t.test(df17$SG.APR ~ df17$Win_Binary)

shapiro.test(df17$Average.Scrambling[df17$Win_Binary==1])
shapiro.test(df17$Average.Scrambling[df17$Win_Binary==0])
t.test(df17$Average.Scrambling ~ df17$Win_Binary)


#short game

shapiro.test(df17$Average.Putts[df17$Win_Binary==1])
shapiro.test(df17$Average.Putts[df17$Win_Binary==0])
t.test(df17$Average.Putts ~ df17$Win_Binary)

shapiro.test(df17$Average.SG.Putts[df17$Win_Binary==1])
shapiro.test(df17$Average.SG.Putts[df17$Win_Binary==0])
t.test(df17$Average.SG.Putts ~ df17$Win_Binary)

shapiro.test(df17$SG.ARG[df17$Win_Binary==1])
shapiro.test(df17$SG.ARG[df17$Win_Binary==0])
t.test(df17$SG.ARG ~ df17$Win_Binary)
