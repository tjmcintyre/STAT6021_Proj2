library(tidyverse)
library(ROCR)
library(MASS)
library(dplyr)
library(caret)
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
#Frequency table of wins and losses
table(df17$Win_Binary)
#34 players with a win, 156 players without a win
table(df18$Win_Binary)
############################################# Variables that wouldn't be interesting necessarily ######################################################
#It is obvious the players that score the best, have the highest avg strokes gained and made the most money would be the winners. So I'd avoid these three 
#since it wouldn't make our model necessarily interesting

############################################After fitting some models these may be the most important variables to produce something that works########
par(mfrow=c(3,2))
boxplot(df17$Average.Score~df17$Win_Binary, main = "Boxplots of Avg Score vs Atleast 1 Wins") #try to avoid this variable cause it'd be boring
boxplot(df17$Average.SG.Total~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained Total vs Atleast 1 Wins") #try to avoid this variable cause it'd be boring
boxplot(df17$Money~df17$Win_Binary, main = "Boxplots of Purse vs Atleast 1 Wins")# try to avoid this variable because it'd be boring
boxplot(df17$Points~df17$Win_Binary, main = "Boxplots of Points vs Atleast 1 Wins")
boxplot(df17$Top.10~df17$Win_Binary, main = "Boxplots of Top Tens vs Atleast 1 Wins")
boxplot(df17$Rounds~df17$Win_Binary, main = "Boxplots of Rounds vs Atleast 1 Wins")
########################################################################################################################################################

#############Rounds may be cool to look at to see if purely playing more raises there chances while in the presence of the other variables #############

boxplot(df17$Rounds~df17$Win_Binary, main = "Boxplots of Rounds vs Atleast 1 Wins")

########################################################################################################################################################

###################################3 VaRIABLES THAT ARE CONSIDERED YOUR LONG GAME IN GOLF ##############################################################
par(mfrow=c(1,3))
boxplot(df17$Fairway.Percentage~df17$Win_Binary, main = "Boxplots of Fairway Percentage vs Atleast 1 Wins") #large spread in comparison
boxplot(df17$Avg.Distance~df17$Win_Binary, main = "Boxplots of Avg Distance vs Atleast 1 Wins") #avg distance looks interesting
boxplot(df17$SG.OTT~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained off the tee vs Atleast 1 Wins") # may be a combination of the two above

########################################################################################################################################################

################################### VaRIABLES THAT ARE CONSIDERED YOUR mEDIUM GAME IN GOLF #############################################################

boxplot(df17$gir~df17$Win_Binary, main = "Boxplots of GIR vs Atleast 1 Wins") #does not look very different
boxplot(df17$SG.APR~df17$Win_Binary, main = "Boxplots of SG.APR vs Atleast 1 Wins") #decent spread
boxplot(df17$Average.Scrambling~df17$Win_Binary, main = "Boxplots of Scrambling vs Atleast 1 Wins") #decent spread between the two, better players less scrambling
 
########################################################################################################################################################


################################### VaRIABLES THAT ARE CONSIDERED YOUR SHORT GAME IN GOLF ##############################################################

boxplot(df17$Average.Putts~df17$Win_Binary, main = "Boxplots of Avg Putts vs Atleast 1 Wins") # better players have smaller spread of avg putts
boxplot(df17$Average.SG.Putts~df17$Win_Binary, main = "Boxplots of SG.Putts vs Atleast 1 Wins") # better players gain more strokes on putts
boxplot(df17$SG.ARG~df17$Win_Binary, main = "Boxplots of SG.ARG vs Atleast 1 Wins") # better players gain more strokes chipping

########################################################################################################################################################

####### New approach Stepwise regression using all numerics

full.model <- glm(Win_Binary ~ Rounds + Fairway.Percentage + Avg.Distance
                  + gir + Average.Putts + Average.Scrambling  
                  + Points + Average.SG.Putts + Average.SG.Total + SG.OTT 
                  + SG.OTT + SG.APR + SG.ARG + Money, data = df17, family = binomial)


coef(full.model)

step.model <- full.model %>% stepAIC(trace = FALSE)
step.model

summary(step.model)
1-pchisq(step.model$null.deviance-step.model$deviance,5)

df18 = df[c(1:190),]
par(mfrow=c(1,1))
preds<-predict(step.model,newdata=df18, type="response")
preds[is.na(preds)] <- 0
rates<-prediction(preds, df18$Win_Binary)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main = "ROC Plot")
lines(x = c(0,1), y = c(0,1), col="red")

auc=performance(rates, measure = "auc")
auc@y.values

thresh = .15
table(df18$Win_Binary, preds>thresh)

TN= table(df18$Win_Binary, preds>thresh)[1]
FN= table(df18$Win_Binary, preds>thresh)[2]
FP= table(df18$Win_Binary, preds>thresh)[3]
TP= table(df18$Win_Binary, preds>thresh)[4]
FPR = FP / (TN + FP)
FNR = FN / (FN + TP)

#False positive rate
FPR
#False negative rate
FNR


#get rid of the boring variables, only on playing variables now

full.model <- glm(Win_Binary ~  Fairway.Percentage + Avg.Distance
                  + gir + Average.Putts + Average.Scrambling 
                   + Average.SG.Putts  + SG.OTT 
                  + SG.OTT + SG.APR + SG.ARG, data = df17, family = binomial)

coef(full.model)

step.model <- full.model %>% stepAIC(trace = FALSE)
step.model

summary(step.model)
1-pchisq(step.model$null.deviance-step.model$deviance,6)



df18 = df[c(1:190),]

preds<-predict(step.model,newdata=df18, type="response")
preds[is.na(preds)] <- 0
rates<-prediction(preds, df18$Win_Binary)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result, main="ROC Plot")
lines(x = c(0,1), y = c(0,1), col="red")

auc=performance(rates, measure = "auc")
auc@y.values

thresh = .4
table(df18$Win_Binary, preds>thresh)

TN= table(df18$Win_Binary, preds>thresh)[1]
FN= table(df18$Win_Binary, preds>thresh)[2]
FP= table(df18$Win_Binary, preds>thresh)[3]
TP= table(df18$Win_Binary, preds>thresh)[4]
FPR = FP / (TN + FP)
FNR = FN / (FN + TP)

#False positive rate
FPR
#False negative rate
FNR
