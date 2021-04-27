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
 
boxplot(df17$Average.Score~df17$Win_Binary, main = "Boxplots of Avg Score vs Atleast 1 Wins") #try to avoid this variable cause it'd be boring
boxplot(df17$Average.SG.Total~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained Total vs Atleast 1 Wins") #try to avoid this variable cause it'd be boring
boxplot(df17$Money~df17$Win_Binary, main = "Boxplots of Purse vs Atleast 1 Wins")# try to avoid this variable because it'd be boring
boxplot(df17$Points~df17$Win_Binary, main = "Boxplots of Purse vs Atleast 1 Wins")
boxplot(df17$Top.10~df17$Win_Binary, main = "Boxplots of Purse vs Atleast 1 Wins")
########################################################################################################################################################

#############Rounds may be cool to look at to see if purely playing more raises there chances while in the presence of the other variables #############

boxplot(df17$Rounds~df17$Win_Binary, main = "Boxplots of Rounds vs Atleast 1 Wins")

########################################################################################################################################################

###################################3 VaRIABLES THAT ARE CONSIDERED YOUR LONG GAME IN GOLF ##############################################################

boxplot(df17$Fairway.Percentage~df17$Win_Binary, main = "Boxplots of Fairway Percentage vs Atleast 1 Wins") #large spread in comparison
boxplot(df17$Avg.Distance~df17$Win_Binary, main = "Boxplots of Avg Distance vs Atleast 1 Wins") #avg distance looks interesting
boxplot(df17$SG.OTT~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained off the tee vs Atleast 1 Wins") # may be a combination of the two above

########################################################################################################################################################

################################### VaRIABLES THAT ARE CONSIDERED YOUR mEDIUM GAME IN GOLF #############################################################

boxplot(df17$gir~df17$Win_Binary, main = "Boxplots of Greens in Regulation vs Atleast 1 Wins") #does not look very different
boxplot(df17$SG.APR~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained Approaching the green vs Atleast 1 Wins") #decent spread
boxplot(df17$Average.Scrambling~df17$Win_Binary, main = "Boxplots of Avg Scrambling in Regulation vs Atleast 1 Wins") #decent spread between the two, better players less scrambling
 
########################################################################################################################################################


################################### VaRIABLES THAT ARE CONSIDERED YOUR SHORT GAME IN GOLF ##############################################################

boxplot(df17$Average.Putts~df17$Win_Binary, main = "Boxplots of Avg Putts vs Atleast 1 Wins") # better players have smaller spread of avg putts
boxplot(df17$Average.SG.Putts~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained Putts vs Atleast 1 Wins") # better players gain more strokes on putts
boxplot(df17$SG.ARG~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained around the green vs Atleast 1 Wins") # better players gain more strokes chipping

########################################################################################################################################################


#First want to build a model purely on the Strokes gained variables
#SG.OTT is looking at how much better they do off the tee or their long game
#SG.APR is looking at how much better they do with irons and woods. Usually the second/third shots aka their medium game
#SG.ARG is looking at how much better they do with their wedges. Chipping around the green, aka their short game
#SG.Putts is looking at how much better they do putting. 
# I think this could be interesting model in predicting winners and what is truly the most important facet of a golfers game.

sg.mod = glm(df17$Win_Binary ~ df17$SG.OTT + df17$SG.APR + df17$SG.ARG + df17$Average.Putts, family = "binomial")
summary(sg.mod)

1-pchisq(sg.mod$null.deviance-sg.mod$deviance,4)

#reject the null and conclude atleast one of the coefficients is significant in making the model useful

#lets remove SG.APR and SG.AGR, aka the middle game and short game
sg.mod.red = glm(df17$Win_Binary ~ df17$SG.OTT + df17$Average.Putts, family = "binomial")
summary(sg.mod.red)

1-pchisq(sg.mod.red$deviance-sg.mod$deviance,2)
#fail to reject the null hypothesis and go with the reduced model

df18 = df[c(1:190),] #removed 3 rows to make ROC work, need same number of rows

preds<-predict(sg.mod.red,newdata=df18, type="response")
rates<-prediction(preds, df18$Win_Binary)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result)
lines(x = c(0,1), y = c(0,1), col="red")

#ROC curve is barely better then random guessing, thus lets just add the SG,Total variable in instead of it broken down

sg.tot =  glm(df17$Win_Binary ~ df17$Average.SG.Total, family = "binomial")
summary(sg.tot)

preds<-predict(sg.tot,newdata=df18, type="response")
rates<-prediction(preds, df18$Win_Binary)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result)
lines(x = c(0,1), y = c(0,1), col="red")

#having SG.Total produces a worse ROC plot so we will return to sg.mod.red
#lets add all other variables into the sg.mod.red model besides the one label rounds, money, score and ,sg total
mod.full = glm(df17$Win_Binary ~ df17$SG.OTT + df17$Average.Putts + df17$Avg.Distance + df17$Fairway.Percentage + df17$gir + df17$Average.Scrambling
               +df17$gir + df17$Average.Scrambling , family = "binomial")
summary(mod.full)

preds<-predict(mod.full,newdata=df18, type="response")
rates<-prediction(preds, df18$Win_Binary)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result)
lines(x = c(0,1), y = c(0,1), col="red")

#poor fit as well as predicted from the summary output

### boring variable model
mod.boring =  glm(df17$Win_Binary ~ df17$Rounds +df17$Points + df17$Money +df17$Average.SG.Total + df17$Top.10 , family = "binomial")
summary(mod.boring)

mod.boring.red = glm(df17$Win_Binary ~ df17$Points  +df17$Average.SG.Total +df17$Top.10 , family = "binomial")
summary(mod.boring.red)

1-pchisq(mod.boring.red$deviance-mod.boring$deviance,2)
#fail to reject null and go with smaller model

preds<-predict(mod.boring.red,newdata=df18, type="response")
rates<-prediction(preds, df18$Win_Binary)
roc_result<-performance(rates,measure="tpr", x.measure="fpr")
plot(roc_result)
lines(x = c(0,1), y = c(0,1), col="red")



