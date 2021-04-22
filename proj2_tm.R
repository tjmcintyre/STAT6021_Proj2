####### Project 2 PGA Tour Analysis #######
data = read.csv('pgaTourData.csv')
data = as.data.frame(data)

# The bottom half of the data set contains many empty observations 
df = data[-c(1679:2312),]

#Create Atleast 1 win binary variable
df$Win_Binary <- ifelse(df$Wins >=1 , 1,0)

#For the NA observations in the column adjust them to 0
df$Win_Binary[is.na(df$Win_Binary)] <- 0

#Select only year 2017 for training set
df18 = df[c(1:193),]
#Select only year 2018 for test set
df17 = df[c(194:383),]


boxplot(df17$Rounds~df17$Win_Binary, main = "Boxplots of Rounds vs Atleast 1 Wins")
boxplot(df17$Fairway.Percentage~df17$Win_Binary, main = "Boxplots of Fairway Percentage vs Atleast 1 Wins")
boxplot(df17$Avg.Distance~df17$Win_Binary, main = "Boxplots of Avg Distance vs Atleast 1 Wins")
boxplot(df17$gir~df17$Win_Binary, main = "Boxplots of Greens in Regulation vs Atleast 1 Wins")
boxplot(df17$Average.Putts~df17$Win_Binary, main = "Boxplots of Avg Putts vs Atleast 1 Wins")
boxplot(df17$Average.Scrambling~df17$Win_Binary, main = "Boxplots of Avg Scrambling in Regulation vs Atleast 1 Wins")
boxplot(df17$Average.Score~df17$Win_Binary, main = "Boxplots of Avg Score vs Atleast 1 Wins")
boxplot(df17$Average.SG.Putts~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained Putts vs Atleast 1 Wins")
boxplot(df17$Average.SG.Total~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained Total vs Atleast 1 Wins")
boxplot(df17$SG.OTT~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained off the tee vs Atleast 1 Wins")
boxplot(df17$SG.APR~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained Approaching the green vs Atleast 1 Wins")
boxplot(df17$SG.ARG~df17$Win_Binary, main = "Boxplots of Avg Srokes Gained around the green vs Atleast 1 Wins")
boxplot(df17$Money~df17$Win_Binary, main = "Boxplots of Purse vs Atleast 1 Wins")



