####### Project 2 PGA Tour Analysis #######
data = read.csv('pgaTourData.csv')
data = as.data.frame(data)

# The bottom half of the data set contains many empty observations 
df = data[-c(1679:2312),]

#Create multiple top ten binary variable
df$MTTF <- ifelse(df$Top.10 >=3 , 1,0)

#For the NA observations in the column adjust them to 0
df$MTTF[is.na(df$MTTF)] <- 0

boxplot(df$Rounds~df$MTTF, main = "Boxplots of Rounds vs Multiple Top Ten Finishes")
boxplot(df$Fairway.Percentage~df$MTTF, main = "Boxplots of Fairway Percentage vs Multiple Top Ten Finishes")
boxplot(df$Avg.Distance~df$MTTF, main = "Boxplots of Avg Distance vs Multiple Top Ten Finishes")
boxplot(df$gir~df$MTTF, main = "Boxplots of Greens in Regulation vs Multiple Top Ten Finishes")
boxplot(df$Average.Putts~df$MTTF, main = "Boxplots of Avg Putts vs Multiple Top Ten Finishes")
boxplot(df$Average.Scrambling~df$MTTF, main = "Boxplots of Avg Scrambling in Regulation vs Multiple Top Ten Finishes")
boxplot(df$Average.Score~df$MTTF, main = "Boxplots of Avg Score vs Multiple Top Ten Finishes")
boxplot(df$Average.SG.Putts~df$MTTF, main = "Boxplots of Avg Srokes Gained Putts vs Multiple Top Ten Finishes")
boxplot(df$Average.SG.Total~df$MTTF, main = "Boxplots of Avg Srokes Gained Total vs Multiple Top Ten Finishes")
boxplot(df$SG.OTT~df$MTTF, main = "Boxplots of Avg Srokes Gained off the tee vs Multiple Top Ten Finishes")
boxplot(df$SG.APR~df$MTTF, main = "Boxplots of Avg Srokes Gained Approaching the green vs Multiple Top Ten Finishes")
boxplot(df$SG.ARG~df$MTTF, main = "Boxplots of Avg Srokes Gained around the green vs Multiple Top Ten Finishes")
boxplot(df$Money~df$MTTF, main = "Boxplots of Purse vs Multiple Top Ten Finishes")
