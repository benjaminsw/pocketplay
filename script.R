
###For all levels and versions:
   ###The amount of passes (event: 101LevelSuccess)
   ###The amount of failures (event: 102LevelFailed)
   ###The min, mean, mode and max of retries (how many failures did a user have on a particular level before succeding)
   ###The min, mean, mode and max of moves used for the level.
###A text describing how the above data differs for the different versions of the game.

data <- read.csv('cleaned_data.csv', header = TRUE)
data$Event <- as.character(data$Event)

###The amount of passes (event: 101LevelSuccess)
###The amount of failures (event: 102LevelFailed)
passfail <- as.data.frame(table(data$Event))

###The min, mean, mode and max of retries (how many failures did a user have on a particular level before succeding
#device. installdate, devicemodel, facebookid

####-------------------------------------------------------------------
#data$failed <- data$LevelNumber == data$FurthestLevel
#faileddata <- subset(data, data$failed == TRUE, )
#min <- nrow(faileddata); max <- 0; sum <- 0; mode <- 0; n <- 0

#for (i in unique(faileddata$Session)){
  #tmp <- subset(faileddata, faileddata$Session == i,)
  #for (j in unique(tmp$LevelNumber)){
    #failures <- subset(tmp, tmp$LevelNumber == j,)
    #if (nrow(failures) < min) min <- nrow(failures)
    #if (nrow(failures) > max) max <- nrow(failures)
    #if (is.na(mode[nrow(failures)])) mode[nrow(failures)] <- 0
    #mode[nrow(failures)] = mode[nrow(failures)] + 1
    #n = n+1
  #}
#}
#min
#max
#mode
#nrow(faileddata)/n
#n
###--------------------------------------------------------------------------------------


data$pseudoID <- paste(data$Device, data$InstallDate, data$DeviceModel, data$FacebookID, sep = '')
retries <- subset(data, data$Event == '002_RetryLevel', )
retstat <- data.frame(Level = numeric(), Min = numeric(), Max = numeric(),  Mode = character(), Mean = numeric(), stringsAsFactors=FALSE)                  


for(i in unique(sort(retries$LevelNumber))){
  min <- nrow(retries); max <- 0; sum <- 0; mode <- NA; n <- 0; sum <- 0
  for(j in unique(retries$pseudoID)){
    tmp <- subset(retries, retries$LevelNumber == i & retries$pseudoID == j ,)
    if (nrow(tmp) != 0 && nrow(tmp) < min) min <- nrow(tmp)
    if (nrow(tmp) != 0 && nrow(tmp) > max) max <- nrow(tmp)
    #if (nrow(tmp) != 0 && is.na(mode[nrow(tmp)])) mode[nrow(tmp)] <- 0
    #mode[nrow(tmp)] = mode[nrow(tmp)] + 1
    if (nrow(tmp) != 0) {
      sum <- sum + nrow(tmp)
      n <- n + 1
      mode[length(mode)+1] <- nrow(tmp)
    }
  }
  #retstat[nrow(retstat)+1,] <- c(i, min, max, match(max(mode, na.rm = TRUE),mode), sum/n)
  tmpmode <- paste(mfv(mode, na.rm=TRUE), collapse = ",")
  retstat[nrow(retstat)+1,] <- c(i, min, max, tmpmode, sum/n)
  #print(paste("level = ", i, sep = '' ))

}
#write.csv(retstat, file = 'retstat.csv')
#device. installdate, devicemodel, facebookid
faileddata$pseudoID <- paste(faileddata$Device, faileddata$InstallDate, faileddata$DeviceModel, faileddata$FacebookID, sep = '')
min <- nrow(faileddata); max <- 0; sum <- 0; mode <- 0; n <- 0

for (i in unique(faileddata$pseudoID)){
  tmp <- subset(faileddata, faileddata$pseudoID == i,)
  for (j in unique(tmp$LevelNumber)){
    failures <- subset(tmp, tmp$LevelNumber == j,)
    if (nrow(tmp) != 0 & nrow(failures) < min) min <- nrow(failures)
    if (nrow(tmp) != 0 & nrow(failures) > max) max <- nrow(failures)
    if (is.na(mode[nrow(failures)])) mode[nrow(failures)] <- 0
    mode[nrow(failures)] = mode[nrow(failures)] + 1
    n = n+1
  }
}
min
max
mode
nrow(faileddata)/n
n

###The min, mean, mode and max of moves used for the level.
library(modeest)
tmp1 <- tapply(data$MovesUsed, data$LevelNumber, summary)
tmp1df <- as.data.frame(do.call(rbind, tmp1))
tmp1df <- tmp1df[,c(1,4,6)]
tmp2 <- tapply(data$MovesUsed, data$LevelNumber, mlv, na.rm = TRUE)
tmp2df <- as.data.frame(do.call(rbind, tmp2))
tmp2df <- tmp2df[,1:2]
colnames(tmp2df)[1] <- "Mode"
movesused <- cbind(tmp1df, tmp2df)

#lapply(tmp2, write, "tmp2.csv", append = TRUE, ncolumns=1000, sep = ",")

###A text describing how the above data differs for the different versions of the game.
pfversion <- as.data.frame(table(data$Version, data$Event))
colnames(pfversion) <- c("Version", "Event", "Count")
pfversion <- subset(pfversion, pfversion$Event == "101_LevelSuccess" | pfversion$Event == "102_LevelFailed",)
rtsversion <- tapply(faileddata$LevelNumber, faileddata$Version, summary)
rtsversiondf <- as.data.frame(do.call(rbind, rtsversion))
rtsversiondf <- rtsversiondf[,c(1,4,6)]
muv <- tapply(data$MovesUsed, data$Version, summary)
muvdf <- as.data.frame(do.call(rbind, muv))
muvdf <- muvdf[,c(1,4,6)]
muv2 <- tapply(data$MovesUsed, data$Version, mlv, na.rm = TRUE)
muv2df <- as.data.frame(do.call(rbind, muv2))
muv2df <- muv2df[,1:2]
colnames(muv2df)[1] <- "Mode"
muversion <- cbind(muvdf, muv2df)
