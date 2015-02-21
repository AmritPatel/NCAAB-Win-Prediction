library(caret)
library(XML)
library(httr)
library(dplyr)

# set_config(use_proxy(url="148.184.186.50",port=80,username="LANID",password="")) # only needed if at work

# dir <- "C:/Users/axp13/Dropbox/NCAA Bracket Challenge"
# dir <- "~/Dropbox/NCAA Bracket Challenge"
dir <- "/home/ubuntu/NCAAB-Win-Prediction"
setwd(dir)

### Step 1: Get BPI data

## Need to write function to grab this daily for continual cross-validation.

espn.bpi <- "http://espn.go.com/mens-college-basketball/bpi" # this is the site where you can get daily BPI data
url <- GET(espn.bpi) # a wrapper function used by the httr package (this is necessary if a proxy is needed to scrape)
bpi.table <- readHTMLTable(rawToChar(url$content), header=T, which=1, stringsAsFactors=F) # scrape the data
colnames(bpi.table) <- bpi.table[1,] # name the variables based on the 1st row of data
bpi.table <- bpi.table[-1, ] # remove the 1st row where the variable names were pulled from
rownames(bpi.table) <- NULL # remove the unnecessary row.names column
bpi.table$DATE <- as.character(Sys.Date()) # add the date
write.table(bpi.table, file="bpi.csv", append=TRUE, quote=FALSE, # keep a daily data log
            sep=",", col.names=TRUE, row.names=FALSE) 

### Step 2: Get head-to-head results by day

## Need to write function to grab this daily for continual cross-validation.
## Need to write function to grab BPI based on team.

frameScores <- # a function to create a data frame formatted for model training from the scraped data
function(date) {
    
    # 'date' needs to be input in %m-%d-%Y format; default is set to previous day's date
    date <- as.character(Sys.Date() - 1)
  
    yahoo.scores <- "http://sports.yahoo.com/college-basketball/scoreboard/?conf=all&date=" # this is a site where you can get daily scores
    yahoo.scores <- paste(yahoo.scores, date, sep="") # add data to query    
    url <- GET(yahoo.scores) # apply the httr wrapper
    
    scores <- readHTMLTable(rawToChar(url$content), header=F, which=2, stringsAsFactors=F) # list of tables w/ game results
    scores <- na.exclude(scores) # remove NAs
#     scores <- scores[-20,]
    
    dfTeams <- read.csv("teams.csv", stringsAsFactors=FALSE)
    bpi.table <- read.csv("bpi.csv", stringsAsFactors=FALSE) # read bpi.csv back in because we need previous day's data

    bpi <- bpi.table

#     bpi$TEAM <- as.factor(bpi$TEAM)
    bpi$BPI.RK <- as.numeric(bpi$BPI.RK)
    bpi$BPI <- as.numeric(bpi$BPI)      

    bpi <- bpi %>% 
      filter(BPI.RK <= 351) %>%
      group_by(TEAM) %>%
      select(DATE, TEAM, BPI.RK, BPI)
  
    recentPerf <-
      bpi %>% 
      mutate(recentPerf=(BPI-mean(BPI))/mean(BPI)*100) %>%
      filter(DATE==as.character(Sys.Date()-1)) %>%
      group_by() %>%
      arrange(desc(recentPerf))

    result <- data.frame(date=c(), team=c(), opp=c(), tBPI=c(), oBPI=c(), tScore=c(), oScore=c(), tWin=c(), bpiPred=c(),
                         tRP=c(), oRP=c(), stringsAsFactors=FALSE)
    for (i in 1:nrow(scores)) {
        team      <- scores[i,2]; team <- gsub("\\(\\d+\\) ", "", team, perl=TRUE)
        opp       <- scores[i,4]; opp  <- gsub("\\(\\d+\\) ", "", opp, perl=TRUE)
        tESPN     <- dfTeams[dfTeams$Yahoo==team, 1]; tESPN <- bpi.table[bpi.table$TEAM==tESPN, ] 
        tBPI      <- as.numeric(tESPN[grep(date, tESPN$DATE), 4])
        tBPI      <- if (length(tBPI) == 0L) 0 else as.numeric(tESPN[grep(date, tESPN$DATE), 4])
        oESPN     <- dfTeams[dfTeams$Yahoo==opp, 1];  oESPN <- bpi.table[bpi.table$TEAM==oESPN, ] 
        oBPI      <- as.numeric(oESPN[grep(date, oESPN$DATE), 4])
        oBPI      <- if (length(oBPI) == 0L) 0 else as.numeric(oESPN[grep(date, oESPN$DATE), 4])
        tmpScores <- scores[i,3]; tmpScores <- regmatches(tmpScores, gregexpr("\\d+", tmpScores, perl=TRUE))
        tScore    <- as.numeric(tmpScores[[1]][1])
        oScore    <- as.numeric(tmpScores[[1]][2])
        boolRes   <- try(tScore > oScore, silent=TRUE) # verify that scores are present
        if (!is.na(boolRes)) { # if the result of the comparison is not an NA, 
          tWin      <- if (boolRes) 1 else 0 # then assign a winner
        }
        bpiPred   <- if (tBPI > oBPI) 1 else 0
        pretRP    <- dfTeams[dfTeams$Yahoo==team, 1]; pretRP <- recentPerf[recentPerf$TEAM==pretRP, ]
        tRP       <- as.numeric(pretRP[grep(date, pretRP), 5])
        tRP       <- if (length(tRP) == 0L) 0 else as.numeric(pretRP[grep(date, pretRP), 5])
        preoRP    <- dfTeams[dfTeams$Yahoo==opp, 1]; preoRP <- recentPerf[recentPerf$TEAM==preoRP, ]
        oRP       <- as.numeric(preoRP[grep(date, preoRP), 5])
        oRP       <- if (length(oRP) == 0L) 0 else as.numeric(preoRP[grep(date, preoRP), 5])     
        temp      <- data.frame(date=c(date), team=c(team), opp=c(opp), tBPI=c(tBPI), oBPI=c(oBPI),
                             tScore=c(tScore), oScore=c(oScore), tWin=c(tWin), bpiPred=c(bpiPred),
                             tRP=c(tRP), oRP=c(oRP),
                             stringsAsFactors=FALSE)
        if (is.na(tScore) | is.na(oScore)) { # added this block in case of no score from Yahoo!
          temp      <- data.frame(date=c(), team=c(), opp=c(), tBPI=c(), oBPI=c(),
                                  tScore=c(), oScore=c(), tWin=c(), bpiPred=c(),
                                  tRP=c(), oRP=c(),
                                  stringsAsFactors=FALSE)
        }
        result    <- rbind(result, temp)
    }
    return(result)
}

dfScores <- frameScores() # data frame with most recent game results (currently need to fill in BPI data manually);
                          # this is the new test dataset
dfScores[is.na(dfScores)] <- 0 # Replace any NAs with 0 (this happens if a NCAA D1 team plays a non-D1 team)

training <- read.csv("scores.csv", stringsAsFactors=FALSE) # before we write new game data, load all previous game results as new training set
# training <- tbl_df(training)
# training <- training %>% filter(date <= as.Date("2015-01-24"))

# ###
# bpi <- tbl_df(read.csv("bpi.csv", stringsAsFactors=FALSE))
# bpi$TEAM <- as.factor(bpi$TEAM)
# bpi$BPI.RK <- as.numeric(bpi$BPI.RK)
# bpi$BPI <- as.numeric(bpi$BPI)
# 
# bpi <- bpi %>% filter(BPI.RK <= 351)%>% group_by(TEAM) %>% select(DATE, TEAM, BPI.RK, BPI)
# 
# recentPerf <-
#   bpi %>% 
#   mutate(recentPerf=(BPI-mean(BPI))/mean(BPI)*100) %>%
#   filter(DATE==as.character(Sys.Date()-1)) %>%
#   group_by() %>%
#   arrange(desc(recentPerf))
# 
# recentPerf %>% filter(TEAM %in% training$team)
# 
# ###

#bpi.table <- read.csv("bpi.csv", stringsAsFactors=FALSE) # read bpi.csv back in because we need previous day's data
#preSearch <- bpi.table[grep("Boise", bpi.table$TEAM), ] # BPI search function; type in team name (or part of) in parentheses
#preSearch[grep(as.character(Sys.Date()-1), preSearch$DATE), ] # filter preceding statement for previous day's date

# write.table(dfScores, file="scores.csv", append=TRUE, quote=FALSE, sep=",", col.names=FALSE, row.names=FALSE) # append the new daily data

### Step 3: Train model -- This model uses team and opponent BPI as features

# modFit <- train(tWin ~ tBPI + oBPI, method="glm", family="binomial", data=training) # model using logistic regression 
# modFit <- train(tWin ~ tBPI + oBPI, method="rf", family="binomial", data=training) # model using random forests
modFit <- train(tWin ~ tBPI + tRP + oBPI + oRP, method="rf", data=training)
modFit$finalModel # look at model confusion matrix
modFit$times # look at execution times

newdata <- data.frame(tBPI=as.numeric(dfScores$tBPI), tRP=as.numeric(dfScores$tRP),
                      oBPI=as.numeric(dfScores$oBPI), oRP=as.numeric(dfScores$oRP))
dailyPred <- predict(modFit, newdata)
dailytWins <- dfScores$tWin
dailybpiPred <- dfScores$bpiPred
dfScores <- cbind(dfScores, dailyPred)

upsets <- tbl_df(data.frame(dailytWins, dailybpiPred, dailyPred=round(dailyPred))) # look at upsets
upsets <- upsets %>%
  mutate(upset = dailytWins != dailybpiPred) %>%
  mutate(upsetPred = dailytWins != dailybpiPred & dailyPred != dailybpiPred)

dfScores <- cbind(dfScores[,1:9], dfScores[,12], upsets$upset, upsets$upsetPred, dfScores[,10:11])

write.table(dfScores, file="scores.csv", append=TRUE, quote=FALSE, sep=",", col.names=FALSE, row.names=FALSE) # append the new daily data

write(as.character(Sys.Date()-1), file="modFit.finalModel.out", append=TRUE) # print date to output file
capture.output(modFit$finalModel, file="modFit.finalModel.out", append=TRUE) # print model confusion matrix to output file
write(as.character(Sys.Date()-1), file="modFit.times.out", append=TRUE) # print date to output file
capture.output(modFit$times, file="modFit.times.out", append=TRUE) # print train runtime to output file

# dfScores <- read.csv("scores.csv") # restore new and previous scores data to corresponding data frame
# dfScores <- dfScores[dfScores$date == as.character(Sys.Date()-1),] # subset the scores data to include 
#                                                                    # only previous day (i.e. only the test dataset)
# 
# newdata <- data.frame(tBPI=dfScores$tBPI, # create a data frame containing the test dataset for which predictions are desired
#                       oBPI=dfScores$oBPI)

#---BETA--------------------------------------------------------------------------

# # Let's look at the ROC curve...
# 
# library(pROC) # useful for displaying and analyzing ROC curves
# 
# modFitProbs <- predict(modFit, newdata, type = "prob") # get prediction probabilities
# levels(modFitProbs$tWin) <- c(0, 1) # specify factor levels
# modFitROC <- roc(predictor = modFitProbs$tWin, response = dfScores$tWin, # build ROC object for plotting ROC
#                  levels = rev(levels(modFitProbs$tWin)))
# modFitROC # print area under curve to screen
# write(as.character(Sys.Date()-1), file="modFitROC.out", append=TRUE) # print date to output file
# capture.output(modFit$finalModel, file="modFitROC.out", append=TRUE) # print area under curve to output file
# plot(modFitROC, type = "S")
# dev.print(png, file="modFit_ROC.png", width=800, height=600)
# dev.off()

#---BETA--------------------------------------------------------------------------

write(as.character(Sys.Date()-1), file="results.out", append=TRUE) # print date to results output
capture.output(table(pred=round(dailyPred), truth=dailytWins), # print table of predicted vs. actual results to output
               file="results.out", append=TRUE)

# levels(dfScores$tWin) <- c(0, 1) # specify factor levels for confusionMatrix
# confMat <- confusionMatrix(table(predict(modFit, newdata), dfScores$tWin)) # check accuracy and kappa in validation set
# write(as.character(Sys.Date()-1), file="resultsMod.out", append=TRUE) # print date to results output
# capture.output(confMat, file="resultsMod.out", append=TRUE) # print confusion matrix to output

count <- sum(dailytWins == dailybpiPred) # calculate number of correct predictions based on BPI
bpiAcc <- count / length(dailytWins) # calculate BPI prediction accuracy

upsetAcc <- sum(upsets$upsetPred)/sum(upsets$upset) # calculate upset prediction accuracy

misClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)} # function calculating % misclassified
modelAcc <- 1 - misClass(dailytWins, round(dailyPred)) # calculate model accuracy
write(paste(as.character(Sys.Date()-1), modelAcc, bpiAcc, upsetAcc, sep=","), file="accuracy.out", append=TRUE)  # print prediction model accuracy to output w/ date

save.image()

paste(as.character(Sys.Date()-1), modelAcc, bpiAcc, upsetAcc, sep=",")
