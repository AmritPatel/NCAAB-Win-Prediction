library(shiny)
library(dplyr)
library(RCurl)

pNCAAb <- function() {
  
  library(randomForest)
  library(caret)
 
  x <- getURL("https://raw.githubusercontent.com/AmritPatel/NCAAB-Win-Prediction/master/scores.csv")
  training <- read.csv(text = x, stringsAsFactors=FALSE)

  modFit <- train(tWin ~ tBPI + tRP + oBPI + oRP, method="rf", data=training)

  return(modFit)
  
}

## Load the model

model <- pNCAAb()  

winning <- function(team, opp) {
  
  ### Load and process data
  
  # This is BPI data scraped daily from the ESPN website

  x <- getURL("https://raw.githubusercontent.com/AmritPatel/NCAAB-Win-Prediction/master/bpi.csv")
  bpi <- tbl_df(read.csv(text = x, stringsAsFactors=FALSE))
  bpi$BPI.RK <- as.numeric(bpi$BPI.RK)
  bpi$BPI <- as.numeric(bpi$BPI) 
  
  bpi <- bpi %>% filter(BPI.RK <= 351) %>% group_by(TEAM) %>% select(DATE, TEAM, BPI.RK, BPI)
  
  recentPerf <-
    bpi %>% 
    mutate(recentPerf=(BPI-mean(BPI))/mean(BPI)*100) %>%
    filter(DATE==as.character(bpi[nrow(bpi),1])) %>%
    group_by() %>%
    arrange(desc(recentPerf))
  
  ### Predict
  
  # Read in team map
  
  x <- getURL("https://raw.githubusercontent.com/AmritPatel/NCAAB-Win-Prediction/master/teams.csv")
  dfTeams <- read.csv(text = x, stringsAsFactors=FALSE)
  
  # Get corresponding BPI data
  
  tESPN     <- dfTeams[dfTeams$Yahoo==team, 1]; tESPN <- recentPerf[recentPerf$TEAM==tESPN, ]
  tBPI      <- as.numeric(tESPN[grep(as.character(bpi[nrow(bpi),1]), tESPN$DATE), 4])
  tBPI      <- if (length(tBPI) == 0L) 0 else as.numeric(tESPN[grep(as.character(bpi[nrow(bpi),1]), tESPN$DATE), 4])
  oESPN     <- dfTeams[dfTeams$Yahoo==opp, 1];  oESPN <- recentPerf[recentPerf$TEAM==oESPN, ] 
  oBPI      <- as.numeric(oESPN[grep(as.character(bpi[nrow(bpi),1]), oESPN$DATE), 4])
  oBPI      <- if (length(oBPI) == 0L) 0 else as.numeric(oESPN[grep(as.character(bpi[nrow(bpi),1]), oESPN$DATE), 4])
  
  # Get corresponding recent performance data
  
  pretRP    <- dfTeams[dfTeams$Yahoo==team, 1]; pretRP <- recentPerf[recentPerf$TEAM==pretRP, ]
  tRP       <- as.numeric(pretRP[grep(as.character(bpi[nrow(bpi),1]), pretRP), 5])
  tRP       <- if (length(tRP) == 0L) 0 else as.numeric(pretRP[grep(as.character(bpi[nrow(bpi),1]), pretRP), 5])
  preoRP    <- dfTeams[dfTeams$Yahoo==opp, 1]; preoRP <- recentPerf[recentPerf$TEAM==preoRP, ]
  oRP       <- as.numeric(preoRP[grep(as.character(bpi[nrow(bpi),1]), preoRP), 5])
  oRP       <- if (length(oRP) == 0L) 0 else as.numeric(preoRP[grep(as.character(bpi[nrow(bpi),1]), preoRP), 5]) 
  
  newdata <- data.frame(tBPI=tBPI, tRP=tRP, oBPI=oBPI, oRP=oRP)
  
  result <- predict(model, newdata)
  if (round(result) == 1) team else opp
  
}

shinyServer(    
  function(input, output) {
    output$prediction <- renderPrint({winning(team=input$team1, opp=input$team2)})
  }
)