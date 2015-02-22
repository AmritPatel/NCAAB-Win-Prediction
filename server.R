library(shiny)
library(dplyr)
library(RCurl)
library(ggplot2)

## Get data

getBPI <- function() {
  
  # This is BPI data scraped daily from the ESPN website
  
  x <- getURL("https://raw.githubusercontent.com/AmritPatel/NCAAB-Win-Prediction/master/bpi.csv")
  bpi <- tbl_df(read.csv(text = x, stringsAsFactors=FALSE))
  bpi$BPI.RK <- as.numeric(bpi$BPI.RK)
  bpi$BPI <- as.numeric(bpi$BPI)
  
  return(bpi)
  
}

getTeams <- function() {

  # Read in team map

  x <- getURL("https://raw.githubusercontent.com/AmritPatel/NCAAB-Win-Prediction/master/teams.csv")
  dfTeams <- read.csv(text = x, stringsAsFactors=FALSE)

  return(dfTeams)

}

bpiData <- getBPI()
dfTeams <- getTeams()

## Load the model

# Download (from GitHub ) then load the .RData file containing latest trained model

library(downloader)

download("https://raw.github.com/AmritPatel/NCAAB-Win-Prediction/master/.RData", ".RData", mode="wb")
load(".RData")
model <- modFit

## Get prediction

winning <- function(team, opp) {
    
  bpi <- bpiData %>% filter(BPI.RK <= 351) %>% group_by(TEAM) %>% select(DATE, TEAM, BPI.RK, BPI)
  
  recentPerf <-
    bpi %>% 
    mutate(recentPerf=(BPI-mean(BPI))/mean(BPI)*100) %>%
    filter(DATE==as.character(bpi[nrow(bpi),1])) %>%
    group_by() %>%
    arrange(desc(recentPerf))
    
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

## Define BPI plotting function

plotBPI <- function(team, opp) {
    
  bpi <- bpiData %>%
         filter(TEAM == dfTeams[dfTeams$Yahoo==team, 1] | TEAM == dfTeams[dfTeams$Yahoo==opp, 1]) %>%
         group_by(TEAM) %>% 
         select(DATE, TEAM, BPI.RK, BPI)
  
  ggplot(bpi, aes(x=DATE, y=BPI, color=TEAM)) + geom_point() + facet_wrap(~TEAM) + 
  geom_smooth(method=lm, aes(group=1)) +
  xlab("Date") +
  ylab("BPI") +
  ggtitle("BPI Over Time") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), legend.position="none")
  
}

shinyServer(    
  function(input, output) {
    ## Output the accuracy plot
    output$img0 <- renderImage({

          library(reshape2)
          library(gridExtra)
          library(scales)
          
          x <- getURL("https://raw.githubusercontent.com/AmritPatel/NCAAB-Win-Prediction/master/accuracy.out")          
          acc <- melt(read.csv(text = x), id.vars=1, measure.vars=c(2, 3, 4),
                      variable.name = "Type", value.name = "Accuracy")
          
          p1 <- ggplot(data=acc %>% filter(as.Date(date) >= "2015-02-07"), aes(as.Date(date), Accuracy, group=Type)) + 
                geom_line(aes(color=Type), size=2) +
                xlab("") +
                ylab("Accuracy") +
                theme(legend.position="bottom")
          
          x <- getURL("https://raw.githubusercontent.com/AmritPatel/NCAAB-Win-Prediction/master/scores.csv")
          scores <- tbl_df(read.csv(text = x))
          sumScores <- tbl_df(scores) %>% group_by(date) %>% summarise(n=n())
          
          p2 <- ggplot(data=sumScores %>% filter(as.Date(date) >= "2015-02-07"), aes(as.Date(date), n)) +
                geom_histogram(stat="identity") + 
                xlab("Date") +
                ylab("Number of Games Played")
                    
          # A temp file to save the output.
          outfile <- tempfile(fileext='.png')
          
          png(outfile, height=400)
          grid.arrange(p1, p2, nrow=2, main="Model Accuracy vs. Time")
          dev.off()
          
          # Return a list containing the filename
          list(src = outfile,
               contentType = 'image/png',
               height = 400,
               alt = "This is alternate text")    
    }, deleteFile = TRUE)
    
    ## Output the BPI plots
    output$img1 <- renderPlot({plotBPI(team=input$team1, opp=input$team2)})
    ## Output the prediction
    output$prediction <- renderPrint({winning(team=input$team1, opp=input$team2)})
  }
)