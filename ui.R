library(shiny)
library(RCurl)

# Read in list of teams
x <- getURL("https://raw.githubusercontent.com/AmritPatel/NCAAB-Win-Prediction/master/teams.csv")
dfTeams <- read.csv(text = x, stringsAsFactors=FALSE)

shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel(a(href="http://en.wikipedia.org/wiki/NCAA_Men%27s_Division_I_Basketball_Tournament", "2015 NCAAB Tournament Predictor"),
                windowTitle="2015 NCAAB Tournament Predictor"),
    
    sidebarPanel(
      p("This app takes two Division I NCAA basketball teams as input and will output the predicted winner. The prediction model is trained with",
      a(href="http://espn.go.com/mens-college-basketball/bpi", "daily ESPN BPI data"), " and a recent performance indicator."),
      
      strong(em("Wait (about 30 seconds) for the initial prediction to complete before selecting teams.")),
      
      # Two input fields are added
      selectInput(inputId="team1", label = h3("Pick Team 1"), 
                  choices = dfTeams$Yahoo, selected = "Florida"),
      selectInput(inputId="team2", label = h3("Pick Team 2"), 
                  choices = dfTeams$Yahoo, selected = "Kentucky"),
      
      submitButton('Submit'),
      width = 6
    ),
    
    mainPanel(
      h3('Results of Prediction'),
      verbatimTextOutput("prediction"),
      width = 4
    )  
  )
)