# source("C:/Users/axp13/Dropbox/NCAA Bracket Challenge/NCAA 2015 Bracket Challenge.R")
source("/home/ubuntu/NCAAB-Win-Prediction/NCAA 2015 Bracket Challenge.R")

predNCAAb()

library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(scales)

acc <- melt(read.csv("accuracy.out"), id.vars=1, measure.vars=c(2, 3, 4),
            variable.name = "Type", value.name = "Accuracy")

p1 <- ggplot(data=acc %>% filter(as.Date(date) >= "2015-02-07"), aes(as.Date(date), Accuracy, group=Type)) + 
      geom_line(aes(color=Type), size=2) +
      xlab("") +
#       scale_x_date(labels = date_format("%m/%d")) +
      ylab("Accuracy") +
#      ggtitle("Model Accuracy vs. Time") +
      theme(legend.position="bottom")

scores <- tbl_df(read.csv("scores.csv"))
sumScores <- tbl_df(scores) %>% group_by(date) %>% summarise(n=n())

p2 <- ggplot(data=sumScores %>% filter(as.Date(date) >= "2015-02-07"), aes(as.Date(date), n)) +
      geom_histogram(stat="identity") + 
      xlab("Date") +
#       scale_x_date(labels = date_format("%m/%d")) +    
      ylab("Number of Games Played")

grid.arrange(p1, p2, nrow=2, main="Model Accuracy vs. Time")

dev.print(png, file="NCAAb_prediction_model_accuracy.png", width=800, height=600)
dev.off()
