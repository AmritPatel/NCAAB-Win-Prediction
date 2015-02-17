#!/bin/sh

#  crossVal_push.sh
#  
#
#  Created by Amrit Patel on 2/17/15.
#

# Perform daily cross-validation

cd "/Users/im_apatel/Dropbox/NCAA Bracket Challenge"
Rscript "Run Daily.R"

# Copy updated files to local GitHub repo

cp "bpi.csv" "../Coursera/Data Science/Developing Data Products/NCAAB-Win-Prediction/"
cp "scores.csv" "../Coursera/Data Science/Developing Data Products/NCAAB-Win-Prediction/"
cp "NCAAb_prediction_model_accuracy.png" "../Coursera/Data Science/Developing Data Products/NCAAB-Win-Prediction/"

# Push repo changes to remote

cd "/Users/im_apatel/Dropbox/Coursera/Data Science/Developing Data Products/NCAAB-Win-Prediction"

git commit -am 'Updates'
git push origin master