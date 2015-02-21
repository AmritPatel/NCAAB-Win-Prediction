#!/bin/sh

#  crossVal_push.sh
#  
#
#  Created by Amrit Patel on 2/17/15.
#

# Perform daily cross-validation

cd "/home/ubuntu/NCAAB-Win-Prediction"
Rscript "Run Daily.R"

# Push repo changes to remote

git commit -am 'Updates'
git push origin master
