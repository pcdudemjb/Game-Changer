# Project Overview

This was my Capstone Project from the Data Science program at Bethel Tech.

The powerpoint presentation contains all of my findings to the general question of, "Can all 76 years of NBA team stats be used to make accurate predictions for teams today?"

## Analyses Performed
1. Teams' records in the last ten years (helped me explain a little more information about my dataset)
2. Relationship between shooting percentages and team wins? (Simple linear regression)
3. Relationship between turnovers and team wins? (Exponential model)
4. Most Valuable Team Stats? (Stepwise binary logistic regression w/ backwards elimination)
5. Can machine learning algorithms be used to predict season outcomes for teams?  Which team stats are best for that analysis? (Supervised machine learning model w/ recursive feature elimination)

## Created a streamlit application to test the ML model in realtime.

Streamlit file is included and will run on a local machine.

I used it to predict the Chicago Bulls' record by using stats from a month or two before I gave my presentation.  The result was only 1 win away from being correct.

### Folder Contents
-Team Stats Per Game & Team Summaries .csv files that I concatenated into nbaR.csv and eventually wrangled into nbaR2.csv

-I used both R & Python as you will see Final Project Code(.R & .ipynb)

-NBA Presentation is my slideshow

-Streamlit.py has my interactive application

-Theme.zip is the powerpoint theme (credit to the creator in my slideshow)

-model.pkl was used to create the Streamlit application
