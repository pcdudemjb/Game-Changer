# Import Data & Libraries
library(readr)
nbaR <- read_csv("C:/Users/pcdud/OneDrive/Desktop/Data Science Classes/Final Project/nbaR.csv")
library(nortest)
library(car)
library(caret)
library(gvlma)
library(predictmeans)
library(e1071)
library(lmtest)
library(magrittr)
library(dplyr)
library(tidyr)
library(lmtest)
library(popbio)
library(corrplot)

#------------------------------------------------------------------------------#
#
#
#
#
#------------------------------------------------------------------------------#
## Analysis 2:  
# Is there a relationship between a team's shooting percentage and their win-loss record?
#------------------------------------------------------------------------------#

# Testing Assumptions:

# Used to test normality of variables.
hist(nbaR$fg_percent)
qqnorm(nbaR$fg_percent)
qqline(nbaR$fg_percent)
# fg_percent NOT normally distributed

hist(nbaR$x3p_percent)
qqnorm(nbaR$x3p_percent)
qqline(nbaR$x3p_percent)
# x3p_percent NOT normally distributed

hist(nbaR$x2p_percent)
qqnorm(nbaR$x2p_percent)
qqline(nbaR$x2p_percent)
# x2p_percent NOT normally distributed

hist(nbaR$ts_percent)
qqnorm(nbaR$ts_percent)
qqline(nbaR$ts_percent)
# ts_percent NOT normally distributed

# All field goal variables are not normally distributed.  Recommend "spearman" method.

#------------------------------------------------------------------------------#

## Field Goal Percentage
# 1.  Testing for a linear relationship between variables.
scatter.smooth(x=nbaR$fg_percent, y=nbaR$w, main= "Field Goal Percentage vs. Wins")
cor(nbaR$w, nbaR$fg_percent, method = "spearman", use = "complete.obs")
# Correlation coefficient = 0.44.  Moderate, positive, linear relationship.

# 2.  Testing for homoscedasticity.
nbaRNA <- na.omit(nbaR)

model1 <- lm(w ~ fg_percent, data = nbaR)
par(mfrow=c(2,2))
plot(model1)
lmtest::bptest(model1)
# p-value = 0.12.  Since this is greater than 0.05, test isn't significant.  
# Ergo homoscedastic.
car::ncvTest(model1)
# p-value = 0.19.  Greater than 0.05, test isn't significant.  
# Indicates homoscedasticity.

# 3.  Testing for homogeneity of variance.
# Mostly evenly distributed residuals.
gvlma(model1)
# Heteroscedasticity  Value = 0.8495, p-value = 3.567e-01.  Assumptions acceptable.

# Testing for outliers in x space. (Distance, Leverage & Influential Points)
CookD(model1, group=NULL, plot=TRUE, idn=3, newwd=FALSE)
lev1 = hat(model.matrix(model1))
plot(lev1)
table(lev1 > 0.2)
# No outliers in x space

# Testing for outliers in y space.
car::outlierTest(model1)
# No studentized residuals with Bonferroni p < 0.05

# Testing for outliers in x and y space.
summary(influence.measures(model1))
# No values > 1.  No influential outliers.

# All assumptions met.

# Testing hypothesis
summary(model1)
# F-statistic: 306.2 on 1 and 1725 DF (significant)
# p-value: < 2.2e-16 (significant)

## Field goal percentage significantly impacts teams' record in the NBA.
## Field goal percentage accounts for 15% of variation in team wins.

## Prediction:  y=131.74(x)-20.  If a team shoots 49% (.49) for the season, 
##              this data indicates an estimated 45 wins for the season. 

#------------------------------------------------------------------------------#

## Three Point Percentage
# 1.  Testing for a linear relationship between variables.
scatter.smooth(x=nbaR$x3p_percent, y=nbaR$w, main= "Three Point Percentage vs. Wins")
cor(nbaR$w, nbaR$x3p_percent, method = "spearman", use = "complete.obs")
# Correlation coefficient = 0.23.  Weak, positive, linear relationship.

# 2.  Testing for homoscedasticity.
model2 <- lm(w ~ x3p_percent, data = nbaR)
par(mfrow=c(2,2))
plot(model2)
lmtest::bptest(model2)
# p-value = .08.  Since this is greater than 0.05, test isn't significant.  
# Ergo homoscedastic.
car::ncvTest(model2)
# p-value = 0.16.  Greater than 0.05, test isn't significant.  
# Indicates homoscedasticity.

# 3.  Testing for homogeneity of variance.
# Weakly, evenly distributed residuals.
gvlma(model2)
# Heteroscedasticity  Value = 2.00, p-value = 1.569e-01.  Assumptions acceptable.

# Testing for outliers in x space. (Distance, Leverage & Influential Points)
CookD(model2, group=NULL, plot=TRUE, idn=3, newwd=FALSE)
lev2 = hat(model.matrix(model2))
plot(lev2)
table(lev2>.2)
# No outliers in x space

# Testing for outliers in y space.
car::outlierTest(model2)
# No studentized residuals with Bonferroni p < 0.05

# Testing for outliers in x and y space.
summary(influence.measures(model2))
# No values > 1.  No influential outliers.

# All assumptions were met.

# Testing hypothesis
summary(model2)
# F-statistic: 25.24 on 1 and 1316 DF (significant)
# p-value: < 5.749e-07 (significant)

## Three point percentage not as significant as field goal percentage.
## Can confirm that three point percentage impacts teams' record in the NBA.
## Three point percentage accounts for about 2% of variation in team wins.

## Prediction:  y=36.64(x)+27.8.  If a team shoots 40% (.40) from behind
##              the three point line for the season, this data indicates an 
##              estimated 42 wins for the season. 

#------------------------------------------------------------------------------#

## Two Point Percentage
# 1.  Testing for a linear relationship between variables.
scatter.smooth(x=nbaR$x2p_percent, y=nbaR$w, main= "Two Point Percentage vs. Wins")
cor(nbaR$w, nbaR$x2p_percent, method = "spearman", use = "complete.obs")
# Correlation coefficient = 0.36.  Weak, positive, linear relationship.

# 2.  Testing for homoscedasticity.
model3 <- lm(w ~ x2p_percent, data = nbaR)
par(mfrow=c(2,2))
plot(model3)
lmtest::bptest(model3)
# p-value = 0.016.  Since this is less than 0.05, test is significant.  
# Ergo heteroscedastic.
car::ncvTest(model3)
# p-value = 0.042.  Less than 0.05, test is significant.  
# Indicates heteroscedasticity.

# Correcting for heteroscedasticity.
# BoxCox requires no missing values.  Using dataset nbaRNA (NA values removed).
distBCMod3 <- caret::BoxCoxTrans(nbaRNA$x2p_percent)
print(distBCMod3)
nbaRNA <- cbind(nbaRNA, dist_new3=predict(distBCMod3, nbaRNA$x2p_percent))
model3BC <- lm(w~dist_new3, data = nbaRNA)
lmtest::bptest(model3BC)
# p-value = 0.005.  Less than 0.05, cannot correct heteroscedasticity.
car::ncvTest(model3BC)
# p-value = 0.024.  Less than 0.05, cannot correct heteroscedasticity.

# 3.  Testing for homogeneity of variance.
# Mostly evenly distributed residuals.
gvlma(model3BC)
# Heteroscedasticity  Value = 3.662, p-value = 5.57e-02.  Assumptions acceptable...

# Testing for outliers in x space. (Distance, Leverage & Influential Points)
CookD(model3BC, group=NULL, plot=TRUE, idn=3, newwd=FALSE)
lev3 = hat(model.matrix(model3BC))
plot(lev3)
table(lev3>.2)
# No outliers in x space

# Testing for outliers in y space.
car::outlierTest(model3BC)
# No studentized residuals with Bonferroni p < 0.05

# Testing for outliers in x and y space.
summary(influence.measures(model3BC))
# No values > 1.  No influential outliers.

# NOT all assumptions met.  Proceeding with caution.  Expecting bias.

# Testing hypothesis
summary(model3BC)
# F-statistic: 67.96 on 1 and 775 DF (significant)
# p-value: < 7.098e-16 (significant)

## Two point percentage significantly impacts teams' record in the NBA.
## Field goal percentage accounts for 8% of variation in team wins.

# Using original model to try predictions...
summary(model3)

## Prediction:  y=88.70(x)-2.32.  If a team shoots 45% (.45) for the season, 
##              this data indicates an estimated 38 wins for the season.

#------------------------------------------------------------------------------#

## True Shooting Percentage (True shooting percentage is a measure of shooting efficiency that takes into account field goals, 3-point field goals, and free throws)
# 1.  Testing for a linear relationship between variables.
scatter.smooth(x=nbaR$ts_percent, y=nbaR$w, main= "True Shooting Percentage vs. Wins")
cor(nbaR$w, nbaR$ts_percent, method = "spearman", use = "complete.obs")
# Correlation coefficient = 0.40.  Moderate, positive, linear relationship.

# 2.  Testing for homoscedasticity.
model4 <- lm(w ~ ts_percent, data = nbaR)
par(mfrow=c(2,2))
plot(model4)
lmtest::bptest(model4)
# p-value = 0.9676.  Since this is greater than 0.05, test isn't significant.  
# Ergo homoscedastic
car::ncvTest(model4)
# p-value = 0.97244.  Greater than 0.05, test isn't significant.  
# Indicates homoscedasticity.

# 3.  Testing for homogeneity of variance.
# Weakly, evenly distributed residuals.
gvlma(model4)
# Heteroscedasticity  Value = 4.002, p-value = 4.544e-02.  Assumptions not acceptable...

# Testing for outliers in x space. (Distance, Leverage & Influential Points)
CookD(model4, group=NULL, plot=TRUE, idn=3, newwd=FALSE)
lev4 = hat(model.matrix(model4))
plot(lev4)
table(lev4>.2)
# No outliers in x space

# Testing for outliers in y space.
car::outlierTest(model4)
# No studentized residuals with Bonferroni p < 0.05

# Testing for outliers in x and y space.
summary(influence.measures(model4))
# No values > 1.  No influential outliers.

# All assumptions were met.

# Testing hypothesis
summary(model4)
# F-statistic: 263.4 on 1 and 1725 DF (significant)
# p-value: < 2.2e-16 (significant)

## True Shooting percentage is not as significant as field goal percentage.
## Can confirm that true shooting percentage impacts teams' record in the NBA.

## True Shooting percentage accounts for about 13% of variation in team wins.

## Prediction:  y=110.64(x)-18.23.  If a team shoots 39% (.39) true shooting
##              percentage throughout the season, this data indicates an 
##              estimated 25 wins for the season. 

#------------------------------------------------------------------------------#
#
#
#
#
#------------------------------------------------------------------------------#
## Analysis 3:  
# Do teams that have a higher number of turnovers tend to have a worse win-loss record?
# Is there a certain threshold of turnovers that negatively impacts a team's success?
#------------------------------------------------------------------------------#

# Used to test normality of variables.
hist(nbaR$w)
qqnorm(nbaR$w)
qqline(nbaR$w)
# "w" is normally distributed.

hist(nbaR$tov_per_game)
qqnorm(nbaR$tov_per_game)
qqline(nbaR$tov_per_game)
# "tov_per_game" is not normally distributed.

# Creating a scatterplot to examine linearity.
scatter.smooth(x=nbaR$tov_per_game, y=nbaR$w, main="Scatterplot of Wins vs. Turnovers Per Game",
               xlab= "Turnovers Per Game", ylab= "Wins")
# Best fit line starts doesn't show the best linear relationship.
cor(nbaR$tov_per_game, nbaR$w, method = "spearman", use = "complete.obs")
# Correlation coefficient = -0.13.  not a linear relationship.

# Trying an exponential model.
Expo <- nls(w ~ a * exp(b * tov_per_game), data = nbaR, start = list(a = 1, b = 0))
summary(Expo)

# Checking Turnover Range
summary(nbaR$tov_per_game)

# min 11.1, max 24.5

# Creating new variables for the scatterplot
newx <- seq(11, 24.5, length.out = 1437)
newy <- predict(Expo, newdata = data.frame(x = newx))


# Recreating a scatterplot to examine linearity.
scatter.smooth(x=newx, y=newy, main="Scatterplot Of Wins vs. Turnovers Per Game",
               xlab= "Turnovers Per Game", ylab= "Wins",
               col="blue", col.line="red")
cor(newx, newy, method = "spearman", use = "complete.obs")
# Correlation coefficient = - 0.77.  Strong, negative linear relationship!

# This test yields a strong correlation, but the T.O. numbers are VERY high.
# Most teams in recent years average between 11 and 16 T.O.'s per game...


# Testing for Homoscedasticity
# Create a linear model
lmWvTOV <- lm(w~tov_per_game, data=nbaR)
par(mfrow=c(2,2))
plot(lmWvTOV)
# Double checking with Breush-Pagan test & NCV test.
lmtest::bptest(lmWvTOV)
# 	studentized Breusch-Pagan test
#
# data:  lmWvTOV
# BP = 0.5423, df = 1, p-value = 0.4615
car::ncvTest(lmWvTOV)
# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 0.3776253, Df = 1, p = 0.53888

# Since these p-values are greater than .05, it means it is not significant.
# Met assumption of homoscedasticity!

# Testing for homogeneity of variance.
gvlma(lmWvTOV)
# Heteroscedasticity  0.4095 5.222e-01    Assumptions acceptable.

# Testing for outliers in X Space.
CookD(lmWvTOV, group=NULL, plot=TRUE, idn=3, newwd=FALSE)
WvTOVlev = hat(model.matrix(lmWvTOV))
plot(WvTOVlev)
table(WvTOVlev>.2)
# No values over .2

# Testing for outliers in y space.
car::outlierTest(lmWvTOV)
# No studentized residuals with Bonferroni p < 0.05

# Testing for outliers in x and y space.
summary(influence.measures(lmWvTOV))
# No values > 1.  No influential outliers.

# All assumptions were met.

# Testing hypothesis
summary(lmWvTOV)
# F-statistic:  15.2 on 1 and 1435 DF (weak significance)
# p-value: < 0.0001012 (significant)

## Turnovers per game "significantly" impacts teams' record in the NBA.
## But only accounts for 1% of variation in team wins...

## Prediction:  y=-0.55(x)+48.81.  If a team averages 10 turnovers per game
##              for the season, this data indicates an estimated 43 wins for the season.

# Seems low.  Given the lowest turnover average across a season ever was 11.1,
# I believe the exponential nature of this model doesn't accurately portray a 
# correct value of wins.  Possible skew due to historical values.


#------------------------------------------------------------------------------#
#
#
#
#
#------------------------------------------------------------------------------#
## Analysis 4:  
# Which statistics are the most strongly correlated with team success?
# (Measured by reaching playoffs)
#------------------------------------------------------------------------------#

# Creating a binary column from the boolean column which indicates if teams made
# the playoffs.

nbaRNA$playoffs_01 <- ifelse(nbaRNA$playoffs, 1, 0)

# List out the column names for the model.
names(nbaRNA)

# Create logistic model.
FitAll <- glm(playoffs_01 ~ mp_per_game + fg_per_game + fga_per_game +
                fg_percent + x3p_per_game + x3pa_per_game + x3p_percent +
                x2p_per_game + x2pa_per_game + x2p_percent + ft_per_game +
                fta_per_game + ft_percent + orb_per_game + drb_per_game +
                trb_per_game + ast_per_game + stl_per_game + blk_per_game +
                tov_per_game + pf_per_game + pts_per_game + age + w +
                l + mov + sos + srs + o_rtg + d_rtg + n_rtg + pace +
                f_tr + x3p_ar + ts_percent + e_fg_percent + tov_percent +
                orb_percent + ft_fga + opp_e_fg_percent + opp_tov_percent +
                opp_drb_percent + opp_ft_fga, data=nbaRNA, family="binomial")
summary(FitAll)

# Stepwise regression (backward elimination technique)
step(FitAll, direction = "backward")

# Recreating model with variables selected.
FitSome <- glm(playoffs_01 ~ fga_per_game + fg_percent + x3p_percent + x2pa_per_game +
                 x2p_percent + fta_per_game + ft_percent + drb_per_game + trb_per_game +
                 tov_per_game + pf_per_game + w + l + srs + d_rtg, data=nbaRNA, family="binomial")

summary(FitSome)
# All but one variable (Personal Fouls per Game) were shown as significant.
# When adjusting model by removing that variable, AIC actually increased. 

# Testing outcomes of model
ll.null <- FitSome$null.deviance/-2
ll.proposed <-FitSome$deviance/-2
(ll.null - ll.proposed) / ll.null
# [1] 0.7689298  Pseudo R2 = 0.77

1 - pchisq(2*(ll.proposed - ll.null), df=length(FitSome$coefficients)-1)
# [1] 0.   Significant

# Creating a new dataset with predicted data from the model.
predicted.data <- data.frame(probability_of_playoffs = FitSome$fitted.values,
                             playoffs_01 = nbaRNA$playoffs_01)
# Ordering the values for graphing
predicted.data <- predicted.data[order(predicted.data$probability_of_playoffs,
                                       decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

# Creating the graph
ggplot(data=predicted.data, aes(x=rank, y=probability_of_playoffs)) +
  geom_point(aes(color=playoffs_01), alpha = 1, shape = 4, stroke = 2) +
  xlab("Index") +
  ylab("Predicted Probability of Making Playoffs")

# Saves graph as a PDF!!
ggsave("Probablilty Of Making Playoffs.pdf")

# Actual prediction numbers
probabilities <- (predicted.data$probability_of_playoffs)
predicted.data$predicted <- ifelse(probabilities > .5, "pos", "neg")
# Recode the predicted column
predicted.data$predicted_R[predicted.data$predicted=='pos'] <- 1
predicted.data$predicted_R[predicted.data$predicted=='neg'] <- 0
# Convert to factors for confusion matrix
predicted.data$predicted_R <- as.factor(predicted.data$predicted_R)
predicted.data$playoffs_01 <- as.factor(predicted.data$playoffs_01)
#Confusion Matrix
cm <- caret::confusionMatrix(predicted.data$predicted_R, 
                             predicted.data$playoffs_01)
cm
# Confusion Matrix and Statistics
#
#             Reference
# Prediction     0   1
#            0 328  23
#            1  27 399
#
# Accuracy : 0.9356         
# 95% CI : (0.916, 0.9519)
# No Information Rate : 0.5431         
# P-Value [Acc > NIR] : <2e-16         
#
# Kappa : 0.8702         
#
# Mcnemar's Test P-Value : 0.6714         
#                                         
#            Sensitivity : 0.9239         
#            Specificity : 0.9455         
#         Pos Pred Value : 0.9345         
#         Neg Pred Value : 0.9366         
#             Prevalence : 0.4569         
#         Detection Rate : 0.4221         
#   Detection Prevalence : 0.4517         
#      Balanced Accuracy : 0.9347         
#                                         
#       'Positive' Class : 0

# Accuracy of model is 93.6%.

# Logit Linearity
nbaL <- nbaRNA[, c('fga_per_game' , 'fg_percent' , 'x3p_percent' , 'x2pa_per_game' ,
                   'x2p_percent' , 'fta_per_game' , 'ft_percent' , 'drb_per_game' , 'trb_per_game' ,
                   'tov_per_game' , 'pf_per_game' , 'w' , 'l' , 'srs' , 'd_rtg')]

predictors <- colnames(nbaL)

nbaL <- nbaL %>%
  mutate(logit=log(probabilities/(1-probabilities))) %>%
  gather(key= "predictors", value="predictor.value", -logit)

# Graph to assess for linearity.
ggplot(nbaL, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method= "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")


# Testing for multicollinearity
nbaM <- nbaRNA[, c('fga_per_game' , 'fg_percent' , 'x3p_percent' , 'x2pa_per_game' ,
                   'x2p_percent' , 'fta_per_game' , 'ft_percent' , 'drb_per_game' , 'trb_per_game' ,
                   'tov_per_game' , 'pf_per_game' , 'w' , 'l' , 'srs' , 'd_rtg')]

# Correlation Matrix to check for highly correlated columns.
correlation_matrix <- cor(nbaM)
# w and l = .89
# w and srs = .93
# l and srs = .93

vif_values <- vif(FitSome)
print(vif_values)
#  fga_per_game    fg_percent   x3p_percent x2pa_per_game 
#    15.387536     92.095360      4.155952    102.042907 
# x2p_percent  fta_per_game    ft_percent  drb_per_game 
#  51.533180      6.147814      5.330680     63.956990 
# trb_per_game  tov_per_game   pf_per_game        w 
#  50.610438     16.979159      2.456462      1.915018 
#     l           srs         d_rtg 
# 3.416392     30.175377     65.716232 

# Retesting after removing highly correlated column 'l'.
nbaM2 <- nbaRNA[, c('fga_per_game' , 'fg_percent' , 'x3p_percent' , 'x2pa_per_game' ,
                   'x2p_percent' , 'fta_per_game' , 'ft_percent' , 'drb_per_game' , 'trb_per_game' ,
                   'tov_per_game' , 'pf_per_game' , 'w' , 'srs' , 'd_rtg')]
correlation_matrix2 <- cor(nbaM2)
# w and srs = .93

# Retesting after removing highly correlated column 'srs'.
nbaM3 <- nbaRNA[, c('fga_per_game' , 'fg_percent' , 'x3p_percent' , 'x2pa_per_game' ,
                    'x2p_percent' , 'fta_per_game' , 'ft_percent' , 'drb_per_game' , 'trb_per_game' ,
                    'tov_per_game' , 'pf_per_game' , 'w' , 'd_rtg')]
correlation_matrix3 <- cor(nbaM3)
# No highly correlated columns. 


# Recreating model.
BetterFit <- glm(playoffs_01 ~ fga_per_game + fg_percent + x3p_percent + x2pa_per_game +
                 x2p_percent + fta_per_game + ft_percent + drb_per_game + trb_per_game +
                 tov_per_game + pf_per_game + w + d_rtg, data=nbaRNA, family="binomial")

summary(BetterFit)
step(BetterFit, direction = "backward")


# Testing for independent errors
dwtest(BetterFit, alternative="two.sided")
# 	Durbin-Watson test
#
# data:  BetterFit
# DW = 1.8883, p-value = 0.09516
# alternative hypothesis: true autocorrelation is not 0

# Test is statistically significant (> .05), however, if it is significant,
# you can then look at the actual value of the Durbin-Watson test statistic.
# If it is between 1 and 3, then the assumption of independent errors is met.

# Screening for outliers.
infl <- influence.measures(BetterFit)
summary(infl)
# If dfb.1_ or dffit values are greater than 1, or if hat is greater than .3 or
# so, you probably have an outlier.  None detected!

# Assumptions met and completed.

# Results
summary(BetterFit)
# Narrowed it down to 5 significant IV's:
# -Wins
# -Defensive Rating
# -Field Goal Attempts per Game
# -Three Point percentage
# -Turnovers per Game

logi.hist.plot(nbaRNA$w,nbaRNA$playoffs_01,boxp=FALSE, type="hist", col="gray")
logi.hist.plot(nbaRNA$d_rtg,nbaRNA$playoffs_01,boxp=FALSE, type="hist", col="gray")
logi.hist.plot(nbaRNA$fga_per_game,nbaRNA$playoffs_01,boxp=FALSE, type="hist", col="gray")
logi.hist.plot(nbaRNA$x3pa_per_game,nbaRNA$playoffs_01,boxp=FALSE, type="hist", col="gray")
logi.hist.plot(nbaRNA$tov_per_game,nbaRNA$playoffs_01,boxp=FALSE, type="hist", col="gray")

nbaLog <- nbaRNA[, c('fga_per_game' , 'fg_percent' , 'x3p_percent' , 'x2pa_per_game' ,
                   'x2p_percent' , 'fta_per_game' , 'ft_percent' , 'drb_per_game' , 'trb_per_game' ,
                   'tov_per_game' , 'pf_per_game' , 'w' , 'd_rtg')]


# Retesting outcomes of model
ll.null <- BetterFit$null.deviance/-2
ll.proposed <-BetterFit$deviance/-2
(ll.null - ll.proposed) / ll.null
# [1] 0.732304  Pseudo R2 = 0.73

1 - pchisq(2*(ll.proposed - ll.null), df=length(BetterFit$coefficients)-1)
# [1] 0.   Significant

# Creating a new dataset with predicted data from the model.
predicted_data <- data.frame(probability_of_playoffs = BetterFit$fitted.values,
                             playoffs_01 = nbaRNA$playoffs_01)
# Ordering the values for graphing
predicted_data <- predicted_data[order(predicted_data$probability_of_playoffs,
                                       decreasing = FALSE),]
predicted_data$rank <- 1:nrow(predicted_data)

# Creating the graph
ggplot(data=predicted_data, aes(x=rank, y=probability_of_playoffs)) +
  geom_point(aes(color=playoffs_01), alpha = 1, shape = 4, stroke = 2) +
  xlab("Index") +
  ylab("Predicted Probability of Making Playoffs")

# Saves graph as a PDF!!
ggsave("Probablilty Of Making Playoffs Final.pdf")

# Actual prediction numbers
probabilities2 <- (predicted_data$probability_of_playoffs)
predicted_data$predicted <- ifelse(probabilities > .5, "pos", "neg")
# Recode the predicted column
predicted_data$predicted_R[predicted_data$predicted=='pos'] <- 1
predicted_data$predicted_R[predicted_data$predicted=='neg'] <- 0
# Convert to factors for confusion matrix
predicted_data$predicted_R <- as.factor(predicted_data$predicted_R)
predicted_data$playoffs_01 <- as.factor(predicted_data$playoffs_01)
#Confusion Matrix
cm2 <- caret::confusionMatrix(predicted_data$predicted_R, 
                             predicted_data$playoffs_01)
cm2

# Confusion Matrix and Statistics
#
#             Reference
# Prediction   0   1
#          0 324  27
#          1  31 395
#
# Accuracy : 0.9254          
# 95% CI : (0.9046, 0.9428)
# No Information Rate : 0.5431          
# P-Value [Acc > NIR] : <2e-16          
#
# Kappa : 0.8495          
#
# Mcnemar's Test P-Value : 0.6936          
#                                          
#            Sensitivity : 0.9127          
#            Specificity : 0.9360          
#         Pos Pred Value : 0.9231          
#         Neg Pred Value : 0.9272          
#             Prevalence : 0.4569          
#         Detection Rate : 0.4170          
#   Detection Prevalence : 0.4517          
#      Balanced Accuracy : 0.9243          
#                                          
#       'Positive' Class : 0   

# Final Model shows 92.5% Accuracy in predicting if a team will make the playoffs
# based on the selected variables provided by the Stepwise Binary Logistic Regression Model.







