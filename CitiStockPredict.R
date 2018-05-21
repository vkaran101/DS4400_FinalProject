setwd('/Users/vandanakaran/Documents/School/Spring_2018/Data_Science/Term_Project/')

library(tidyverse)
library(readr)
library(ggvis)
library(corrplot)
library(DT)
library(psych)
library(dataQualityR)
library(knitr)
library(pROC)
library(quantmod)
library(data.table)
library(e1071)
library(Quandl)
library(zoo)
library(tidyquant)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(boot)

# STEP 1: Creating the first set of descriptive variables and target variable.
# All variables are describing a month from 1990-2018. Date will be kept
# to help sort and organize data, but will not be analyzed as a descriptive variable.

# Loading Stock information using quantmod

getSymbols('C', from='1989-04-01', to='2018-04-01')
getSymbols('^GSPC', from='1989-04-01', to='2018-04-01')

# Converting information into monthly averages

CITI <- data.frame(to.monthly(C))
CITI <- rownames_to_column(CITI, var='Date')

SP500 <- data.frame(to.monthly(GSPC))
SP500 <- rownames_to_column(SP500, var='Date')

# Calculating monthly returns

c_monthly_returns <- data.frame(monthlyReturn(C))
c_monthly_returns <- rownames_to_column(c_monthly_returns, var='Date')
c_monthly_returns$Date <- as.Date(c_monthly_returns$Date)

sp_monthly_returns <- data.frame(monthlyReturn(GSPC))
sp_monthly_returns <- rownames_to_column(sp_monthly_returns, var='Date')
sp_monthly_returns$Date <- as.Date(sp_monthly_returns$Date)

# Loading Treasury Data information using Quandl to get data from
# Federal Reserve Economic Data (FRED). Computer monthly risk-free rate
# Using the 3-month Treasury Yield

treasury_data <- data.frame(Quandl("USTREASURY/YIELD"))
treasury_data$Date <- as.Date(treasury_data$Date)
treasury_data$Month <- months(treasury_data$Date)
treasury_data$Year <- format(treasury_data$Date,format="%Y")
rf_data <- aggregate( X3.MO ~ Month + Year , treasury_data , mean )
rf_data$yearmon <- paste(rf_data$Year, '-', rf_data$Month, sep='')
rf_data$yearmon <- as.yearmon(rf_data$yearmon, '%Y-%b')
rf_data <- rf_data[order(rf_data$yearmon),]

# Create data frame to hold information about risk: Standard Deviation,
# Variance, and Covaraince.
data_risk <- data.frame(c_monthly_returns$Date, runSD(c_monthly_returns$monthly.returns), 
                runCov(c_monthly_returns$monthly.returns, 
                       sp_monthly_returns$monthly.returns), 
                runVar(sp_monthly_returns$monthly.returns))

colnames(data_risk) <- c('Date', 'C.stdev', 'C.SP.cov', 'SP.var')
# data_risk <- data.frame(data_risk)
# data_risk <- rownames_to_column(data_risk, var='Date')
data_risk$Date <- as.Date(data_risk$Date)
data_risk$beta <- (data_risk$C.SP.cov/data_risk$SP.var)
data_risk <- data_risk[data_risk$Date >= '1990-01-01',]

# save a list of returns 1990 and onwards
class <- c_monthly_returns[c_monthly_returns$Date >= '1990-01-01',]

# pull Federal Funds Rate and Unemployment Ratedata from FRED using tidyquant
FEDFUNDS <- data.frame(tq_get('FEDFUNDS', get='economic.data', 
                              from='1990-01-01',env=NULL))
UNRATE <- data.frame(tq_get('UNRATE', get='economic.data', 
                            from='1990-01-01',env=NULL))

# Create data frame to predict end of month increase/decrease
data <- data.frame(FEDFUNDS$date, data_risk[,-1], 
                   class$monthly.returns, 
                   sp_monthly_returns[sp_monthly_returns$Date >= '1990-01-01',]$monthly.returns, 
                   rf_data$X3.MO[-nrow(rf_data)], FEDFUNDS$price, UNRATE$price)

colnames(data) <- c('Date', 'C.stdev', 'C.SP.cov', 
                    'GSPC.var', 'beta', 'Class', 
                    'GSPC.monthly_returns', 'Rf_rate', 'FFR', 'UNRATE')
data$Date <- as.Date(data$Date)
data <- data[,c("Date", "C.stdev", "beta", "FFR", "UNRATE", 
                "Rf_rate", "Class")]


# Create data frame to predict next month increase/decrease
data_future_class <- data.frame(FEDFUNDS$date, data_risk[,-1], 
                                sp_monthly_returns[sp_monthly_returns$Date >= '1990-01-01',]$monthly.returns, 
                                rf_data$X3.MO[-nrow(rf_data)], 
                                FEDFUNDS$price, UNRATE$price)
colnames(data_future_class) <- c('Date', 'C.stdev', 'C.SP.cov', 
                                 'GSPC.var', 'beta', 
                                 'GSPC.monthly_returns', 'Rf_rate', 
                                 'FFR', 'UNRATE')

data_future_class <- data_future_class[-nrow(data_future_class),]
data_future_class$Class <- class[2:nrow(class),2]
data_future_class$Date <- as.Date(data_future_class$Date)
data_future_class <- data_future_class[,c("Date", "C.stdev", "beta", 
                                          "FFR", "UNRATE", "Rf_rate", 
                                          "Class")]

## STEP 2: Data Visualization and Data Quality Report

# Charting Series
chartSeries(C)

# Identifying minimum and maximum points. As expected, market was not doing
# well during the economic recession between 2007-2010.
data[which.max(data$C.stdev),]
data[which.max(data$UNRATE),]
data[which.min(data$FFR),]
data[which.max(data$beta),]

# Histogram of Continuous Variables
hist(data$C.stdev)
hist(data$beta)
hist(data$UNRATE)
hist(data$Class)
hist(data$FFR)


# Correlation:
# Positive correlation between Unempoyment and Beta
# Perfect correlation between FFR and Risk-Free Rate
pairs.panels(data[,-1])
pairs.panels(data_future_class[,-1])

## STEP 3: Refining Data based on Data Quality Report
# Remove FFR (redundant with Risk-free rate)

data <- data[,c("Date", "C.stdev", "beta", "UNRATE", 
                "Rf_rate", "Class")]

data_future_class <- data_future_class[,c("Date", "C.stdev", "beta", "UNRATE", 
                             "Rf_rate", "Class")]

# Create data with factors
data_with_factors <- data
data_with_factors$Class <- as.factor(ifelse(data_with_factors$Class >= 0, 'UP', 'DOWN'))
data_future_with_factors <- data_future_class
data_future_with_factors$Class <- as.factor(ifelse(data_future_with_factors$Class >= 0, 'UP', 'DOWN'))

# Create data with binary- some tests only run when target variable is in binary
data_with_binary <- data
data_with_binary$Class <- ifelse(data_with_binary$Class >= 0, 1, 0)
data_future_with_binary <- data_future_class
data_future_with_binary$Class <- ifelse(data_future_with_binary$Class >= 0, 1, 0)

## STEP 4: Create Training and Testing Data sets
# Data prior to 2010 will be Training, Data after 2010 will be testing.

train_factor <- data_with_factors[data_with_factors$Date < '2010-01-01',]
test_factor <- data_with_factors[data_with_factors$Date >= '2010-01-01',]

train_binary <- data_with_binary[data_with_binary$Date < '2010-01-01',]
test_binary <- data_with_binary[data_with_binary$Date >= '2010-01-01',]

train_fut_factor <- data_future_with_factors[data_future_with_factors$Date < '2010-01-01',]
test_fut_factor <- data_future_with_factors[data_future_with_factors$Date >= '2010-01-01',]

train_fut_binary <- data_future_with_binary[data_future_with_binary$Date < '2010-01-01',]
test_fut_binary <- data_future_with_binary[data_future_with_binary$Date >= '2010-01-01',]

## STEP 5: Creating and Evaluating Logistic Regression Models

# Logistic Model to Predict end of month increase/decrease
logistic_model <- glm(Class~. , data=train_binary[, -1], family = 'binomial')
pred_logistic <- predict(logistic_model, test_binary[,-1])
roc <- roc(test_binary[,-1]$Class, pred_logistic)
auc <- auc(roc)
plot(roc)

# Logistic Model to Predict next month increase/decrease
logistic_model2 <- glm(Class~. , data=train_fut_binary[, -1], family = 'binomial')
pred_logistic2 <- predict(logistic_model2, test_fut_binary[,-1])
roc2 <- roc(test_fut_binary[,-1]$Class, pred_logistic2)
auc2 <- auc(roc2)
plot(roc2)

# Refined Logistic Model to Predict next month increase/decrease
logistic_model3 <- glm(Class~ C.stdev + UNRATE + Rf_rate, data=train_fut_binary[, -1], family = 'binomial')
pred_logistic3 <- predict(logistic_model3, test_fut_binary[,-1])
roc3 <- roc(test_fut_binary[,-1]$Class, pred_logistic3)
auc3 <- auc(roc3)
plot(roc3)

## STEP 6: Creating and Evaluating Classification Tree Models

# Classification Model to Predict current month increase/decrease
tree <- rpart(Class~. , data = train_factor[,-1], method="class")
prp(tree, type = 1)
pred_tree <- predict(tree, test_factor[,-1], type='class')
kable(table(pred_tree, test_factor[,-1]$Class), caption='Current Month Class Prediction Accuracy')

# Pruning the tree
fitTree1 <- prune(tree, cp = tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
prp(fitTree1, type = 1)
pred_fitTree <- predict(fitTree1, test_factor[,-1], type='class')
kable(table(pred_fitTree, test_factor[,-1]$Class), caption='Current Month Prediction Accuracy: Pruned Tree')

# Classification Model to Predict next month increase/decrease
tree2 <- rpart(Class~. , data = train_fut_factor[,-1], method="class")
prp(tree2, type = 1)
pred_tree2 <- predict(tree2, test_fut_factor[,-1], type='class')
table(pred_tree2, test_fut_factor[,-1]$Class)
kable(table(pred_tree2, test_fut_factor[,-1]$Class), caption='Next Month Class Prediction Accuracy')

# Pruning the tree
fitTree2 <- prune(tree2, cp = tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"])
prp(fitTree2, type = 1)
pred_fitTree2 <- predict(fitTree2, test_fut_factor[,-1], type='class')
table(pred_fitTree2, test_fut_factor[,-1]$Class)
kable(table(pred_fitTree2, test_fut_factor[,-1]$Class), caption='Current Month Prediction Accuracy: Pruned Tree')

# Repeat of Tree 1, analyzing class as a binary variable instead of
# as a factor. This allows us to make an ROC curve.

tree3 <- rpart(Class~. , data = train_binary[,-1], method="anova")
prp(tree3, type = 1)
pred_tree3 <- predict(tree3, test_binary[,-1])
table(pred_tree3, test_binary[,-1]$Class)

# Creating ROC, calculating AUC
tree3_roc <- roc(test_binary[,-1]$Class, pred_tree3)
tree3_auc <- auc(tree3_roc)

# Pruning the Tree
fitTree3 <- prune(tree3, cp = tree3$cptable[which.min(tree3$cptable[,"xerror"]),"CP"])
pred_fitTree3 <- predict(fitTree3, test_binary[,-1])
table(pred_fitTree3, test_binary[,-1]$Class)

fitTree3_roc <- roc(test_binary[,-1]$Class, pred_fitTree3)
fitTree3_auc <- auc(fitTree3_roc)

# Repeat of Tree 3, analyzing class as a binary variable instead of
# as a factor. This allows us to make an ROC curve.
tree4 <- rpart(Class~. , data = train_fut_binary[,-1], method="anova")
prp(tree4, type = 1)
pred_tree4 <- predict(tree4, test_fut_binary[,-1])
table(pred_tree4, test_fut_binary[,-1]$Class)

# Creating ROC, calculating AUC
tree4_roc <- roc(test_fut_binary[,-1]$Class, pred_tree4)
tree4_auc <- auc(tree4_roc)

# Pruning the Tree
fitTree4 <- prune(tree4, cp = tree4$cptable[which.min(tree4$cptable[,"xerror"]),"CP"])
pred_fitTree4 <- predict(fitTree4, test_fut_binary[,-1])
table(pred_fitTree4, test_fut_binary[,-1]$Class)

fitTree4_roc <- roc(test_fut_binary[,-1]$Class, pred_fitTree4)
fitTree4_auc <- auc(fitTree4_roc)

## STEP 7: Creating a different data set of of descriptive variables and target variable
# This time, the data will be daily instead of monthly, seeing that
# we could not gain much insight about monthly stock prices.
# FFR, Unemployment were not that significant predictors- omit them
# Focus instead on oscillators and moving averages, popular indicators
# in technical analysis.

getSymbols('C', from='1989-01-01', to='2018-05-01')
getSymbols('^GSPC', from='1990-01-01', to='2018-05-01')

# Get the daily returns information
c_daily_returns <- data.frame(dailyReturn(C))
c_daily_returns <- rownames_to_column(c_daily_returns, var='Date')
c_daily_returns$Date <- as.Date(c_daily_returns$Date)

# Calculate RSI, MACD, OBV, and Daily Risk-free rate.
c_rsi <- rownames_to_column(data.frame(RSI(C$C.Close)), var='Date')
c_macd = rownames_to_column(data.frame(MACD(C$C.Close, nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)), var='Date')
c_obv <- rownames_to_column(data.frame(OBV(C$C.Close, C$C.Volume)), var='Date')
rf_daily <- na.omit(treasury_data[ , c('Date', 'X3.MO')])
c_daily_returns <- data.frame(c_daily_returns, c_rsi, c_macd$macd, c_macd$signal, c_obv$obv, runSD(c_daily_returns$daily.returns))

# Create new data set to predict end of the day increase/decrease
data_adj <- merge(c_daily_returns, rf_daily)[,-3]
colnames(data_adj) <- c('Date', 'Class', 'RSI', 'MACD', 'Signal', 'OBV', 'SD', 'Rf')

# Create new data set to predict next day increase/decrease. We will be using this one.
data_adj_future_class <- data_adj[-nrow(data_adj),]
data_adj_future_class$daily.returns <- data_adj$daily.returns[2:nrow(data_adj)]

# Data with factors
data_adj_future_with_factors <- data_adj_future_class
data_adj_future_with_factors$Class <- as.factor(ifelse(data_adj_future_with_factors$Class >= 0, 'UP', 'DOWN'))

# Create data with binary
data_adj_future_with_binary <- data_adj_future_class
data_adj_future_with_binary$Class <- ifelse(data_adj_future_with_binary$Class >= 0, 1, 0)

## STEP 8: Create Test and Training Data sets.
train_fut_factor_adj <- data_adj_future_with_factors[data_adj_future_with_factors$Date < '2010-01-01',]
test_fut_factor_adj <- data_adj_future_with_factors[data_adj_future_with_factors$Date >= '2010-01-01',]

train_fut_binary_adj <- data_adj_future_with_binary[data_adj_future_with_binary$Date < '2010-01-01',]
test_fut_binary_adj <- data_adj_future_with_binary[data_adj_future_with_binary$Date >= '2010-01-01',]

## STEP 9: Create and Evaluate Logistic Models
set.seed(100)

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
logistic_model4 <- glm(Class~. , data=train_fut_factor_adj[, -1], family = 'binomial', na.action = na.exclude)
pred_logistic4 <- predict(logistic_model4, test_fut_factor_adj[,-1])
roc4 <- roc(test_fut_factor_adj[,-1]$Class, pred_logistic4)
auc4 <- auc(roc4)
plot(roc4)
acc4 <- 1 - cv.glm(test_fut_factor_adj[,-1], logistic_model4, cost, K = 10)$delta[1]

logistic_model5 <- glm(Class ~ RSI, data=train_fut_factor_adj[, -1], family = 'binomial', na.action = na.exclude)
pred_logistic5 <- predict(logistic_model5, test_fut_factor_adj[,-1])
roc5 <- roc(test_fut_factor_adj[,-1]$Class, pred_logistic5)
auc5 <- auc(roc5)
plot(roc5)
acc5 <- 1 - cv.glm(test_fut_factor_adj[,-1], logistic_model5, cost, K = 10)$delta[1]

logistic_model6 <- glm(Class ~ RSI + MACD, data=train_fut_factor_adj[, -1], family = 'binomial', na.action = na.exclude)
pred_logistic6 <- predict(logistic_model6, test_fut_factor_adj[,-1])
roc6 <- roc(test_fut_factor_adj[,-1]$Class, pred_logistic6)
auc6 <- auc(roc6)
plot(roc6)
acc6 <- 1 - cv.glm(test_fut_factor_adj[,-1], logistic_model6, cost, K = 10)$delta[1]

logistic_model7 <- glm(Class ~ RSI + MACD + Signal, data=train_fut_factor_adj[, -1], family = 'binomial', na.action = na.exclude)
pred_logistic7 <- predict(logistic_model7, test_fut_factor_adj[,-1])
roc7 <- roc(test_fut_factor_adj[,-1]$Class, pred_logistic7)
auc7 <- auc(roc7)
plot(roc7)
acc7 <- 1 - cv.glm(test_fut_factor_adj[,-1], logistic_model7, cost, K = 10)$delta[1]

logistic_model8 <- glm(Class ~ RSI + MACD + Signal + SD, data=train_fut_factor_adj[, -1], family = 'binomial', na.action = na.exclude)
pred_logistic8 <- predict(logistic_model8, test_fut_factor_adj[,-1])
roc8 <- roc(test_fut_factor_adj[,-1]$Class, pred_logistic8)
auc8 <- auc(roc8)
plot(roc8)
acc8 <- 1 - cv.glm(test_fut_factor_adj[,-1], logistic_model4, cost, K = 10)$delta[1]

# Create and Evaluate Classification Tree Models

# Creating a Classification Tree to predict next day increase/decrease
tree5 <- rpart(Class~. , data = train_fut_factor_adj[,-1], method="class")
prp(tree5, type = 1)
pred_tree5 <- predict(tree5, test_fut_factor_adj[,-1], type='class')
table(pred_tree5, test_fut_factor_adj[,-1]$Class)
kable(table(pred_tree5, test_fut_factor_adj[,-1]$Class), caption='Next Month Class Prediction Accuracy')

# Pruning the Tree
fitTree5 <- prune(tree5, cp = tree5$cptable[which.min(tree5$cptable[,"xerror"]),"CP"])
prp(fitTree5, type = 1)
pred_fitTree5 <- predict(fitTree5, test_fut_factor_adj[,-1], type='class')
table(pred_fitTree5, test_fut_factor_adj[,-1]$Class)
kable(table(pred_fitTree5, test_fut_factor_adj[,-1]$Class), caption='Current Month Prediction Accuracy: Pruned Tree')

# Recreating Tree 5 with Class as a binary variable
# This allows us to create an ROC.
tree6 <- rpart(Class~. , data = train_fut_binary_adj[,-1], method="anova")
prp(tree6, type = 1)
pred_tree6 <- predict(tree6, test_fut_binary_adj[,-1])
table(pred_tree6, test_fut_binary_adj[,-1]$Class)

tree6_roc <- roc(test_fut_binary_adj[,-1]$Class, pred_tree6)
tree6_auc <- auc(tree6_roc)

fitTree6 <- prune(tree6, cp = tree6$cptable[which.min(tree6$cptable[,"xerror"]),"CP"])
pred_fitTree6 <- predict(fitTree6, test_fut_binary_adj[,-1])
table(pred_fitTree6, test_fut_binary_adj[,-1]$Class)

fitTree6_roc <- roc(test_fut_binary_adj[,-1]$Class, pred_fitTree6)
fitTree6_auc <- auc(fitTree6_roc)

# Boosting with randomForest
boosted_fit <- randomForest(Class~ ., data=train_fut_binary_adj[,-1])
boosted_pred <- predict(boosted_fit, test_fut_binary_adj[,-1])
boosted_roc <- roc(test_fut_binary_adj$Class, boosted_pred)
boosted_auc <- auc(boosted_roc)