#CcSCB_RanodmForest
#
# Logistic regression on presence/absence of loggerhead turtles
# in the SCB from aerial survey data.


# Tomo Eguchi
# 12 April 2016

rm(list=ls())
library(party)
library(caret)
source('CcSCB_functions.R')

# prep4logisticRegression_04Nov2016.R created logistic regression
# data
load('RData/data4logisticRegression_2016-12-07.RData')

ntree <- 5000

model.1 <- as.formula("fPresence ~ mean.depth + sd.depth + mean.log.chla_0 +
mean.log.chla_8 + mean.log.chla_30 + mean.sst_0 + mean.sst_8 + mean.sst_30 +
rv")

p <- length(attr(terms(model.1), "term.labels"))
# floor(sqrt(p)) and try half and 2x that
#mtry <- floor(sqrt(p)/2)
mtry <- floor(sqrt(p))

dat.all.df$mean.log.chla_0 <- log(dat.all.df$mean.chla_0)
dat.all.df$mean.log.chla_8 <- log(dat.all.df$mean.chla_8)
dat.all.df$mean.log.chla_30 <- log(dat.all.df$mean.chla_30)
dat.all.df.0 <- dat.all.df
dat.all.df <- dat.all.df[dat.all.df$year == 2015,]

dat.all.df$presence <- 0
dat.all.df$presence[dat.all.df$nSI.CC > 0] <- 1
dat.all.df$fPresence <- as.factor(dat.all.df$presence)

dat.all.df$rv <- runif(nrow(dat.all.df), min = 0, max = 100)


dat.RF <- na.omit(dat.all.df[, c('fPresence',
                                 attr(terms(model.1),
                                      'term.labels'))])

RF.1 <- cforest(formula = model.1,
                data = dat.RF,
                control = cforest_unbiased(ntree = ntree,
                                           mtry = mtry,
                                           trace = 500))

varImp_RF.1 <- sort(varimp(RF.1), decreasing = F)
varImp_AUC_RF.1 <- sort(varimpAUC(RF.1), decreasing = F)

newVars <- names(varImp_RF.1[varImp_RF.1 > varImp_RF.1[names(varImp_RF.1) == "rv"]])
tmpMod <- paste(newVars, collapse = '+')
newmodel <- as.formula(paste('fPresence~', tmpMod))

RF.2 <- cforest(formula = newmodel,
                data = dat.RF,
                control = cforest_unbiased(ntree = ntree,
                                          mtry = mtry,
                                          trace = 500))

pred.RF.2 <- predict(RF.2, newdata = NULL)
confMat <- confusionMatrix(as.factor(pred.RF.2), dat.RF$fPresence)
# horrible predictions!




