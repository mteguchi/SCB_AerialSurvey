#CcSCB_logisticRegression
# should fit into the chunk named dsm_analysis in Cc in SCB.Rmd


rm(list=ls())
#source('CcSCB_functions.R')

load('RData/data4logisticRegression_2016-11-23.RData')

# something strange happened and all pres_abs became 1...
# redo the variable.
dat.all.df$pres_abs <- 0
dat.all.df$pres_abs[dat.all.df$nSI.CC > 0] <- 1

dat.all.df$log.mean.chla_0 <- log(dat.all.df$mean.chla_0)
dat.all.df$log.mean.chla_8 <- log(dat.all.df$mean.chla_8)
dat.all.df$log.mean.chla_30 <- log(dat.all.df$mean.chla_30)

dat.log.reg <- dat.all.df[, c('newX', 'newY',
                              'mean.depth',
                              'log.mean.chla_0',
                              'log.mean.chla_8',
                              'log.mean.chla_30',
                              'mean.sst_0',
                              'mean.sst_8',
                              'mean.sst_30',
                              'pres_abs')]

dat.log.reg <- na.omit(dat.log.reg)

dat.log.reg.scaled <- data.frame(scale(dat.log.reg[, c('mean.depth',
                                           'log.mean.chla_0',
                                           'log.mean.chla_8',
                                           'log.mean.chla_30',
                                           'mean.sst_0',
                                           'mean.sst_8',
                                           'mean.sst_30')],
                            center = TRUE, scale = TRUE))

dat.log.reg.scaled$pres_abs <- dat.log.reg[, c('pres_abs')]

# standardize predictors here:
formulas <- list(as.formula(pres_abs ~ mean.depth  + log.mean.chla_0*mean.sst_0),
                 as.formula(pres_abs ~ mean.depth  + log.mean.chla_8*mean.sst_8),
                 as.formula(pres_abs ~ mean.depth  + log.mean.chla_30*mean.sst_30),
                 as.formula(pres_abs ~ mean.depth  + log.mean.chla_0*mean.sst_8),
                 as.formula(pres_abs ~ mean.depth  + log.mean.chla_0*mean.sst_30),
                 as.formula(pres_abs ~ mean.depth  + log.mean.chla_8*mean.sst_0),
                 as.formula(pres_abs ~ mean.depth  + log.mean.chla_8*mean.sst_30),
                 as.formula(pres_abs ~ mean.depth  + log.mean.chla_30*mean.sst_0),
                 as.formula(pres_abs ~ mean.depth  + log.mean.chla_30*mean.sst_8))

all.fit <- lapply(formulas, FUN = glm, family = 'binomial',
                  data = dat.log.reg.scaled)

AIC.all <- do.call(rbind, lapply(all.fit, FUN = extractAIC))
AIC.all.idx <- 1:dim(AIC.all)[1]

best.fit <- all.fit[[AIC.all.idx[AIC.all[,2] == min(AIC.all[,2])]]]
