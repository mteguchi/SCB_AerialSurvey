#Estim_abundance
# analysis of design-based abundance estimation from the best
# model in CcSCB_Distance_May2016

rm(list=ls())
source('CcSCB_functions.R')

load('RData/HN_Cos_Bft_out.RData')

estim.bysample.df <- run.hn.cos.bft$dht$individuals$bysample
estim.N.hat.bysample.df <- run.hn.cos.bft$dht$individuals$Nhat.by.sample

# if we assume the entire area was their habitat the total
# abundance is
Nhat <- run.hn.cos.bft$dht$individuals$N
Nhat.cv <- Nhat$cv   # pretty big! 21%
Nhat.95CI <- c(Nhat$lcl, Nhat$ucl)


