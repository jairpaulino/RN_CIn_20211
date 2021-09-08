#Title: RN
#Author: AM, JP, S
#Date: Jul/2021

# Setup ----
# Cleaning R environment
rm(list=ls()); graphics.off() 

# Libraries
library(dplyr)
library(forecast)
library(caTools)
library(caret)
library(neuralnet)
# library(keras)
# library(tensorflow)
# library(shapr)
# library(xgboost)
# library(ggplot2)

# Importing functions
#source("PerformanceAnalytics")
source("R/timeSeriesModelling.R")
source("R/optimalArimaETS.R")
source("R/dataPreprocessing.R")
source("R/optimalANN.R")
source("R/combinedModels.R")
source("R/performanceMetrics.R")
# Importing data ----
inputData = read.csv(file = "Data/Traffic/input.csv", sep = ",")
#View(inputData); head(data, 3)
outputData = read.csv(file = "Data/Traffic/output.csv", sep = ",", encoding = "UTF-8")
#View(outputData); head(data, 3)

tsRecife = getAccidentTS(outputData)
plot.ts(tsRecife$QntAcid)
plot.ts(tsRecife$Qntvit)

tsBoaViagem = getAccidentTS(outputData, local = "BOA VIAGEM")
plot.ts(tsBoaViagem$QntAcid)
plot.ts(tsBoaViagem$Qntvit)

acf(tsBoaViagem$QntAcid)
pacf(tsBoaViagem$QntAcid)

# Phase 01 - Data Preprocessing #####
# Splitting data into training and test sets
split.data = getSplitData(tsRecife$QntAcid, training_set_size = 0.8)

# Normalizing training and test sets
normalized.data = getNormalizedData(split.data, lim_inf = 0.1, lim_sup = 0.9)

# Phase 02 - Training phase (modelling) #####
# Get optimal ARIMA, ETS, NNAR and MLP models, respectively
# ARIMA
beginARIMA = proc.time()
arima_model = getOptimalARIMA(normalized.data$training_set)
procTimeARIMA = proc.time() - beginARIMA
# ETS
beginETS= proc.time()
ets_model = getOptimalETS(normalized.data$training_set)
procTimeETS = proc.time() - beginETS
# NNAR
# beginNNAR = proc.time()
# nnar_model = getOptimalNNAR(normalized.data$training_set)
# procTimeNNAR = proc.time() - beginNNAR
# MLP
mlp_model = getMLP(normalized.data$training_set, normalized.data$test_set)
procTimeMLP = mlp_model$proc_time_train 

# Creating result table to train set
resultsTrain = as.data.frame(matrix(nrow = length(normalized.data$training_set), 
                                    ncol = 4))
colnames(resultsTrain) = c('obs', 'ARIMA', 'ETS', 'MLP')
resultsTrain$obs = normalized.data$training_set
resultsTrain$ARIMA = arima_model$fitted
resultsTrain$ETS = ets_model$fitted
#resultsTrain$NNAR = nnar_model$fitted
resultsTrain$MLP = c(rep(NA, 12), mlp_model$train)
# View(resultsTrain)

# Get SA, SM
resultsTrain = na.omit(resultsTrain)
# SA
beginSA = proc.time()
resultsTrain$SA = (resultsTrain$ARIMA + resultsTrain$ETS + resultsTrain$MLP)/3
procTimeSA = proc.time() - beginSA
# SM
beginSM = proc.time()
resultsTrain$SM = getSM(resultsTrain)
procTimeSM = proc.time() - beginSM

#head(resultsTrain); tail(resultsTrain)

# Phase 03 - Test phase (forecasting) #####
# One-step ahead approach#
onestep_arima = getARIMAForecasts(normalized.data$test_set, arima_model)
onestep_ets = getETSForecasts(normalized.data$test_set, model = ets_model)
#onestep_nnar = getNNARForecasts(normalized.data$test_set, model = nnar_model)
onestep_mlp = mlp_model$test

resultsTest = as.data.frame(matrix(nrow = length(normalized.data$test_set), 
                                   ncol = 4))
colnames(resultsTest) = c('obs', 'ARIMA', 'ETS', 'MLP')
resultsTest$obs = normalized.data$test_set
resultsTest$ARIMA = onestep_arima
resultsTest$ETS = onestep_ets
#resultsTest$NNAR = onestep_nnar
resultsTest$MLP = c(rep(NA, 12), mlp_model$test)
#View(resultsTest)

# Get SA, SM and DE
resultsTest = na.omit(resultsTest)
resultsTest$SA = (resultsTest$ARIMA + resultsTest$ETS + resultsTest$MLP)/3
resultsTest$SM = getSM(resultsTest)
DE_onestep = getDeepEnsemble(resultsTrain, resultsTest)
resultsTest$DE = DE_onestep[[1]]
# View(resultsTest)
write.csv(resultsTest, file = paste0('Results/', '_onestep.txt'))

# Phase 04 - Performance analysis #####
# Proc time
procTime_df = as.data.frame(matrix(nrow = 1, ncol = 6))
names(procTime_df) = c('ARIMA', 'ETS', 'MLP', 'SA', 'SM', 'DE')
procTime_df$ARIMA = procTimeARIMA[3]
procTime_df$ETS = procTimeETS[3]
#procTime_df$NNAR = procTimeNNAR[3]
procTime_df$MLP = procTimeMLP[3]
procTime_df$SA = procTimeSA[3]
procTime_df$SM = procTimeSM[3]
procTime_df$DE = as.numeric(DE_onestep[2])
#View(procTime_df)

write.csv(procTime_df, file = paste0('Results/', '_procTime.txt'))

#Metrics
metricsTest = getCalculatedMetricsTest(resultsTest) #View(metricsTest)
write.csv(metricsTest, file = paste0('Results/', '_metrics.txt'))

# graphics
cor = c(1, "#32CD32", "#0000FF", 2, "#1E90FF", "#900C3F") 
linha = c(1, 2, 3, 4, 5, 6, 7)
simbolo = c(NA, 15, 16, 17, 18, 19, 20)
legenda = c("TS", "BJ", "ETS        ", "MLP")

a = 67; b = length(resultsTest$obs); b-a
jpeg(filename = paste("Results/","_onestep_teste_IND.jpeg", sep=""), width = 7, height = 6, units = 'in', res = 300)
plot.ts(resultsTest$obs, lwd = 2, xlab = "Index (test set)", 
        ylab = "TS")#), ylim = c(min(resultsTest[a:b,1:4]), max(resultsTest[a:b,1:4])*1.15))
# ARIMA
lines(resultsTest$ARIMA, lwd = 2, col = cor[2], lty = linha[2], pch = simbolo[2])
points(resultsTest$ARIMA, col = cor[2], pch = simbolo[2])
# ETS
lines(resultsTest$ETS, lwd = 2, col = cor[3], lty = linha[3], pch = simbolo[3])
points(resultsTest$ETS, col = cor[3], pch = simbolo[3])
# NNAR
#lines(resultsTest$NNAR[a:b], lwd = 2, col = cor[4], lty = linha[4], pch = simbolo[4])
#points(resultsTest$NNAR[a:b], col = cor[4], pch = simbolo[4])
# MLP
lines(resultsTest$MLP, lwd = 2, col = cor[4], lty = linha[4], pch = simbolo[5])
points(resultsTest$MLP, col = cor[4], pch = simbolo[4])

# legenda
legend("top", legenda, col = cor, horiz = F, x.intersp = 0.5,
       cex = 1, lty = linha, lwd = 2, border = T,
       bty = "o", pch = simbolo, inset = 0.05, y.intersp = 0.3,
       bg = "white", box.col = "white", ncol = 4)
dev.off()

# Combined
cor = c(1, "#32CD32", "#0000FF", 2, "#1E90FF", "#900C3F") 
linha = c(1, 2, 3, 4, 5, 6, 7)
simbolo = c(NA, 15, 16, 17, 18, 19, 20)
legenda = c("TS", "SA        ", "SM", "DE")

# Individual
a = 67; b = length(resultsTest$obs); b-a

jpeg(filename = paste("Results/", "_onestep_teste_COM.jpeg", sep=""), width = 7, height = 6, units = 'in', res = 300)
plot.ts(resultsTest$obs, lwd = 2, xlab = "Index (test set)")#, 
        #ylab = names)#, ylim = c(min(resultsTest[a:b,c(1, 5:7)]), max(resultsTest[a:b,c(1, 5:7)])*1.15))
# SA
lines(resultsTest$SA, lwd = 2, col = cor[2], lty = linha[2], pch = simbolo[2])
points(resultsTest$SA, col = cor[2], pch = simbolo[2])
# SM
lines(resultsTest$SM, lwd = 2, col = cor[3], lty = linha[3], pch = simbolo[3])
points(resultsTest$SM, col = cor[3], pch = simbolo[3])
# DE
lines(resultsTest$DE, lwd = 2, col = cor[4], lty = linha[4], pch = simbolo[3])
points(resultsTest$DE, col = cor[4], pch = simbolo[4])

legend("top", legenda, col = cor, horiz = F, x.intersp = 0.5,
       cex = 1, lty = linha, lwd = 2, border = T,
       bty = "o", pch = simbolo, inset = 0.05, y.intersp = 0.3,
       bg = "white", box.col = "white",  ncol = 4)
dev.off()

metricsTest
# Optimization (in process) ####
#matriz = getAnnMatrix(normalized.data$training_set, ar = 4, ss = 10, sar = 3)
#View(matriz)

# set.seed(123)
# model_mlp = neuralnet(t_0 ~ .,
#                       data = matriz,
#                       learningrate = 0.05,
#                       algorithm = "rprop+",
#                       hidden = c(10, 10),
#                       rep = 10)
# plot(model_mlp)

# source('Codes/optimalANN.R')
# annParamenters = getOptGAParameters() 

# A = getAnnMatrix(ar = 2, ss = 12, sar = 3, time_series = 1:50)
# View(A)
