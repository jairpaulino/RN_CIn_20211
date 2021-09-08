#Title: RN
#Author: AM, JP, S
#Date: Jul/2021

# Setup ----
# Cleaning R environment
rm(list=ls()); graphics.off() 

# Libraries
library(dplyr)
library(mltools)
library(data.table)
library(caret)
library(neuralnet)
library(keras)
library(tensorflow)
library(shapr)
library(xgboost)

# Importing functions
library("PerformanceAnalytics")

# Importing data ----
data = read.csv(file = "Data/Projeto/dados/acidentes/acidentes_2020-novo_adj.csv", sep = ";")
#View(data); head(data, 3)
glimpse(data)
summary(data)

NATACIN = as.factor(data$natureza_acidente)
#View(NATACIN)
TIPO = one_hot(as.data.table(as.factor(data$tipo)))
#View(TIPO)
vei_auto = data$auto; vei_auto[is.na(vei_auto)] = 0
vei_moto = data$moto; vei_moto[is.na(vei_moto)] = 0
vei_ciclom = data$ciclom; vei_ciclom[is.na(vei_ciclom)] = 0
vei_ciclista = data$ciclista; vei_ciclista[is.na(vei_ciclista)] = 0
vei_pedestre = data$pedestre; vei_pedestre[is.na(vei_pedestre)] = 0
vei_onibus = data$onibus; vei_onibus[is.na(vei_onibus)] = 0
vei_caminhao = data$caminhao; vei_caminhao[is.na(vei_caminhao)] = 0
vei_viatura = data$viatura; vei_viatura[is.na(vei_viatura)] = 0
vei_outros = data$outros; vei_outros[is.na(vei_outros)] = 0
VEI = data.frame(vei_auto, vei_moto, vei_ciclom,
                 vei_ciclista, vei_pedestre, vei_onibus,
                 vei_caminhao, vei_viatura, vei_outros)
#View(VEI)
VIT = one_hot(as.data.table(as.factor(data$vitimas)))
#View(VIT)
VIT_FAT = data$vitimasfatais
VIT_FAT[is.na(VIT_FAT)] = 0
#View(VIT_FAT)
TEMPCLIMA = one_hot(as.data.table(as.factor(data$tempo_clima)))
#View(TEMPCLIMA)
SITSEMAFORO = one_hot(as.data.table(as.factor(data$situacao_semaforo)))
#View(SITSEMAFORO)
SINALIZACAO = one_hot(as.data.table(as.factor(data$sinalizacao)))
#View(SINALIZACAO)
SITPLACA = one_hot(as.data.table(as.factor(data$situacao_placa)))
#View(SITPLACA)
VELMAXVIA = one_hot(as.data.table(as.factor(data$velocidade_max_via)))
#View(VELMAXVIA)
MAODIRECAO = one_hot(as.data.table(as.factor(data$mao_direcao)))
#View(MAODIRECAO)

newdata = data.frame(NATACIN, TIPO, VEI, TEMPCLIMA, 
                     SITSEMAFORO, SINALIZACAO, SITPLACA,
                     VELMAXVIA, MAODIRECAO)

newdata = data.frame(NATACIN, TIPO, VEI, TEMPCLIMA, 
                     SITSEMAFORO, SINALIZACAO, SITPLACA,
                     VELMAXVIA, MAODIRECAO)


#View(newdata)
set.seed(123)
ind = sample(2, nrow(newdata), replace = T,
             prob = c(0.7, 0.3))
train = as.matrix(newdata[ind == 1, 2:84])
trainTarget = as.factor(newdata[ind==1, 1])
test = as.matrix(newdata[ind == 2, 2:84])
testTarget = as.matrix(newdata[ind==2, 1])
trainLabels = as.matrix(one_hot(as.data.table(trainTarget)))
testLabels = as.matrix(one_hot(as.data.table(testTarget)))
#View(trainLabels); View(testLabels)
#write.csv(newdata, file = "Data/newdata.csv")

neuralModel = neuralnet(NATACIN ~ ., 
                        hidden = c(1),
                        data = newdata[1:150,],
                        linear.output = FALSE)
plot(neuralModel)
res = round(neuralModel$net.result[[1]])

test = predict(neuralModel, newdata[3501:4092,])
resTest = round(test)
accuracy <- mean(resTest == newdata[3501:4092,])
accuracy
