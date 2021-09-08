# Categoria de veículo
sumAuto = sum(na.omit(data$auto))
sumMoto = sum(na.omit(data$moto))
sumCiclo = sum(na.omit(data$ciclom))
sumCiclista = sum(na.omit(data$ciclista))
sumPedestre = sum(na.omit(data$pedestre))
sumOnibus = sum(na.omit(data$onibus))
sumCaminhao = sum(na.omit(data$caminhao))
sumViatura = sum(na.omit(data$viatura))
sumOutros = sum(na.omit(data$outros))

categoriaVeículos = c(Auto = sumAuto, Moto = sumMoto, Ciclo = sumCiclo,
                          Ciclista = sumCiclista, Pedestre = sumPedestre, 
                          Onibus = sumOnibus, Caminhão = sumCaminhao,
                          Viatura = sumViatura, Outros = sumOutros)
barplot(categoriaVeículos)

library(qcc)
pareto.chart(categoriaVeículos, main="",
             ylab = "Frequência", ylab2 = "Percentual acumulado")

# Tempo/Clima
library(plyr)
tempoClima = as.factor(na.omit(data$tempo_clima))
count(tempoClima)
tempoObs = (c(Bom = 3083, Chuvoso = 279, Nublado = 15))
jpeg("Results/tempoClima2.jpeg", width = 600)
pareto.chart(tempoObs, main="",
             ylab = "Frequência", ylab2 = "Percentual acumulado")
dev.off()
# Semaforo
semafotoro = as.factor(data$situacao_semaforo)
count(semafotoro)
semaforoObs = c(Defeito = 9, Desligado = 4, Intermitente = 19,
                Não_existe = 1952, Normal = 1343)
pareto.chart(semaforoObs, main="",
             ylab = "Frequência", ylab2 = "Percentual acumulado")


# Condição da via
condicaoVia = as.factor(data$condicao_via)
count(condicaoVia)
condicaoViaObs = c(Molhada = 247, Oleosa = 4, Outros = 2,
                Seca = 3112)
pareto.chart(condicaoViaObs, main="",
             ylab = "Frequência", ylab2 = "Percentual acumulado")

# Condição da via
naturezaAcidente = as.factor(data$natureza_acidente)
count(naturezaAcidente)
naturezaAcidenteObs = c(Com_vítima = 1754, Sem_vítima = 2317,
                   Vítima_fatal = 20)
jpeg("Results/naturezaAcidente.jpeg", width = 600)
pareto.chart(naturezaAcidenteObs, main="",
             ylab = "Frequência", ylab2 = "Percentual acumulado")
dev.off()
