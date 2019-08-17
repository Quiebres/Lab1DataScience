# Paquetes a usar
require(ggpubr)
require(tidyverse)
require(corrplot)

# Leyendo el dataset de csv
train <- read.csv("train.csv", TRUE, ",")
# Volviendo el csv en un data frame
class(train)

# ------------ Laboratorio 1 ---------------

# Exploración rápida del dataset usando un resumen
# summary(train$HouseStyle)

# Se separan las variables cuantitativas del dataset train
trainCuan <- train[,c(4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]
# Se separan las variables cualitativas del dataset train
trainCual <- train[,-c(4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]


# --- Elaboración de la matriz de correlación --- 

# Se usa la función cor la cual calcula la correlación de variables numéricas
# Se usa la función round la cual redondea el resultado a 3 decimales
mcorrelacion <- round(cor(trainCuan,use="complete.obs"),3)

# Se visualiza la matriz de correlación de forma gráfica
corrplot(mcorrelacion, type="upper")
corrplot(mcorrelacion, type="lower")
