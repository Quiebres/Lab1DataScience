# Leyendo el dataset de csv
train <- read.csv("train.csv", TRUE, ",")
# Volviendo el csv en un data frame
class(train)
# Exploración rápida del dataset usando un resumen
summary(train)