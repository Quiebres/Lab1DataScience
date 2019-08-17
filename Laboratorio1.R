# Instalación de paquetes
install.packages("rela")
install.packages("psych")
install.packages("FactoMineR")
install.packages("corrplot")
install.packages("cluster")
install.packages("fpc")
install.packages("NbClust")
install.packages("factoextra")
install.packages("REdaS")
require(ggpubr) # Para mejorar la visualización gráfica
require(tidyverse) # Para explotar, manipular y visualizar datos que comparten info
require(corrplot) # Para visualizar la matriz de correlación
require(cluster) #Para calcular la silueta
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el numero de clusters optimo
library(factoextra) #Para hacer graficos bonitos de clustering
library(rela) #Para poder utilizar paf()
library(psych) #Para poder utilizar KMO()
library(FactoMineR)
library(corrplot)
library(REdaS)
# Leyendo el dataset de csv
train <- read.csv("train.csv", TRUE, ",")
# Volviendo el csv en un data frame
class(train)

# ------------ Laboratorio 1 ---------------

# Exploración rápida del dataset usando un resumen
summary(train)

# Se separan las variables cuantitativas del dataset train
trainCuan <- train[,c(4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]
# Se separan las variables cualitativas del dataset train
trainCual <- train[,-c(4,5,18,19,20,21,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,60,62,63,67,68,69,70,71,72,76,77,78,81)]





# --- Elaboración de la matriz de correlación --- 

# Se usa la función cor la cual calcula la correlación de variables numéricas
# Se usa la función round la cual redondea el resultado a 3 decimales
mcorrelacion <- round(cor(trainCuan,use="complete.obs"),3)
corre <- cor(trainCuan)
# Se visualiza la matriz de correlación de forma gráfica
corrplot(mcorrelacion, type="upper")
corrplot(mcorrelacion, type="lower")





# ---------- Clustering -----------

# Elimina los NAs de las variables cuantitativas
trainCuan <- na.omit(trainCuan)

# Se calcula la cantidad de clusters necesarios según el método de Ward
ward <- (nrow(trainCuan[,1:36])-1)*sum(apply(trainCuan[,1:36],2,var))
for (i in 2:10) 
  ward[i] <- sum(kmeans(trainCuan[,1:36], centers=i)$withinss)

# Se grafica el resultado para visualizar mejor
plot(1:10, ward, type="b", xlab="Numero de clusters",  ylab="Grupos suma de cuadrados")

# Agrupamiento por medio de las k-medias
numericas <- trainCuan
km <- kmeans(trainCuan[,1:36],3) # Se calculan 3 clusters
numericas$grupo <- km$cluster # Se crea una nueva columna con el numero de cluster

# Se grafican los 3 grupos de cluster
fviz_cluster(km, data = trainCuan[,1:36],geom = "point", ellipse.type = "norm")

# Método de la silueta
silcluster <- silhouette(km$cluster,dist(trainCuan[,1:36]))
mean(silcluster[,3]) 
# Da como resultado 0.5616

# Grafica el numero optimo de clusters
fviz_nbclust(scale(numericas), kmeans, method = "silhouette", k.max = 10) + theme_minimal() + ggtitle("The Silhouette Plot")



# ---------- PCA -----------
KMO(corre) #Mala adecuacion muestral
bartlett.test(trainCuan) # valor p aproximadamente 0, por lo tanto hay suficiente info para rechazar Ho
pcaTrainCuan <- PCA(trainCuan)
summary(pcaTrainCuan)
bart_spher(mcorrelacion)

# No es posible realizar un PCA para el dataset

