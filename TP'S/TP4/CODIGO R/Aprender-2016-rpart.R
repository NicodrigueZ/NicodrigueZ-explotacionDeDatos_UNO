
##############################################################
# Arboles de desicion del dataset Aprender 2016.                     
#                                                            
# Creado: 19/10/2023                                         
# Version: 1.0                                               
# Autor: Grupo 4                                    

##############################################################

##### Importamos las biliotecas #####
library(plyr)  
library(rpart)
library(rpart.plot) 
library(caret)
library(ggplot2)
library(gridExtra) 
library(tidyverse) 
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(readr)

library(dplyr)
library(tidyr)
library(corrplot)
#library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)

library(randomForest)
library(ggpubr)
library(plotly)

##### Lectura y verificacion de la informacion leida #####
# Seteamos el entorno de desarrollo.
setwd(paste0(getwd(),"/" ))

aprender <- read_delim("./UNO_EDD/TP4_Arboles_Decision/aprender2016-primaria-3.csv", 
                       delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", 
                                                                           grouping_mark = "."), trim_ws = TRUE)
View(aprender)
dim(aprender)
nrow(aprender)

head(aprender)


# Verificamos si hay valores nulos.
sapply(aprender, function(x) sum(is.na(x)))

# complete.cases() devuelve un vector con los valores completos y que sean no nulos.
aprender_clean <- aprender[complete.cases(aprender), ]

# Verificamos si hay valores nulos.
sapply(aprender_clean, function(x) sum(is.na(x))) 

summary(aprender_clean)
aprender_clean$mpondera
##### Normalizacion y analisis #####
# Grafico dinamico de boxplot para identificacion de outliers
grafico.boxplot <- plot_ly(y=aprender_clean$ponder, type="box", name="ponder") %>%
  add_trace(y=aprender_clean$lpondera, type="box", name="lpondera") %>%
  add_trace(y=aprender_clean$mpondera, type="box", name="mpondera")

grafico.boxplot

names(aprender_clean)

aprender_clean = subset(aprender_clean, select = c(Ap1, Ap2, Ap3, Ap4, Ap5, Ap6, Ap7, 
                                                   Ap8a, Ap8b, Ap8c, Ap9, Ap10, Ap11,
                                                   Ap12, Ap13, Ap14, Ap15, lpuntaje,
                                                   ldesemp, mpuntaje, mdesemp, sector,
                                                   ambito, iclima,autoconl, autoconm
))


# Normalizamos campos.
aprender_clean$Ap1 <- as.factor(mapvalues(aprender_clean$Ap1,
                                                 from=c(1,2,3,4,5, -1,-9),
                                                 to=c("7 años o menos", "8 años","9 años","10 años","11 años o mas","En blanco","No respondio")))

aprender_clean$Ap2 <- as.factor(mapvalues(aprender_clean$Ap2,
                                          from=c(1,2, -1,-9),
                                          to=c("varon", "mujer","En blanco","No respondio")))

aprender_clean$Ap3 <- as.factor(mapvalues(aprender_clean$Ap3,
                                          from=c(1,2,3,4, -1,-9),
                                          to=c("antes 4anios", "a 4anios","desde 5anios","no_fui_jardin","blanco","No_respondio")))

aprender_clean$Ap4 <- as.factor(mapvalues(aprender_clean$Ap4,
                                          from=c(1,2,3,4,5, -1,-9),
                                          to=c("nunca", "una vez","dos veces","tres veces o mas","no me acuerdo","En blanco","No respondio")))

aprender_clean$Ap5 <- as.factor(mapvalues(aprender_clean$Ap5,
                                          from=c(1,2,3,4, -1,-9),
                                          to=c("muy bien", "bien","mas o menos bien","mal","En blanco","No respondio")))

aprender_clean$Ap6 <- as.factor(mapvalues(aprender_clean$Ap6,
                                          from=c(1,2,3,4, -1,-9),
                                          to=c("muy bien", "bien","mas o menos bien","mal","En blanco","No respondio")))

aprender_clean$Ap7 <- as.factor(mapvalues(aprender_clean$Ap7,
                                          from=c(1,2,3,4, -1,-9),
                                          to=c("muy bien", "bien","mas o menos bien","mal","En blanco","No respondio")))


aprender_clean$Ap8a <- as.factor(mapvalues(aprender_clean$Ap8a,
                                          from=c(1,2,3,-1,-9),
                                          to=c("muchas veces", "algunas veces","casi nunca","En blanco","No respondio")))

aprender_clean$Ap8b <- as.factor(mapvalues(aprender_clean$Ap8b,
                                          from=c(1,2,3, -1,-9),
                                          to=c("muchas veces", "algunas veces","casi nunca","En blanco","No respondio")))

aprender_clean$Ap8c <- as.factor(mapvalues(aprender_clean$Ap8c,
                                           from=c(1,2,3, -1,-9),
                                           to=c("muchas veces", "algunas veces","casi nunca","En blanco","No respondio")))


aprender_clean$Ap9 <- as.factor(mapvalues(aprender_clean$Ap9,
                                          from=c(1,2,3,4, -1,-9),
                                          to=c("con todos", "con la mayoria","con algunos","con ninguno","En blanco","No respondio")))

aprender_clean$Ap10 <- as.factor(mapvalues(aprender_clean$Ap10,
                                          from=c(1,2,3,4, -1,-9),
                                          to=c("alegria", "lo mismo","poca tristeza","mucha tristeza","En blanco","No respondio")))


aprender_clean$Ap11 <- as.factor(mapvalues(aprender_clean$Ap11,
                                          from=c(1,2,3, -1,-9),
                                          to=c("siempre o casi siempre","a veces", "nunca o casi nunca","En blanco","No respondio")))


aprender_clean$Ap12 <- as.factor(mapvalues(aprender_clean$Ap12,
                                          from=c(1,2,3, -1,-9),
                                          to=c("siempre o casi siempre","a veces", "nunca o casi nunca","En blanco","No respondio")))


aprender_clean$Ap13 <- as.factor(mapvalues(aprender_clean$Ap13,
                                          from=c(1,2,3, -1,-9),
                                          to=c("siempre o casi siempre","a veces", "nunca o casi nunca","En blanco","No respondio")))

aprender_clean$Ap14 <- as.factor(mapvalues(aprender_clean$Ap14,
                                           from=c(1,2,3,4, -1,-9),
                                           to=c("muy facil","un poco facil", "un poco dificil","dificil","En blanco","No respondio")))


aprender_clean$Ap15 <- as.factor(mapvalues(aprender_clean$Ap15,
                                           from=c(1,2,3,4, -1,-9),
                                           to=c("muy facil","un poco facil", "un poco dificil","dificil","En blanco","No respondio")))

aprender_clean$ldesemp <- as.factor(mapvalues(aprender_clean$ldesemp,
                                           from=c(1,2,3,4),
                                           to=c("por debajo del nivel basico","basico", "satisfactorio","avanzado")))

aprender_clean$mdesemp <- as.factor(mapvalues(aprender_clean$mdesemp,
                                              from=c(1,2,3,4),
                                              to=c("por debajo del nivel basico","basico", "satisfactorio","avanzado")))

aprender_clean$sector <- as.factor(mapvalues(aprender_clean$sector,
                                              from=c(1,2,3),
                                              to=c("estatal","privada", "sin datos")))

aprender_clean$ambito <- as.factor(mapvalues(aprender_clean$ambito,
                                              from=c(1,2,3),
                                              to=c("urbano","rural", "sin datos")))

aprender_clean$iclima <- as.factor(mapvalues(aprender_clean$iclima,
                                                from=c(1,2,3,-1),
                                                to=c("bajo","medio", "alto","en blanco")))

aprender_clean$autoconl <- as.factor(mapvalues(aprender_clean$autoconl,
                                             from=c(1,2,3,-1),
                                             to=c("bajo","medio", "alto","en blanco")))

aprender_clean$autoconm <- as.factor(mapvalues(aprender_clean$autoconm,
                                               from=c(1,2,3,-1),
                                               to=c("bajo","medio", "alto","en blanco")))
aprender_clean %>% distinct(Ambito)

names(aprender_clean)

aprender %>% distinct(ambito)
names(aprender_clean) <- c("anios",
                        "genero", 
                        "asistio_jardin", 
                        "repitio", 
                        "como_lees", 
                        "como_escribis", 
                        "como_resolv_mat", 
                        "interrum_maestros", 
                        "maestros_se_enojan",
                        "maestros_explican",
                        "relacion_companeros",
                        "sent_camb_escuela",
                        "contento_escuela",
                        "abuurido",
                        "incomodo",
                        "examen_lengua",
                        "examen_mat",
                        "puntaje_lengua",
                        "desempeno_lengua",
                        "puntaje mat",
                        "desempeno mat",
                        "sector",
                        "ambito",
                        "clima escolar",
                        "autoconcepto lengua",
                        "autoconcepto matematica"
)

attach(aprender_clean)
aprender_clean %>% distinct (sector)
#aprender_clean_cordoba <- subset(aprender_clean, aprender_clean$Provincia == "Cordoba")
#head(aprender_clean_cordoba)

##### Modelado - Arbol de desicion #####
set.seed(1250) # Numero inicial del cual comenzara a generar una secuencia aleatoria.
split_train_test <- createDataPartition(aprender_clean$sector, p=0.7, list=FALSE) #p = porcentaje de los datos a usar, list = Si el resultado devolvera una lista o una matriz. 
dtrain<- aprender_clean[split_train_test,]
dtest<-  aprender_clean[-split_train_test,]

sqldf('
SELECT asistio_jardin, count(*)
FROM dtrain
WHERE `puntaje lengua` < 463
GROUP BY asistio_jardin
ORDER BY 2 DESC
')

dtrain$`puntaje lengua`

sqldf('
SELECT DISTINCT repitio 
FROM dtrain

')
#11.853 < 463 --> 33%
#22.299 > 463 --> 66%
#34153

tr_fit <- rpart(sector ~., data = dtrain, method="class") # Indicamos que deseamos un arbol de clasificacion, tambien podemos armar un arbol de regresion.
tr_fit # Nuestro arbol obtenido.
rpart.plot(tr_fit, tweak = 1.6) # Graficamos el arbol.

prp(tr_fit, 
    type = 2, #Espefica que el cada nodo quede etiquetado, y que el split quede debajo de cada nodo
    extra = 104, #Muestra la probabilidad de cada clase en el nodo
    nn = TRUE, #Etiqueta el nro de nodo
    fallen.leaves = TRUE, #Muestra los nodos hojas abajo de todo el grafico
    faclen = 10, #Se utiliza para abreviar el nombre de las clases en 4caracteres
    varlen = 10, #Se utiliza para abreviar el nombre de las variables en 8caracteres
    shadow.col = "gray")

#Importancia de las variables.
qplot(x = names(tr_fit$variable.importance), y=tr_fit$variable.importance,
      xlab="Variable", ylab="Importancia", main="rpart - Importancia de las variables") 

##########################################################################################
######################## 05 - TEST Y VALIDACION DEL MODELO ###############################
##########################################################################################
#EVALUACION DE CALIDAD DEL MODELO: MATRIZ DE CONFUSION:

#5.1 - CREAMOS EL OBJETO DE PREDICCION SOBRE EL CUAL SE HARÁ LA MATRIZ DE CONFUSION
aprender_rpart_pred <- predict(tr_fit, dtest, type="class")
aprender_rpart_pred
#5.2 - MATRIZ DE CONFUSION
tabla_mc_aprender <-confusionMatrix(table(dtest$sector, aprender_rpart_pred, 
      dnn = c("Actual", "Predicho")))

tabla_mc_aprender

#5.3 - DIAGRAMA ROC: MIDE EL RENDIMIENTO: NOS DICE QUE TAN BIEN PUEDE CLASIFICAR EL MODELO ENTRES 2 CLASES
#5.3 - SE GENERA EL INDICE DE PROBABILIDAD EN LUGAR DE LAS CANTIDADES DE CLASIFICACIONES
#aprender_rpart_pred2 <- predict(tr_fit, dtest, type = "prob")
#aprender_rpart_pred2
#5.3.1 - CON QUE PROB CLASIFICA?
#head(aprender_rpart_pred2)

#aprender_rpart_pred2_roc <- prediction(aprender_rpart_pred2[,2], dtest[, "sector"])
#aprender_rpart_pred2_roc_perf <- performance(aprender_rpart_pred2_roc, "tpr", "fpr")
#plot(aprender_rpart_pred2_roc_perf)

aprender_rpart_pred2_roc
aprender_rpart_pred2_roc_perf
