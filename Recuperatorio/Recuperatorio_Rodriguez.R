##########################################################################################
#################################### BIBLIOTECAS #########################################
##########################################################################################

#TP2 RLM
library(xtable)
library(corr)
library(readr)
library(dplyr)
library(sqldf)
library(stringi)
library(plotly)
library(tidyverse)
library(TTR)
library(fpp3)
library(scales)
library(xts)
library(plotly)
library(nortest)
library(lmtest)
library (car)
#library(fmsb)
library(broom)
#TP3 ACP-CLUSTERING
library(readr)
library(dplyr)
library(psych)
library(tidyverse)
library(sqldf)
library(xtable)
library(factoextra)
library(broom)
library(factoextra)
#TP4 ARBOLES DE DECISION
install.packages("ROCR")
library(TTR)
library(tsibble)
library(readr)
library(sqldf)
library(tidyverse)
library(stringi)
library(stringr)
library(ROCR)
library(plyr)  
library(caret)
library(gridExtra) 
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(rpart)
library(pROC)
library(MASS)
library(e1071)
library(ggpubr)
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(ROCR)
library(gplots)
library(randomForest)
library(rpart.plot)
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
library(corrplot)
library(randomForest)
library(ggpubr)
library(plotly)

#--------------------------------------------------------------------------------------------------------------
rm(list = ls())
options(scipen = 6) # para evitar notacion cientifica.


#CARGAMOS LOS DATOS
rm(list = ls())
getwd()
setwd("C:/Users/Facundo/Desktop/UNO/Explotacion de Datos/Parcial - Recuperatorio/Recuperatorio/Datos")

data_fb <- read_delim("data.fb.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)

str(data_fb)
View(data_fb)

# VERIFICAMOS SI HAY VALORES NULOS 

sapply(data_fb, function(x) sum(is.na(x))) #VER SI HAY VALORES NULOS

#CASTEAMOS LOS DATOS POR LAS DUDAS
data_fb$x1 <- suppressWarnings(as.numeric(data_fb$x1))
data_fb$x2 <- suppressWarnings(as.numeric(data_fb$x2))
data_fb$x3 <- suppressWarnings(as.numeric(data_fb$x3))
data_fb$x4 <- suppressWarnings(as.numeric(data_fb$x4))
data_fb$x5 <- suppressWarnings(as.numeric(data_fb$x5))
data_fb$x6 <- suppressWarnings(as.numeric(data_fb$x6))
data_fb$x7 <- suppressWarnings(as.numeric(data_fb$x7))
data_fb$x8 <- suppressWarnings(as.numeric(data_fb$x8))
data_fb$y <- suppressWarnings(as.numeric(data_fb$y))

attach(data_fb)
nrow(data_fb) #490

# COMO SE PUEDE OBSERVAR NO HAY VALORES NULOS EN NINGUNA DE LAS VARIABLES
#--------------------------------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------------------------
# SUMMARY O RESUMEN DE LOS DATOS

summary(data_fb)

# PODEMOS NOTAR QUE ENTRE LOS QUARTILES DE LAS VARIABLES HAY UNA AMPLIA DIFERENCIA 
# ENTRE EL VALOR DE LAS OBSERVACIONES, DONDE EN ALGUNOS CASOS EL VALOR MINIMO MANEJA UNIDADES
# PARA LUEGO PASAR AL PRIMER CUARTIL CON OBSERVACIONES QUE SU VALOR PASAN LOS 3 DIGITOS
# ADEMAS ACLARAR QUE EN MISMO DATASET MANEJAMOS TOTALES SEGUN EL DICCIONARIO DE DATOS
# Y ENTRE ESTAS VARIABLES SE OBSERVA UN VALOR NULO QUE NO TIENE SENTIDO SU EXISTENCIA PARA ESTE CASO
# EN LA VARIABLE X3 DONDE SU VALOR MINIMO ES -17.

# ELIMINAMOS ESE VALOR 

data_fb <- data_fb[-77,]
nrow(data_fb) #489

#--------------------------------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------------------------

# REALIZAMOS EL BOXPLOT DE TODAS LAS VARIABLES PARA DETECTAR POSIBLES OUTLIERS

data_fb_bp <- plot_ly(y=data_fb$x1, type="box", name="x1") %>%
  add_trace(y=data_fb$x2, type="box", name="x2") %>%
  add_trace(y=data_fb$x3, type="box", name="x3") %>%
  add_trace(y=data_fb$x4, type="box", name="x4") %>%
  add_trace(y=data_fb$x5, type="box", name="x5") %>%
  add_trace(y=data_fb$x6, type="box", name="x6") %>%
  add_trace(y=data_fb$x7, type="box", name="x7") %>%
  add_trace(y=data_fb$x8, type="box", name="x8") %>%
  add_trace(y=data_fb$y, type="box", name="y")

data_fb_bp

# PODEMOS VISUALIZAR QUE HAY UNA GRAN CANTIDAD DE VALORES ATIPICOS U OUTLIERS PARA LA VARIABLE 
# X1 Y X6 PERO MAS QUE NADA SE PUEDE NOTAR EN LA VARIABLE X2 CON VALORES QUE LLEGAN HASTA LAS 500K
# DE VALOR EN LAS OBSERVACIONES. SE PUEDE DEDUCIR QUE ES UN OUTLIERS PARA ESA VARIABLE YA QUE VEMOS 
# QUE EL VALOR MAXIMO PARA LOS BIGOTES DEL BOXPLOT ES DE 42K. CON RESPECTO A LA VARIABLE X1 VEMOS 
# DISTRIBUCION SECUENCIAL DE LOS OUTLIERS Y EN CUANTO A LA VARIABLE X6 PASANDO LOS BIGOTES DE LA CAJA
# VEMOS UNA DISTRIBUCION SECUENCIAL DE LOS MISMO HASTA QUE PODEMOS VISUALIZAR 2 PUNTOS ALEJADOS
# AL RESTO. CON RESPECTO A LAS DEMAS VARIABLES VEMOS QUE SI BIEN MANEJAN VALORES NULOS NO SON A NIVELES
# COMO X1 Y X2

# EL ANALISIS SE REALIZARA TENIENDO EN CUENTA ESTOS OUTLIERS SI BIEN SE PUEDE INDAGAR MAS EN EL ANALISIS
# TRATANDO LOS MISMOS Y VER LOS RESULTADOS DE LA MATRIZ DE CONFUSION YA QUE LOS ARBOLES SON MUY SENSIBLES
# A LA HORA DE MANEJAR OUTLEIRS PUEDE VARIAR NUESTRO RESULTADO

#--------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------
#DISCRETIZAMOS LA VARIABLE "Y" TENIENDO EN CUENTA LOS SIGUIENTES VALORES 
#1st Qu. = PRIMER CUARTIL
#Mean = SEGUNDO CUARTIL
#3rd Qu. = TERCER CUARTIL

summary(data_fb[,9])

#1st Qu.:  71.0  
#Mean   : 183.2  
#3rd Qu.: 222.0 

data_fb$y_disc <- 1 # Agrego columna que contendra la cantidad de habitantes discretizada, por el momento el valor sera de 1.

data_fb <- data_fb %>%  
  mutate(y_disc  = case_when(.$y <= 71 ~ "bajo",
                                   .$y <= 183 ~ "medio",
                                   .$y <= 222 ~ "alto",
                                   TRUE ~ "altisimo"
  ))

# LO QUE REALIZAMOS AQUI ES UNA DISCRETIZACION DE LA VARIABLE Y EN LA CUAL EN BASE A LOS DATOS DE LOS
# CUARTILES DEL SUMMARY DE LA VARIABLE Y DISCRETIZAMOS LOS VALORES BASANDONOS EN EL CONTEXTO DEL DATA SET
# EL CUAL ES UNO DE INTERACCIONES DE FACEBOOK SIENDO LA VARIABLE PREDICTORA LA REPRESENTANTE DE LAS INTERACCIONES TOTALES


#--------------------------------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------------------------
# PROCEDEMOS A HACER UNA PARTICION EN VARIABLE DE ENTRENAMIENTO Y DE TESTEO DEL DATA SET
# CON UNA DIVISION DEL 70% PARA LA VARIABLE DE ENTRENAMIENTO Y UN 30% PARA LA VARIABLE DE 
# TESTEO.

#GENERAMOS UNA SEMILLA ALEATORIA

set.seed(2019)

#GENERAMOS LA PARTICION DEL 70% DEL DATA SET YA CON NUESTRA VARIABLE DISCRETIZADA 

data_fb_partition <- createDataPartition(data_fb$y, p=0.8, list=FALSE) 

data_fb_partition2 <- createDataPartition(data_fb$y_disc, p=0.8, list=FALSE) 


#SEPARAMOS EN LAS VARIABLES DE TESTEO Y ENTRENAMIENTO.

dtrain <- data_fb[data_fb_partition,]
dtest <- data_fb[-data_fb_partition,]

dtrain <- dtrain[,-9]
dtest <- dtest[,-9]


view(dtrain)
view(dtest)
#--------------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------------
# PROCEDEMOS A REALIZAR APLICAR EL USO DE ARBOLES SOBRE NUESTRA VARIABLE DE ENTRENAMIENTO 
# UTILIZANDO EL METODO DE RANDOM FOREST


# data_fb_rf <- randomForest(x = dtrain, #VARIABLES INDEPENDIENTES
#                            y = dtrain$y, #VARIABLE DEPENDIENTE
#                            ntree = 500, #CANTIDAD DE ARBOLES. POR DEFECTO SON 500
#                            keep.forest = TRUE) #TRUE = ME DEJA LOS ARBOLES INTERMEDIOS. OJO QUE ES COSTOSO, Y EN LA REALIDAD NO ES RECOMENDABLE

# data_fb_rf2 <- randomForest(x = dtrain2, #VARIABLES INDEPENDIENTES
#                           y = dtrain2$y_disc, #VARIABLE DEPENDIENTE
#                           ntree = 500, #CANTIDAD DE ARBOLES. POR DEFECTO SON 500
#                           keep.forest = TRUE) #TRUE = ME DEJA LOS ARBOLES INTERMEDIOS. OJO QUE ES COSTOSO, Y EN LA REALIDAD NO ES RECOMENDABLE



tr_fit <- rpart(y_disc ~., data = dtrain, method="class") # Indicamos que deseamos un arbol de clasificacion, tambien podemos armar un arbol de regresion.

rpart.plot(tr_fit)

prediccion <- predict(tr_fit, newdata=dtest, type="class")


confusionMatrix(table(dtest$y_disc, prediccion, 
                      dnn = c("Actual", "Predicho")))

#PODEMOS VER QUE NUESTRA MATRIZ DE CONFUSION DEL MODELO NOS DA UN TOTAL DE 96 CASOS EN TOTAL PREDICHOS
#CONTRA EL VALOR ACTUAL. LA TAZA DE EXACTITUD DE NUESTRO MODELO(ACCURACY) ES DE UN 0.6042 O LO QUE ES UN
#%60 DE CASOS PREDICHOS CORRECTOS SOBRE EL TOTAL DE TODA LA MUESTRA. NUESTOR INDICE DE FIALDAD DICHO EN SIMPLES
#PALABRAS (KAPPA) ES DE UN 0.4029 O LO QUE ES UN 40%. LA SENSIBLIDAD DE NUESTRO MODELO PARA CADA CLASE
#ES DE UN APROXIMADO DEL 60% EN CADA UNA SALVO POR LA CLASE "ALTO" QUE ES N/A, SE PODRIA DECIR QUE EL
#60% DE CASOS POSITIVOS FUERON AGRUPADOS CORRECTAMENTE. LA ESPECIFICIDAD DE NUESTRO A DIFERENCIA DE LA 
#SENSIBILIDAD ES MAYOR PARA CADA UNA DE LAS CLASES SIENDO PARA LA CLASE "ALTO" EL MAS ALTO CON UN 09375 Y
#EL MAS BAJO PARA LA CLASE "MEDIO" CON UN 0.72.
