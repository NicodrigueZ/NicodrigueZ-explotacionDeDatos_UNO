##########################################################################################
#################################### BIBLIOTECAS #########################################
##########################################################################################

library(xtable)
library(corrplot)
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
library(fmsb)
library(broom)
library(readr)
library(dplyr)
library(psych)
library(tidyverse)
library(sqldf)
library(xtable)
library(factoextra)
library(readr)
library(dplyr)
library(psych)
library(tidyverse)
library(sqldf)
library(xtable)
library(factoextra)
library(philentropy)
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
library(stringi)
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
library(dplyr)

################################################################################################
######################################### PRIMER PUNTO #########################################
################################################################################################
rm(list = ls())
getwd()
#setwd("C:/Users/Facundo/Desktop/UNO/Explotacion de Datos/Parcial - Recuperatorio/Parcial/Datos")

data_fb <- read_delim("data.fb.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(data_fb)

str(data_fb)

#VALORES NULOS

sapply(data_fb, function(x) sum(is.na(x)))

data_fb<-na.omit(data_fb)

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

#RESUMEN DE DATOS

summary(data_fb)

#ELIMINAMOS LA OBSERVACION -17 PORQUE LA MISMA NO TIENE SENTIDO EN EL CONTEXTO QUE MANEJAMOS
#CLARAMENTE SE TRATA DE UN VALOR ANOMALO
data_fb <- data_fb[x3 != -17,]

nrow(data_fb)
view(data_fb)

##########################################################################################
#################################### REGRESION LINEAL ####################################
##########################################################################################


#DATASET == REEMPLAZAR POR SET DE DATOS A TRABAJAR
#VARIABLE / VARIABLEN == REEMPLAZAR POR LA VARIABLE O VARIABLES A TRABAJAR

################ COSAS A TENER EN CUENTA PARA EL ANALISIS ######################


#-Linealidad = Dispersion sin patron alguno de residuos, linealidad en los 
#4 plots del modelo

#-E(e)= 0 (calidad de un modelo) =  boxplot con mediana 0 (de residuos)

#-Var(e)=o^2 In   =  observa el gráfico de residuos vs. valores predichos. Si los puntos 
#se distribuyen aleatoriamente alrededor de la línea de referencia (por lo general, 
#la línea horizontal en y = 0), se sugiere que se cumple la suposición de 
#homogeneidad de varianza.

#-Los errores no están correlacionados  =  Se puede utilizar el test de normalidad de Durbin-Watson, para evaluar la autocorrelación en los residuales. El resultado de esta prueba puede proporcionar información sobre si los errores están correlacionados o no. P valor cercano a 1 y valor dw cercano a 2

#-El vector e se distribuye Normal (0, o^2 In)  =  Se puede utilizar un QQ plot para 
#evaluar si los residuales siguen una distribución normal.

#-Las variables regresoras no son colineales  =   Si el VIF de una variable es mayor que 10, se 
#considera que esa variable está relacionada con las demás.

################### PASOS PARA AJUSTAR UN RLM #############################


#• Ajusto el modelo con todas
#• Veo significatividad del modelo y de los coeficientes de cada variable.
#• Decido sacar variables si hace falta (con métodos automáticos o a mano)
#• Comparo los modelos que tengo disponibles (R2 ajustado, AIC, etc)
#• Verifico supuestos
#• VIFs para multicolinealidad, influyentes (Cooks)
#• Elijo mi modelo e interpreto coeficientes.
#• Hago predicción para nuevos datos

######################## RESUMEN DE LOS DATOS ##################################

summary(data_fb)

################################## BOXPLOT #####################################

data_fb_box <- plot_ly(y=data_fb$x1, type="box", name="Lifetime Post Total Reach") %>%
  add_trace(y=data_fb$x2, type="box", name="Lifetime Post Total Impressions") %>%
  add_trace(y=data_fb$x3, type="box", name="Lifetime Engaged Users") %>%
  add_trace(y=data_fb$x4, type="box", name="Lifetime Post Consumers") %>%
  add_trace(y=data_fb$x5, type="box", name="Lifetime Post Consumptions") %>%
  add_trace(y=data_fb$x6, type="box", name="Lifetime Post Impressions by people who have liked your Page") %>%
  add_trace(y=data_fb$x7, type="box", name="Lifetime Post reach by people who like your Page")%>%
  add_trace(y=data_fb$x8, type="box", name="Lifetime People who have liked your Page and engaged with your post")%>%
  add_trace(y=data_fb$y, type="box", name="Total Interactions")

data_fb_box

datafb_out <- boxplot(data_fb$x6, col="skyblue", frame.plot=F)
datafb_out$out

data_fb <- data_fb[!(data_fb$x2 %in% c(497910,457509,453213)),] #ELIMINAR OUTLIERS ESPECIFICOS
data_fb <- data_fb[!(data_fb$x6 %in% c(160270,184270)),]

#ELIMINAMOS POSIBLES OUTLIERS CON EL CRITERIO DE QUE LOS MISMOS SI ESTAN MUY ALEJADOS DE LOS BIGOTES
#DE LA CAJA SE ELIMINAN. AL FINAL NOS QUEDAMOS DE MOMENTO CON 484 DE 490 OBSERVACIONES ELIMINANDO DICHOS VALORES
#CON EL CRITERIO APLICADO

########REALIZAMOS LA PARTICION EN SUBSETS DE ENTRENAMIENTO Y DE TESTEO##########


#SETEAMOS UNA SEMILLA RANDOM

set.seed(63) 

#REALIZAMOS LA PARTICION EN SUBSET DE ENTRENAMIENTO Y DE TESTEO

data_fb_particion <- createDataPartition(data_fb$y, p=0.7, list=FALSE)
datafb_train<-data_fb[data_fb_particion,]
datafb_test<-data_fb[-data_fb_particion,]

########################## CORRPLOT ##########################

dataTrain_corr <- cor(datafb_train) #UNICAMENTE CON VARIABLES NUMERICAS
dataTrain_corr
corrplot(dataTrain_corr, method="number",tl.col="black",tl.cex=0.5)

#PODEMOS NOTAR GRAN CORRELACION DE LA VARIABLE RESPUESTA CON LAS VARIABLES PREDICTORAS X1
#X2, X6, X7 Y UN POCO CON X3 LAS DEMAS VARIABLES PRESENTAN POCA CORRELACION A COMPARACION
#DE LAS ANTES MENCIONADAS. SE PUEDE OBSERVAR PEQUEÑOS CUADRANTES RELACIONADOS ENTRE ELLOS COMO X6 Y X7
#JUNTO CON X1 Y X2 AL IGUAL QUE X2 SE RELACIONA BASTANTE BIEN CON X1, ESTO PODRIA SER UN INDICIO DE 
#POSIBLE MULTICOLINEALIDAD. 

############################### RLM #################################

#VARIABLE Y == VARIABLE PREDICTORA

#-P-Value: Resultado de hacer un test a cada variable, incluyendo el intercepto, 
#para ver la valides del resultado de los datos.

#-H0(Hipotesis Nula): Afirmacion Contraria a la que llega el investigador. 
#Es la que se busca Rechazar.

#-H1(Hipotesis Alternativa): Afirmacion a lo que el investigador ha llegado.

#-Intercept + BETA(duracion) * BETA Anterior (duracion geyser anterior) = Proximo Geyser

#-R-Cuadrado: Porcentaje que explica la totalidad del problema 

#-F: Que tan significativo es mi modelo.

datatrain_rlm <- lm(y ~ ., data = datafb_train)

summary(datatrain_rlm)

View(datatrain_rlm)

#NOTAMOS SIGNIFICANCIA EN CUANTO A CONSTRASTE DE HIPOTESIS SOBRE EL MODELO CON UN P-VALUE
#QUE ES IGUAL O CASI IGUAL A UN 0. 

######################### GRAFICO RECTA DE RESIDUOS ############################


ggplot(datatrain_rlm, aes(x= x1 + x2 +x3 + x4 + x5 + x6 + x7 + x8, y=y))+ 
  geom_point() +
  geom_smooth(method='lm',se=FALSE, col='green') +
  theme_light()

#SE OBSERVA UNA CONCENTRACION AL PRINCIPIO DE LA RECTA PARA POSTERIORMENTE OBSERVAR UNA PEQUEÑA
#DISPERCION DE LAS OBSERVACIONES AL REDEDOR DE LA RECTA PERO MUY PEGADA A ELLA DICHA DISTRIBUCION


################## METODOS DE SELECCION DE VARIABLES AUTOMATICO ################

datatrain_backward <- step(datatrain_rlm, direction = "backward",trace=T)


#DATATRAIN RLM BACK
summary(datatrain_backward)

#DATATRAIN RLM
summary(datatrain_rlm)

#PODEMOS NOTAR QUE LOS 2 MODELOS NOTAN SIGNIFICANCIA TENIENDO EN CUENTA EL CONTRASTE DE HIPOTESIS NULA
#DEL P-VALUE SOBRE NUESTRO MODELO SIENDO CASI UN 0, EL METODO DE SELECCION AUTOMATICO DE VARIABLE 
#POSEE LA MISMA SIGNIFICANCIA TENIENDO UNICAMENTE EN CUENTA LA VARIABLE X8

########################### PLOT MODELOS ####################################

#Residuales: Diferencia entre el valor predicho y el obtenido.

#################### Residuals vs Leverage: ###################################
  
#Este grafico nos muestra valores atipicos, por lo que me da a entender posibles outliers quizas, 
#que pueden influenciar mucho en nuestro modelo. Los puntos mas alejados se podrian eliminar y rehacer 
#el Script para obtener mejores resultados si se desea. 

#Todo esto en base a la distancia de Cook, la distancia de Cook mide la influencia de los puntos 
#influyentes, si la distancia de Cook es muy alta para un punto quiere decir que es un punto muy 
#influyente a mi modelo y es un posible valor atipico que no se ajustan bien al modelo.  Algo a Agregar 
#es que la distancia de Cook debe estar entre 2 y -2.

########################### Scale-Location #####################################

#Al igual que el grafico de residuos se busca que los residuos, esta vez ajustadoso estandarizados, 
#esten dispersos a travez de todo el set de datos. Se Busca que la varianza sea constante en base
#a la distribucion de los residuos con el mismo criterio, la dispersion de los mismos.

############################# Q-Q Plot o Q-Q Normal: #########################
  
#Se comparan los Residuos con una Curva de Lineas Punteadas Normal para comparar que tan Normal 
#son los Residuos. Los Residuos se estandarizan para hacer la comparativa con otras curvas normales. 
#Si los Residuos estan pegados a la curva quiere decir que esta es normal, caso contrario, esa curva no 
#es normal. 

############################ Residuals vs Fitted ##############################

#Hipotesis del modelos Residuos: Los Residuos deben de estar distribuidos de forma 
#alteatoria a travez del todo el set de datos. 

plot(datatrain_rlm)
plot(datatrain_backward)

par(mfrow=c(2,2)) # VER LOS 4 GRAFICOS JUNTOS 


######################### ANALISIS DE LOS SUPUESTOS ############################

datatrain_rlm_residuos = residuals(datatrain_rlm)
datatrainback_rlm_residuos = residuals(datatrain_backward)

boxplot(datatrain_rlm_residuos, col = "blue",horizontal=TRUE,ylim = c(-2,2),main="Box-plot de residuos")
plot_datasetTrain_residuos <- plot_ly(y = ~datatrain_rlm_residuos, type = "box")
plot_datasetTrain_residuos 

datafb_res_out <- boxplot(datatrain_rlm_residuos, col="skyblue", frame.plot=F)


datatrain_backward = residuals(datatrainback_rlm_residuos)

boxplot(datatrain_backward, col = "blue",horizontal=TRUE,ylim = c(-2,2),main="Box-plot de residuos")
plot_datatrain_backward_residuos <- plot_ly(y = ~datatrainback_rlm_residuos, type = "box")
plot_datatrain_backward_residuos 

#EN AMBOS MODELOS SE PODRIA DECIR QUE TENEMOS UNA DISTRIBUCION DE RESIDUOS NORMAL, SIN EMBARGO
#SE PUEDE OBSERVAR EN AMBOS MODELOS QUE HAY UN RESIDUO QUE SOBREPASA EL LIMITE DE LOS BIGOTES DE LA CAJA
#DE BOXPLOT POR BASTANTE DIFERENCIA EN COMPARACION A LOS OTROS, EL BOXPLOT DENTRO DE TODO SE MANTIENE
#BASTANTE BIEN CON UNA MEDIA CERCANA A 0.

########################## TESTE DE SHAPIRO-WILK ##############################

#Test de normalidad de Shapiro-Wilk (muestras chicas)
#Cuanto más cercano esté W a 1, más probable es que los datos se ajusten a una distribución normal.
#Si el p-valor es mayor que el nivel de significancia (ejemplo 0.05), se asume que los datos siguen una 
#distribución normal.
modeltest <- datatrain_rlm

datatrain_rlm_residuos.test <- shapiro.test(datatrain_rlm_residuos)
print(datatrain_rlm_residuos.test)

#SEGUN LA PREMISA DE ESTE TEST MIENTRAS MAS CERCANO NUESTRO "W" SEA A 1 Y SI NUESTRO CONTRASTE DE 
#HIPOTESIS SEA MAYOR A 0.05 NUESTROS DATOS SE AJUSTAN BASTANTE BIEN A UNA DISTRIBUCION NORMAL.
#SIN EMBARGO NUESTROS DATOS NO CUMPLEN NINGUNO DE ESTOS 2 REQUISITOS POR ENDE SE PODRIA DECIR QUE NUESTROS
#NO SE AJUSTAN A UNA DISTRIBUCION NORMAL. 

################################# TEST DE DURBIN-WATSON #######################

#Se puede utilizar el test de normalidad de Durbin-Watson, para evaluar la 
#autocorrelación en los residuales. El resultado de esta prueba puede 
#proporcionar información sobre si los errores están correlacionados o no.

#Un valor de Durbin-Watson cercano a 2 y un p-value cercano a 1 respaldan 
#la independencia de los errores. Un p-value muy pequeño sugiere que los 
#errores están correlacionados, lo que indica una violación de la suposición 
#de independencia en el modelo. La hipótesis nula del test es que no hay 
#autocorrelación en los errores.

dwtest(modeltest)

#PODEMOS OBSERVAR QUE NUESTRO DW CUMPLE CON LA CONDICION DE SER CERCANO A 2 SIENDO PRACTICAMENTE IGUAL A 
#DICHO VALOR Y UN P-VALOR CERCANO A 1 LO QUE PODRIA RESPALDAR QUE LOS ERRORES NO ESTA CORRELACIONADOS 
#DEMOSTRANDO ASI LA INDEPENDENCIA DE LOS RESIDUOS. 

################################# ANALISIS DE MULTICOLINEALIDAD #######################

# 6.1 Analizamos multicolinealidad e influyentes:
# VIF: Si es mayor a 10, entonces hay correlacion entra variables
# VIF = 1: NO HAY MULTICOLINEALIDAD
# VIF > 1: MULTICOLINEALIDAD ACEPTABLE
# VIF > 5: MULTICOLINEALIDAD ALTA

vif(datatrain_rlm)
dataTrain_corr

#POR ULTIMO PODEMOS EL TEST DEL VIF PODEMOS NOTAR QUE GRAN PARTE DE LAS VARIABLES NO CUMPLEN LA CONDICION
#DE UN VIF BAJO PARA CONSIDERAR UNA MULTICOLINEALIDAD ACEPTABLE, PODEMOS OBSERVAR QUE LA VARIABLE X5
#ES LA UNICA QUE POSEE DICHA CARACTERISTICA DE NO TENER UNA ALTA MULTICOLINEALIDAD. AGREGANDO TAMBIEN
#QUE SI UNO OBSERVA LA MATRIZ DE CORRELACION LA MISMA ES LA QUE MENOS CORRELACIONADA ESTA CON LA VARIABLE
#DEPENDIENTE Y CON LAS DEMAS VARIABLES. 



#################################################################################################
######################################### SEGUNDO PUNTO #########################################
#################################################################################################

datafb_subset = subset(data_fb, x1=5000,x2=7390,x3=103,x4=101,x5=161,x6=3001,x7=2010,x8=254)

predict(datatrain_rlm,datafb_subset)







