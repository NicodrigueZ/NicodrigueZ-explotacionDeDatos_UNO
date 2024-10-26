##########################################################################################
################################## PAQUETES NECESARIOS ###################################
##########################################################################################
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
##########################################################################################
############################### 01 - IMPORTAR LOS DATOS ##################################
##########################################################################################
emse <- read.csv("./UNO_EDD/TP4_Arboles_Decision/emse_datosabiertos/EMSE_DatosAbiertos.csv")
str(emse)
View(emse)
names(emse)

emse_red <- emse[,(4:144)]
names(emse_red)
rm(emse_red)
emse_red = subset(emse_red, select = c(texto_q1,
                                       texto_q2,
                                       texto_q3,
                                       texto_q6,
                                       texto_q15,
                                       texto_q16,
                                       texto_q17,
                                       texto_q18,
                                       texto_q19,
                                       texto_q22,
                                       texto_q23,
                                       texto_q24,
                                       texto_q25,
                                       texto_q26,
                                       texto_q27,
                                       texto_q28,
                                       texto_q29,
                                       texto_q31,
                                       texto_q34,
                                       texto_q35,
                                       texto_q38,
                                       texto_q39,
                                       texto_q40,
                                       texto_q41,
                                       texto_q42,
                                       texto_q43,
                                       texto_q44,
                                       texto_q45,
                                       texto_q46,
                                       texto_q49,
                                       texto_q51,
                                       texto_q53,
                                       texto_q55,
                                       texto_q56,
                                       texto_q57,
                                       texto_q59,
                                       texto_q60,
                                       texto_q61,
                                       texto_q62,
                                       texto_q65,
                                       texto_q66,
                                       texto_q68,
                                       texto_q73,
                                       texto_q74,
                                       texto_q76,
                                       texto_q79
))
##########################################################################################
################################# 02 - ETL DEL MODELO ####################################
##########################################################################################
#borrar: 
#2.1.1 - RENOMBRAR COLUMNAS
names(emse_red) <- c("edad",
                     "sexo",
                     "grado",
                     "dias_con_hambre",
                     "ult_12meses_atac_fisi",
                     "ult_12meses_part_pelea",
                     "lesion_grave",
                     "lesion_mas_seria",
                     "causa_lesion_seria",
                     "sentirse_solo",
                     "preocupacion",
                     "consideraste_suicidio", #pred
                     "plan_de_suicidio", #pred
                     "veces_intentaste_suici",
                     "amigos_cercanos",
                     "edad_fumador",
                     "ult_30dias_fumaste",
                     "dejaste_fumar",
                     "edad_alcohol",
                     "dias_alcohol",
                     "veces_tomaste_acohol",
                     "problem_entorno_alcohol",
                     "edad_drogas",
                     "veces_maria",
                     "ult_30dias_consu_maria",
                     "veces_consu_metanfet",
                     "tuviste_sexo",
                     "edad_sexo_1ra_vez",
                     "cuantas_perso_tuv_sex",
                     "ult_7dias_activ_fisica",
                     "vas_a_edu_fisica",
                     "ult_30dias_faltaste",
                     "padres_verfic_tarea",
                     "ult_30dias_padres_atend_salud",
                     "ult_30dias_padres_sabian_activi",
                     "educacion_padre",
                     "educacion_madre",
                     "ult_7dias_frutas",
                     "ult_7dias_verd",
                     "comida_grasa",
                     "intimidacion_en_esc",
                     "intimidacion_en_int",
                     "que_bebida_tomas",
                     "con_quien_tomas",
                     "si_te_ofrecen_tomas",
                     "quedaste_embarazada"
)

#2.1.2 CORRECCION DE CARACTERES ESPECIALES
#CAMBIO DE VALORES POR CONFLICTO EN APLICACION DE FUNCIONES CON LOS MISMOS
names(emse_red)
str(emse_red)
library(stringi)
attach(emse_red)
emse_red$veces_intentaste_suici
emse_red %>% distinct(veces_intentaste_suici)

emse_red$veces_intentaste_suici <- stri_replace_all_regex(emse_red$veces_intentaste_suici,
                                                      pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
                                                      replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
                                                      vectorize=FALSE)

emse_red$causa_lesion_seria <- stri_replace_all_regex(emse_red$causa_lesion_seria,
                                                      pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
                                                      replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
                                                      vectorize=FALSE)

emse_red$causa_lesion_seria <- stri_replace_all_regex(emse_red$causa_lesion_seria,
                                                    pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
                                                    replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
                                                    vectorize=FALSE)

emse_red$lesion_mas_seria <- stri_replace_all_regex(emse_red$lesion_mas_seria,
                                                pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
                                                replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
                                                vectorize=FALSE)

emse_red$lesion_grave <- stri_replace_all_regex(emse_red$lesion_grave,
                                        pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
                                        replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
                                        vectorize=FALSE)
##
emse_red$edad <- stri_replace_all_regex(emse_red$edad,
                                       pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
                                       replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
                                       vectorize=FALSE)

emse_red$grado <- stri_replace_all_regex(emse_red$grado,
                                        pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
                                        replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
                                        vectorize=FALSE)

emse_red$ult_12meses_part_pelea <- stri_replace_all_regex(emse_red$ult_12meses_part_pelea,
                                                         pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
                                                         replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
                                                         vectorize=FALSE)

emse_red$ult_12meses_atac_fisi <- stri_replace_all_regex(emse_red$ult_12meses_atac_fisi,
                                         pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
                                         replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
                                         vectorize=FALSE)

#2.1.3 - TRATA DE VALORES NAs
View(emse_red)
str(emse_red)

emse_red[emse_red == ""] <- NA
emse_red[emse_red == "Dato perdido"] <- NA

#CONTAR NAs
View(summarise_all(emse_red, funs(sum(is.na(.)))))

#ELIMINAR TODOS LOS NAs
emse_red <- na.omit(emse_red)

View(summarise_all(emse_red, funs(sum(is.na(.)))))

emse_red <- data.frame(lapply(emse_red, as.factor))
str(emse_red)
##########################################################################################
############################ 03 - DIVIDIMOS EN TRAINEE Y TEST #############################
##########################################################################################
#ACLARACION: EN RF, NO NECESARIAMENTE DIVIDIR LOS DATOS PARA MODELO, YA QUE EL PROPIO MODELO
#YA TIENE INCORPORADO EN EL ALGORITMO LA PARTICION.
set.seed(2018)
#plan_de_suicidio
#consideraste_suicidio

split_train_test <- createDataPartition(emse_red$plan_de_suicidio, p=0.7, list=FALSE) #p = porcentaje de los datos a usar, list = Si el resultado devolvera una lista o una matriz. 
dtrain <- emse_red[split_train_test,]
dtest <- emse_red[-split_train_test,]

sqldf('
SELECT plan_de_suicidio, count(*)
FROM dtrain
GROUP BY plan_de_suicidio
ORDER BY 2 DESC
')
#     plan_de_suicidio cantidad
#1               No    18787
#2               Si     3519
##########################################################################################
############################### 04 - CONSTRUCCION DEL MODELO #############################
##########################################################################################
names(emse_red)
emse_red_rf <- randomForest(x = dtrain[,-13], #VARIABLES INDEPENDIENTES
                            y = dtrain$plan_de_suicidio, #VARIABLE DEPENDIENTE
                            ntree = 500, #CANTIDAD DE ARBOLES. POR DEFECTO SON 500
                            keep.forest = TRUE) #TRUE = ME DEJA LOS ARBOLES INTERMEDIOS. OJO QUE ES COSTOSO, Y EN LA REALIDAD NO ES RECOMENDABLE

#KEEP.FOREST: POR DEFECTO, EL MODELO NO SE QUEDA CON LOS ARBOLES QUE FUE UTILIZANDO PARA CONSTRUIR EL MODELO.
#POR ENDE DICHOS ARBOLES NO SE USAN PARA PREDECIR SOBRE LOS CUALES SE VA CLASIFICAR. SIN EMBARGO, SE PUEDE FORZAR.

##########################################################################################
############################### MATRIZ DE CONFUSION ##################################
##########################################################################################
#NOTA: PREDICT CATALOGA/CLASIFICA; PREDICTION HACE UNA PREDICION PORCENTUAL
#4.1 - SE PREDICE LA BONDAD DEL MODELO: SE PREDICE EN BASE AL MODELO ACTUAL, LA CLASIFICACION DE LOS DATOS QUE NO INGRESARON AL MODELO
emse_red_rf_pred <- predict(emse_red_rf, dtest, type = "class")

#4.2 - MATRIZ DE CONFUSION: 
emse_red_rf_pred_mc <-confusionMatrix(table(dtest$plan_de_suicidio, emse_red_rf_pred, 
                                          dnn = c("Actual", "Predicho")))

emse_red_rf_pred_mc

#CONCLUSION 1:  Balanced Accuracy : 0.8404  NOS INDICA QUE EL MODELO PRESENTA UN BALANCEO DE CLASES
#EN DONDE LA CLASE A PRESENTA MAYOR CANTIDAD DE CASOS QUE LA OTRA CLASE.

#4.3 - PREDICR DE PROBABILIDADES: SE PREDICE CON QUE PROBABILIDAD EL MODELO PUEDE CLASIFICAR SOBRE LOS
#DATOS DE TEST
#banknote_probs <- predict(banknote_rf, banknote[-training.ids,], type = "prob")
#head(banknote_probs)

#4.4 - ROC: SOBRE LOS EXITOS, 
#banknote_probs_pred <- prediction(banknote_probs[,2], banknote[-training.ids,"class"])
#banknote_probs_pred_perf <- performance(banknote_probs_pred, "tpr", "fpr") #TPR: TRUE POSITIVE RATE; FALSE POSITVE RATE 
#plot(banknote_probs_pred_perf)

#CONCLUSION: RF ES COSTOSO CONPUTANCIONALMENTE, PERO CLASIFICA MUY BIEN.



