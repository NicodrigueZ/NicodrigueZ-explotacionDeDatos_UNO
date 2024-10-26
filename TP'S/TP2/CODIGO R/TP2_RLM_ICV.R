# Materia: Explotacion de Datos
# Fecha: 09/09/2023
# Tema: Regresion Lineal Multiple
# Objetivo: Modelo didactico de RLM
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

##########################################################################################
############################### 01. IMPORTACION DE DATOS ################################# 
##########################################################################################
rm(icv_corr)
icv <- read.table(file.choose(),
                        sep=',',
                        header=TRUE,
                        fill = TRUE,
                        row.names = NULL,
                        stringsAsFactors = FALSE)

dim_entorno <- read.table(file.choose(),
                  sep=',',
                  header=TRUE,
                  fill = TRUE,
                  row.names = NULL,
                  stringsAsFactors = FALSE)


dim_edu <- read.table(file.choose(),
                  sep=',',
                  header=TRUE,
                  fill = TRUE,
                  row.names = NULL,
                  stringsAsFactors = FALSE)


dim_salud <- read.table(file.choose(),
                  sep=',',
                  header=TRUE,
                  fill = TRUE,
                  row.names = NULL,
                  stringsAsFactors = FALSE)


dim_vivienda <- read.table(file.choose(),
                  sep=',',
                  header=TRUE,
                  fill = TRUE,
                  row.names = NULL,
                  stringsAsFactors = FALSE)

View(icv)
str(icv)
names(icv)

attach(icv)
attach(dim_vivienda)
attach(dim_salud)
attach(dim_entorno)
attach(dim_edu)

##########################################################################################
######################################### 02. ETL ######################################## 
##########################################################################################
#-----------------------------------------------------------------------------------------
#2.1 REDUCCION DE DIMENSIONALIDAD DE LOS DATOS:

#ICV
View(icv)
str(icv)
rm(icv_red)
icv_red = subset(icv, select = c(sup_ha, pobl_tot, dens_ha, icv))
View(icv_red)
str(icv_red)

#SALUD
View(dim_salud)
str(dim_salud)
rm(dim_salud_red)
dim_salud_red = subset(dim_salud, select = c(ind_efe_sa, ind_ssb, ind_rsu))
View(dim_salud_red)
str(dim_salud_red)

#EDUCACION
View(dim_edu)
str(dim_edu)
rm(dim_edu_red)
dim_edu_red = subset(dim_edu, select = c(a_esp, a_prom))
View(dim_edu_red)
str(dim_edu_red)

#VIVIENDA
View(dim_vivienda)
str(dim_vivienda)
rm(dim_vivienda_red)
dim_vivienda_red = subset(dim_vivienda, select = c(ind_hac, ind_gr, ind_matv, ind_dom))
View(dim_vivienda_red)
str(dim_vivienda_red)

#ENTORNO
View(dim_entorno)
str(dim_entorno)
rm(dim_entorno_red)
dim_entorno_red = subset(dim_entorno, select = c(ind_trpu, ind_inunda, ind_evp, ind_cavas, ind_indus))
View(dim_entorno_red)
str(dim_entorno_red)

rm(icv)
rm(dim_edu)
rm(dim_entorno)
rm(dim_salud)
rm(dim_vivienda)

#-----------------------------------------------------------------------------------------
#2.2 UNIFICAMOS LOS DATASETS
dim_entorno %>% distinct(ind_inunda)

rm(icv_a_modelo)
str(icv_red)
icv_a_modelo <- cbind(icv_red, dim_edu_red, dim_salud_red, dim_entorno_red, dim_vivienda_red)
View(icv_a_modelo)
str(icv_a_modelo)
#---------------------------------------------------------------------------------------------
#2.3 RENOMBRAMOS LAS COLUMNAS
icv_a_modelo
View(icv_a_modelo)
rm(normalizacion_nombres)
normalizacion_nombres<-c("sup_ha","pobl_tot","dens_ha","icv","edu_escolar_ninos","edu_escolar_adul","salud_ate_prim",
                         "salud_serv_sanit","salud_disp_rsu","entor_acceso_transp","entor_inunda","entor_area_verde","entor_cavas",
                         "entor_indus","vivi_hacinamiento","vivi_gas_red","vivi_calid_casa","vivi_dominio")
names(icv_a_modelo)<-normalizacion_nombres
#---------------------------------------------------------------------------------------------
#2.4 LIMPIAMOS LOS DATOS:
#¿CUANTOS NA TENGO POR COLUMNA?
View(summarise_all(icv_a_modelo, funs(sum(is.na(.)))))
names(icv_a_modelo)
#CASTEAMOS LOS DATOS, YA QUE A SU VEZ ASIGNAMOS LOS VACIOS O NULOS COMO NAs
icv_a_modelo$sup_ha <- suppressWarnings(as.numeric(icv_a_modelo$sup_ha))
icv_a_modelo$pobl_tot <- suppressWarnings(as.numeric(icv_a_modelo$pobl_tot))
icv_a_modelo$dens_ha <- suppressWarnings(as.numeric(icv_a_modelo$dens_ha))
icv_a_modelo$icv <- suppressWarnings(as.numeric(icv_a_modelo$icv))
icv_a_modelo$edu_escolar_ninos <- suppressWarnings(as.numeric(icv_a_modelo$edu_escolar_ninos))
icv_a_modelo$edu_escolar_adul <- suppressWarnings(as.numeric(icv_a_modelo$edu_escolar_adul))
icv_a_modelo$salud_ate_prim <- suppressWarnings(as.numeric(icv_a_modelo$salud_ate_prim))
icv_a_modelo$salud_serv_sanit <- suppressWarnings(as.numeric(icv_a_modelo$salud_serv_sanit))
icv_a_modelo$salud_disp_rsu <- suppressWarnings(as.numeric(icv_a_modelo$salud_disp_rsu))
icv_a_modelo$entor_acceso_transp <- suppressWarnings(as.numeric(icv_a_modelo$entor_acceso_transp))
icv_a_modelo$entor_inunda <- suppressWarnings(as.numeric(icv_a_modelo$entor_inunda))
icv_a_modelo$entor_area_verde <- suppressWarnings(as.numeric(icv_a_modelo$entor_area_verde))
icv_a_modelo$entor_cavas <- suppressWarnings(as.numeric(icv_a_modelo$entor_cavas))
icv_a_modelo$entor_indus <- suppressWarnings(as.numeric(icv_a_modelo$entor_indus))
icv_a_modelo$vivi_hacinamiento <- suppressWarnings(as.numeric(icv_a_modelo$vivi_hacinamiento))
icv_a_modelo$vivi_gas_red <- suppressWarnings(as.numeric(icv_a_modelo$vivi_gas_red))
icv_a_modelo$vivi_calid_casa <- suppressWarnings(as.numeric(icv_a_modelo$vivi_calid_casa))
icv_a_modelo$vivi_dominio <- suppressWarnings(as.numeric(icv_a_modelo$vivi_dominio))

View(summarise_all(icv_a_modelo, funs(sum(is.na(.)))))
##########################################################################################
######################### 03. ANALISIS EXPLOTATORIO DE DATOS ############################# 
##########################################################################################
#QUE TIPOS DE ZONAS TENEMOS?
attach(icv_a_modelo)

#3.1 analizamos un poco los datos: descriptivos
str(icv_a_modelo)

#La funcion summary nos permite entender de manera descriptiva los datos:
#Podemos sacar conclusiones sobre: NAs, Outliers, Concenstracion de datos en Quartiles
summary(icv_a_modelo)
summary(icv_a_modelo$salud_serv_sanit)
#---------------------------------------------------------------------------------------------
#3.2 BOXPLOT:
icv_a_modelo_salud_serv_sanit <- plot_ly(y = ~icv_a_modelo$salud_serv_sanit, type = "box")
icv_a_modelo_salud_serv_sanit

icv_a_modelo_vivi_hacinamiento <- plot_ly(y = ~icv_a_modelo$vivi_hacinamiento, type = "box")
icv_a_modelo_vivi_hacinamiento

icv_a_modelo_vivi_gas_red <- plot_ly(y = ~icv_a_modelo$vivi_gas_red, type = "box")
icv_a_modelo_vivi_gas_red

icv_a_modelo_vivi_calid_casa <- plot_ly(y = ~icv_a_modelo$vivi_calid_casa, type = "box")
icv_a_modelo_vivi_calid_casa

icv_a_modelo_vivi_dominio <- plot_ly(y = ~icv_a_modelo$vivi_dominio, type = "box")
icv_a_modelo_vivi_dominio

rm(icv_outliers)
str(icv_a_modelo)
icv_outliers <- subset(icv_a_modelo, select = c(salud_serv_sanit, vivi_hacinamiento, vivi_gas_red, vivi_calid_casa, vivi_dominio))


#AGRUPAMOS TODOS LOS CAMPOS CON POSIBLES OUTLIERS PARA LATEX
xtable(summary(icv_a_modelo))
xtable(summary(icv_outliers))
#---------------------------------------------------------------------------------------------
#3.3 Vemos correlaciones y graficamos:
#El siguiente codigo muestra el indice de correlacion existente en nuestris atributos
#El mismo nos indica en una matriz la fuerza de correlacion positiva o negativa que se tiene.
#Tambien se utiliza dicha matriz para generar o ajustar un modelo de Regresion.

rm(icv_mod_full_corr)
icv_mod_full_corr <- cor(icv_a_modelo) #requiere corrplot. Las variables deben ser numéricas.
icv_mod_full_corr

#Visualizacion con indice de correlacion para cada atributo
corrplot(icv_mod_full_corr, method="number",tl.col="black",tl.cex=0.5)

corrplot(icv_mod_full_corr, type = "upper", 
         method = "square", 
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 10)

#Visualizacion con nivel de correlacion para cada atribut
corrplot(icv_mod_full_corr, method="color",tl.col="black",tl.cex=0.5)

#Visualizacion con grafico de dispersion por cada atributo
plot(icv_a_modelo)
#---------------------------------------------------------------------------------------------
##################################################################################
###################### 04 APLICACION DE MODELOS DE RLM ###########################
##################################################################################
#4.1 MODELO FULL DE RLM
str(icv_a_modelo)
names(icv_a_modelo)

icv_mod_full <- lm(icv ~ ., data = icv_a_modelo)
class(icv_mod_full)
str(icv_mod_full)
summary(icv_mod_full)

#SACAR CONCLUSION NUESTRA SOBRE EL:
#P-VALUE Y LOS CONTRASTES DE HIPOTESIS:
#R CUADRADO Y R CUADRADO AJUSTADO
#F ESTADISTICO

#ANALISIS DE SUMMARY
#1: FORMULA DE LA RECTA: ICV ~ RESTO
#2: Residuos: Es la diferencia entre el valor observado(dato real) y el predicho (datos de la recta de regresion)
#3: Los coeficientes determinar el incremento o decremento del valor de mi Y (MPG) por cada unidad de aumento de Y (MPG)
#4: El Pr(t) nos indica el nivel de significancia que tiene mi variable dependiente con mis variables independientes.
#5: P-Value: Nos indica el contraste de hipotesis bajo la distribucion normal: Si el p-value es menor a 0.05 entonces se rechaza la Ho (Hipotesis nula)
#5.1: Ho: Si el p-value para nuestra variable es menor a 0.05, entonces rechazamos la Ho
#5.2: Ho: Si el p-value es mayor que 0.05 entonces significa que no nuestro dato no es significante

#Ho: Digo que mi valor no es significativo. VOY A DESAPROBAR LA MATERIA
#H1: Digo que mi valor es significativo. VOY A APROBAR LA MATERIA

#Contraste teniendo en cuenta el indice convencional: 0.05
#Para rechazar el valor tiene que ser < 0.05

#Concluyo:
#Ho: NOTA tiene un p-value de 0.00002, ¿Puedo aceptar la Ho?:
#---------------------------------------------------------------------------------------------
#4.2 ANALISIS DE RESIDUOS SOBRE EL MODELO FULL
names(icv_a_modelo)
#GRAFICO DE RESIDUOS Y RECTA
ggplot(icv_mod_full, aes(x=sup_ha + pobl_tot + dens_ha + edu_escolar_ninos + edu_escolar_adul +
                           salud_ate_prim + salud_serv_sanit + salud_disp_rsu + entor_acceso_transp +
                           entor_inunda + entor_area_verde + entor_cavas + entor_indus + vivi_hacinamiento +
                           vivi_gas_red + vivi_calid_casa + vivi_dominio, y=icv))+ 
  geom_point() +
  geom_smooth(method='lm',se=FALSE, col='green') +
  theme_light()

#EXPLICAR LOS RESIDUOS EN EL MODELO FULL
##################################################################################
################ 05 AJUSTE Y APLICACION DEL NUEVO MODELO #########################
##################################################################################
#---------------------------------------------------------------------------------------------
#5.1 Métodos automáticos para selección de variables: "backward" / "forward" / "both"
#EXPLICAR CADA METODO
icv_mod_full_fordward <- step(icv_mod_full, direction = "forward",trace=T) 
icv_mod_full_backward <- step(icv_mod_full, direction = "backward",trace=T)
icv_mod_full_both <- step(icv_mod_full, direction = "both",trace=F)
summary(icv_mod_full_fordward)
summary(icv_mod_full_backward)
summary(icv_mod_full_both)

#PLOT DEL MODELO AJUSTADO
plot(icv_mod_full)
plot(icv_mod_full_backward)
par(mfrow=c(2,2))
plot(icv_mod_full_backward, scale = "adjr2", main = "R^2 ajustado")

#---------------------------------------------------------------------------------------------
#5.2 ANALISIS DE SUPUESTOS DE RESIDUOS: Analizamos supuestos estudiando residuos

#ANALISIS SOBRE EL MODELO BACKWARD:
icv_mod_full_backward_residuos = residuals(icv_mod_full_backward)

boxplot(icv_mod_full_backward_residuos, col = "blue",horizontal=TRUE,ylim = c(-2,2),main="Box-plot de residuos")
plot_icv_mod_full_backward_residuos <- plot_ly(y = ~icv_mod_full_backward_residuos, type = "box")
plot_icv_mod_full_backward_residuos
#---------------------------------------------------------------------------------------------
#5.3 CONTRUCCION DEL NUEVO MODELO CON BASE AL ANALISIS PREVIO
#5.3.1 DETECCION DE OUTLIERS CON EL DATASET ORIGINAL

#TABLA CON TODOS LOS CAMPOS CON POSIBLES OUTLIERS A TRATAR (ELIMINAR)
icv_outliers

icv_outliers_vivi_dominio_plotly <- plot_ly(y = ~icv_outliers$vivi_dominio, type = "box")
icv_outliers_vivi_dominio_plotly

rm(icv_outliers_vivi_dominio)
icv_outliers_vivi_dominio<-boxplot(icv_outliers$vivi_dominio, col="skyblue", frame.plot=F)
icv_outliers_vivi_dominio$out

icv_outliers_vivi_calid_casa<-boxplot(icv_outliers$vivi_calid_casa, col="skyblue", frame.plot=F)
icv_outliers_vivi_calid_casa$out

icv_outliers_vivi_gas_red<-boxplot(icv_outliers$vivi_gas_red, col="skyblue", frame.plot=F)
icv_outliers_vivi_gas_red$out

icv_outliers_vivi_hacinamiento<-boxplot(icv_outliers$vivi_hacinamiento, col="skyblue", frame.plot=F)
icv_outliers_vivi_hacinamiento$out

icv_outliers_salud_serv_sanit<-boxplot(icv_outliers$salud_serv_sanit, col="skyblue", frame.plot=F)
icv_outliers_salud_serv_sanit$out

par(mfrow=c(2, 3))
icv_outliers_vivi_dominio<-boxplot(icv_outliers$vivi_dominio, col="skyblue", frame.plot=F)
icv_outliers_vivi_calid_casa<-boxplot(icv_outliers$vivi_calid_casa, col="skyblue", frame.plot=F)
icv_outliers_vivi_gas_red<-boxplot(icv_outliers$vivi_gas_red, col="skyblue", frame.plot=F)
icv_outliers_vivi_hacinamiento<-boxplot(icv_outliers$vivi_hacinamiento, col="skyblue", frame.plot=F)
icv_outliers_salud_serv_sanit<-boxplot(icv_outliers$salud_serv_sanit, col="skyblue", frame.plot=F)


#5.3.2 ELIMINO LOS OUTLIERS
rm(icv_a_modelo_v2)
icv_a_modelo_v2 <- icv_a_modelo
View(icv_a_modelo_v2)
View(icv_outliers)
icv_a_modelo_v2 <- icv_a_modelo_v2[!(icv_a_modelo_v2$vivi_dominio %in% icv_outliers_vivi_dominio$out),]
icv_a_modelo_v2 <- icv_a_modelo_v2[!(icv_a_modelo_v2$vivi_calid_casa %in% icv_outliers_vivi_calid_casa$out),]
icv_a_modelo_v2 <- icv_a_modelo_v2[!(icv_a_modelo_v2$vivi_gas_red %in% icv_outliers_vivi_gas_red$out),]
icv_a_modelo_v2 <- icv_a_modelo_v2[!(icv_a_modelo_v2$vivi_hacinamiento %in% icv_outliers_vivi_hacinamiento$out),]
icv_a_modelo_v2 <- icv_a_modelo_v2[!(icv_a_modelo_v2$salud_serv_sanit %in% icv_outliers_salud_serv_sanit$out),]

#CHEQUEO
rm(icv_a_modelo_v2_vivi_hacinamiento_out)
icv_a_modelo_v2_vivi_dominio_out <- plot_ly(y = ~icv_a_modelo_v2$vivi_dominio, type = "box")
icv_a_modelo_v2_vivi_dominio_out

prueba_salud_serv_sanit_vivi_calid_casa_out <- plot_ly(y = ~icv_a_modelo_v2$vivi_calid_casa, type = "box")
prueba_salud_serv_sanit_vivi_calid_casa_out

icv_a_modelo_v2_vivi_gas_red_out <- plot_ly(y = ~icv_a_modelo_v2$vivi_gas_red, type = "box")
icv_a_modelo_v2_vivi_gas_red_out

icv_a_modelo_v2_vivi_hacinamiento_out <- plot_ly(y = ~icv_a_modelo_v2$vivi_hacinamiento, type = "box")
icv_a_modelo_v2_vivi_hacinamiento_out

icv_a_modelo_v2_salud_serv_sanit_out <- plot_ly(y = ~icv_a_modelo_v2$salud_serv_sanit, type = "box")
icv_a_modelo_v2_salud_serv_sanit_out

View(icv_a_modelo_v2)

par(mfrow=c(2, 3))
icv_a_modelo_v2_vivi_dominio_out<-boxplot(icv_a_modelo_v2$vivi_dominio, col="skyblue", frame.plot=F)
prueba_salud_serv_sanit_vivi_calid_casa_out<-boxplot(icv_a_modelo_v2$vivi_calid_casa, col="skyblue", frame.plot=F)
icv_a_modelo_v2_vivi_gas_red_out<-boxplot(icv_a_modelo_v2$vivi_gas_red, col="skyblue", frame.plot=F)
icv_a_modelo_v2_vivi_hacinamiento_out<-boxplot(icv_a_modelo_v2$vivi_hacinamiento, col="skyblue", frame.plot=F)
icv_a_modelo_v2_salud_serv_sanit_out<-boxplot(icv_a_modelo_v2$salud_serv_sanit, col="skyblue", frame.plot=F)

#¿CUANTOS NA TENGO POR COLUMNA?
View(summarise_all(icv_a_modelo_v2, funs(sum(is.na(.)))))
names(prueba)

#AGRUPAMOS TODOS LOS CAMPOS CON POSIBLES OUTLIERS PARA LATEX
xtable(summary(icv_a_modelo_v2))
#---------------------------------------------------------------------------------------------
#5.4 CORRELACIONES DEL NUEVO DATASET *********************************************************

rm(icv_a_modelo_v2_corr)
icv_a_modelo_v2_corr <- cor(icv_a_modelo_v2) #requiere corrplot. Las variables deben ser numéricas.
icv_a_modelo_v2_corr

#Visualizacion con indice de correlacion para cada atributo
corrplot(icv_a_modelo_v2_corr, method="number",tl.col="black",tl.cex=0.5)

corrplot(icv_a_modelo_v2_corr, type = "upper", 
         method = "square", 
         addCoef.col = "black", 
         tl.col = "black", tl.srt = 0.8)

#Visualizacion con nivel de correlacion para cada atribut
corrplot(icv_a_modelo_v2_corr, method="color",tl.col="black",tl.cex=0.8)

#Visualizacion con grafico de dispersion por cada atributo
#plot(icv_a_modelo)
#---------------------------------------------------------------------------------------------
#5.5 MODELO AJUSTADO DE RLM
summary(icv_mod_full) #r-cuadrado: 0.9964 r-cuadrado ajustado: 0.9964 
summary(icv_mod_full_backward) #r-cuadrado: 0.9964 r-cuadrado ajustado: 0.9964 
str(icv_a_modelo_v2) 
names(icv_a_modelo_v2)

rm(icv_mod_ajus)
icv_mod_ajus <- lm(icv ~ sup_ha + pobl_tot + dens_ha + edu_escolar_adul + salud_ate_prim +
                     salud_serv_sanit + salud_disp_rsu + entor_acceso_transp + entor_inunda +
                     entor_area_verde  + vivi_hacinamiento +
                     vivi_gas_red + vivi_calid_casa, data = icv_a_modelo_v2)
class(icv_mod_ajus)
str(icv_mod_ajus)
summary(icv_mod_ajus)

#PRINCIPIO DE PARSIMONIA: MENOS VARIABLES EXPLICAN MEJOR EL MODELO
#---------------------------------------------------------------------------------------------
#4.2 ANALISIS DE RESIDUOS SOBRE EL MODELO FULL
#names(icv_a_modelo_v2)
#GRAFICO DE RESIDUOS Y RECTA
#ggplot(icv_mod_ajus, aes(x=sup_ha + pobl_tot + dens_ha + edu_escolar_adul + salud_ate_prim +
#                           salud_serv_sanit + salud_disp_rsu + entor_acceso_transp + entor_inunda +
#                           entor_area_verde  + vivi_hacinamiento + vivi_gas_red + vivi_calid_casa, y=icv))+ 
#  geom_point() +
#  geom_smooth(method='lm',se=FALSE, col='green') +
#  theme_light()

#EXPLICAR LOS RESIDUOS EN EL MODELO FULL


#---------------------------------------------------------------------------------------------
#5.6 ANALISIS DE SUPUESTOS DE RESIDUOS: Analizamos supuestos estudiando residuos

#ANALISIS SOBRE EL MODELO AJUSTADO:
#residuos alrededor de 0: ver en el boxplot
icv_mod_ajus_residuos = residuals(icv_mod_ajus)

boxplot(icv_mod_ajus_residuos, col = "blue",horizontal=TRUE,ylim = c(-0.2,0.2),main="Box-plot de residuos")
plot_icv_mod_ajus_residuos <- plot_ly(y = ~icv_mod_ajus_residuos, type = "box")
plot_icv_mod_ajus_residuos
#---------------------------------------------------------------------------------------------
#5.7 homogeneidad de varianza: ver en residuos vs predichos
#normalidad de residuos: ver en QQplot
par(mfrow=c(2, 2))
plot(icv_mod_ajus)
#---------------------------------------------------------------------------------------------
#5.8 Test de normalidad de Shapiro-Wilk (muestras chicas)
# Test de normalidad de Shapiro-Wilk (muestras chicas)
#SI EL P-VALUE ES MAYOR A 0.10
#H0: La variable presenta una distribución normal
#H1: La variable presenta una distribución no normal

#Sig(p valor) > alfa: No rechazar H0 (normal).
#Sig(p valor) < alfa: Rechazar H0 (no normal)

#Donde alfa representa la significancia, que en este ejemplo hipotético es igual al 5% (0,05).
icv_mod_ajus_residuos.test <- shapiro.test(icv_mod_ajus_residuos)
print(icv_mod_ajus_residuos.test)
#---------------------------------------------------------------------------------------------
#5.9 Test de normalidad de Kolmogorov-Smirnov (muestras grandes)
lillie.test(icv_mod_ajus_residuos)
#---------------------------------------------------------------------------------------------
# 6.0 Test de Durbin-Watson test para determinar correlacion entre los Residuos
# errores independientes (no correlacionados, es equivalente si hay normalidad)
# DW debe ser cercano a 2 y el p-value cercano a 1 para demostrarlo
# un pvalue muy pequenio indica que no hay independencia (están correlacionados)
# La hipotesis nula es que no hay autocorrelacion.

dwtest(icv_mod_ajus)
#---------------------------------------------------------------------------------------------
# 6.1 Analizamos multicolinealidad e influyentes:
# VIF: Si es mayor a 10, entonces hay correlacion entra variables
# VIF = 1: NO HAY MULTICOLINEALIDAD
# VIF > 1: MULTICOLINEALIDAD ACEPTABLE
# VIF > 5: MULTICOLINEALIDAD ALTA

vif(icv_mod_ajus)
#---------------------------------------------------------------------------------------------
# 6.2 DISTANCIA DE COOK: INFLUYENTES: Se calcula para determinar cuan influyente es cada obs
# en las estimaciones del modelo
cooks=cooks.distance(icv_mod_ajus)
plot(cooks.distance(icv_mod_ajus))
#---------------------------------------------------------------------------------------------
# 6.3 COMPARAMOS MODELO MEDIANTE ANOVA
# anova(icv_mod_full, icv_a_modelo) #compara si las sumas de cuadrados son signif diferentes

#---------------------------------------------------------------------------------------------
## PREDICCION de nuevos datos
icv_predecir<-data.frame(disp=c(130,152,305),
                        potencia=c(51,49,200),
                        peso=c(2444,3100,4800),
                        aceleracion=c(11,16,14))

#valores a predecir
predict(mod_full,nuevo)

#---------------------------------------------------------------------------------------------
# RECTA DE REGRESION:

#icv ~ sup_ha + pobl_tot + dens_ha + edu_escolar_adul + salud_ate_prim + salud_serv_sanit + salud_disp_rsu + entor_acceso_transp + entor_inunda +  entor_area_verde  + vivi_hacinamiento + vivi_gas_red + vivi_calid_casa


