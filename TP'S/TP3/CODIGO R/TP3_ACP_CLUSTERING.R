#METADATOS Y DOCUMENTACION
#ICA: https://monitorpisa.acumar.gob.ar/sistema-de-indicadores/calidad-ambiental/indice-de-calidad-de-agua-superficial-uso-iv/
##########################################################################################
############################### 01 PAQUETES NECESARIOS ###################################
##########################################################################################
#1.1 - INSTALACION DE PAQUETES

library(readr)
library(dplyr)
library(psych)
library(tidyverse)
library(sqldf)
library(xtable)
library(factoextra)

##########################################################################################
##################### 02. CARGA, TRANSFORMACION Y LIMPIEZA DE DATOS ###################### 
##########################################################################################
rm(mca_rlp)
mca_rlp <- read.table("./agc_y_riodelaplata2023_2da_camp.csv",sep=";",dec=".",header = T)
names(mca_rlp)

mca_rlp<-mca_rlp[, c(-1,-3,-4,-5,-6,-30,-31)] 
names(mca_rlp)

#colnames(mca_rlp)<-c("Zona","campana","tem_agua","tem_aire","oxi_disu","ph","f_olor","f_color",
#                              "f_espum","f_mat_flot", "col_fecales","escher_coli_ufc","enteroc_ufc",
#                              "nitrato_mg","amonio","fosforo_tot","fosforo_ort","clorofila","microcistina",
#                              "turbiedad_ntu","hidr_deriv_petro","cromo_total","cadmio_total","clorofila_ug",
#                              "microcistina_ug", "ica_num", "ica_det")

colnames(mca_rlp)<-c("zona","tem_agua","tem_aire","oxido_disuelto","ph","f_olor","f_color",
                      "f_espum","f_mat_flot", "coliformes_fecales","escherichia_coli","enterococos",
                      "nitrato","amonio","fosforo","fosfato","dbo","dqo",
                      "turbiedad_ntu","hidrocarburos_petro","cromo","cadmio","clorofila",
                      "microcistina")

View(mca_rlp)
names(mca_rlp)

attach(mca_rlp)


#mca_rlp$campana <- stri_replace_all_regex(mca_rlp$campana,
#                                                pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
#                                                replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
#                                                vectorize=FALSE)

mca_rlp[mca_rlp == "no se midiÃ³"] <- NA
mca_rlp[mca_rlp == "no se muestreÃ³"] <- NA
mca_rlp[mca_rlp == "sin muestra"] <- NA
mca_rlp[mca_rlp == "sin equipo"] <- NA
mca_rlp[mca_rlp == "no funcionÃ³"] <- NA
mca_rlp[mca_rlp == "N/R"] <- NA
mca_rlp[mca_rlp == "falto un frasco"] <- NA
mca_rlp[mca_rlp == ""] <- NA
mca_rlp[mca_rlp == "no se pudo calcular"] <- NA

#-----------------------------------------------------------------------------------------
#ANALISIS PARA CONTROLAR CUANTA INFORMACION SE PERDERIA AL ELIMINAR NAs
#rm(prueba)
#prueba <- mca_rlp
#prueba<-mca_rlp[, c(-24)]
#ELIMINAMOS LA COLUMNA DE ICA PORQUE TIENE DEMASIADOS NAs. SI DEJAMOS LA COLUMNA
#ICA Y ELIMINAMOS TODOS LOS NAs NOS RESULTAN 43OBSERVACIONES (51% DE PERDIDA) LAS RESTANTES.
#SIN EMBARGO, SI ELIMINAMOS ICA, Y LUEGO ELIMINAMOS TODOS LOS NAs, NOS RESULTAN 53 OBSERVACIONES

#View(summarise_all(prueba, funs(sum(is.na(.)))))
#EN INFORME: SE PIERDE UN 36% DE LOS DATOS DESPUES DE ELIMINAR LOS NA
#-----------------------------------------------------------------------------------------
#REEMPLAZAR LOS CARACTERES ESPECIALES POR VACIOS
names(mca_rlp)

mca_rlp$zona <- stri_replace_all_regex(mca_rlp$zona,
                                       pattern=c('Ã‰','Ã©','Ã¡','=>',    'Ã±','Â°','Ã³','Ãº','Âª','Ã','=<'),
                                       replacement=c('E','e','a','mayor a ','ni','',   'o','u', 'er','i','menor a '),
                                       vectorize=FALSE)

mca_rlp$tem_agua <- stri_replace_all_regex(mca_rlp$tem_agua,
                                                   pattern=c('=<', "<", ">=", ">"),
                                                   replacement=c(''),
                                                   vectorize=FALSE)

mca_rlp$tem_aire <- stri_replace_all_regex(mca_rlp$tem_aire,
                                                    pattern=c('=<', "<", ">=", ">"),
                                                    replacement=c(''),
                                                    vectorize=FALSE)

mca_rlp$oxido_disuelto <- stri_replace_all_regex(mca_rlp$oxido_disuelto,
                                                    pattern=c('=<', "<", ">=", ">"),
                                                    replacement=c(''),
                                                    vectorize=FALSE)

mca_rlp$nitrato <- stri_replace_all_regex(mca_rlp$nitrato,
                                                    pattern=c('=<', "<", ">=", ">"),
                                                    replacement=c(''),
                                                    vectorize=FALSE)

mca_rlp$amonio <- stri_replace_all_regex(mca_rlp$amonio,
                                                      pattern=c('=<', "<", ">=", ">"),
                                                      replacement=c(''),
                                                      vectorize=FALSE)

mca_rlp$fosforo <- stri_replace_all_regex(mca_rlp$fosforo,
                                                  pattern=c('=<', "<", ">=", ">"),
                                                  replacement=c(''),
                                                  vectorize=FALSE)


mca_rlp$fosfato <- stri_replace_all_regex(mca_rlp$fosfato,
                                                  pattern=c('=<', "<", ">=", ">"),
                                                  replacement=c(''),
                                                  vectorize=FALSE)

mca_rlp$dbo <- stri_replace_all_regex(mca_rlp$dbo,
                                                  pattern=c('=<', "<", ">=", ">"),
                                                  replacement=c(''),
                                                  vectorize=FALSE)

mca_rlp$dqo <- stri_replace_all_regex(mca_rlp$dqo,
                                                     pattern=c('=<', "<", ">=", ">"),
                                                     replacement=c(''),
                                                     vectorize=FALSE)

mca_rlp$turbiedad_ntu <- stri_replace_all_regex(mca_rlp$turbiedad_ntu,
                                                     pattern=c('=<', "<", ">=", ">"),
                                                     replacement=c(''),
                                                     vectorize=FALSE)

mca_rlp$hidrocarburos_petro <- stri_replace_all_regex(mca_rlp$hidrocarburos_petro,
                                                         pattern=c('=<', "<", ">=", ">"),
                                                         replacement=c(''),
                                                         vectorize=FALSE)

mca_rlp$cromo <- stri_replace_all_regex(mca_rlp$cromo,
                                                         pattern=c('=<', "<", ">=", ">"),
                                                         replacement=c(''),
                                                         vectorize=FALSE)

mca_rlp$cadmio <- stri_replace_all_regex(mca_rlp$cadmio,
                                                       pattern=c('=<', "<", ">=", ">"),
                                                       replacement=c(''),
                                                       vectorize=FALSE)

mca_rlp$clorofila <- stri_replace_all_regex(mca_rlp$clorofila,
                                                       pattern=c('=<', "<", ">=", ">"),
                                                       replacement=c(''),
                                                       vectorize=FALSE)

mca_rlp$microcistina <- stri_replace_all_regex(mca_rlp$microcistina,
                                                        pattern=c('=<', "<", ">=", ">"),
                                                        replacement=c(''),
                                                        vectorize=FALSE)

#SE CUANTIFICA LAS VARIABLES CATEGORICAS: VARIABLES DUMMY
mca_rlp[mca_rlp == "Presencia"] <- 1
mca_rlp[mca_rlp == "Ausencia"] <- 0
#-----------------------------------------------------------------------------------------
#CASTEO DE DATOS:
str(mca_rlp)
names(mca_rlp)

#Casteo de datos
mca_rlp$tem_agua <- suppressWarnings(as.numeric(mca_rlp$tem_agua))
mca_rlp$tem_aire <- suppressWarnings(as.numeric(mca_rlp$tem_aire))
mca_rlp$oxido_disuelto <- suppressWarnings(as.numeric(mca_rlp$oxido_disuelto))
mca_rlp$ph <- suppressWarnings(as.numeric(mca_rlp$ph))
mca_rlp$f_olor <- suppressWarnings(as.numeric(mca_rlp$f_olor))
mca_rlp$f_color <- suppressWarnings(as.numeric(mca_rlp$f_color))
mca_rlp$f_espum <- suppressWarnings(as.numeric(mca_rlp$f_espum))
mca_rlp$f_mat_flot <- suppressWarnings(as.numeric(mca_rlp$f_mat_flot))
mca_rlp$coliformes_fecales <- suppressWarnings(as.numeric(mca_rlp$coliformes_fecales))
mca_rlp$escherichia_coli <- suppressWarnings(as.numeric(mca_rlp$escherichia_coli))
mca_rlp$enterococos <- suppressWarnings(as.numeric(mca_rlp$enterococos))
mca_rlp$nitrato <- suppressWarnings(as.numeric(mca_rlp$nitrato))
mca_rlp$amonio <- suppressWarnings(as.numeric(mca_rlp$amonio))
mca_rlp$fosforo <- suppressWarnings(as.numeric(mca_rlp$fosforo))
mca_rlp$fosfato <- suppressWarnings(as.numeric(mca_rlp$fosfato))
mca_rlp$dbo <- suppressWarnings(as.numeric(mca_rlp$dbo))
mca_rlp$dqo <- suppressWarnings(as.numeric(mca_rlp$dqo))
mca_rlp$turbiedad_ntu <- suppressWarnings(as.numeric(mca_rlp$turbiedad_ntu))
mca_rlp$hidrocarburos_petro <- suppressWarnings(as.numeric(mca_rlp$hidrocarburos_petro))
mca_rlp$cromo <- suppressWarnings(as.numeric(mca_rlp$cromo))
mca_rlp$cadmio <- suppressWarnings(as.numeric(mca_rlp$cadmio))
mca_rlp$clorofila <- suppressWarnings(as.numeric(mca_rlp$clorofila))
mca_rlp$microcistina <- suppressWarnings(as.numeric(mca_rlp$microcistina))

str(mca_rlp)
summary(mca_rlp)
names(mca_rlp)
View(summarise_all(mca_rlp, funs(sum(is.na(.)))))
mca_rlp <- na.omit(mca_rlp)

#BORRO LOS FLAGS
mca_rlp<-mca_rlp[, c(-6,-7,-8,-9)] 

#QUE DEBEMOS CONSIDERAR PARA ELIMINAR UNA COLUMNA?:
#MUCHAS NAs; OUTLIERS; VALOR CONSTANTE
#AJUSTE: NO SIGNIFICANTE EN LA MATRIZ DE CORR
#-----------------------------------------------------------------------------------------
#BOXPLOT DE LOS DATOS CUANTITATIVOS:
#CON GRAFICO DINAMICO:
summary(mca_rlp)
names(mca_rlp)
rm(mca_rlp_boxplot)
mca_rlp_boxplot <- plot_ly(y = ~mca_rlp$microcistina, type = "box")
mca_rlp_boxplot

#col_fecales; microcistina; escher_coli_ufc; enteroc_ufc; nitrato_mg; amonio; turbiedad_ntu; clorofila_ug

#SUMMARY DE OUTLIERS:
#rm(mca_rlp_out)
#names(mca_rlp_out)
#mca_rlp_out <- mca_rlp[,c(10,11,13,17,18,22)]
#summary(mca_rlp_out)
#xtable(summary(mca_rlp_out) ) #  para sacar en latex

par(mfrow=c(3, 3))
mca_rlp_escher_coliformes_fecales_out <- boxplot(mca_rlp$coliformes_fecales, col="skyblue", frame.plot=F)
mca_rlp_escher_escherichia_coli_out <- boxplot(mca_rlp$escherichia_coli, col="skyblue", frame.plot=F)
mca_rlp_enterococos_out <- boxplot(mca_rlp$enterococos, col="skyblue", frame.plot=F)
mca_rlp_nitrato_out <- boxplot(mca_rlp$nitrato, col="skyblue", frame.plot=F)
mca_rlp_amonio_out <- boxplot(mca_rlp$amonio, col="skyblue", frame.plot=F)
mca_rlp_dqo_out <- boxplot(mca_rlp$dqo, col="skyblue", frame.plot=F)
mca_rlp_turbiedad_ntu_out <- boxplot(mca_rlp$turbiedad_ntu, col="skyblue", frame.plot=F)
mca_rlp_clorofila_out <- boxplot(mca_rlp$clorofila, col="skyblue", frame.plot=F)
mca_rlp_microcistina_out <- boxplot(mca_rlp$microcistina, col="skyblue", frame.plot=F)

mca_rlp_microcistina_out$out

#ELIMINO LOS OUTLIERS
mca_rlp <- mca_rlp[!(mca_rlp$coliformes_fecales %in% c(32000,22000,21000)),] #PROBAR ASI, SINO BORRAR LAS DE 15K
mca_rlp <- mca_rlp[!(mca_rlp$escherichia_coli %in% c(32000,11200,7200,6800)),] #PROBAR ASI, SINO BORRAR 5600
mca_rlp <- mca_rlp[!(mca_rlp$enterococos %in% c(9300,5000,5500)),]
mca_rlp <- mca_rlp[!(mca_rlp$nitrato %in% c(44.0)),]
mca_rlp <- mca_rlp[!(mca_rlp$amonio %in% c(18.0,11.0,6.3)),]
mca_rlp <- mca_rlp[!(mca_rlp$dqo %in% c(230,90,70)),] #** PROBAR, SINO BORRAR TODA LA COLUMNA O LOS 59 PARA ABAJO
mca_rlp <- mca_rlp[!(mca_rlp$turbiedad_ntu %in% c(432)),]
mca_rlp <- mca_rlp[!(mca_rlp$clorofila %in% c(740.93,152.11)),]
mca_rlp <- mca_rlp[!(mca_rlp$microcistina %in% c(5.00,2.98,2.61,0.99,0.90)),]#**PROBAR ASI, SINO BORRAR LA COLUMNA

#PIERDO 11 REGISTROS: de 53 pasamos a 31

#CONTROL:
par(mfrow=c(3, 3))
mca_rlp_escher_coliformes_fecales_out <- boxplot(mca_rlp$coliformes_fecales, col="skyblue", frame.plot=F)
mca_rlp_escher_escherichia_coli_out <- boxplot(mca_rlp$escherichia_coli, col="skyblue", frame.plot=F)
mca_rlp_enterococos_out <- boxplot(mca_rlp$enterococos, col="skyblue", frame.plot=F)
mca_rlp_nitrato_out <- boxplot(mca_rlp$nitrato, col="skyblue", frame.plot=F)
mca_rlp_amonio_out <- boxplot(mca_rlp$amonio, col="skyblue", frame.plot=F)
mca_rlp_dqo_out <- boxplot(mca_rlp$dqo, col="skyblue", frame.plot=F)
mca_rlp_turbiedad_ntu_out <- boxplot(mca_rlp$turbiedad_ntu, col="skyblue", frame.plot=F)
mca_rlp_clorofila_out <- boxplot(mca_rlp$clorofila, col="skyblue", frame.plot=F)
mca_rlp_microcistina_out <- boxplot(mca_rlp$microcistina, col="skyblue", frame.plot=F)

rm(mca_rlp_escher_coliformes_fecales_out,mca_rlp_escher_escherichia_coli_out,
   mca_rlp_enterococos_out,mca_rlp_nitrato_out,
   mca_rlp_amonio_out,mca_rlp_dqo_out,
   mca_rlp_turbiedad_ntu_out,mca_rlp_clorofila_out,
   mca_rlp_microcistina_out)

#ELIMINO LOS NAs
mca_rlp <- na.omit(mca_rlp)
##########################################################################################
##################### 03. ANALISIS DE COMPONENTES PRINCIPALES ############################
##########################################################################################
set.seed(101)

#3.1 - ESCALAMOS LOS DATOS:
names(mca_rlp)

#3.2.3 - AJUSTAMOS ELIMINANDO LOS FLAGS DE PRESENCIA DE OLOR, COLOR, ESPUMA Y METALES FLOTANTES
#3.1.2 - ELIMINAMOS hidr_deriv_petro PORQUE MANEJA UN INDICE MUY BAJO QUE NO PERMITE ESCALAR
rm(mca_rlp_ajus)
names(mca_rlp)
mca_rlp_ajus <- mca_rlp[, c(-7,-14,-16,-20)]

rm(mca_rlp_esc)
mca_rlp_esc <- scale(mca_rlp_ajus[,-1])

rm(nom_zona)
nom_zona <- mca_rlp_ajus$zona
#-----------------------------------------------------------------------------------------
#3.2 - MATRIZ DE CORRELACION:
#Correlaciones para justificar el Aepf_cp (argumentar)
#cor(epf[, -1]) # matriz de correlacion
#corrplot(cor(epf[, -1])) # scatter plot de correlaciones
rm(mca_rlp_esc_cor)
mca_rlp_esc_cor <- cor(mca_rlp_esc) #requiere corrplot. Las variables deben ser numéricas.
mca_rlp_esc_cor

#Visualizacion con indice de correlacion para cada atributo
corrplot(mca_rlp_esc_cor, method="number",tl.col="black",tl.cex=0.7 )

#VERIFICAR hidrocarburos_petro; CROMO; CADMIO ***************
#-----------------------------------------------------------------------------------------
#Es relevante aplicar Aepf_cp?: Se comprueba mediante un test de Barlett
#N = Cantidad registros
#
# La prueba de esfericidad de Bartlett prueba
#H0: no hay correlaciones (esfericidad) por lo que si pvalor chico entonces está habilitado Aepf_cp
cortest.bartlett(cor(mca_rlp_esc),n=31) #n=tamaño de muestra
#------------------------------------------------------------------------------------
#KMO  #Kaiser-Meyer-Olkin analiza los autovalores de la matriz de covarianzas
#sirve para comparar los valores de correlacion de las variables y sus correlaciones parciales
#si es cercano a 1, tiene sentido el analisis de componentes principales.


#Cuanto más cerca de 1 tenga el valor obtenido del test KMO: implica que la relación entres las variables es alta. 
#Si KMO ≥ 0.9, el test es muy bueno; 
#notable para KMO ≥ 0.8; 
#mediano para KMO ≥ 0.7; 
#bajo para KMO ≥ 0.6; 
#y muy bajo para KMO < 0.5.
KMO(cor(mca_rlp_esc))


#------------------------------------------------------------------------------------
### Aepf_cp usando Rbase
# La función prcomp() calcula automáticamente el valor de las 
# componentes principales para cada observación 
rm(mca_rlp_esc_cp)
mca_rlp_esc_cp <- prcomp(mca_rlp_esc, scale = FALSE) # Analizo los componentes principales.
# Por defecto, prcomp() centra las variables para que tengan media cero
# si se quiere además que su desviación estándar sea de uno, hay que indicar scale = TRUE.
summary(mca_rlp_esc_cp) #Obtenemos el porcentaje de explicacion de los Aepf_cp *****
names(mca_rlp_esc_cp)
# Los elementos center y scale almacenados en el objeto pca contienen la media y desviación típica 
# de las variables previa estandarización (en la escala original).
#mca_rlp_esc_cp$center  
#mca_rlp_esc_cp$scale   
#mca_rlp_esc_cp$sdev    
# rotation contiene el valor de los autovalores para cada componente (eigenvector). 
# El número máximo de componentes principales se corresponde con el mínimo(n-1,p),
# que en este caso es min(24,9)= 9.
mca_rlp_esc_cp$rotation  # **********
mca_rlp_esc_cp$x #autovectores ***************

#------------------------------------------------------------------------------------
# Grafico de Sedimentacion de las componentes
plot(mca_rlp_esc_cp, 
     type="l", 
     main="Gráfico de sedimentación",
     col=c("blue4"))
abline(0.7,0,col=c("brown3")) # linea horizontal en 1 del eje y.

#4.6.2 - Usamos un grafico de barras. USAR ESTE EN INFORME
fviz_screeplot(mca_rlp_esc_cp, 
               addlabels = TRUE, 
               ylim = c(0, 80),
               main="mca_rlp_esc_cp mas significativas con Screeplot")

#para graficar autovalores ordenados (gráfico de sedimentación)
#fviz_screeplot(epf_cp, addlabels = TRUE, ylim = c(0, 60))
#---------------------------------------------------------------------------------------------
#4.7 - GRAFICO DE BIPLOT
biplot(x = mca_rlp_esc_cp, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

# Biplot con puntos. Se ven las variables y los casos.
# El grafico entre la Componente Principal 1 y 2, se puede apreciar
# dos grandes agrupamientos de variables, indicando correlacion positiva en
# cada grupo, y que estos grupos estan de forma perpendicular, indicando correlacion nula
#biplot(x = mca_rlp_esc_cp, scale = 0, cex = 0.6, xlabs=rep(".", nrow(mca_rlp_esc)),col = c("grey", "brown3"))
##############################################################################################
#################################### APLICACION DE CLUSTERING ################################
##############################################################################################
#names(mca_rlp_ajus_esc)
#str(mca_rlp_ajus)
View(mca_rlp_esc)
summary(mca_rlp_esc)
##############################################################################################
################################ 01 CLUSTERING JERARQUICO ####################################
##############################################################################################
#1.1 - DEFINIMOS EL TIPO DE DISTANCIA: PROBAR CON OTROS TIPOS DE DISTANCIAS:
rm(mca_rlp_esc_jer_dist_eu)
row.names(mca_rlp_esc) = nom_zona
mca_rlp_esc_jer_dist_eu <- dist(x = mca_rlp_esc, method = "euclidean")
mca_rlp_esc_jer_dist_man <- dist(x = mca_rlp_esc, method = "manhattan")
mca_rlp_esc_jer_dist_eu
#---------------------------------------------------------------------------------------------
#2.1 - DEFINIMOS EL LINKAJE COMPLETO O POR PROMEDIO: PROBAR OTROS LINCAJES (WARD|CENTROIDE|...)
rm(agc_ajus_esc_jer_dist_eu_completo)

mca_rlp_esc_jer_dist_eu_completo <- hclust(d = mca_rlp_esc_jer_dist_eu, method = "complete")
mca_rlp_esc_jer_dist_eu_average  <- hclust(d = mca_rlp_esc_jer_dist_eu, method = "average")
mca_rlp_esc_jer_dist_eu_completo
mca_rlp_esc_jer_dist_eu_average

summary(mca_rlp_ajus_esc)

mca_rlp_esc_jer_dist_man_completo <- hclust(d = mca_rlp_esc_jer_dist_man, method = "complete")
mca_rlp_esc_jer_dist_man_average  <- hclust(d = mca_rlp_esc_jer_dist_man, method = "average")
#---------------------------------------------------------------------------------------------
#3.1 - EL COEFICIENTE COFRENETICO MIRA LAS CORRELACIONES: Se quiere que sea cercana a 1
# X = AL METODO DE DISTANCIA; COFRENETICO = AL METODO DE LINKAJE
cor(x = mca_rlp_esc_jer_dist_eu, cophenetic(mca_rlp_esc_jer_dist_eu_completo)) 
cor(x = mca_rlp_esc_jer_dist_eu, cophenetic(mca_rlp_esc_jer_dist_eu_average))

cor(x = mca_rlp_esc_jer_dist_man, cophenetic(mca_rlp_esc_jer_dist_man_completo)) 
cor(x = mca_rlp_esc_jer_dist_man, cophenetic(mca_rlp_esc_jer_dist_man_average))
#CONCLUSION: ME QUEDO CON EUCLIDEA AVERAGE PORQUE ME DA MEJOR EL COEFICIENTE COFRENETICO
#---------------------------------------------------------------------------------------------
#04 Graficamos el dendrograma con distancia euclidea y linkaje average
fviz_dend(x = mca_rlp_esc_jer_dist_eu_average, k = 3, cex = 0.6) + 
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Clustering jerárquico",
       subtitle = "Distancia euclídea, Lincage Average, k=3")

fviz_dend(x = mca_rlp_esc_jer_dist_man_average, k = 3, cex = 0.6) + 
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Clustering jerárquico",
       subtitle = "Distancia manhattan, Lincage Average, k=3")

#para ver a qué grupo se asignó cada caso (CASO = observacion por cluster):
cutree(mca_rlp_esc_jer_dist_eu_average, k = 3)#[5] #ENTRE CORCHETES MIRAMOS LA POS. DE LA OBS

#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
######################################################################################
############################## CLUSTER POR K-MEANS ###################################
######################################################################################
library(broom)
library(factoextra)

set.seed(101)
#------------------------------------------------------------------------------------
km_cluster_mca_rlp_eu <- kmeans(x = mca_rlp_esc_jer_dist_eu, centers = 3, nstart = 50)
km_cluster_mca_rlp_eu

km_cluster_mca_rlp_man <- kmeans(x = mca_rlp_esc_jer_dist_man, centers = 3, nstart = 50)
km_cluster_mca_rlp_man
#------------------------------------------------------------------------------------
#???
#fviz_nbclust(x = mca_rlp_esc , FUNcluster = kmeans, method = "silhouette", k.max = 11) +
#  labs(title = "Numero optimo de clusters", diss = mca_rlp_esc_jer_dist_eu)
row.names(mca_rlp_esc) = NULL
row.names(mca_rlp_esc)
Viw(mca_rlp_esc)
fviz_cluster(object = km_cluster_mca_rlp_eu, data = mca_rlp_esc, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means con k=3 y distancia eucledean") +
  theme_bw() +
  theme(legend.position = "none")

fviz_cluster(object = km_cluster_mca_rlp_man, data = mca_rlp_esc, show.clust.cent = TRUE,
             ellipse.type = "manhattan", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means con k=3 y distancia manhattan") +
  theme_bw() +
  theme(legend.position = "none")

#------------------------------------------------------------------------------------
#library(NbClust)

#km_clusters <- eclust(x = mca_rlp_esc, FUNcluster = "kmeans", k = 2, # Resultados para K = 2, seed = 123,
#                        hc_metric = "manhattan", nstart = 50, graph = FALSE)

#fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
#                ggtheme = theme_classic()) 
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------