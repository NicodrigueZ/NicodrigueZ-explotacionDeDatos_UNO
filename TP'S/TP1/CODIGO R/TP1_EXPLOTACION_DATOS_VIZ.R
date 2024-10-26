#---------------------------------------------------------------------------------------------
#IMPORTACION DE DATOS
rm(defunciones)
defunciones <- read.table(file.choose(),
                        sep=',',
                        header=TRUE,
                        fill = TRUE,
                        row.names = NULL,
                        stringsAsFactors = FALSE)

View(defunciones)
str(defunciones)
names(defunciones)
head(defunciones)
#---------------------------------------------------------------------------------------------
#01. CAMBIO DE NOMBRE EN COLUMNAS
defunciones <- defunciones %>% rename (municipio_nom = municipio_nombreÂ., 
                                      causa_muerte_agrup = causa_muerte_capitulo,
                                      causa_muerte_detalle = causa_muerte_agrupamiento)
#---------------------------------------------------------------------------------------------
#02. REDUCCION DEL DATASET
rm(defunciones_red)
defunciones_red = subset(defunciones, select = c(anio, municipio_nom, sexo, grupo_edad, 
                                                 causa_muerte_agrup, causa_muerte_detalle, 
                                                 defunciones_cantidad))

View(defunciones_red)
str(defunciones_red)
#---------------------------------------------------------------------------------------------
#CASTEO LOS DATOS
defunciones_red$anio <- suppressWarnings(as.numeric(defunciones_red$anio))
defunciones_red$municipio_nom <- as.factor(defunciones_red$municipio_nom)
defunciones_red$sexo <- as.factor(defunciones_red$sexo)
defunciones_red$grupo_edad <- as.factor(defunciones_red$grupo_edad)
defunciones_red$causa_muerte_agrup <- as.factor(defunciones_red$causa_muerte_agrup)
defunciones_red$causa_muerte_detalle <- as.factor(defunciones_red$causa_muerte_detalle)
defunciones_red$defunciones_cantidad <- suppressWarnings(as.numeric(defunciones_red$defunciones_cantidad))
#---------------------------------------------------------------------------------------------
#¿COMO BUSCO LOS VALORES RAROS PARA LUEGO QUITARLOS?:
#prueba1 %>% distinct(causa_muerte_detalle)

#EXPLORACION DE DATOS: ¿CUANTOS DATOS CON VALOR "IGNORADO TENGO"?
sqldf('
SELECT anio, municipio_nom, count(*)
FROM prueba1
WHERE municipio_nom = "NA"
GROUP BY anio, municipio_nom
ORDER BY 3 DESC
')

#CONVIERTO LOS VALORES "IGNORADOS" (MUNICIPIO NOMBRE) y "SIN DATO" (EDAD) A NA PARA ELIMINARLOS
#rm(prueba1)
#prueba1 <- defunciones_red
defunciones_red[defunciones_red == "Ignorado"] <- NA
defunciones_red[defunciones_red == "Sin dato"] <- NA
defunciones_red[defunciones_red == ""] <- NA
View(defunciones_red)

#¿CUANTOS NA TENGO POR COLUMNA?
View(summarise_all(defunciones_red, funs(sum(is.na(.)))))

#ELIMINO LOS NA
defunciones_red <- na.omit(defunciones_red)

#¿CUANTOS NA TENGO POR COLUMNA? (REPASO)
View(summarise_all(defunciones_red, funs(sum(is.na(.)))))
#---------------------------------------------------------------------------------------------
#03. FILTRO POR ANIO
#prueba <- defunciones_red %>% filter(between(anio, 2010, 2019))
#prueba %>% distinct(anio)
#prueba %>% group_by(anio, municipio_nombreÂ.) %>% summarise(cantidad = n()) %>% arrange(desc(cantidad))
#---------------------------------------------------------------------------------------------
#05. FILTRO POR MERLO
defunciones_red %>% distinct(municipio_nom)

defunciones_red_merlo <- dplyr::filter(defunciones_red, 
                                 municipio_nom %in% c("Merlo"))
#---------------------------------------------------------------------------------------------
#06. AGRUPAMOS: POR CAUSA DE MUERTE Y 2011 VS 2019
#FILTRAMOS 2011 Y 2019
rm(defunciones_red_merlo_2019)
defunciones_red_merlo_2011 <- filter(defunciones_red_merlo, anio == 2011)
defunciones_red_merlo_2019 <- filter(defunciones_red_merlo, anio == 2019)

#ME QUEDO CON DOS COLUMNAS:
defunciones_red_merlo_2011 = subset(defunciones_red_merlo_2011, select = c(causa_muerte_agrup, defunciones_cantidad))
defunciones_red_merlo_2019 = subset(defunciones_red_merlo_2019, select = c(causa_muerte_agrup, defunciones_cantidad))

#AGRUPAMOS
defunciones_red_merlo_2011 <- defunciones_red_merlo_2011 %>% group_by(causa_muerte_agrup) %>% 
  summarise(across(c(defunciones_cantidad), sum))

defunciones_red_merlo_2019 <- defunciones_red_merlo_2019 %>% group_by(causa_muerte_agrup) %>% 
  summarise(across(c(defunciones_cantidad), sum))

#AGRUPAMOS BIEN???
sqldf('
SELECT anio, sexo, grupo_edad, causa_muerte_agrup, sum(defunciones_cantidad)
FROM defunciones_red_merlo
WHERE anio = 2009
AND SEXO = "Femenino"
AND grupo_edad = "0-4"
GROUP BY anio, sexo, grupo_edad, causa_muerte_agrup
')
#---------------------------------------------------------------------------------------------
#07. VIZ
View(defunciones_red_merlo_2011 %>% distinct(sexo))
names(defunciones_red_merlo_2011)

rm(fig)
str(defunciones_red_merlo_2011)

fig_defunciones_red_merlo_2011 <- defunciones_red_merlo_2011 %>% plot_ly()
fig_defunciones_red_merlo_2011_2 <- fig_defunciones_red_merlo_2011 %>% add_trace(x = ~causa_muerte_agrup, y = ~defunciones_cantidad, type = 'bar',
                                                  
                                                  marker = list(color = 'rgb(158,202,225)',
                                                                line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig_defunciones_red_merlo_2011_2

fig_defunciones_red_merlo_2019 <- defunciones_red_merlo_2019 %>% plot_ly()
fig_defunciones_red_merlo_2019_2 <- fig_defunciones_red_merlo_2019 %>% add_trace(x = ~causa_muerte_agrup, y = ~defunciones_cantidad, type = 'bar',
                                                                                 
                                                                                 marker = list(color = 'rgb(158,202,225)',
                                                                                               line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig_defunciones_red_merlo_2019_2

