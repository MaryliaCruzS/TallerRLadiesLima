
######################################################################################
#                                R LADIES LIMA
######################################################################################
#                   TALLER :  DATOS ELECTORALES CON R
######################################################################################

## PARTICIPACIÓN DE LAS MUJERES EN ELECCIONES SUBNACIONALES - NIVEL PROVINCIAL

### Paquetes a utilizar

#install.packages("dplyr")
library(dplyr) # para el gráfico estático

#install.packages("ggplot2")
library(ggplot2) # para el gráfico estático

#install.packages("gganimate")
library(gganimate) # para el gráfico animado

#install.packages("gifski")
library(gifski)  # para el gráfico animado

#install.packages("av")
library(av) # para el gráfico animado

### Cambiar de directorio de trabajo

setwd("~/Documentos/GitHub/TallerRLadiesLima/Bases de datos")

#Una vez descargadas las bases de datos, crea una carpeta en tu ordenador 
#donde se ubiquen todas las bases de datos a utilizar. En mi caso, cree 
#la carpeta TallerRLadiesLima.
#La ruta es mi carpeta es "~/Documentos/GitHub/TallerRLadiesLima"
# Si no sabes la ruta de tu carpeta, también puede ir al menú : 
# SESSION-SET WORKING DIRECTORY-CHOOSE DIRECTORY
# SESIÓN-ELEGIR DIRECTORIO DE TRABAJO-CAMBIAR DE DIRECTORIO

### APERTURA DE LAS BASES DE DATOS
### Utilizamos el comando "read_excel" para abrir las bases de datos en excel.

library(readxl)
Candidatos2002 <- read_excel("ERM2002_Candidatos_Provincial.xlsx")
Candidatos2006 <- read_excel("ERM2006_Candidatos_Provincial.xlsx")
Candidatos2010 <- read_excel("ERM2010_Candidatos_Provincial.xlsx")
Candidatos2014 <- read_excel("ERM2014_Candidatos_Provincial.xlsx")
Candidatos2018 <- read_excel("ERM2018_Candidatos_Provincial.xlsx")

Ubigeo <- read_excel("Ubigeo.xlsx")

###El comando "names" muestra el nombre de las variables.

names(Candidatos2002)
names(Candidatos2006)
names(Candidatos2010)
names(Candidatos2014)
names(Candidatos2018)

### AÑADIR LA VARIABLE AÑO DE ELECCION

#Creo una nueva variable "anio" que indica el año de elección en cada base
#de datos

Candidatos2002$anio=2002
Candidatos2006$anio=2006
Candidatos2010$anio=2010
Candidatos2014$anio=2014
Candidatos2018$anio=2018

###Junto todas las bases de datos en una sola base de datos. 

lista=list(Candidatos2002,Candidatos2006,Candidatos2010,Candidatos2014,Candidatos2018)
#list = crea una lista de elementos
# rbind= junta filas de distintas bases de datos
# do.call =  repite una acción del código
# as.data.frame =  como base de datos
Candidatos=as.data.frame(do.call(rbind,lista))

### UNIR LA BASE DE CANDIDATOS CON LA BASE DE UBIGEO

###Utilizo el comando merge para juntar las bases de datos de los candidatos 
###de las elecciones provinciales y la de ubigeo 
Candidatos=merge(Candidatos,Ubigeo,by.x="Provincia",by.y="Provincia")

#Verifico los nombres de la variable Ubigeo
names(Candidatos)

# Variable categórica
Candidatos$Sexo=as.factor(Candidatos$Sexo)
Candidatos$Cargo=as.factor(Candidatos$Cargo)

###############################ESPACIO PARA PREGUNTAS#########################################

############################################################################################

##################PREGUNTA 1: ¿Se ha incrementado la participación política de las mujeres en las elecciones subnacionales- provinciales?


### CREANDO TABLAS
# El comando "table" siver para crear tablas de frecuencias.
tablaresumen=table(Candidatos$Sexo,Candidatos$anio)
tablaresumen
# El comando "prop.table" siver para crear tablas de proporciones
# El número 2  indica que se desea porcentajes por columnas.
# Se coloca *100 para multiplicar por 100 las proporciones de 0 a 1.
prop.table(table(Candidatos$Sexo,Candidatos$anio),2)*100
round(prop.table(table(Candidatos$Sexo,Candidatos$anio),2)*100)


####GRAFICO SENCILLO SIN GGPLOT2

###El comando "barplot" se utiliza para realizar gráfico de barras  
barplot(prop.table(tablaresumen,2)*100,
        xlab = "Frecuencias relativas (%)",
        legend = T,
        width = 0.3, ylim = c(0, 1.5),
        horiz = T)
###############################ESPACIO PARA PREGUNTAS#########################################

###############################UTILIZANDO GGPLOT##################################


###INSUMOS PARA GRÁFICO

#tablaresumen=table(Candidatos$Sexo,Candidatos$anio)
as.data.frame(tablaresumen)

#Uso el comando "aggregate" para resumir tablas y calcular la suma total de candidatos por años
tabla_general=aggregate(Freq ~ Var2, data = tablaresumen, sum)

#Usar el comando "merge" para juntar la tabla anterior con la tabla resumen
Candidatos_tabla1=merge(tablaresumen,tabla_general,by=c("Var2"))

#Calcular la proporción de la cantidad de mujeres y la cantidad total de candidatos
Candidatos_tabla1$Proporcion=Candidatos_tabla1$Freq.x/Candidatos_tabla1$Freq.y

# Se crea una lista de nombres nuevos para renombrar  Candidatos_tabla1.
names(Candidatos_tabla1) <- c("anio", "Sexo","Frecuencia","Total_Candidatos","Proporcion")

#Redefino el formato de la variable anio . Utilizo el comando "as.numeric" para 
# definir como variable numérica a anio.
Candidatos_tabla1$anio=as.numeric(Candidatos_tabla1$anio)

###GRÁFICO ESTÁTICO
grafico1estatico=ggplot(Candidatos_tabla1,aes(x=anio, y=Proporcion, group=Sexo,
                                              color=Sexo))+
  geom_line(size=3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x="Año de la elección",y="Porcentaje de Candidatos",title="Gráfico estático de candidaturas a nivel provincial") 

grafico1estatico+ scale_x_discrete(limits=c("2002","2006","2010","2014","2018"))

###############################ESPACIO PARA PREGUNTAS#########################################

###GRÁFICO ANIMADO
grafico1dinamico=ggplot(Candidatos_tabla1,aes(x=anio, y=Proporcion, group=Sexo,
                                              color=Sexo))+
  geom_line(size=3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x="año de la elección",y="Porcentaje de Candidatos",title="Gráfico animado de candidaturas a nivel provincial" ) + 
  transition_reveal(anio)

grafico1dinamico + scale_x_discrete(limits=c("2002","2006","2010","2014","2018"))

# Guardar el gif
anim_save("grafico1dinamico.gif")

###############################ESPACIO PARA PREGUNTAS#########################################

##################PREGUNTA 2 : ¿A qué cargos postulan más las mujeres? ####

###INSUMOS PARA GRÁFICO

#Uso el comando "table" para crear una tabla de frecuencia de sexo, anio y cargo.
tabla=as.data.frame(table(Candidatos$Sexo,Candidatos$anio,Candidatos$Cargo))
tabla

#El comando "aggregate" sirve crear una tabla para calcular cantidad total de
# candidatos y de cantidad total de candidatos a regidores a nivel provincial
tabla_frecuencia=aggregate(Freq ~ Var2+Var3, data = tabla, sum)
tabla_frecuencia

# Uso el comando "merge" para unir las dos tablas anteriores
Candidatos_tabla2=merge(tabla,tabla_frecuencia,by=c("Var2","Var3"))
Candidatos_tabla2

# Calculo la proporción de candidatos por sexo
Candidatos_tabla2$Proporcion=Candidatos_tabla2$Freq.x/Candidatos_tabla2$Freq.y

# Se crea una lista de nombres nuevos para renombrar  Candidatos_tabla2
names(Candidatos_tabla2) <- c("anio", "Cargo","Sexo","Frecuencia","Total_Candidatos","Proporcion")

# Redefino el formato de la variable anio . Utilizo el comando "as.numeric" para 
# definir como variable numérica a anio.
Candidatos_tabla2$anio=as.numeric(Candidatos_tabla2$anio)

##GRÁFICO ESTÁTICO
grafico2estatico=ggplot(Candidatos_tabla2,aes(x=anio, y=Proporcion, group=Sexo,
                                              color=Sexo))+
  geom_line(size=3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x="Año de la elección",y="Porcentaje de Candidatos",title="Gráfico estático de candidatos según el cargo al que postula") + 
  facet_wrap(~Cargo,ncol=2,strip.position = "top")
grafico2estatico+ scale_x_discrete(limits=c("2002","2006","2010","2014","2018"))

# GRÁFICO ANIMADO
grafico2dinamico=ggplot(Candidatos_tabla2,aes(x=anio, y=Proporcion, group=Sexo,
                                              color=Sexo))+
  geom_line(size=3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  labs(x="Año de la elección",y="Porcentaje de Candidatos",title="Gráfico animado de candidatos según el cargo al que postula") + 
  facet_wrap(~Cargo,ncol=2,strip.position = "top")+
  transition_reveal(anio)
grafico2dinamico+ scale_x_discrete(limits=c("2002","2006","2010","2014","2018"))


# Guardar el gif
anim_save("grafico2dinamico.gif")

###############################ESPACIO PARA PREGUNTAS#########################################


#PREGUNTA 3 : ¿En qué provincias las mujeres postulan más?
# Porcentajes de mujeres que se presentan a la alcaldia provincial

#Crear una tabla de conteo de alcaldes mujeres y varones por cada provincia

TabladeConteo=as.data.frame(table(Candidatos$Sexo,Candidatos$anio,Candidatos$UBIGEO))

#Crear la tabla de mujeres , y otra de varones
CandidatAs <- subset(TabladeConteo, TabladeConteo$Var1 == "Femenino")
CandidatOs <- subset(TabladeConteo, TabladeConteo$Var1 == "Masculino")

#Cambios de nombres a las bases de datos creadas
names(CandidatAs) <- c("Femenino", "anio","UBIGEO","FrecuenciaMujeres")
names(CandidatOs) <- c("Masculino", "anio","UBIGEO","FrecuenciaVarones")

#Usamos el comando "merge" para unir las dos bases anteriores entre UBIGEO y anio.
DataConteo=merge(CandidatOs,CandidatAs,by.x=c("UBIGEO","anio"),by.y=c("UBIGEO","anio"))

#Seleccionamos las vables que utilizaremos
DataConteo=select(DataConteo, UBIGEO, anio,FrecuenciaMujeres,FrecuenciaVarones)

#Creamos una variable "el número de candidatos".
DataConteo$FrecuenciaTotal=DataConteo$FrecuenciaMujeres+DataConteo$FrecuenciaVarones

#Creamos las variables sobre porcentajes de mujeres candidatas y hombres candidatos
DataConteo$Porcentaje_Mujeres=(DataConteo$FrecuenciaMujeres/DataConteo$FrecuenciaTotal)*100
DataConteo$Porcentaje_Varones=(DataConteo$FrecuenciaVarones/DataConteo$FrecuenciaTotal)*100

#Resumen de las variables
summary(DataConteo$Porcentaje_Mujeres)
summary(DataConteo$Porcentaje_Varones)

# El comando na.omit sirve para eliminar 
DataConteo <- na.omit(DataConteo)

#Juntamos la data anterior con Ubigeo para saber los nombres.
DataConteo=merge(DataConteo,Ubigeo,by="UBIGEO",all.x = TRUE)

#Reordenamos la base de datos de mayor a menor por el Porcentaje de Mujeres. 
#Usamos el comando order para lo mencionado.
DataConteo <- DataConteo[with(DataConteo, order(-DataConteo$Porcentaje_Mujeres)), ] #
#¿Cuales son las provincias donde más participan las mujeres en elecciones?
head(DataConteo, 5)

#Reordenamos la bse de datos de menor a mayor por el Porcentaje de Mujeres. 
#Usamos el comando order para lo mencionado.
DataConteo <- DataConteo[with(DataConteo, order(DataConteo$Porcentaje_Mujeres)), ] #
#¿Cuales son las provincias donde más participan las mujeres en elecciones?
head(DataConteo, 5)

###Guarda la base de datos
library(openxlsx) 
write.xlsx(DataConteo,"DataConteo.xlsx")

###GRÁFICOS BOXPLOT


ggplot(aes(y = Porcentaje_Mujeres, x = anio), data = DataConteo) + geom_boxplot(fill=2:6)+theme_bw()+ylim(0,100)+labs(x="Año de elección provincial")

ggplot(aes(y = Porcentaje_Varones, x = anio), data = DataConteo) + geom_boxplot(fill=2:6)+theme_bw()+ylim(0,100)+labs(x="Año de elección provincial")


#Filtremos la base de datos para obtener solo los datos del 2018. Utilizamos el comando "subset" para ello.
DataConteo2018 <- subset(DataConteo, DataConteo$anio == "2018")
DataConteo2018 <- DataConteo2018[with(DataConteo2018, order(-DataConteo2018$Porcentaje_Mujeres)), ] #
head(DataConteo2018, 5)


DataConte2018 <- DataConteo2018[with(DataConteo2018, order(DataConteo2018$Porcentaje_Mujeres)), ] #
head(DataConteo2018, 5)

### En Cañete, Putumayo, Sechura, Grau y Tarma son las 5 provincias con mayor porcentaje de participación 
### de las mujeres como candidatas a regionas o alcaldesas.

###############################ESPACIO PARA PREGUNTAS#########################################

#PREGUNTA 4 : ¿En qué provincias las mujeres postulan más a alcaldes?

# Seleccionar un subconjuntos de datos de las mujeres candidatas al alcalde provincia.
CandidatosAalcaldes <- subset(Candidatos, Candidatos$Cargo == "ALCALDE PROVINCIAL")

# Crear una tabla por sexo, ubigeo y anio
TablaAlcaldes=as.data.frame(table(CandidatosAalcaldes$Sexo,CandidatosAalcaldes$UBIGEO,CandidatosAalcaldes$anio))
# Filtramos la tabla por Femenino
TablaAlcaldesAs <- subset(TablaAlcaldes, TablaAlcaldes$Var1 == "Femenino")
# Filtramos la tabla por Masculino
TablaAlcaldEs <- subset(TablaAlcaldes, TablaAlcaldes$Var1 == "Masculino")

# Renombramos las variables de nuevas bases de datos creadas
names(TablaAlcaldesAs) <- c("Femenino", "UBIGEO","anio","FrecuenciaMujeres")
names(TablaAlcaldEs) <- c("Masculino", "UBIGEO","anio","FrecuenciaVarones")

# Juntamos ambas bases de datos
DataConteoAlcades=merge(TablaAlcaldesAs,TablaAlcaldEs,by.x=c("UBIGEO","anio"),by.y=c("UBIGEO","anio"))

# Seleccionar las variables de la bases de datos
DataConteoAlcades=select(DataConteoAlcades, UBIGEO, anio,FrecuenciaMujeres,FrecuenciaVarones)

# Crear una nueva variable, suma de la cantidad de total de candidatos a alcaldes
DataConteoAlcades$FrecuenciaTotal=DataConteoAlcades$FrecuenciaMujeres+DataConteoAlcades$FrecuenciaVarones

# Calcular el porcentaje de mujeres a alcaldes
DataConteoAlcades$Porcentaje_Mujeres=(DataConteoAlcades$FrecuenciaMujeres/DataConteoAlcades$FrecuenciaTotal)*100

# Calcular el porcentaje de varones a alcaldes
DataConteoAlcades$Porcentaje_Varones=(DataConteoAlcades$FrecuenciaVarones/DataConteoAlcades$FrecuenciaTotal)*100

# Usar el comando "merge" para juntar la base de datos por ubigeo
DataConteoAlcades=merge(DataConteoAlcades,Ubigeo,by="UBIGEO",all.x = TRUE)

# Ordenar la base de datos de mayor a menor
DataConteoAlcades <- DataConteoAlcades[with(DataConteoAlcades, order(-DataConteoAlcades$Porcentaje_Mujeres)), ] #

# El comando "head" muestra las cinco primeras filas.
head(DataConteoAlcades, 5)

# Ordenar la base de datos de menor a mayor
DataConteoAlcades <- DataConteoAlcades[with(DataConteoAlcades, order(DataConteoAlcades$Porcentaje_Mujeres)), ] #

# El comando "head" muestra las cinco primeras filas.
head(DataConteoAlcades, 5)

###PARA GUARDAR LA BASE DATOS CREADA

##install.packages("openxlsx") 
library(openxlsx) 
# Se usa el comando "write.xlsx"  para guardar datos en la data de excel. 
write.xlsx(DataConteoAlcades,"DataConteoAlcades.xlsx")

###GRÁFICOS BOXPLOT


ggplot(aes(y = Porcentaje_Mujeres, x = anio), data = DataConteoAlcades) + geom_boxplot(fill=2:6)+theme_bw()+ylim(0,100)+labs(x="Año de elección provincial")

ggplot(aes(y = Porcentaje_Varones, x = anio), data = DataConteoAlcades) + geom_boxplot(fill=2:6)+theme_bw()+ylim(0,100)+labs(x="Año de elección provincial")



###Crear una base de datos de provincias con ninguna candidata a alcalcde
NingunaMujerCandidataAlcade=DataConteoAlcades[which(DataConteoAlcades$Porcentaje_Mujeres == 0),]

#Filtremos la base de datos para obtener solo los datos del 2018. Utilizamos el comando "subset" para ello.

DataConteoAlcades2018 <- subset(DataConteoAlcades, DataConteoAlcades$anio == "2018")
DataConteoAlcades2018 <- DataConteoAlcades2018[with(DataConteoAlcades2018, order(-DataConteoAlcades2018$Porcentaje_Mujeres)), ] #
head(DataConteoAlcades2018, 5)

#Ordenamos la base de datos según el 2018.
DataConteoAlcades2018 <- DataConteoAlcades2018[with(DataConteoAlcades2018, order(DataConteoAlcades2018$Porcentaje_Mujeres)), ] #
head(DataConteoAlcades2018, 5)

#Provincias donde no han postulado mujeres al cargo de alcades
NingunaMujerCandidataAlcade2018=DataConteoAlcades2018[which(DataConteoAlcades2018$Porcentaje_Mujeres == 0),]
NingunaMujerCandidataAlcade2018[, "Provincia"]

