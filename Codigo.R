
#########################################################################
#                  R LADIES LIMA

#########################################################################

#                   TALLER :  DATOS ELECTORALES CON R

#########################################################################

## PARTICIPACIÓN DE LAS MUJERES EN ELECCIONES SUBNACIONALES - NIVEL PROVINCIAL


### ABRIR LAS BASES

library(readxl)
Candidatos2002 <- read_excel("MUNICIPAL PROVINCIAL 2002\\ERM2002_Candidatos_Provincial.xlsx")
Candidatos2006 <- read_excel("MUNICIPAL PROVINCIAL 2006\\ERM2006_Candidatos_Provincial.xlsx")
Candidatos2010 <- read_excel("MUNICIPAL PROVINCIAL 2010\\ERM2010_Candidatos_Provincial.xlsx")
Candidatos2014 <- read_excel("MUNICIPAL PROVINCIAL 2014\\ERM2014_Candidatos_Provincial.xlsx")
Candidatos2018 <- read_excel("MUNICIPAL PROVINCIAL 2018\\ERM2018_Candidatos_Provincial.xlsx")

Ubigeo <- read_excel("Ubigeo.xlsx")

### VERIFICO EL NOMBRE DE LAS VARIABLES DE LAS BASES DE DATOS
#names es el comando que pide el nombre de las variables de las bases de datos
names(Candidatos2002)
names(Candidatos2006)
names(Candidatos2010)
names(Candidatos2014)
names(Candidatos2018)

### AÑADIR LA VARIABLE AÑO DE ELECCION

#Creo una nueva variable "anio" que indica el año de elección en cada base de datos
Candidatos2002$anio=2002
Candidatos2006$anio=2006
Candidatos2010$anio=2010
Candidatos2014$anio=2014
Candidatos2018$anio=2018

### UBIGEO

#Utilizo el comando merge para juntar las bases de datos de los candidatos de las elecciones provinciales y la de ubigeo 

Candidatos2002_ubigeo=merge(Candidatos2002,Ubigeo,by.x="Provincia",by.y="Provincia")
Candidatos2006_ubigeo=merge(Candidatos2006,Ubigeo,by.x="Provincia",by.y="Provincia")
Candidatos2010_ubigeo=merge(Candidatos2010,Ubigeo,by.x="Provincia",by.y="Provincia")
Candidatos2014_ubigeo=merge(Candidatos2014,Ubigeo,by.x="Provincia",by.y="Provincia")
Candidatos2018_ubigeo=merge(Candidatos2018,Ubigeo,by.x="Provincia",by.y="Provincia")


### JUNTO TODAS LAS BASES DE DATOS EN UNA SOLA BASE DE DATOS

lista=list(Candidatos2002_ubigeo,Candidatos2006_ubigeo,Candidatos2010_ubigeo,Candidatos2014_ubigeo,Candidatos2018_ubigeo)
# rbind= junta filas de distintas bases de datos
# do.call =  repite una acción del código
# as.data.frame =  como base de datos
Candidatos=as.data.frame(do.call(rbind,lista))

names(Candidatos)

# Variable categórica

Candidatos$Sexo=as.factor(Candidatos$Sexo)
Candidatos$Cargo=as.factor(Candidatos$Cargo)
Candidatos$anio<- as.Date(as.character(Candidatos$anio), format = "%Y")

library(lubridate)
Candidatos$anio<-year(Candidatos$anio)
###PREGUNTA 1: ¿Se ha incrementado la participación política de las mujeres en las elecciones subnacionales- provinciales?
### Tablas generales

names(Candidatos)


tablaresumen=table(Candidatos$Sexo,Candidatos$anio)

prop.table(table(Candidatos$Sexo,Candidatos$anio),2)*100
prop.table(table(Candidatos$Sexo,Candidatos$anio),2)

barplot(prop.table(tablaresumen,2)*100,
        main = "Resultados de la encuesta",
        xlab = "Frecuencias relativas (%)",
        legend = T,
        width = 0.5, ylim = c(0, 2.5),
        horiz = T)

#INSUMOS PARA GRÁFICO

#tablaresumen=table(Candidatos$Sexo,Candidatos$anio)
as.data.frame(tablaresumen)
tabla_general=aggregate(Freq ~ Var2, data = tablaresumen, sum)
Candidatos_tabla1=merge(tablaresumen,tabla_general,by=c("Var2"))
Candidatos_tabla1$porcentaje=Candidatos_tabla1$Freq.x/Candidatos_tabla1$Freq.y
names(Candidatos_tabla1) <- c("anio", "Sexo","Frecuencia","Total_Candidatos","Porcentaje")

Candidatos_tabla1$anio=as.numeric(Candidatos_tabla1$anio)

##GRÁFICO ESTÁTICO

#install.packages("dplyr")
library(dplyr)

#install.packages("ggplot2")
library(ggplot2)


ggplot(Candidatos_tabla1,aes(x=anio, y=Porcentaje, group=Sexo,
                             color=Sexo))+
  geom_line(size=3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = seq(2002, 2018, by = 4))+ 
  labs(x="año de la elección",y="Porcentaje de Candidatos",title="Gráfico animado") 



## GRAFICO ANIMADO


#install.packages("gganimate")
library(gganimate)


#install.packages("gifski")
library(gifski)

#install.packages("av")
library(av)

ggplot(Candidatos_tabla1,aes(x=anio, y=Porcentaje, group=Sexo,
                             color=Sexo))+
  geom_line(size=3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = seq(2002, 2018, by = 4))+ 
  labs(x="año de la elección",y="Porcentaje de Candidatos",title="Gráfico animado de candidaturas a nivel provincial" ) + 
  transition_reveal(anio)



#PREGUNTA 2 : ¿A qué cargos postulan más las mujeres?

#INSUMOS PARA GRÁFICO

tabla=as.data.frame(table(Candidatos$Sexo,Candidatos$anio,Candidatos$Cargo))

tabla_frecuencia=aggregate(Freq ~ Var2+Var3, data = tabla, sum)

Candidatos_tabla2=merge(tabla,tabla_frecuencia,by=c("Var2","Var3"))

Candidatos_tabla2$porcentaje=Candidatos_tabla2$Freq.x/Candidatos_tabla2$Freq.y

names(Candidatos_tabla2) <- c("anio", "Cargo","Sexo","Frecuencia","Total_Candidatos","Porcentaje")

Candidatos_tabla2$anio=as.numeric(Candidatos_tabla2$anio)

##GRÁFICO ESTÁTICO

ggplot(Candidatos_tabla2,aes(x=anio, y=Porcentaje, group=Sexo,
           color=Sexo))+
  geom_line(size=3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = seq(2002, 2018, by = 4))+ 
  labs(x="año de la elección",y="Porcentaje de Candidatos",title="Gráfico animado") + 
  facet_wrap(~Cargo,ncol=2,strip.position = "top")

# GRÁFICO ANIMADO

ggplot(Candidatos_tabla2,aes(x=anio, y=Porcentaje, group=Sexo,
             color=Sexo))+
  geom_line(size=3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_continuous(breaks = seq(2002, 2018, by = 4))+ 
  labs(x="año de la elección",y="Porcentaje de Candidatos",title="Gráfico animado") + 
  facet_wrap(~Cargo,ncol=2,strip.position = "top")+
  transition_reveal(anio)





#PREGUNTA 3 : ¿En qué provincias las mujeres postulan más a la alcaldía?
# Porcentajes de mujeres que se presentan a la alcaldia provincial

#Base de solo candidatos a alcades

CandidatosAalcaldes <- subset(Candidatos, Candidatos$Cargo == "ALCALDE PROVINCIAL")

#Número de candidatos
install.packages("dplyr")
library(dplyr)

CandidatosAalcaldes=CandidatosAalcaldes %>% group_by(UBIGEO,anio) %>% mutate(counter = row_number()) 

NumeroCandidatosAlcaldesAnio=aggregate(counter ~ UBIGEO+anio, data = CandidatosAalcaldes, max)
names(NumeroCandidatosAlcaldesAnio)
colnames(NumeroCandidatosAlcaldesAnio)[3] = "MaxCandidatos"


#CandidatosAalcaldes=merge(CandidatosAalcaldes,NumeroCandidatosAlcaldesAnio,by=c("UBIGEO","anio"))


Candidatas <- subset(CandidatosAalcaldes, CandidatosAalcaldes$Sexo == "Femenino")
Candidatas=Candidatas %>% group_by(UBIGEO,anio) %>% mutate(counter = row_number()) 


NumeroCandidatAsAlcaldesAnio=aggregate(counter ~ UBIGEO+anio, data = Candidatas, max)


data=merge(NumeroCandidatosAlcaldesAnio,NumeroCandidatAsAlcaldesAnio,by=c("UBIGEO","anio"),all.x = TRUE)

data[is.na(data)] = 0

data$PorcentajeCandidaturas=data$counter/data$MaxCandidatos

data=merge(data,Ubigeo,by=c("UBIGEO"))

# 
install.packages("openxlsx") 
library(openxlsx) 
write.xlsx(data,"data.xlsx")


#PREGUNTA 4 : ¿Cual es el ratio de candidatas y electores?

PadronElectoral2002 <- read_excel("Documentos/12 PUCP-Docencia/R ladies/Provincias/MUNICIPAL PROVINCIAL 2002\\ERM2002_Padron_Provincial.xlsx")
PadronElectoral2006 <- read_excel("Documentos/12 PUCP-Docencia/R ladies/Provincias/MUNICIPAL PROVINCIAL 2006\\ERM2006_Padron_Provincial.xlsx")
PadronElectoral2010 <- read_excel("Documentos/12 PUCP-Docencia/R ladies/Provincias/MUNICIPAL PROVINCIAL 2010\\ERM2010_Padron_Provincial.xlsx")
PadronElectoral2014 <- read_excel("Documentos/12 PUCP-Docencia/R ladies/Provincias/MUNICIPAL PROVINCIAL 2014\\ERM2014_Padron_Provincial.xlsx")
PadronElectoral2018 <- read_excel("Documentos/12 PUCP-Docencia/R ladies/Provincias/MUNICIPAL PROVINCIAL 2018\\ERM2018_Padron_Provincial.xlsx")


names(PadronElectoral2002) <- c("Region", "Provincia","Distrito","NumeroElectores","ElectoresVarones","Porcentaje_ElectoresVarones","ElectoresMujeres","Porcentaje_ElectoresMujeres","ElectoresJovenes","Porcentaje_ElectoresJovenes","ElectoresMayores70anios","Porcentaje_ElectoresMayores70anios")
names(PadronElectoral2006) <- c("Region", "Provincia","Distrito","NumeroElectores","ElectoresVarones","Porcentaje_ElectoresVarones","ElectoresMujeres","Porcentaje_ElectoresMujeres","ElectoresJovenes","Porcentaje_ElectoresJovenes","ElectoresMayores70anios","Porcentaje_ElectoresMayores70anios")
names(PadronElectoral2010) <- c("Region", "Provincia","Distrito","NumeroElectores","ElectoresVarones","Porcentaje_ElectoresVarones","ElectoresMujeres","Porcentaje_ElectoresMujeres","ElectoresJovenes","Porcentaje_ElectoresJovenes","ElectoresMayores70anios","Porcentaje_ElectoresMayores70anios")
names(PadronElectoral2014) <- c("Region", "Provincia","Distrito","NumeroElectores","ElectoresVarones","Porcentaje_ElectoresVarones","ElectoresMujeres","Porcentaje_ElectoresMujeres","ElectoresJovenes","Porcentaje_ElectoresJovenes","ElectoresMayores70anios","Porcentaje_ElectoresMayores70anios")
names(PadronElectoral2018) <- c("Region", "Provincia","Distrito","NumeroElectores","ElectoresVarones","Porcentaje_ElectoresVarones","ElectoresMujeres","Porcentaje_ElectoresMujeres","ElectoresJovenes","Porcentaje_ElectoresJovenes","ElectoresMayores70anios","Porcentaje_ElectoresMayores70anios")



PadronElectoral2002 =aggregate(cbind(NumeroElectores,ElectoresVarones,ElectoresMujeres,ElectoresJovenes,ElectoresMayores70anios)~Provincia,data=PadronElectoral2002,sum)
PadronElectoral2006 =aggregate(cbind(NumeroElectores,ElectoresVarones,ElectoresMujeres,ElectoresJovenes,ElectoresMayores70anios)~Provincia,data=PadronElectoral2006,sum)
PadronElectoral2010 =aggregate(cbind(NumeroElectores,ElectoresVarones,ElectoresMujeres,ElectoresJovenes,ElectoresMayores70anios)~Provincia,data=PadronElectoral2010,sum)
PadronElectoral2014 =aggregate(cbind(NumeroElectores,ElectoresVarones,ElectoresMujeres,ElectoresJovenes,ElectoresMayores70anios)~Provincia,data=PadronElectoral2014,sum)
PadronElectoral2018 =aggregate(cbind(NumeroElectores,ElectoresVarones,ElectoresMujeres,ElectoresJovenes,ElectoresMayores70anios)~Provincia,data=PadronElectoral2018,sum)

PadronElectoral2002$anio=2002
PadronElectoral2006$anio=2006
PadronElectoral2010$anio=2010
PadronElectoral2014$anio=2014
PadronElectoral2018$anio=2018

lista=list(PadronElectoral2002,PadronElectoral2006,PadronElectoral2010,PadronElectoral2014,PadronElectoral2018)
PadronElectoralSerie=as.data.frame(do.call(rbind,lista))

PadronElectoral_ubigeo=merge(PadronElectoralSerie,Ubigeo,by.x="Provincia",by.y="Provincia")

PadronElectoral_ubigeo$anio=as.factor(PadronElectoral_ubigeo$anio)
tablaresumen1=aggregate(cbind(NumeroElectores,ElectoresVarones,ElectoresMujeres,ElectoresJovenes,ElectoresMayores70anios)~anio,data=PadronElectoral_ubigeo,sum)
tablaresumen1$porcentajemujeres=tablaresumen1$ElectoresMujeres/tablaresumen1$NumeroElectores
