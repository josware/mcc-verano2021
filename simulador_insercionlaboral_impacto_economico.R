### Simulación Inserción laboral e Impacto económico
#Revisa la lista de paquetes requeridos antes de mandarlos llamar
source("installRequiredPackages.txt")

library(dplyr)
library(reshape2)
library(tidyr)
library(rpivotTable)

#Neceistas definir en este archivo tu directorio de trabajo i.e. directorio.de.trabajo <- "SU_DIRECTORIO_DE_TRABAJO"
source("inivars.txt")
setwd(directorio.de.trabajo)
getwd()

## parámetros generales
#region <- "COSTA SUR"
#Region sierra.occidental
#region <- "COSTA SUR"
region <- "SIERRA OCCIDENTAL"
#Region sierra.occidental (parametro)
#region.param <- "costa.sur"
region.param <- "sierra.occidental"

## lectura de datos

## Carreras Mario Molina
load("carreras.mario.molina.total.sel.R")
head(carreras.mario.molina.total.sel)

## proyección de la región
load(paste0("proyeccion.agregada",region,".R"))
head(proyeccion.agregada)

## Datos educación superior
load("metricas.educacion.por.municipio.R")
metricas.educacion.por.municipio
## Datos educación media
load("metricas.educacion.media.por.municipio.R")
metricas.educacion.media.por.municipio
## Datos económicos

load("denue.base.72.carr.62.61.43.46.pob.op.R")
head(denue.base.72.carr.62.61.43.46.pob.op)

denue.base <- denue.base.72.carr.62.61.43.46.pob.op
rm(denue.base.72.carr.62.61.43.46.pob.op)
## vectores municipales
municipios.centro <- c(2,29,39,44,45,51,70,71,97,98,101,114,120,124)
municipios.sierra.occidental <- c(12,17,28,38,58,62,80,84)
municipios.sierra.amula <- c(11,32,34,37,52,54,88,90,102,106,110)
municipios.valles <- c(3,5,6,7,9,24,36,40,55,75,77,83,94,95)
municipios.costa.sur <- c(15,21,22,27,43,68)
municipios.costa.norte <- c(20, 67, 100) 
municipios.altos.norte <- c(35,53,64,72,73,91,109,116)
municipios.cienega <-c(13,16,18,30,33,47,50,63,66,96,105,107,123)
municipios.altos.sur <- c(1,8,46,48,60,74,78,93,111,117,118,125)
municipios.sureste <- c(26,49,56,57,59,65,69,85,87,112)
municipios.sur <- c(4,10,14,23,79,82,86,89,92,99,103,108,113,119,121,122)
municipios.norte <- c(19, 25, 31, 41, 42, 61, 76, 81, 104, 115)

#municipios.actual <- municipios.costa.sur
municipios.actual <- municipios.sierra.occidental
  
denue.base.sel <- denue.base[denue.base$cve_mun %in% municipios.actual,]
head(denue.base.sel)
nrow(denue.base.sel)
denue.base.sel$region <- region
denue.base.sel$`62.Total.Emplados.Dependientes`
mean(denue.base.sel$`62.Total.Emplados.Dependientes`,na.rm = T)
mean(denue.base.sel$`62.Total.Emplados.Dependientes`[!is.na(denue.base.sel$`62.Total.Emplados.Dependientes`)])
denue.base.sel.agg <- aggregate(x=denue.base.sel,by=list(denue.base.sel$region),FUN=mean,na.rm=T)
#?aggregate
head(denue.base.sel.agg)
summary(denue.base.sel)

denue.base.sel.agg$region <- NULL
denue.base.sel.agg$cve_mun <- NULL
colnames(denue.base.sel.agg)[1] <- "region"


#?numericInput

save(denue.base.sel.agg,file=paste0("denue.base.sel.agg",region,".R"))

colnames(denue.base.sel.agg)


datos.totales <- denue.base.sel.agg


## filtrado municipios

carreras.mario.molina.total.sim <- carreras.mario.molina.total.sel[carreras.mario.molina.total.sel$MUNICIPIO %in% municipios.actual,]
getwd()
nombres.carreras <- unique(carreras.mario.molina.total.sim$CARRERA)
save(nombres.carreras, file="nombres.carreras.R")

## funciones para generar las métricas educativas. 

metricas.educacion.por.municipio.sim <- metricas.educacion.por.municipio[metricas.educacion.por.municipio$region==region.param,]

metricas.educacion.media.por.municipio.sim <- metricas.educacion.media.por.municipio[metricas.educacion.media.por.municipio$region==region.param,]


## simulación

carrera <- "LICENCIATURA EN ADMINISTRACIÓN"

calcula.datos.educativos <- function(carrera) {
  carrera.simulacion <- carreras.mario.molina.total.sel[carreras.mario.molina.total.sel$CARRERA==carrera,]$categoria.carrera[1]
  datos.actuales.carrera <- carreras.mario.molina.total.sim[carreras.mario.molina.total.sim$categoria.carrera == carrera.simulacion,]
  datos.actuales.carrera <- datos.actuales.carrera[,c("nuevo.ingreso.TOTAL", "egresos.TOTAL", "titulados.TOTAL","categoria.carrera")]
  datos.actuales.carrera$categoria.carrera <- as.numeric(datos.actuales.carrera$categoria.carrera)
  str(datos.actuales.carrera)
  datos.actuales.carrera.agg <- aggregate(x=datos.actuales.carrera,by=list(datos.actuales.carrera$categoria.carrera),FUN=sum,a.rm=T)
  datos.actuales.carrera.agg$categoria.carrera
  matricula.actual <- datos.actuales.carrera.agg$nuevo.ingreso.TOTAL
  
  #municipios.costa.sur <- c(15,21,22,27,43,68)
  municipios.sierra.occidental <- c(12,17,28,38,58,62,80,84)
  municipios.actual <- municipios.sierra.occidental
  carreras.mario.molina.total.sim <- carreras.mario.molina.total.sel[carreras.mario.molina.total.sel$MUNICIPIO %in% municipios.actual,]
  metricas.educacion.por.municipio.sim <- carreras.mario.molina.total.sim[carreras.mario.molina.total.sim$categoria.carrera == carrera.simulacion,]
  datos.actuales.superio <- metricas.educacion.por.municipio.sim[,c("nuevo.ingreso.TOTAL", "egresos.TOTAL", "titulados.TOTAL","categoria.carrera")]
  datos.actuales.superio$categoria.carrera <- as.numeric(datos.actuales.superio$categoria.carrera)
  str(datos.actuales.superio)
  datos.actuales.superior.agg <- aggregate(x=datos.actuales.superio,by=list(datos.actuales.superio$categoria.carrera),FUN=sum,a.rm=T)
  competencia.directa <- datos.actuales.superior.agg$egresos.TOTAL
  metricas.educacion.media.por.municipio.sim <- metricas.educacion.media.por.municipio[metricas.educacion.media.por.municipio$region==region.param,]
  estudiantes.bachillerato <- metricas.educacion.media.por.municipio.sim$total.TOTAL.media.sup
  return(data.frame(matricula.actual=matricula.actual, competencia.directa=competencia.directa, estudiantes.bachillerato=estudiantes.bachillerato))
  
}

save(calcula.datos.educativos, file="calcula.datos.educativos.R")

datos.educativos <- calcula.datos.educativos(carrera)




## parámetros
anio.incial <- 2020
desercion.porcentaje <- .15
presupuesto <- 10000000
tiempo.anios <- 1
carreras.objetivo <- "Administracion"
cambio.oferta.educativa <- 100
colnames(proyeccion.agregada)
tiempo.carrera <- 4

unique(carreras.mario.molina.total.sel$CARRERA)
carrera.simulacion <- carreras.mario.molina.total.sel[carreras.mario.molina.total.sel$CARRERA=="LICENCIATURA EN ADMINISTRACIÓN",]$categoria.carrera[1]
# codigo.carrera.simulacion <- carreras.mario.molina.total.sel[carreras.mario.molina.total.sel$categoria.carrera=="50420",]




## variables independientes
#metricas.educacion.por.municipio
carreras.mario.molina.total.sim 

datos.actuales.carrera <- carreras.mario.molina.total.sim[carreras.mario.molina.total.sim$categoria.carrera == carrera.simulacion,]
datos.actuales.carrera <- datos.actuales.carrera[,c("nuevo.ingreso.TOTAL", "egresos.TOTAL", "titulados.TOTAL","categoria.carrera")]
datos.actuales.carrera$categoria.carrera <- as.numeric(datos.actuales.carrera$categoria.carrera)
str(datos.actuales.carrera)
datos.actuales.carrera.agg <- aggregate(x=datos.actuales.carrera,by=list(datos.actuales.carrera$categoria.carrera),FUN=sum,a.rm=T)
datos.actuales.carrera.agg$categoria.carrera

# rationale
# baseline

estudiantes.bachillerato <- metricas.educacion.media.por.municipio.sim$total.TOTAL.media.sup


datos.actuales.superio <- metricas.educacion.por.municipio.sim[,c("nuevo.ingreso.TOTAL", "egresos.TOTAL", "titulados.TOTAL","categoria.carrera")]
datos.actuales.superio$categoria.carrera <- as.numeric(datos.actuales.superio$categoria.carrera)
str(datos.actuales.superio)
datos.actuales.superior.agg <- aggregate(x=datos.actuales.superio,by=list(datos.actuales.superio$categoria.carrera),FUN=sum,a.rm=T)
datos.actuales.superior.agg$categoria.carrera



# factores negativos para la inserción laboral
competencia.directa <- datos.actuales.superior.agg$egresos.TOTAL
competencia.directa
matricula.actual <- datos.actuales.carrera.agg$nuevo.ingreso.TOTAL
matricula.actual

datos.totales$matricula.actual <- matricula.actual
datos.totales$estudiantes.bachillerato <- estudiantes.bachillerato

#ERROR
head(datos.totales)
competencia.directa
length(competencia.directa)

#ERROR
#datos.totales$competencia.directa <- competencia.directa
datos.totales$competencia.directa <- length(competencia.directa)
#ERROR
insercion.laboral <- -competencia.directa    * .1 
insercion.laboral
# -%incremento

## variables dependientes
insercion.laboral

#ERROR
#crecimiento.economico


# factores positivos para la inserción laboral
# % bachillerato a carrera? 



metricas.educacion.por.municipio.sim[metricas.educacion.por.municipio.sim$categoria.carrera==carrera.simulacion,]
metricas.educacion.por.municipio.sim <- metricas.educacion.por.municipio.sim[metricas.educacion.por.municipio.sim$categoria.carrera == carrera.simulacion,]




metricas.educacion.por.municipio.sim 

metricas.educacion.media.por.municipio.sim


#metricas.educacion.media.por.municipio

denue.base.sel.agg
#denue.base.sel.agg


#Fixing Termino ecuacion 

clean.terminos.ecuacion <- function() {
  termino <- "J05"
  signo <- 1
  coeficiente <- 25
  termino.nuevo <- data.frame(termino=termino, signo=signo, coeficiente=coeficiente)
  terminos.ecuacion <- termino.nuevo
  terminos.ecuacion <- terminos.ecuacion[-1,]
  save(terminos.ecuacion,file="terminos.ecuacion.R")
}

clean.terminos.ecuacion ()

load("terminos.ecuacion.R")
termino <- "J05"
signo <- 1
coeficiente <- 25
termino.nuevo <- data.frame(termino=termino, signo=signo, coeficiente=coeficiente)
terminos.ecuacion <- termino.nuevo
terminos.ecuacion <- terminos.ecuacion[-1,]
save(terminos.ecuacion,file="terminos.ecuacion.R")
terminos.ecuacion

#termino.nuevo <- data.frame(termino=termino, signo=signo, coeficiente=coeficiente)
#terminos.ecuacion <- rbind(terminos.ecuacion, termino.nuevo)




#terminos.ecuacion <- terminos.ecuacion[-1,]
#save(terminos.ecuacion, file="terminos.ecuacion.R")


## Ecuaciones 
# Capital_1=(1-s)*Output_0+Capital_0
# Labour_1=(1+n)*labour_0. 
# Output_1=Capital_1*Labour_1*k

datos.completos <- cbind(datos.educativos,denue.base.sel.agg)
datos.completos

save(datos.completos, file="datos.completos.R")


###NPV: Net Present Value

terminos.ecuacion


aplica.formula <- function(terminos.ecuacion, n) {
  calculo.intermedio <- 0
  for (i in 1:nrow(terminos.ecuacion)) {
    valor.termino <- datos.completos[[terminos.ecuacion[i,"termino"]]]
    valor.termino <- valor.termino * terminos.ecuacion[i,"signo"] * terminos.ecuacion[i,"coeficiente"] * n
    #print(valor.termino)
    calculo.intermedio <- calculo.intermedio + valor.termino
    #print(calculo.intermedio)
    }
  return(calculo.intermedio)
}

save(aplica.formula, file="aplica.formula.R")



proyeccion.agregada

## proyeccion carrera

corre.simulacion <- function(n) {
  Output <- aplica.formula(terminos.ecuacion, 1)
  ### deben resolver este mapeo, ahorita está hardcodeado. 
  datos.proyeccion <- proyeccion.agregada$Administracion[11:21]
  generator <- function(steps,n){
    for(i in 2:(steps)){
      Output[i] <- mean(Output[i-1]+aplica.formula(terminos.ecuacion, n)) + datos.proyeccion[i] ## cómo usan los datos de la proyección?
      }
    return(data.frame(Output = Output))
    }
  result <- generator(steps=n, n=1.05)
  return(data.frame(result=result))

}

save(corre.simulacion, file="corre.simulacion.R")


load("terminos.ecuacion.R")
terminos.ecuacion
if(nrow(terminos.ecuacion) <1) {
  termino <- "J05"
  signo <- 1
  coeficiente <- 25
  termino.nuevo <-
    data.frame(termino = termino,
               signo = signo,
               coeficiente = coeficiente)
  terminos.ecuacion <- rbind(terminos.ecuacion, termino.nuevo)
}
#rm(terminos.ecuacion)
nrow(terminos.ecuacion)
terminos.ecuacion

load("corre.simulacion.R")
load("aplica.formula.R")
load("terminos.ecuacion.R")
load("datos.completos.R")
region <- "SIERRA OCCIDENTAL"
load(paste0("proyeccion.agregada",region,".R"))

corre.simulacion(5)



y <- corre.simulacion(5)
class(y)
plot(1:5,y$Output, type="b", col = "purple", cex = .9)
?plot

hist(y$Output, n.unique = 3, nclass = "compute")

library(FinancialMath)

NPV(cf0=100,cf=c(50,40),times=c(3,5),i=.01)
NPV(cf0=100,cf=50,times=3,i=.05)
NPV(cf0=100,cf=c(50,60,10,20),times=c(1,5,9,9),i=.045)


?NPV



generator <- function(steps,s, n, k){
  for(i in 2:(steps)){
    Capital[i] <- (1-s)*Output[i-1]+Capital[i-1]
    Labour[i] <- n*Labour[i-1]
    Output[i] <- Capital[i]*Labour[i]*k
  }
  return(data.frame(Output = Output, Capital = Capital, Labour = Labour))
}

#print(generator(steps=10, s=.01, n=1.002, k=1.01))



