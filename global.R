#Loads
load("carreras.mario.molina.total.sel.R")

#Vars
region <- "SIERRA OCCIDENTAL"
region.param <- "sierra.occidental"
municipios.sierra.occidental <- c(12,17,28,38,58,62,80,84)

#Objects
carreras.mario.molina.total.sim <- carreras.mario.molina.total.sel[carreras.mario.molina.total.sel$MUNICIPIO %in% municipios.actual,]


load("aplica.formula.R")
load("calcula.datos.educativos.R")
load("carreras.mario.molina.total.sel.R")
load("corre.simulacion.R")
load("datos.completos.R")
load("denue.base.sel.aggCOSTA SUR.R")0
load("metricas.educacion.por.municipio.R")
load("metricas.educacion.media.por.municipio.R")
load("nombres.carreras.R")
load("proyeccion.agregadaCOSTA SUR.R")
load("proyeccion.agregadaSIERRA OCCIDENTAL.R")
load("proyeccionesCOSTA SIERRA OCCIDENTAL.R")
load("terminos.ecuacion.R")

#Vars
region <- "SIERRA OCCIDENTAL"
region.param <- "sierra.occidental"

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

municipios.actual <- municipios.sierra.occidental

# denue.base.sel <- denue.base[denue.base$cve_mun %in% municipios.actual,]
# head(denue.base.sel)
# nrow(denue.base.sel)
# denue.base.sel$region <- region
# denue.base.sel$`62.Total.Emplados.Dependientes`
# mean(denue.base.sel$`62.Total.Emplados.Dependientes`,na.rm = T)
# mean(denue.base.sel$`62.Total.Emplados.Dependientes`[!is.na(denue.base.sel$`62.Total.Emplados.Dependientes`)])
# denue.base.sel.agg <- aggregate(x=denue.base.sel,by=list(denue.base.sel$region),FUN=mean,na.rm=T)
# #?aggregate
# head(denue.base.sel.agg)
# summary(denue.base.sel)
# 
# denue.base.sel.agg$region <- NULL
# denue.base.sel.agg$cve_mun <- NULL
# colnames(denue.base.sel.agg)[1] <- "region"
# 
# 
# #?numericInput
# 
# save(denue.base.sel.agg,file=paste0("denue.base.sel.agg",region,".R"))
# 
# colnames(denue.base.sel.agg)
# 
# 
# datos.totales <- denue.base.sel.agg


## filtrado municipios

carreras.mario.molina.total.sim <- carreras.mario.molina.total.sel[carreras.mario.molina.total.sel$MUNICIPIO %in% municipios.actual,]


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

