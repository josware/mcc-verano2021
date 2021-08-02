# vectores regiones
# partición por región
# municipios.centro <- c(2,29,39,44,45,51,70,71,97,98,101,114,120,124)
municipios.costa.sierra.occidental <- c(12,17,28,38,58,62,80,84)
# municipios.sierra.amula <- c(11,32,34,37,52,54,88,90,102,106,110)
# municipios.valles <- c(3,5,6,7,9,24,36,40,55,75,77,83,94,95)
# municipios.costa.sur <- c(15,21,22,27,43,68)
# municipios.costa.norte <- c(20, 67, 100) 
# municipios.altos.norte <- c(35,53,64,72,73,91,109,116)
# municipios.cienega <-c(13,16,18,30,33,47,50,63,66,96,105,107,123)
# municipios.altos.sur <- c(1,8,46,48,60,74,78,93,111,117,118,125)
# municipios.sureste <- c(26,49,56,57,59,65,69,85,87,112)
# municipios.sur <- c(4,10,14,23,79,82,86,89,92,99,103,108,113,119,121,122)
# municipios.norte <- c(19, 25, 31, 41, 42, 61, 76, 81, 104, 115)
# end vectores regiones
region.actual.municipios <- municipios.costa.sierra.occidental


denue.2021 <- read.csv("denue_inegi_14_2021_may.csv",stringsAsFactors = F, fileEncoding = "iso-8859-1") 
columnas.elegidas <- c("id","nom_estab","codigo_act","nombre_act","cve_mun","municipio","fecha_alta","per_ocu")
denue.columnas.seleccion <- denue.2021[,columnas.elegidas]

carreras.l4.data <- read.csv("mapeocarreral4scian.csv",stringsAsFactors = F)
region <- "COSTA SIERRA OCCIDENTAL"

#Vars genera.datos.prediccion.l4
limite.superior.relevancia <- 2
ventana.prediccion <- 15.5
anio.prediccion <- 2030
proyecciones <- NULL


denue.columnas.seleccion.demanda <- denue.columnas.seleccion
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$per_ocu == "0 a 5 personas", "demanda"] <- 1
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$per_ocu == "6 a 10 personas", "demanda"] <- 2
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$per_ocu =="11 a 30 personas", "demanda"] <- 4
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$per_ocu == "31 a 50 personas", "demanda"] <- 7
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$per_ocu == "51 a 100 personas", "demanda"] <- 14
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$per_ocu == "101 a 250 personas", "demanda"] <- 30
denue.columnas.seleccion.demanda[denue.columnas.seleccion.demanda$per_ocu == "251 y más personas", "demanda"] <- 50

table(denue.columnas.seleccion.demanda$demanda)

denue.columnas.seleccion.demanda$subscian <- substr(as.character(denue.columnas.seleccion.demanda$codigo_act),1,3)
# esta es la columna para hacer el mapeo
denue.columnas.seleccion.demanda$l4 <-substr(as.character(denue.columnas.seleccion.demanda$codigo_act),1,4)

head(denue.columnas.seleccion.demanda)
nrow(denue.columnas.seleccion.demanda)
denue.columnas.seleccion.demanda

denue.base <- denue.columnas.seleccion.demanda
datos.region.actual <- denue.base[denue.base$cve_mun %in% region.actual.municipios,]

denue.columnas.seleccion.demanda 



mario.molina.relevante <- function(region) {
  if (region == "CENTRO") {
    return(c("TECMM.Zapopan", "TECMM.Zapotlanejo", "UTZMG","ITTJ.TECNM.Tlajomulco"))
  }
  if (region == "VALLES") {
    return(c("TECMM.Tequila","TECMM.Tala","TECMM.Cocula"))
  }
  else {
    case_when(
      region == "ALTOS SUR" ~ c("TECMM.Arandas"),
      region == "COSTA SUR" ~ c("TECMM.La.Huerta"),
      region == "COSTA NORTE" ~ c("TECMM.Puerto.Vallarta"),
      region == "SIERRA AMULA" ~ c("TECMM.El.Grullo"),
      region == "COSTA SIERRA OCCIDENTAL" ~ c("TECMM.Mascota"),
      region == "NORTE" ~ c("TECMM.Tala"),
      region == "CIENEGA" ~ c("TECMM.Chapala"),
      region=="SUR"~c("TECMM.Tamazula"),
      region=="SURESTE"~c("TECMM.Tamazula"),
      region == "ALTOS NORTE" ~ c("TECMM.Lagos.de.Moreno"),
      TRUE ~ c("INDEFINIDO")
    )
  }
}


listado.carreras.por.centros <- function(tecnologicos) {
  vector.resultado <- vector()
  if (tecnologicos[1] == "INDEFINIDO") return(vector.resultado)
  for (i in 1:length(tecnologicos)) {
    tecnologico.actual <- tecnologicos[i]
    carreras.relevantes.a.la.region <- carreras.l4.data[,tecnologico.actual]
    for(i in 1:length(carreras.relevantes.a.la.region)) {
      if (carreras.relevantes.a.la.region[i]=="P")
        vector.resultado <- c(vector.resultado, carreras.l4.data[i,1])
    }
  }
  return(vector.resultado)
}


datos.para.proyeccion.carrera.region <- function(datos.fuente, carrera, region) {
  # este es el archivo de mapeo que trabajamos al inicio
  ventana.prediccion <- 15.5
  anio.prediccion <- 2030
  archivo.mapeo.carreras.l4 <- "mapeocarreral4scian.csv"
  limite.superior.relevancia <- 2
  colnames(datos.fuente)
  carreras.l4.data$Carrera
  carreras.l4.data <- read.csv(archivo.mapeo.carreras.l4,stringsAsFactors = F)
  colnames(carreras.l4.data)
  #head(denue.14.sel)
  vector.de.trabajo <- carreras.l4.data[carreras.l4.data$Carrera==carrera,]
  cod.rel.vec.tra <- codigos.relevantes.a.la.carrera(vector.de.trabajo)
  denue.para.l4 <- datos.fuente
  denue.14.sel <- denue.para.l4[denue.para.l4$l4 %in% cod.rel.vec.tra,]
  denue.14.sel$year <- substr(as.character(denue.14.sel$fecha_alta),1,4)
  # parece haber datos desde el 2010, en altos sur, prospectiva, 2010-2019
  denue.14.sel$region <- region
  denue.14.sel$carrera <- carrera
  head(denue.14.sel)
  pred.data <- genera.datos.prediccion.l4(denue.14.sel,carrera,F,region)
  return(pred.data)
}



carreras.relevantes <- listado.carreras.por.centros(unique(mario.molina.relevante(region)))
carreras.relevantes <- unique(carreras.relevantes)
carreras.relevantes

codigos.relevantes.a.la.carrera <- function(vec.trab) {
  cod.rel.vec <- vector()
  for (i in 19:length(vec.trab)) {
    # print(vec.trab[i])
    if (vec.trab[i] >= limite.superior.relevancia) {
      cod.rel.vec <- c(cod.rel.vec,substring(names(vec.trab[i]),2))
    }
  }
  return(cod.rel.vec)
}




genera.datos.prediccion.l4 <- function(datos.melt, filtro, subsector, region) {
  # filtrado inicial
  # filtrado de datos.melt
  prospectiva.vocacionamiento <- datos.melt
  prospectiva.sector <- prospectiva.vocacionamiento %>%
    group_by(year) %>%
    summarise(total.prospectiva = sum(demanda))
  # 2020 tiene datos incompletos
  prospectiva.sector <- data.frame(prospectiva.sector)
  prospectiva.sector <- prospectiva.sector[prospectiva.sector$year!=2020,] 
  # TODO: create lookup table
  prospectiva.sector$region <- region
  prospectiva.sel <- prospectiva.sector[order(prospectiva.sector$year),c("year","total.prospectiva")]
  prospectiva.sel[prospectiva.sel$year=="2010","total.prospectiva"] <- prospectiva.sel[prospectiva.sel$year=="2010","total.prospectiva"] / 10
  prospectiva.aux <- prepare.interp(prospectiva.sel)
  # interpolar
  prospectiva.int <- approx(prospectiva.aux)
  prospectiva.int <- data.frame(prospectiva.int)
  # modelo lineal sector/subsector
  lm.prospectiva <- lm(y ~ x, prospectiva.int)
  # gr??fica, N data points en el futuro
  nrow(prospectiva.int)
  tiempo.nuevo <- seq(10.1, ventana.prediccion, by=.1)
  tiempo.prediccion <- data.frame(x=tiempo.nuevo)
  demanda.prediccion <- predict(lm.prospectiva, newdata = tiempo.prediccion)
  #auxiliar para identificar los valores reales y los pronosticados
  style <- c(rep(1,nrow(prospectiva.int)), rep(4, length(demanda.prediccion)))
  prospectiva.full.data <- c(prospectiva.int$y,demanda.prediccion)
  # empaca los datos en un data.frame
  prediccion.ready.df <- data.frame(x=rep(2010:anio.prediccion,1,each=5),y=prospectiva.full.data,
                                    style=style)
  return(prediccion.ready.df)
}

prepare.interp <- function(data) {
  final.vector <- vector()
  for (i in 2010:2020) {
    if (nrow(data[data$year==as.character(i),])==0)
      final.vector[i-2009] <- 0
    else
      final.vector[i-2009] <- data[data$year==as.character(i),]$total.prospectiva
  }
  return(final.vector)
}


for (i in carreras.relevantes) { 
  print(paste("Procesando Region:",region," Carrera: ",i,sep=" "))
  datos.proyeccion <- datos.para.proyeccion.carrera.region(datos.region.actual,i,region)
  if (is.null(proyecciones)) {
    proyecciones <- datos.proyeccion
    colnames(proyecciones) <- c("periodo",i,"style")
  } else {
    proyecciones[[i]] <- datos.proyeccion$y
  }
}



proyecciones <- rbind(proyecciones,datos.proyeccion$y)
head(proyecciones)


proyecciones.agregadas <- aggregate(x=proyecciones,by=list(proyecciones$periodo),FUN=mean)
colnames(proyecciones.agregadas) <- gsub(" ", ".",colnames(proyecciones.agregadas))
colnames(proyecciones.agregadas) <- gsub("\\(", "",colnames(proyecciones.agregadas))
colnames(proyecciones.agregadas) <- gsub("\\)", "",colnames(proyecciones.agregadas))
proyecciones.agregadas$Group.1 <- NULL
proyecciones.agregadas
