## app.R ##
#Revisa la lista de paquetes requeridos antes de mandarlos llamar
source("installRequiredPackages.txt")

library(shiny)
library(shinydashboard)
library(dplyr)
library(treemap)
library(reshape2)
library(ggplot2)
library(shinythemes)
library(rpivotTable)

#Neceistas definir en este archivo tu directorio de trabajo i.e. directorio.de.trabajo <- "SU_DIRECTORIO_DE_TRABAJO"
source("inivars.txt")
#source("simReqs.R")
setwd(directorio.de.trabajo)

Sys.setlocale("LC_ALL", 'es_ES')


#Vars
titulo <- "Simulador SIERRA OCCIDENTAL 1.0"
region <- "SIERRA OCCIDENTAL"
region.param <- "sierra.occidental"
municipios.sierra.occidental <- c(12,17,28,38,58,62,80,84)
municipios.actual <- municipios.sierra.occidental
#carrera <- "INGENIERÍA EN SISTEMAS COMPUTACIONALES"
carrera <- "LICENCIATURA EN ADMINISTRACIÓN"

#Loads
load("nombres.carreras.R")
#load("carreras.mario.molina.total.sel.R")
load("metricas.educacion.por.municipio.R")
load("metricas.educacion.media.por.municipio.R")
#load("calcula.datos.educativos.R")
load("denue.base.sel.aggCOSTA SUR.R")
#load("terminos.ecuacion.R")
load("datos.completos.R")
load("proyeccion.agregadaCOSTA SUR.R")
#load("aplica.formula.R")
load("corre.simulacion.R")
load("proyeccionesCOSTA SIERRA OCCIDENTAL.R")
load("corre.simulacion.R")
#load("aplica.formula.R")
#load("terminos.ecuacion.R")
load("datos.completos.R")
load(paste0("proyeccion.agregada",region,".R"))

#CSV
carreras.mario.molina.total.sel <- read.csv("carreras.mario.molina.total.sel.csv")

#Objects
carreras.mario.molina.total.sim <- carreras.mario.molina.total.sel[carreras.mario.molina.total.sel$MUNICIPIO %in% municipios.actual,]


#FUNCIONES
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

# clean.terminos.ecuacion <- function() {
#   termino <- "J05"
#   signo <- 1
#   coeficiente <- 25
#   termino.nuevo <- data.frame(termino=termino, signo=signo, coeficiente=coeficiente)
#   terminos.ecuacion <- termino.nuevo
#   terminos.ecuacion <- terminos.ecuacion[-1,]
#   #save(terminos.ecuacion,file="terminos.ecuacion.R")
#   session$userData$terminos.ecuacion <- terminos.ecuacion
# }


datos.educativos <- calcula.datos.educativos(carrera)
datos.completos <- cbind(datos.educativos,denue.base.sel.agg)


#termino <- "BASE"
#signo <- 1
#coeficiente <- 1
#termino.nuevo <- data.frame(termino=termino, signo=signo, coeficiente=coeficiente)
#terminos.ecuacion <- termino.nuevo
#terminos.ecuacion <- terminos.ecuacion[-1,]
#session$userData$terminos.ecuacion <- terminos.ecuacion


#Prep
#clean.terminos.ecuacion ()
dbHeader <- dashboardHeader(title = titulo)

#,
# tags$li(a(href = '', onclick="window.close()",
#           icon("power-off"),
#           title = "Regresar a p??gina de inicio"),
#         class = "dropdown")

ui <- dashboardPage(skin="purple",
  dbHeader,
  dashboardSidebar(
    sidebarMenu(
      menuItem("Generador de Ecuaciones", tabName = "generador", icon = icon("angle-double-right")),
      #menuItem("Simulacion", tabName = "simulacion", icon = icon("bullhorn"))
      #menuItem("Simulacion", tabName = "simulacion", icon = icon("bug"))
      #menuItem("Simulacion", tabName = "simulacion", icon = icon("cat"))
      menuItem("Simulacion", tabName = "simulacion", icon = icon("desktop")),
      menuItem("Comercio al Por Mayor", tabName = "comercio_x_mayor", icon = icon("truck"))
      #menuItem("Comercio al Por Mayor", tabName = "comercio_x_mayor", icon = icon("dragon"))
      #https://fontawesome.com/v5.15/icons?d=gallery&p=2&m=free
      
    )
  ),
  dashboardBody(
    tabItems(
     tabItem(tabName = "generador",
              fluidRow(
                box(
                  title = "Filtros",
                  selectInput(inputId = "carrera", label = "Carrera", choices = nombres.carreras, selected = 1),
                  actionButton("defineparametros", "Definir Parametros")
                ),
                box(
                  title = "Agregar Termino",
                  selectInput(inputId = "var.indepediente", label = "Variable", choices = NULL, selected = 1),
                  checkboxInput("negativo", "Es inversamente proporcional", value = FALSE, width = NULL),
                  numericInput("coeficiente", "Coeficiente", 10, min = 0, max = 100000),
                  actionButton("addtermtoequation", "Agrega Termino"),
                  actionButton("cleanequation", "Limpiar")
                  
                )
              ),
              fluidRow(
                box( 
                  title = "Detalles Carrera",
                  width = 12,
                  tableOutput("parametroseducativos")
                )
              ),
             fluidRow(
               box( 
                 title = "Parametros Ecuación",
                 width = 12,
                 tableOutput("terminosecuacion") )
               )
             # fluidRow(
             #   tableOutput("cleaned")
             #)
              
      
             
      ),
     tabItem(tabName = "simulacion",
             fluidRow(
               box(
                 title = "Parametros Simulacion",
                 numericInput("tiempo", "Tiempo", 5, min = 1, max = 10),
                 actionButton("simulate", "Simular")
               ),
               box(
                 title = titulo,
                 p("Autor: Josue Gómez"),
                 p("Verano 2021")
               )
               ),
               fluidRow(
                 box(
                   title = "Salida Simulacion (tabla)",
                   tableOutput("salidasimulacion") 
                 ),
                 box(
                 title = "Salida Simulacion (gráfica)",
                 plotOutput("plot1", click = "plot_click"),
                 verbatimTextOutput("info")
                 )
               )
             
             
     ),
     tabItem(tabName = "comercio_x_mayor",
             # fluidRow(
             #   box(
             #     title = "Parametros Simulacion",
             #     numericInput("tiempo", "Tiempo", 5, min = 1, max = 10),
             #     actionButton("actionBtn", "ActionBtn")
             #   )),
             
             
             fluidRow(fluidRow(box(
               selectInput(
                 "pickvalue",
                 label = "Filtro por año:",
                 unique(slice(
                   proyecciones.COSTA.SIERRA.OCCIDENTAL, 1:(n() - 1)
                 )$period),
                 selected = NULL,
                 multiple = T
               )
             ))),
             tableOutput("tabla_comercio_x_mayor"))
             
             # fluidRow(
             #   #box(
             #     selectInput("pickvalue", label = "Years", unique(slice(proyecciones, 1:(n() - 1))$period),
             #                 selected = NULL, multiple = T),
             #     tableOutput("tabla_comercio_x_mayor")
             #   #)
             # )
     #)
    )
  )
)

server <- function(input, output, session) {
  
  #???
  datos.primarios.pivote <- reactive({})
  output$pivoteconomia <- renderRpivotTable({}) 
  listado.carreras.centros <- reactive({})
  observe({})
  
  #Reultados Simulacion
  resultados.simulacion <- eventReactive(input$simulate, {
    
    load("datos.completos.R")
    region <- "SIERRA OCCIDENTAL"
    load(paste0("proyeccion.agregada",region,".R"))
    
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
    
    
    corre.simulacion <- function(n) {
      Output <- aplica.formula(session$userData$terminos.ecuacion, 1)
      ### deben resolver este mapeo, ahorita está hardcodeado. 
      datos.proyeccion <- proyeccion.agregada$Administracion[11:21]
      generator <- function(steps,n){
        for(i in 2:(steps)){
          Output[i] <- mean(Output[i-1]+aplica.formula(session$userData$terminos.ecuacion, n)) + datos.proyeccion[i] ## cómo usan los datos de la proyección?
        }
        return(data.frame(Output = Output))
      }
      result <- generator(steps=n, n=1.05)
      return(data.frame(result=result))
      
    }
    
    
    corre.simulacion(input$tiempo)
  })
  
  output$salidasimulacion <- renderTable(resultados.simulacion())
  
  output$plot1 <- renderPlot({
    session$userData$simPlotX <- length(resultados.simulacion()$Output)
    session$userData$simPlotY <- resultados.simulacion()$Output
    plot(1:session$userData$simPlotX,session$userData$simPlotY, type="b", col = "purple", main="Proyección por año", xlab="Años", ylab = "Miles")
  })
  
  output$info <- renderText({
    paste0("x= Años", session$userData$simPlotX, "\ny= Proyección (miles)", session$userData$simPlotY)
  })
  
  datos.educativos.generados <- eventReactive(input$defineparametros, {
    calcula.datos.educativos(input$carrera)
  })
  
  #DEFINE PARAMETROS
  variables.existentes <- eventReactive(input$defineparametros, {
    load("carreras.mario.molina.total.sel.R")
    datos.educativos <- colnames(calcula.datos.educativos(input$carrera))
    datos.educativos <- c(datos.educativos, colnames(denue.base.sel.agg)[-1])
  })
  
  observe({
    updateSelectInput(session = session, inputId = "var.indepediente", choices = variables.existentes())
  })
  
  terminos.ecuacion.data <- eventReactive(input$addtermtoequation, {
      #load("terminos.ecuacion.R")
      termino <- input$var.indepediente
      if (input$negativo) {
        signo <- -1
      } else {
        signo <- 1  
      }
      coeficiente <- input$coeficiente
      termino.nuevo <- data.frame(termino=termino, signo=signo, coeficiente=coeficiente)
      #terminos.ecuacion <- rbind(terminos.ecuacion, termino.nuevo)
      session$userData$terminos.ecuacion <- rbind(session$userData$terminos.ecuacion, termino.nuevo)
      #save(terminos.ecuacion,file="terminos.ecuacion.R")
      return(session$userData$terminos.ecuacion)
  })
  
  # clean.terminos.ecuacion <- eventReactive(input$cleanequation, {
  #   load("terminos.ecuacion.R")
  #   rm(terminos.ecuacion)
  #   file.remove("terminos.ecuacion.R")
  #   terminos.ecuacion <- data.frame(termino="char", signo=100, coeficiente=100)
  #   terminos.ecuacion <- terminos.ecuacion[-1,]
  #   save(terminos.ecuacion, file="terminos.ecuacion.R")
  #   return(terminos.ecuacion)
  # })
  # 
  
  
  ### esto no funciona
  ### abrir ticket the Technical Debt. 
  # clean.terminos.ecuacion <- eventReactive(input$cleanequation, {
  #   rm(terminos.ecuacion)
  #   file.remove("terminos.ecuacion.R")
  #   terminos.ecuacion <- data.frame(termino="char", signo=100, coeficiente=100)
  #   terminos.ecuacion <- terminos.ecuacion[-1,]
  #   save(terminos.ecuacion, file="terminos.ecuacion.R")
  #   return(terminos.ecuacion)
  # })
  
  
  #output$cleaned <- renderTable(clean.terminos.ecuacion())
  
  output$terminosecuacion <- renderTable({
    if(input$cleanequation == 1) {
      #clean.terminos.ecuacion()
      session$reload()
      NULL
      #rm(terminos.ecuacion)
      #file.remove("terminos.ecuacion.R")
      #terminos.ecuacion <- data.frame(termino="char", signo=100, coeficiente=100)
      #terminos.ecuacion <- terminos.ecuacion[-1,]
      #save(terminos.ecuacion, file="terminos.ecuacion.R")
    }else{
    terminos.ecuacion.data()
    }
  })
    
  get.proyecciones <- reactive({
    load("proyeccionesCOSTA SIERRA OCCIDENTAL.R")
    proyecciones.ser <- slice(proyecciones.COSTA.SIERRA.OCCIDENTAL, 1:(n() - 1)) 
    proyecciones.ser
    if (!is.null(input$pickvalue)){
      proyecciones.ser <- proyecciones.ser %>% filter(periodo %in% input$pickvalue)
    } 
    proyecciones.ser <- proyecciones.ser %>% select(-style)
    return(proyecciones.ser)
  })
  output$serviciosproyecciones <- renderTable(get.proyecciones())
  
  
  #output$tabla_comercio_x_mayor<- renderTable(proyecciones.COSTA.SIERRA.OCCIDENTAL)
  output$tabla_comercio_x_mayor<- renderTable(get.proyecciones())
  
  output$parametroseducativos <- renderTable(datos.educativos.generados())

  data.plot2 <- eventReactive(input$generateprediction, {
    data.plot2 <- 10
  })
  data.plot <- reactive({
    aux <- input$regionl4
    data.plot <- NA
    if (input$regionl4 == "SIERRA OCCIDENTAL") {
      data.plot <- 1
    } else {
      data.plot <- 1
    }
  })
  
  output$prospectivaplot <- renderPlot({
    par(mar = c(5,4,4,4))
    plot(datos.para.prediccion.l4()$y, xaxt="n", main=paste(input$carreral4,"(R-squared = ",datos.para.prediccion.l4()[1,]$r.squared,")",sep=" "), ylab="Existencia", xlab="", pch = datos.para.prediccion.l4()$style, col = datos.para.prediccion.l4()$style)
    axis(1, labels=datos.para.prediccion.l4()$x, at=1:nrow(datos.para.prediccion.l4()), las=3)
  })
  
  output$plotdemandacarrera <- renderUI({
    if (is.na(data.plot())) {
      p(paste0("Utilice los selectores para generar una proyeccion",data.plot2()))
    } else {
      plotOutput("prospectivaplot")
    }
      
  }) 
  # end ejercicio
}



shinyApp(ui, server)

