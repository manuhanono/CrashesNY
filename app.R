library(shiny)
library(tidyverse)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)
library(plotly)
library(leaflet)
library(DT)
library(arrow)
library(hrbrthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

ui <- shinyUI(fluidPage(theme = shinytheme("yeti"),useShinydashboard(), list(
  tags$head(
    HTML('<link rel="icon" href="nyc.svg" 
                type="image/svg" />'))),
  navbarPage(title = div(div(img(src="nyc.svg")), "....  .   ............."),
             tabPanel("Análisis General", h1("Análisis sobre los accidentes de tránsito en la ciudad de Nueva York"),
                      h4("Hecho por: Manuel Hanono y Bruno Soifer"), h5("Para este trabajo, se utilizó la base de datos de", em("NYC Open Data"), "sobre todos los accidentes de transito ocurridos en la ciudad de Nueva York desde", strong("julio de 2012", style = "color:#27a4f2"), "hasta mediados de", strong("octubre de 2022.", style = "color:#27a4f2"), " La presentación final consiste de cuatro tabs de acuerdo a lo que se quiere explicar. Las mismas son: Análisis General, Análisis Geográfico, Análisis de Causas y Consecuencias y Conclusiones."),
                      h5("A lo largo del desarrollo del informe, se va a buscar encontrar diferentes conclusiones sobre una base de",strong("1.937.848", style = "color:#27a4f2"), "accidentes.")
                      ,hr(),h3("Comportamiento de los accidentes a lo largo de los años."), h5("Como comienzo del análisis, se puede observar en el primer grafico de la izquierda la cantidad de accidentes de transito ocurridos por año. Se puede ver claramente una", strong("tendencia a la baja", style = "color:#27a4f2"),  "de los accidentes de transito en los últimos años. Si bien se podría creer que esto es debido al encierro en la pandemia, que podría imposibilitar el uso de vehiculos al mismo nivel que se utilizaba antes, se puede ver en el grafico a su lado que evidentemente, mas allá de la pandemia, en los últimos dos años ha habido una baja considerable en los accidentes.",style = "text-align: justify; background-color:white")
                      ,fluidRow(column(plotlyOutput("Grafico2"), width = 4, offset = 1), column(plotlyOutput("Grafico3"), width = 6)) 
                      ,
                      fluidRow(column(h5("Para contextualizar un poco, es posible que la tendencia a la baja vista en los accidentes se deba a una tendencia a la baja en la utilización de vehículos motorizados, impulsada por diversas razones, como pueden ser la",  em("transición del trabajo presencial hacia la modalidad Home Office"), "que impulsó la pandemia desde el 2020. Como se ve en el árticulo de CNBC, donde solo el 10% de los empleados volvieron a la modalidad presencial todos los días y que no más del 50% de los empleados va a la oficina de manera semanal. También puede haber inferencia del" , em("proyecto de ley NYC 25x25"), "propuesto por el alcalde de la ciudad, donde se propone reconvertir un 25% de las calles utilizadas por autos en calles peatonales y bicisendas hacía el 2025.", style = "text-align: justify; background-color:white"), width = 4), column(img(src = "NYC25x25.png", height="100%", width="100%"), width = 4), column(img(src = "NYCHO.png", height="40%", width="100%"), width = 4))
                      ,
                      hr(), h3("Accidentes según los dias de la semana y las horas del día"), 
                      fluidRow( column(h5("A continuación, tras haber observado un pantallazo del panorama general en cuanto al periodo analizado, se va a proceder a observar con más detalle el", em("comportamiento de los accidentes según las horas del día y los días de la semana", style = "color:#27a4f2"), "para poder ver si existen ciertos patrones en estos. A su vez, se buscará ver si al agrupar los días por laborales o fin de semana se puede encontrar alguna tendencia.", style = "text-align: justify; background-color:white"), width = 12))
                      ,
                      fluidRow(column(h5("En este gráfico se puede observar cómo la cantidad de accidentes varía dependiendo de la hora y el día en el que se producen los mismos. En primer lugar, se puede notar que para los 7 días en conjunto hay una tendencia, que se puede notar viendo que el número de accidentes totales desciende en los horarios nocturnos, y alcanza sus puntos más altos en los horarios diurnos.
Aplicando esta separación, se puede ver que para los horarios nocturnos, los días de fin de semana suelen haber mayor cantidad de accidentes respecto de los días de semana. Ocurre lo contrario en los horarios diurnos, donde en los días laborales (lunes a viernes) hay más accidentes respecto de los sábados y domingos.
",style = "text-align: justify;background-color:white"), hr(),img(src = "LateCrashes.png", height="100%", width="100%"), width=4), column(plotlyOutput("Grafico1"), width = 6), column(wellPanel(checkboxInput("Filtrar", "Agrupar a los días por Tipo de Día (Dia Laboral o Fin de Semana)"), style = "text-align: justify; background: white") ,width = 2))
                      ,hr(), h3("Accidentes según las diferentes razones y sus impactos"), h5("En la base de datos, buena parte de los accidentes tienen una razón o causa que haya llevado al mismo. A continuación se observan las razones de los accidentes en sus totalidad, y cómo cambian las", strong("causas",style = "color:#27a4f2"), "de los accidentes a través de las horas, notando que existen causas que se concentran en distintos rangos horarios. Además, se visualizan aquellas causas que generan accidentes que dejan como consecuencia al menos una persona fallecida.", style = "text-align:justify"),
                      fluidRow(column(plotlyOutput("Grafico6"), width = 9), column(wellPanel(radioButtons("Ver", "",c("Ver todos los horarios","Ver razones para las muertes", "Ver por hora")) ,sliderInput("Hora", "Seleccione la hora:", min = 00, max = 23, value = 12), style = "text-align: justify; background: white"), h5(textOutput("TextG6"),style = "text-align: justify;background-color:white"), width = 3)),
                      hr(), h3("Accidentes según actores involucrados y sus consecuencias"), h5("Es importante notar que en un accidente de tránsito puede haber diferentes partes involucradas. La base distingue entre tres diferentes: Las personas en vehículos, los ciclistas y los peatones.", style = "text-align:justify"),
                      fluidRow(column(plotlyOutput("Grafico11"), width = 4), column(plotlyOutput("Grafico12"), width = 4), column(h5("En el primer gráfico de barras se puede notar como la cantidad de", strong("lesionados", style = "color:#27a4f2") ,"es mayor en", strong("gente sobre vehículos
", style = "color:#27a4f2"),"que sobre los peatones o los ciclistas. La cantidad de gente sobre vehículos que resulta lesionada es casi 4 veces
más que los aquellos peatones que sufren lesiones como consecuencia de un accidente, y 8 veces más aproximadamente que los ciclistas.
Por lo tanto, se esperaría que ocurra algo similar con las muertes ocasionadas, más aún teniendo en cuenta que la mayoría de los
choques ocurren entre vehículos de motor.", br(), br(), "Sin embargo, si se analiza la totalidad de", strong("muertes", style = "color:#27a4f2"),"se llega a la conclusión de que los",  strong("peatones", style = "color:#27a4f2"), "son
los que sufren la peor parte de los accidentes. El número de peatones fallecidos como consecuencia de los accidentes 
es el más elevado, ya que tiene casi un 25% más que las personas que circulan con vehículos. En cuanto a los ciclistas,
se observa que solo el 7% de la totalidad de muertes pertenecen a este grupo, que es considerablemente bajo
debido a que los ciclistas no suelen tener tantos accidentes",style = "text-align: justify; background-color:white"), width = 4)),
                      hr(), h3("Cantidad de accidentes según cada condado de Nueva York"), h5("La Ciudad de Nueva York se encuentra dividida en 5 diferentes", strong("Boroughs", style = "color:#27a4f2"), "o condados: Brooklyn, Bronx, Manhattan, Queens y Staten Island. La distribución de los accidentes no es la misma en los 5 condados, por lo que a continuación se analizan los condados de Nueva York según la ubicación en la que ocurrieron los accidentes.
", style = "text-allign: justify"),
                      fluidRow(column(h5("Analizando los accidentes por barrios, se puede decir que la mayoría ocurren en Brooklyn, con más
de 420.000 casos. En segundo lugar se encuentra el barrio de Queens, seguido por el de Manhattan. De
los 5 barrios identificados en la Ciudad de Nueva York, en los 3 mencionados anteriormente se concentran
el 81% de los accidentes de tránsito. Del 19% restante, el 77% proviene del barrio de Bronx, y solo el 
23% de Staten Island (cerca de un 4% del total de accidentes)", style = "text-align: justify; background-color:white"), hr(),img(src = "NOTAB.png", height="100%", width="100%"), width = 4),column(plotOutput("Grafico13"), width = 8))
             )
             ,
             tabPanel("Análisis Geográfico", h1("Análisis Geográfico"),
                      fluidRow(
                        column(h4("Filtros para el mapa:"),radioButtons("VerMapa", "",c("Ver accidentes que generaron muertes","Ver accidentes que involucren lesiones y/o muertes a ciclistas", "Ver accidentes que involucren lesiones y/o muertes a peatones", "Ver por año")) , numericInput("Año", "Insertar un año:", min = 2012, max = 2022, step = 1, value = c(2022))
                               ,width = 4), column(leafletOutput("Grafico5"),width = 8)),
                      fluidRow(column(h5(textOutput("TextG5")), width = 8, offset = 4))),
             
             tabPanel("Análisis de Causas y Consecuencias", titlePanel("Análisis de Causas y Consecuencias"), h5("En esta parte se buscará puntualizar sobre un poco de lo que se fue presentando. La intención es llegar a profundizar más acerca de las consecuencias que pueden generar los accidentes, principalmente enfocando en personas", strong("lesionadas", style = "color:#27a4f2"), "y en personas", strong("fallecidas", style = "color:#27a4f2"), "con el fin de poder ayudar a entes gubernamentales a disminuir la cantidad de estos dos grupos encontrando cuáles son las posibles", strong("causas", styile = "color:#27a4f2"), "que las generan."),                
                      hr(), h3("Accidentes, muertes y lesionados por período de tiempo"),h5("Para comenzar con esta parte del informe, se busca ver si se mantiene cierta", strong("tendencia temporal") ,"para los accidentes, las muertes y los lesionados. Para ello se dividió a los años en 3 períodos de tiempo de 3 años cada uno. Se excluyó de este análisis al 2012 y al 2022, ya que esos años estan incompletos en la base de datos.", style = "text-align:justify"),
                      fluidRow(column(selectInput("ML", "",c("Ver Accidentes","Ver Muertes", "Ver Lesionados")),h5(textOutput("TextG10"), style = "text-align:justify"), width = 4),column(plotlyOutput("Grafico10"), width = 7)),
                      hr(), h3("Muertes y lesionados por hora del día"),h5("Siguiendo con el mismo análisis comparativo entre los accidentes que generan muertes y los que generan lesionados, ahora se procede a observar un análisis en cuanto a la hora del día y que incidencias puede tener el mismo, para observar posibles tendencias y ver si estas son las mismas para los accidentes que generan lesionados y los que generan muertes.", style = "text-align:justify"),
                      fluidRow(column(plotlyOutput("Grafico15"), width = 6, offset = 1), column(h5(textOutput("TextG15"), style = "text-align:justify"),selectInput("ML2", "",c("Ver Muertes", "Ver Lesionados")),hr(), img(src = "LateCrashes.png", height="100%", width="100%"), width = 4)),
                      hr(), h3("Muertes y lesionados por tipo de auto y rango horario"),h5("Teniendo en cuenta que las muertes y los lesionados son causados por distintos tipos de autos, a continuación se analiza cuáles son los tipos de autos que más ocasionan las muertes y los lesionados en promedio, dividiendo el día en distintos rangos horarios.", style = "text-align:justify"),
                      fluidRow(column(plotlyOutput("Grafico16"), width = 6),column(h5(textOutput("TextG16"), style = "text-align:justify"),selectInput("ML3","" ,c("Ver Muertes", "Ver Lesionados")), width = 4)),
                      hr(), h3("Muertes y lesionados con respecto a la cantidad de vehículos involucrados en el accidente"),h5("Para un accidente, en el dataset se detalla entre otros datos, la cantidad de vehículos involucrados que participaron del mismo, las lesiones y la muertes que se ocasionaron. A continuación, se realiza un análisis sobre cómo la cantidad de autos que impactan en un accidente tiene consecuencia en las lesiones y muertes que generan.", style = "text-align:justify"),
                      fluidRow(column(h5(textOutput("TextG17"), style = "text-align:justify"),selectInput("ML5","" ,c("Ver Muertes", "Ver Lesionados")), width = 4), column(plotlyOutput("Grafico17"), width = 6, offset = 1)),
                      hr(), h3("Diferencias entre cantidad de accidentes según los feriados"),h5("A continuación se hace un análisis sobre la diferencia en la cantidad de accidentes en un día, dependiendo si el mismo fue considerado feriado nacional en la Ciudad de Nueva York (se toman en cuenta todos los años de la base).", style = "text-align:justify"),
                      fluidRow(column(h5("Se visualizan dos diagramas de “caja y bigote”, uno que representa aquellos días considerados como", strong("feriados,", style = "color:#27a4f2") ,"y otro en los que representa aquellos días que no lo son. Se observa que el rango intercuartílico y la mediana cobra valores más altos para aquellos días no feriados. Además, en los días no feriados se observan los valores más altos y más bajos. Ambos grupos presentan outliers, que se encuentran por encima del extremo superior de cada diagrama.", br(), br(), "Por lo tanto, con el análisis de ambos diagramas se puede intuir sí", em("existe una diferencia entre los días feriados y no feriados,"), "donde en promedio se puede esperar que en estos últimos haya mayor cantidad de accidentes por día.
", style = "text-align:justify"), width = 4), column(plotlyOutput("Grafico19"), width = 6, offset = 1)),
                      hr(), h3("Cantidad de autos involucrados en los accidentes por día a lo largo de los años"),h5("Para un mismo accidente, en el dataset se encuentran choques que involucran de 1 a 5 vehículos. A continuación se analiza cómo cambia el", strong("promedio de autos involucrados", style = "color:#27a4f2") ,"en un mismo choque, dependiendo del día de la semana.", style = "text-align:justify"),
                      fluidRow(column(h5(textOutput("TextG4"), style = "text-align:justify"),selectInput("ML4","" ,c("Ver según tipo de día", "Ver todos los días")), width = 4), column(plotlyOutput("Grafico4"), width = 6, offset = 1))
                      ,hr(), h3("¿Entre qué tipos de auto son los accidentes entre 2 autos?"),h5("Como extra, se realizó este diagrama de Sankey con el fin de ver los choques entre los tipos de auto.", style = "text-align:justify"),
                      fluidRow(column(plotlyOutput("Grafico18"), width = 8, offset = 2))
                      
             )
             ,
             tabPanel("Conclusiones", titlePanel("Conclusiones generales del análisis"), 
                      valueBox("110.540", h5("Fueron los accidentes en el año", strong("2021.") ,"Es el registro anual más", strong("bajo") ,"de toda la base.", style = "text-align: justify"), icon = NULL, color = "aqua", width =4, href = NULL),
                      valueBox("231.564", h5("Fue la cantidad de accidentes ocurridos en el año", strong("2018."),"Es el registro anual más", strong("alto") ,"de toda la base.", style = "text-align: justify"), icon = NULL, color = "aqua", width = 4, href = NULL),
                      valueBox("2692", h5("Fueron las", strong("muertes"), "causadas por los", strong("2585"), "accidentes mortales en Nueva York, en el periodo analizado.", style = "text-align: justify"), icon = NULL, color = "aqua", width = 4, href = NULL),
                      br(),fluidRow(column(hr(), width = 12)),
                      
                      infoBox("LESIONADOS", value = "21,54%", icon = shiny::icon("user-injured", lib = "font-awesome"), subtitle = h5("Es el porcentaje de accidentes que generan", em("lesionados"), "con respecto al total. Representa a", em("417.531"), "accidentes."), color = "aqua", width = 3, href = NULL, fill = FALSE),
                      infoBox("BROOKLYN", value = "35%", icon = shiny::icon("car", lib = "font-awesome"), subtitle = h5("De los accidentes que generaron lesionados sucedieron en el condado", em("Brooklyn.")), color = "aqua", width = 3, href = NULL, fill = FALSE),
                      infoBox("MUERTES", value = "22%", icon = shiny::icon("arrow-up", lib = "font-awesome"), subtitle = h5("Fue el aumento de la cantidad de muertes desde", em("2018 a 2021."), "El total pasó de", em("231 a 294.")), color = "aqua", width = 3, href = NULL, fill = FALSE),
                      infoBox("PEATONES", value = "50,70%", icon = shiny::icon("person-walking", lib = "font-awesome"), subtitle = h5("De los", em("fallecidos"), "son clasificados como", em("peatones."), "El resto se acumula entre ciclistas y gente en vehículos." ), color = "aqua", width = 3, href = NULL, fill = FALSE),
                      
             )
             #,inverse = TRUE
  ) ))



server <- shinyServer(function(input, output) {
    source("helpers.R")
  
  output$Grafico1 <- renderPlotly({
    if(input$Filtrar){ggplotly(g33)}
    else{ggplotly(g1)}
  })
  
  output$Grafico2 <- renderPlotly({
    ggplotly(g2)
  })
  
  output$Grafico3 <- renderPlotly({
    ggplotly(g4) 
  })
  
  output$Grafico4 <- renderPlotly({
    if(input$ML4 == "Ver según tipo de día"){ggplotly(g8)}
    else if (input$ML4 == "Ver todos los días"){ggplotly(g7)}
  })
  
  data2 <- reactive({
    data <- mapa1
    if(!is.null(input$Año)){
      data<-data %>%
        filter(data$YEAR == input$Año[1]) 
    }
    data
  })
  
  output$Grafico5 <- renderLeaflet({
    data2 <- data2()
    if(input$VerMapa == "Ver accidentes que generaron muertes"){ mapa1 = mapa1 %>% filter(`NUMBER OF PERSONS KILLED` != 0)
    leaflet(mapa1) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions(),
      popup =  ~ paste("Personas que fallecieron: ",`NUMBER OF PERSONS KILLED` , ". Año: ", YEAR, ". Cantidad de autos involucrados:", CANT)
    ) %>% addProviderTiles(providers$CartoDB.Positron)}
    else if(input$VerMapa == "Ver accidentes que involucren lesiones y/o muertes a ciclistas") {
      d = crashes %>% select(lon, lat, `NUMBER OF CYCLIST INJURED`, `NUMBER OF CYCLIST KILLED`) %>% filter(!is.na(lon) & lat > 40 & lat < 42 & lon > -74 & lon < -72 & `NUMBER OF CYCLIST INJURED` !=0)
      leaflet(d) %>% addTiles() %>% addMarkers(
        clusterOptions = markerClusterOptions(),
        popup =  ~ paste("Ciclistas que fallecieron: ",`NUMBER OF CYCLIST KILLED` , ". Ciclistas lesionados:", `NUMBER OF CYCLIST INJURED`)
      ) %>% addProviderTiles(providers$CartoDB.Positron)
    }
    else if(input$VerMapa == "Ver accidentes que involucren lesiones y/o muertes a peatones") {
      d2 = crashes %>% select(lon, lat, `NUMBER OF PEDESTRIANS INJURED`, `NUMBER OF PEDESTRIANS KILLED`) %>% filter(!is.na(lon) & lat > 40 & lat < 42 & lon > -74 & lon < -72 & `NUMBER OF PEDESTRIANS INJURED` !=0)
      leaflet(d2) %>% addTiles() %>% addMarkers(
        clusterOptions = markerClusterOptions(),
        popup =  ~ paste("Peatones que fallecieron: ",`NUMBER OF PEDESTRIANS KILLED` , ". Peatones lesionados:", `NUMBER OF PEDESTRIANS INJURED`)
      ) %>% addProviderTiles(providers$CartoDB.Positron)
    }
    else if (input$VerMapa == "Ver por año"){leaflet(data2) %>% addTiles() %>% addMarkers(
      clusterOptions = markerClusterOptions()
    ) %>% addProviderTiles(providers$CartoDB.Positron)}
  })
  
  output$TextG5 <- renderText({
    if(input$VerMapa == "Ver accidentes que generaron muertes"){"Con respecto a las muertes a lo largo de los años, no se identifica ninguna tendencia en particular. Sí, en cambio, se puede ver que la mayoría de estos accidentes se generan en intersecciones de avenidas con avenidas o calles."}
    else if(input$VerMapa == "Ver accidentes que involucren lesiones y/o muertes a ciclistas"){"Este tipo de accidentes está distribuido alrededor de toda la ciudad, pero se observan que en las avenidas se acumulan una cantidad considerable de los mismos."}
    else if(input$VerMapa == "Ver accidentes que involucren lesiones y/o muertes a peatones"){"Con respecto a los peatones, se observan más accidentes respecto de los ciclistas. Este tipo de accidentes, si bien la mayoría ocurren en avenidas, hay bastantes otros que ocurren en arterias más pequeñas. "}
    else if(input$VerMapa == "Ver por año") {"Se puede observar cada año en particular desde el 2012 hasta el 2022, ingresándolo en el filtro. No se observa ningún patrón o tendencia a lo largo de los años que permita distinguir el comportamiento de los accidentes."}
  })
  
  data3 <- reactive({
    data1 <- graf15
    data1<-data1 %>% filter(Hora == input$Hora[1]) 
    data1
  })
  
  output$Grafico6 <- renderPlotly({
    data3 <- data3() %>% filter(cantidad > 1000)
    t <- list(size = 10, color = "#242424") 
    if(input$Ver == "Ver todos los horarios"){g6}
    else if(input$Ver == "Ver razones para las muertes"){g16}
    else if(input$Ver == "Ver por hora"){ggplotly(data3  %>% ggplot() + geom_col(aes(x = cantidad, y=reorder(as.factor(Factor),+(cantidad)), fill = cantidad)) +
                                                    ggtitle(paste("Principales razones para los accidentes a las", data3$Hora, "hs")) + 
                                                    scale_x_continuous(breaks = seq(from = 0, to = 50000, by = 5000)) + theme_minimal() +
                                                    theme(legend.position = "none") + theme(axis.title.x=element_blank(),axis.title.y =element_blank()), originalData = FALSE) %>% add_text(textfont = t, x=data3$cantidad, text= paste(" ", data3$cantidad),textposition="right")}
  }) 
  output$TextG6 <- renderText({
    if(input$Ver == "Ver todos los horarios"){"Con respecto a las principales causas que generan los accidentes, se ve un claro predominio de la categoria de distracción o desatención del conductor, siendo esta la razón explicativa para aproximadamente el 25% de los accidentes. Cabe aclarar que, en muchos de los accidentes, la causa no estaba especificada. Es interesante ver como el alcohol influye en poco más del 1% de las causas de los accidentes, unos 21792 a lo largo de 10 años."}
    else if(input$Ver == "Ver razones para las muertes"){"Al ver las razones de los accidentes ordenadas según la cantidad de muertes que generaron, se puede observar como cambia bastante con respecto al gráfico general. Razones como el alcohol, velocidades altas y el evitar controles de tránsito pasan a ser mucho más predominantes."}
    else if(input$Ver == "Ver por hora"){"Cuando vemos las razones según la hora, podemos entender que van cambiando a lo largo de los diferentes rangos horarios. A la madrugada, vuelve a aparecer el alcohol como protagonista, mientras que durante el día tiene casi nula incidencia."}
  })
  
  output$Grafico11 <- renderPlotly({g12})
  output$Grafico12 <- renderPlotly({g13})
  output$Grafico13 <- renderPlot({g14})
  output$Grafico14 <- renderPlotly({g11}
  )
  
  output$Grafico10 <- renderPlotly({
    if(input$ML == "Ver Accidentes"){ggplotly(g27)}
    else if (input$ML == "Ver Muertes"){ggplotly(g28)}
    else if(input$ML == "Ver Lesionados"){ggplotly(g29)}
  })
  
  output$TextG10 <- renderText({
    if(input$ML == "Ver Accidentes"){paste("En primer lugar, se visualiza la evolución de la cantidad de accidentes respecto de los tres períodos. Se puede ver como entre los 2 primeros períodos, es decir, de 2013 a 2018, se ubican las líneas muy por encima del tercer y último período, que nunca supera en promedio los 12500 accidentes por mes.", "Además, se puede notar que los máximos y mínimos para los tres períodos no son los mismos. Para los períodos 2013 a 2015 y 2016 a 2018, el momento del año donde se alcanzan más accidentes es cerca del mes de junio, y los puntos más bajos se registran al comienzo del año. Sin embargo, para el período más reciente, existen dos momentos donde se alcanzan los máximos; en los meses de enero y julio. Los meses con menor cantidad de casos para el período mencionado son marzo y abril." , sep="\n") }
    else if (input$ML == "Ver Muertes"){"Con respecto a la cantidad de muertes a lo largo de los meses, se observa un patrón un poco más parecido en los tres períodos, respecto de la cantidad de accidentes. Se puede ver que hasta el mes de abril, el período 2013-2015 es el que tiene mayor cantidad de casos en promedio. Sin embargo, desde abril hasta el final del año el período 2019-2021 ocupa el primer puesto en muertes, algo que es poco esperable si se toma en cuenta el análisis hecho para la cantidad de casos en conjunto. El período 2016 a 2018, si no se toma en cuenta el mes de agosto, siempre está en último lugar."}
    else if(input$ML == "Ver Lesionados"){"En el análisis para los lesionados, se nota un comportamiento bastante distinto con respecto a las muertes. A diferencia de este, donde el período 2016-2018 se encontraba casi todo el año en tercer lugar, ahora este mismo período se aleja considerablemente de los otros 2 para ocupar el primer lugar. En el mes de junio, en los años 2016 a 2018 se llegaron a registrar más de 4000 lesionados en promedio. Analizando los otros dos períodos, se encuentran bastantes similitudes, como sus máximos o mínimos, sin embargo, se nota un comportamiento más lineal para el período 2013-2015. En el mismo, el número de lesionados sube constantemente desde que arranca el año hasta el mes de junio, donde empieza a decaer."}
  })
  
  output$Grafico15 <- renderPlotly({
    if(input$ML2 == "Ver Muertes"){ggplotly(g18)}
    else if (input$ML2 == "Ver Lesionados"){ggplotly(g19)}
  })
  
  output$Grafico16 <- renderPlotly({
    if(input$ML3 == "Ver Muertes"){ggplotly(g166)}
    else if (input$ML3 == "Ver Lesionados"){ggplotly(g15)}
  })
  
  output$TextG15 <- renderText({
    if(input$ML2 == "Ver Muertes"){"En el gráfico de muertes por hora del día, se puede ver que los números más altos se encuentran entre las 18 y las 00hs. Sin embargo, el pico se encuentra en las 04hs, horario en el cual se produjeron un total de 150 muertes a lo largo de los años. Los números más bajos de muertes se producen cuando hay más accidentes, que es en el rango horario
desde las 07 hasta las 16hs. Además, hay una tendencia a la subida en cantidad de muertes en estos horarios, que se dispara desde las 18hs"
    }
    else if (input$ML2 == "Ver Lesionados"){"Con respecto a la cantidad total de lesionados por hora en la que ocurre el accidente, se puede notar un comportamiento distinto respecto del gráfico de fallecidos. El gráfico alcanza su pico en los horarios donde ocurren más accidentes, entre las 14 y las 18hs. Además, se puede notar que cuando se alcanzan los números más bajos (sobre la noche), hay una tendencia a que el número incremente con el paso de las horas. Una vez que se llega a las 18hs, el número de lesionados comienza a bajar."
    }
  })
  
  output$TextG16 <- renderText({
    if(input$ML3 == "Ver Muertes"){"La primera diferencia que se observa con el presente gráfico a diferencia del anterior, es que las muertes que se ocasionan en los accidentes de tránsito no están únicamente concentradas en tres tipos de autos, ni tampoco los mismos tienen los mismos porcentajes que tenían. Ahora a los autos de tipo sedan, vehículos deportivos y taxis, se suman camiones y motos. Cabe aclarar que los porcentajes, al igual que antes, representan al tipo de auto que causa el accidente fatal, y no aquel que lo sufre. A pesar de que cambia la distribución y los porcentajes con respecto al gráfico de lesionados, en los horarios nocturnos (00 a 07hs y 20 a 23hs) los autos de tipo sedán son los que causan mayores muertes, y en los horarios diurnos (08 a 19 hs) los vehículos deportivos se llevan el mayor porcentaje. Se destaca que los taxis tienen porcentajes menos elevados cuando se analizan las muertes respecto del análisis hecho para los lesionados. Además se puede apreciar como la mayoría de los accidentes que provocan los camiones ocurren durante el día, y que las motos manejan números bastante parecidos a lo largo del día"}
    else if (input$ML3 == "Ver Lesionados"){"En el gráfico de barras se puede visualizar, dependiendo del rango horario, qué tipo de autos son los que causan en mayor porcentaje las lesiones. Se observa como entre los grupos de Sedan, vehículos deportivos y taxis ocasionan más del 90% en la mayoría de los horarios. En los horarios nocturnos (de 00 a 03hs, 04 a 07hs y 20 a 23hs) el auto de tipo Sedan es aquel que causa la mayor cantidad de lesionados. En los horarios diurnos (08 a 11hs, 12 a 15hs y 16 a 19hs), el auto que causa más lesionados es el de tipo deportivo. En todos los casos, los taxis se encuentran en tercera posición, acumulando poco menos del 25% del total de lesionados. El rango horario más 'disparejo' en términos de porcentaje de lesionados es el de 00 a 03hs, donde los autos Sedan son los que concentran el 40% del total de las lesiones. Además, en este horario entre los autos Sedan, vehículos deportivos y taxis, concentran el 94% de la suma total de lesionados"}
  })
  
  output$TextG4 <- renderText({
    if(input$ML4 == "Ver según tipo de día"){"Si se analiza cada día por separado, se puede notar que hasta el año 2015 inclusive el promedio de autos por accidente estaba muy cerca de 2, mientras que a partir del año 2016 este promedio ha ido descendiendo, notando otra fuerte caída en el año 2020. Por lo tanto, no solo se puede concluir que los accidentes con el paso del tiempo han ido bajando (teniendo en cuenta gráficos anteriores), sino que también que menos vehículos chocan entre sí. "}
    else if (input$ML4 == "Ver todos los días"){"Segmentando los días entre días laborales y fines de semana, se puede afirmar que a lo largo de los años, si bien el promedio fue bajando, siempre los días de fin de semana mantienen un promedio de autos involucrados mayor respecto de los días laborales. Esto último puede llegar a deducir que dado que un choque se produzca un sábado o un domingo, la gravedad del mismo podría ser mayor, asumiendo que cuantos más vehículos impactan entre sí, la gravedad del accidente podría ser mayor."}
  })
  
  output$Grafico17 <- renderPlotly({
    if(input$ML5 == "Ver Muertes"){ggplotly(g20)}
    else if (input$ML5 == "Ver Lesionados"){ggplotly(g17)}
  })
  
  output$TextG17 <- renderText({
    if(input$ML5 == "Ver Muertes"){"Para este gráfico, se observa a partir de la cantidad de autos involucrados en un accidente, el porcentaje de accidentes en el que hubo al menos una persona fallecidos. Al igual que en el gráfico anterior, donde se veía el porcentaje de lesionados, en este caso el porcentaje de personas fallecidas tiene un comportamiento muy similar. En aquellos casos donde solo 2 vehículos tienen un accidente, el porcentaje de accidentes que reportan al menos un fallecido es realmente bajo, de un 0,05%. A medida que se suman vehículos al accidente, este porcentaje va aumentando, hasta llegar a un 0,91% de accidentes que reportan al menos un fallecido, cuando el accidente involucra a exactamente 5 autos. Por lo tanto, se puede llegar a una conclusión parecida a la anterior, donde se cree que cuantos más autos formen parte de un accidente, la gravedad del mismo sea mayor, y por lo tanto cause más lesionados y más muertes"}
    else if (input$ML5 == "Ver Lesionados"){"El gráfico muestra cómo varía el porcentaje de lesionados en los accidentes, dependiendo de cuántos autos están involucrados en el mismo. Se tiene en cuenta a la categoría 'Hay lesionados' si en el accidente hubo al menos una persona que resultó herida. Teniendo en cuenta que la mayoría de accidentes es entre dos vehículos, es esperable que esta sea la categoría con menor porcentaje de lesionados, ya que al ser el accidente más común de todas variables, se dejan de lado otras variables que puedan incidir sobre la gravedad y por ende los lesionados que pueda dejar un accidente (como tipos de autos, causas, día, horario, mes, etc.). También se puede observar que a medida que aumenta de 3, 4 y hasta 5 vehículos involucrados por choque, el porcentaje de no lesionados va bajando. Se puede intuir que esto ocurre debido a que cuantos más autos se vean involucrados en un accidente, la gravedad del mismo será mayor. En los registros donde se involucra a 5 vehículos en un mismo accidente, casi la mitad de los mismos trae como consecuencia al menos una persona herida."}
  })
  
  output$Grafico18 <- renderPlotly({
    ggplotly(g11)
  })
  
  output$Grafico19 <- renderPlotly({
    g21
  })
  
  
})

server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}


shinyApp(ui = ui, server = server)
