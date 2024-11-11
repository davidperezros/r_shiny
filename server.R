 # Datos poblacion ------------------------
#get_metadata_table_varval(10262)

filter_poblacion<-list("18" = "451", # Ambos sexos
                       "356" = "15668", # Todos los años
                       "349" = "16473", # Total Nacional
                       "70"= "") # Todas ccaa

data_poblacion<-get_data_table(filter = filter_poblacion,idTable = 10262,tip = "AM",metanames = TRUE,metacodes = TRUE, unnest = TRUE)


data_poblacion$Fecha<-format(as.Date(data_poblacion$Fecha),"%m")
data_poblacion<-data_poblacion%>% 
  filter(Fecha== "01")








# Datos sueldo medio ------------------------

filter_dist<-list(
  "18"="451", # Sexo=Ambos Sexos
  "647"="298419", # Media
  "349"="16473", # Total nacional
  "70"="" # Todas comunidades autónomas
)


data_dist<-get_data_table(idTable = 28191,tip="AM",filter=filter_dist,unnest=TRUE,metanames = TRUE,metacodes = TRUE)%>% select("Comunidades.autónomas.Id","Anyo","Valor")









# Server ------------------------------------------------------------------

server <- function(input, output) {
  gg_plot <- reactive({
    ggplot(penguins) + 
      geom_density(aes(fill = !!input$color_by), alpha = 0.2) +
      theme_bw(base_size = 16) +
      theme(axis.title = element_blank())
  }) 
  
  output$bill_length <- renderPlot(gg_plot() + aes(bill_length_mm))
  output$bill_depth <- renderPlot(gg_plot() + aes(bill_depth_mm))
  output$body_mass <- renderPlot(gg_plot() + aes(body_mass_g))
  
  

  #get_metadata_table_varval(13930)
  filter2 = list( 
    "349"="16473", #Total nacional
    "120" = "10758", # Jornada a tiempo completo
    "70" = "",      # Todas ccaa
    "684" = "298931" # Total decil
  )
  
  # Tabl de mortalidad por año, ccaa, ciudadaes, sexo, edad y funciones. 
  # Table url: https://www.ine.es/jaxiT3/Tabla.htm?t=27154&L=0
  esp2 <- get_data_table(idTable = 13930, filter = filter2, nlast = 1, unnest = TRUE,
                         metacodes = TRUE, tip = "AM", validate = FALSE)
  
  # Seleccionamos columnas de interés
  esp2 <- subset(esp2, select = c("Comunidades.y.Ciudades.Autonómas.Id","Anyo","Valor"))
  
  
  # Contornos de las ccaa
  ccaa2 <- read_sf("https://www.ine.es/wstempus/geojs/ES/CONTORNOS/70")
  
  
  
  # join de los contornos y el dataset
  ccaa2 <- merge(ccaa2, esp2, by.x = "id_region", 
                 by.y = "Comunidades.y.Ciudades.Autonómas.Id" )
  world <- reactive({ ccaa2 %>%
      filter(id_region == as.character(input$x)) 
  })
  
  # Create the map
  output$map <- renderLeaflet({
    m2 <- leaflet(world()) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-4, 40, zoom = 5.1) %>%
      addPolygons(fillOpacity = 0.8,
                  fillColor = "#457e76",
                  weight = 1,
                  label = ~nom_region,
                  color = "white",
                  highlightOptions = highlightOptions(fillOpacity = 1, bringToFront = TRUE, 
                                                      weight = 2, color = "white"))
  })
  
  
  
  
  
  
  data_poblacion2<-reactive({data_poblacion %>% 
                                          filter(Anyo== input$anyo) %>% 
                                          filter(`Comunidades.y.ciudades.autónomas.Id` == as.character(input$x)) })

  
  output$poblacion22<-renderText({ aux<-data_poblacion2()
    
    formatC(aux$Valor, digits = 0,format="f", big.mark = ".",decimal.mark = ",")})
  
  
  
  # Salarios medios -----------------------
  
  data_dist_reactive<-reactive({data_dist %>% 
      filter(Anyo== input$anyo) %>% 
      filter(`Comunidades.autónomas.Id` == as.character(input$x))  })
  
  
  output$sueldomedio<-renderText({ aux<-data_dist_reactive()
  
  paste0(formatC(aux$Valor, digits = 0,format="f", big.mark = ".",decimal.mark = ","), " €")})
  
  
  
  # Porcentaje de poblacion
  
  data_poblacion2b<-reactive({100*sum(as.numeric(data_poblacion %>% 
                     filter(Anyo== input$anyo  & `Comunidades.y.ciudades.autónomas.Id` == as.character(input$x)) %>% 
                      select(Valor)))/(as.numeric(data_poblacion %>%
                      filter(Anyo== input$anyo & `Comunidades.y.ciudades.autónomas.Id` == "16473")%>% 
                        select(Valor))) })
  
  
  
  output$poblacion_porcentaje<-renderText(paste0(formatC(data_poblacion2b(), digits = 2,format="f", big.mark = ".",decimal.mark = ",")," %"))
  
  
  }

