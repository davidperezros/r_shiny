# Datos poblacion ------------------------
# get_metadata_table_varval(10262)

filter_poblacion <- list(
  "18" = "451", # Ambos sexos
  "356" = "15668", # Todos los años
  "349" = "16473", # Total Nacional
  "70" = ""
) # Todas ccaa

data_poblacion <- get_data_table(filter = filter_poblacion, idTable = 10262, tip = "AM", metanames = TRUE, metacodes = TRUE, unnest = TRUE)


data_poblacion$Fecha <- format(as.Date(data_poblacion$Fecha), "%m")
data_poblacion <- data_poblacion %>%
  filter(Fecha == "01")








# Datos sueldo medio ------------------------

filter_dist <- list(
  "18" = "451", # Sexo=Ambos Sexos
  "647" = "298419", # Media
  "349" = "16473", # Total nacional
  "70" = "" # Todas comunidades autónomas
)


data_dist <- get_data_table(idTable = 28191, tip = "AM", filter = filter_dist, unnest = TRUE, metanames = TRUE, metacodes = TRUE) %>% select("Comunidades.autónomas.Id", "Anyo", "Valor")




# Para contornos -----------------------


# get_metadata_table_varval(13930)
filter2 <- list(
  "349" = "16473", # Total nacional
  "120" = "10758", # Jornada a tiempo completo
  "70" = "", # Todas ccaa
  "684" = "298931" # Total decil
)

# Tabl de mortalidad por año, ccaa, ciudadaes, sexo, edad y funciones.
# Table url: https://www.ine.es/jaxiT3/Tabla.htm?t=27154&L=0
esp2 <- get_data_table(
  idTable = 13930, filter = filter2, nlast = 1, unnest = TRUE,
  metacodes = TRUE, tip = "AM", validate = FALSE
)

# Seleccionamos columnas de interés
esp2 <- subset(esp2, select = c("Comunidades.y.Ciudades.Autonómas.Id", "Anyo", "Valor"))


# Contornos de las ccaa
ccaa2 <- read_sf("https://www.ine.es/wstempus/geojs/ES/CONTORNOS/70")



# Datos ECOICOP --------------------

# Filtro
filter_ecoicop_1 <- list(
  "762" = "", # Todos GRUPOS de 2 dígitos del ECOICOP
  "3" = "72", # Dato Base
  "57" = "8859" # Gasto medio por persona
) # Dato base


# Obtenemos datos
# nlast=1 para devolver sólo para el último periodo
data_ecoicop_1 <- get_data_table(
  filter = filter_ecoicop_1, idTable = 25143, tip = "AM", unnest = TRUE,
  metacodes = TRUE, validate = FALSE, metanames = TRUE
) %>% select(c("Anyo", "Valor", "Comunidad.autónoma.de.residencia.Id", "Grupos.de.gasto.(2.dígitos)", "Grupos.de.gasto.(2.dígitos).Codigo"))

filter_ecoicop_2 <- list(
  "796" = "", # Todos GRUPOS de 2 dígitos del ECOICOP
  "3" = "72", # Dato base
  "57" = "8859" # Gasto medio por persona
)

# Obtenemos datos
# nlast=1 para devolver sólo para el último periodo
data_ecoicop_2 <- get_data_table(filter = filter_ecoicop_2, idTable = 25144, tip = "AM", unnest = TRUE, metacodes = TRUE, validate = FALSE, metanames = TRUE) %>%
  select(c("Anyo", "Valor", "Comunidad.autónoma.de.residencia.Id", "Subgrupos.de.gasto.(3.dígitos)", "Subgrupos.de.gasto.(3.dígitos).Codigo"))





# quitamos el total
data_ecoicop_2 <- data_ecoicop_2[data_ecoicop_2[, "Subgrupos.de.gasto.(3.dígitos).Codigo"] != "00", ]





# Función que a cada elemento de ECOICOP 3 dígitos, asocia su grupo de agregación de 2 dígitos
get_label <- function(inputs, label) {
  # Extrae los primeros dos caracteres de cada elemento del vector de entrada
  prefixes <- substr(inputs, 1, 2) # tomamos sólo 2 primeros eltos Ej: 01.1 tomamos 01
  # Usa sapply para aplicar la función a cada prefijo
  sapply(prefixes, function(prefix) {
    if (prefix == "01") {
      return(label[2])
    } else if (prefix == "02") {
      return(label[3])
    } else if (prefix == "03") {
      return(label[4])
    } else if (prefix == "04") {
      return(label[5])
    } else if (prefix == "05") {
      return(label[6])
    } else if (prefix == "06") {
      return(label[7])
    } else if (prefix == "07") {
      return(label[8])
    } else if (prefix == "08") {
      return(label[9])
    } else if (prefix == "09") {
      return(label[10])
    } else if (prefix == "10") {
      return(label[11])
    } else if (prefix == "11") {
      return(label[12])
    } else if (prefix == "12") {
      return(label[13])
    } else {
      return(NA) # Devuelve NA si el prefijo no coincide con ninguno
    }
  })
}
























# Server ------------------------------------------------------------------

server <- function(input, output) {
  gg_plot <- reactive({
    ggplot(penguins) +
      geom_density(aes(fill = !!input$color_by), alpha = 0.2) +
      theme_bw(base_size = 16) +
      theme(axis.title = element_blank())
  })



  # Ecoicop reactive ------------------------


  data_ecoicop_1_reactive <- reactive({
    df <- data_ecoicop_1 %>%
      filter(`Comunidad.autónoma.de.residencia.Id` == as.character(input$x)) %>%
      filter(Anyo == input$anyo) %>%
      mutate(parents = ifelse(`Grupos.de.gasto.(2.dígitos).Codigo` == "00", "", "Índice general"))
  })




  data_ecoicop_2_reactive <- eventReactive(data_ecoicop_1_reactive(), {
    aux <- data_ecoicop_1_reactive()
    data_ecoicop_2 %>%
      filter(Comunidad.autónoma.de.residencia.Id == as.character(input$x)) %>%
      filter(Anyo == input$anyo) %>%
      mutate(parents = get_label(`Subgrupos.de.gasto.(3.dígitos).Codigo`, label = aux$`Grupos.de.gasto.(2.dígitos)`))
  })




  final_ecoicop_reactive <- eventReactive(data_ecoicop_2_reactive(), {
    aux1 <- data_ecoicop_1_reactive()
    aux2 <- data_ecoicop_2_reactive()

    df <- data.frame(
      values = c(aux1$Valor, aux2$Valor),
      labels = c(aux1$`Grupos.de.gasto.(2.dígitos)`, aux2$`Subgrupos.de.gasto.(3.dígitos)`),
      parents = c(aux1$parents, aux2$parents)
    )

    df
  })



  output$ecoicop <- renderPlotly({
    g <- plot_ly(final_ecoicop_reactive(),
      type = "treemap",
      labels = ~labels,
      values = ~ as.integer(values),
      parents = ~parents,
      maxdepth = 2,
      textinfo = "label+value+percent entry+percent root",
      hoverinfo = "label+value+percent entry+percent root",
      branchvalues = "total",
      tiling = list(squarifyratio = 2)
    )
    g
  })


  # join de los contornos y el dataset
  ccaa2 <- merge(ccaa2, esp2,
    by.x = "id_region",
    by.y = "Comunidades.y.Ciudades.Autonómas.Id"
  )
  world <- reactive({
    ccaa2 %>%
      filter(id_region == as.character(input$x))
  })

  # Create the map
  output$map <- renderLeaflet({
    m2 <- leaflet(world()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-4, 40, zoom = 5.1) %>%
      addPolygons(
        fillOpacity = 0.8,
        fillColor = "#457e76",
        weight = 1,
        label = ~nom_region,
        color = "white",
        highlightOptions = highlightOptions(
          fillOpacity = 1, bringToFront = TRUE,
          weight = 2, color = "white"
        )
      )
  })






  data_poblacion2 <- reactive({
    data_poblacion %>%
      filter(Anyo == input$anyo) %>%
      filter(`Comunidades.y.ciudades.autónomas.Id` == as.character(input$x))
  })


  output$poblacion22 <- renderText({
    aux <- data_poblacion2()
    formatC(aux$Valor, digits = 0, format = "f", big.mark = ".", decimal.mark = ",")
  })



  # Salarios medios -----------------------

  data_dist_reactive <- reactive({
    data_dist %>%
      filter(Anyo == input$anyo) %>%
      filter(`Comunidades.autónomas.Id` == as.character(input$x))
  })


  output$sueldomedio <- renderText({
    aux <- data_dist_reactive()

    paste0(formatC(aux$Valor, digits = 0, format = "f", big.mark = ".", decimal.mark = ","), " €")
  })



  # Porcentaje de poblacion

  data_poblacion2b <- reactive({
    100 * sum(as.numeric(data_poblacion %>%
      filter(Anyo == input$anyo & `Comunidades.y.ciudades.autónomas.Id` == as.character(input$x)) %>%
      select(Valor))) / (as.numeric(data_poblacion %>%
      filter(Anyo == input$anyo & `Comunidades.y.ciudades.autónomas.Id` == "16473") %>%
      select(Valor)))
  })



  output$poblacion_porcentaje <- renderText(paste0(formatC(data_poblacion2b(), digits = 2, format = "f", big.mark = ".", decimal.mark = ","), " %"))
}
