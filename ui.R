# https://shiny.posit.co/blog/posts/bslib-dashboards/#hello-dashboards
#shinylive 

#shinylive::export("/Users/davpero/INE/r_shiny","/Users/davpero/INE/r_shiny/docs")



#Error in `get_github_wasm_assets()`:
#! Can't find GitHub release for github::es-ine/ineapir@HEAD
#! Ensure a GitHub release exists for the package repository reference: "HEAD".
#ℹ Alternatively, install a CRAN version of this package to use the default Wasm binary
#  repository.
#Caused by error in `gh::gh()`:
#! GitHub API error (404): Not Found
#✖ URL not found: <https://api.github.com/repos/es-ine/ineapir/releases/tags/HEAD>
#ℹ Read more at
#  <https://docs.github.com/rest/releases/releases#get-a-release-by-tag-name>
#Run `rlang::last_trace()` to see where the error occurred.



library(shiny)
library(shinydashboard)
library(bslib)
library(ggplot2)
library(ineapir)
library(dplyr)
library(sf) #read_sf
library(leaflet) #map
# ineapir -------------------------------------------------------------------

# DATOS ECOICOP
# Obtener ids y nombres comu autonomas para selector
ids = get_metadata_table_varval(25143) %>% 
  filter(Fk_Variable %in% c(70, 349)) %>%
  pull(Id)

nombres = get_metadata_table_varval(25143) %>%
  filter(Fk_Variable %in% c(70, 349)) %>%
  pull(Nombre)

# Crear estructura para el selector de años
selector_lugar <- setNames(ids, nombres)








# Bueno -------------------------------------------------------------------







# Setup -------------------------------------------------------------------

data(penguins, package = "palmerpenguins")

# Turn on thematic for theme-matched plots
#thematic::thematic_shiny(font = "auto")
#theme_set(theme_bw(base_size = 16))

# Calculate column means for the value boxes
means <- colMeans(
  penguins[c("bill_length_mm", "bill_length_mm", "body_mass_g")],
  na.rm = TRUE
)


# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  includeCSS("www/style.css"),
  title = "INE dashboard",
  sidebar = sidebar(
    varSelectInput(
      "color_by", "Ambito geográfico", 
      penguins[c("species", "island", "sex")], 
      selected = "species"
    ),
    selectizeInput(
      "x", "Ambito geográfico", 
      selector_lugar,
      selected=9002
    )
    ,
    selectizeInput(
      "anyo", "Año", 
      choices = as.character(seq(2010,2022,1)),
      selected =2022
    )
    
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Número de habitantes", value = textOutput("poblacion22") , theme = value_box_theme(
        bg = "#FFFFFF",
        fg = "#457E76"
      ), showcase = fontawesome::fa_i("people-group"),
      showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
      height = NULL
    ),
    value_box(
      title = "Sueldo medio bruto", value =textOutput("sueldomedio"), theme = value_box_theme(
        bg = "#FFFFFF",
        fg = "#457E76"
      ), showcase = bsicons::bs_icon("currency-euro"),
      showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
      height = NULL
    ),
    value_box(
      title = "Porcenetaje de la población total", value = textOutput("poblacion_porcentaje"), 
      theme = value_box_theme(bg = "#FFFFFF", fg = "#457E76"),
      showcase = bsicons::bs_icon("percent"), showcase_layout = "left center",
      full_screen = FALSE, fill = TRUE, height = NULL
    )
    
    ),
  layout_column_wrap(
    width=1/2,
    heigth=400,
    card(
      full_screen = TRUE,
      card_header("Ambito geográfico"),
      leafletOutput('map')  
    ),
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
    card(
      full_screen = TRUE,
      card_header("Body Mass"),
      plotOutput("body_mass")
    ),
    card(
      full_screen = TRUE,
      card_header("Bill Length"),
      plotOutput("bill_length")
    ),
    card(
      full_screen = TRUE,
      card_header("Bill depth"),
      plotOutput("bill_depth")
    ))
    
  )
)

