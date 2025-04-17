# UI Module for Map Page with File Upload
mod_map_page_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    # File upload section
    fluidRow(
      column(
        width = 12,
        div(
          class = "dark-box",
          div(
            class = "red-title-box",
            h3("Upload Your Shapefile")
          ),
          wellPanel(
            fileInput(ns("shapefile_zip"), "Upload Shapefile (as .zip file)",
                      accept = c(".zip"), multiple = FALSE),
            helpText("Please upload a zip file containing .shp, .dbf, .shx, and .prj files"),
            conditionalPanel(
              condition = sprintf("input['%s-shapefile_zip'] !== null", ns("")),
              selectInput(ns("value_column"), "Select Column for Coloring:", choices = NULL)
            )
          )
        )
      )
    ),
    
    # Conditional panel for when data is ready to display
    conditionalPanel(
      condition = sprintf("input['%s-shapefile_zip'] !== null", ns("")),
      
      fluidRow(
        column(
          width = 12,
          div(
            class = "dark-box",
            div(
              class = "red-title-box",
              textOutput(ns("map_title"))
            ),
            
            # Description
            div(
              style = "margin: 10px; font-size: 14px;",
              "This interactive map displays your uploaded shapefile with color coding based on your selected attribute."
            ),
            
            # Map
            leafletOutput(ns("map"), width = "90%", height = "500px"),
            br(),
            actionButton(ns("btn_fullscreen"), "Enlarge Map"),
            downloadButton(ns("download_map"), "Download Map as HTML")
          )
        )
      )
    )
  )
}

# Server Module for Map Page
mod_map_page_server <- function(id){
  moduleServer(id, function(input, output, session){
    
    # Reactive value to store shapefile data
    shape_data <- reactiveVal(NULL)
    
    # Process uploaded shapefile
    observeEvent(input$shapefile_zip, {
      req(input$shapefile_zip)
      
      # Create a temporary directory to extract the zip file
      temp_dir <- tempdir()
      
      # Extract the zip file
      zip_file <- input$shapefile_zip$datapath
      utils::unzip(zip_file, exdir = temp_dir)
      
      # Find the .shp file in the extracted directory
      shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
      
      if (!is.na(shp_file)) {
        # Read the shapefile
        tryCatch({
          sf_data <- sf::st_read(shp_file)
          shape_data(sf_data)
          
          # Update column choices for coloring
          column_choices <- names(sf_data)[sapply(sf_data, is.numeric)]
          updateSelectInput(session, "value_column", choices = column_choices)
          
        }, error = function(e) {
          showNotification(paste("Error reading shapefile:", e$message), type = "error")
        })
      } else {
        showNotification("No .shp file found in the uploaded zip file", type = "error")
      }
    })
    
    # Reactive title
    output$map_title <- renderText({
      req(shape_data(), input$value_column)
      paste0("Interactive Map: ", input$shapefile_zip$name, " - Colored by ", input$value_column)
    })
    
    # Render Leaflet map
    output$map <- renderLeaflet({
      req(shape_data(), input$value_column)
      
      # Convert to simple features if it's not already
      sf_data <- shape_data()
      
      # Check if selected column exists and is numeric
      if (!(input$value_column %in% names(sf_data)) || 
          !is.numeric(sf_data[[input$value_column]])) {
        return(NULL)
      }
      
      # Create color palette based on the selected column
      pal <- colorNumeric(
        palette = "viridis",
        domain = sf_data[[input$value_column]],
        na.color = "grey50"
      )
      
      # Create the map
      leaflet(sf_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(sf_data[[input$value_column]]),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          popup = ~paste0(
            "<strong>", input$value_column, ":</strong> ", 
            round(sf_data[[input$value_column]], 3)
          )
        ) %>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = sf_data[[input$value_column]],
                  title = input$value_column)
    })
    
    # Fullscreen map in modal
    observeEvent(input$btn_fullscreen, {
      showModal(modalDialog(
        title = "Map in Full Screen",
        size = "l",
        easyClose = TRUE,
        leafletOutput(session$ns("map_full"), width = "100%", height = "600px")
      ))
    })
    
    # Render fullscreen map
    output$map_full <- renderLeaflet({
      req(shape_data(), input$value_column)
      
      sf_data <- shape_data()
      
      # Create color palette based on the selected column
      pal <- colorNumeric(
        palette = "viridis",
        domain = sf_data[[input$value_column]],
        na.color = "grey50"
      )
      
      # Create the map
      leaflet(sf_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(sf_data[[input$value_column]]),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          popup = ~paste0(
            "<strong>", input$value_column, ":</strong> ", 
            round(sf_data[[input$value_column]], 3)
          )
        ) %>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = sf_data[[input$value_column]],
                  title = input$value_column)
    })
    
    # Download map as HTML
    output$download_map <- downloadHandler(
      filename = function() {
        paste0("map-", Sys.Date(), ".html")
      },
      content = function(file) {
        req(shape_data(), input$value_column)
        
        sf_data <- shape_data()
        
        # Create color palette based on the selected column
        pal <- colorNumeric(
          palette = "viridis",
          domain = sf_data[[input$value_column]],
          na.color = "grey50"
        )
        
        # Create the map
        map <- leaflet(sf_data) %>%
          addTiles() %>%
          addPolygons(
            fillColor = ~pal(sf_data[[input$value_column]]),
            fillOpacity = 0.7,
            color = "white",
            weight = 1,
            popup = ~paste0(
              "<strong>", input$value_column, ":</strong> ", 
              round(sf_data[[input$value_column]], 3)
            )
          ) %>%
          addLegend("bottomright", 
                    pal = pal, 
                    values = sf_data[[input$value_column]],
                    title = input$value_column)
        
        saveWidget(map, file)
      }
    )
  })
}

# Main app
ui <- fluidPage(
  titlePanel("Shapefile Viewer"),
  
  # Include CSS for styling
  tags$head(
    tags$style(HTML("
      .dark-box {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
        margin-bottom: 20px;
      }
      .red-title-box {
        background-color: #dc3545;
        color: white;
        padding: 10px;
        margin-bottom: 15px;
        border-radius: 3px;
      }
    "))
  ),
  
  mod_map_page_ui("map_page")
)

server <- function(input, output, session) {
  mod_map_page_server("map_page")
}

# Required packages for the app
# install.packages(c("shiny", "leaflet", "sf", "htmlwidgets"))
library(shiny)
library(leaflet)
library(sf)
library(htmlwidgets)

# Run the app
shinyApp(ui = ui, server = server)