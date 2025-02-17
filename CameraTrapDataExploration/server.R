#
#

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    
    locs <- deployments |>
      select(placename, longitude, latitude) |>
      distinct() |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
      filter(!placename == "ALG069")
    
    cam <- makeAwesomeIcon(
      icon = "camera",
      iconColor = "black",
      library = "ion",
      markerColor = "white"
    )
    
    locs |>
      leaflet() |>
      # Add a satellite image layer
      addProviderTiles(providers$Esri.WorldImagery, group="Satellite") |>  
      addProviderTiles(providers$Esri.WorldTopoMap, group="Base") |>    
      addAwesomeMarkers(popup=paste(locs$placename),
                        icon = cam) |>
      # add a layer control box to toggle between the layers
      addLayersControl(
        baseGroups = c("Satellite", "Base"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  
  output$custom_data_preview <- renderDT({
    
    # Ensure a file is uploaded before proceeding
    req(input$custom_upload)
    
    # Check the file extension
    ext <- tools::file_ext(input$custom_upload$name)
    if (!ext %in% c("csv", "xlsx")) {
      stop("Invalid file; please upload a .csv or .xlsx file")
    }
    
    # Read the uploaded file based on the extension
    if (ext == "csv") {
      df <- read_csv(input$custom_upload$datapath)
    } else if (ext == "xlsx") {
      df <- read_excel(input$custom_upload$datapath)
    }
    
    DT::datatable(df)
    
  })
  
}
