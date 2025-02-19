#
#

server <- function(input, output, session) {
 
  
  # Upload Data ----------------------------------------------------------------------------------------------------------
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
  
  
   
  # Map ------------------------------------------------------------------------
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
  
  # Deployment check -----------------------------------------------------------
  
  output$deployment_dates <- renderPlotly({
    # format dates
    # start dates
    deployments$start_date <- ymd(deployments$start_date)
    # end dates
    deployments$end_date   <- ymd(deployments$end_date)
    # durations
    deployments$days <- interval(deployments$start_date, deployments$end_date)/ddays(1)
    # image timestamps
    images$timestamp <- ymd_hms(images$timestamp)
    
    # Call the plot
    p <- plot_ly()
    
    # We want a separate row for each 'placename' - so lets turn it into a factor
    deployments$placename <- as.factor(deployments$placename)
    
    # loop through each place name
    for(i in seq_along(levels(deployments$placename)))
    {
      #Subset the data to just that placename
      tmp <- deployments[deployments$placename==levels(deployments$placename)[i],]
      # Order by date
      tmp <- tmp[order(tmp$start_date),]
       # Loop through each deployment at that placename
      for(j in 1:nrow(tmp))
      {
        # Add a box reflecting the first and last image for each deployment
        tmp_img <- images[images$deployment_id==tmp$deployment_id[j],]
        # Only run if there are images
        if(nrow(tmp_img>0))
          {
        # Add an orange colour block to show when images were collected
        p <- add_trace(p, 
                       #Use the start and end date as x coordinates
                       x = c(min(tmp_img$timestamp), max(tmp_img$timestamp)), 
                       #Use the counter for the y coordinates
                       y = c(i,i), 
                       # State the type of chart
                       type="scatter",
                       # make a line that also has points
                       mode = "lines", 
                       # Add the deployment ID as hover text
                       hovertext=tmp$deployment_id[j], 
                       color=I("orange"),
                       # Color it all black
                       line=list(
                       width=8,
                       opacity=0.7),
                       # Suppress the legend
                       showlegend = FALSE)
            }
        # Add a black line to 'p' denotting the start and end periods of each deployment
        p <- add_trace(p, 
                       #Use the start and end date as x coordinates
                       x = c(tmp$start_date[j], tmp$end_date[j]), 
                       #Use the counter for the y coordinates
                       y = c(i,i), 
                       # State the type of chart
                       type="scatter",
                       # make a line that also has points
                       mode = "lines+markers", 
                       # Add the deployment ID as hover text
                       hovertext=tmp$deployment_id[j], 
                       # Color it all black
                       color=I("black"), 
                       # Suppress the legend
                       showlegend = FALSE)
      }
      
    }
    # Add a categorical y axis
    p <- p %>%   layout(yaxis = list(
      ticktext = as.list(levels(deployments$placename)), 
      tickvals = as.list(1:length(levels(deployments$placename))),
      tickmode = "array"))
  })



# Independent data creation  ---------------------------------------------------

# I think this needs to be done with reactives ultimately 
# 
  # Effort lookup
  output$effort_lookup <- renderTable({
      
      tmp <- deployments[is.na(deployments$end_date)==F,]
      
      # Create an empty list to store our days
      daily_lookup <- list()
      
      # Loop through the deployment dataframe and create a row for every day the camera is active
      for(i in 1:nrow(tmp))
      {
        if(ymd(tmp$start_date[i])!=ymd(tmp$end_date[i]))
        {
          daily_lookup[[i]] <- data.frame("date"=seq(ymd(tmp$start_date[i]), ymd(tmp$end_date[i]), by="days"), "placename"=tmp$placename[i])
        }
      }
      
      # Merge the lists into a dataframe
      row_lookup <- bind_rows(daily_lookup)
      
      # Remove duplicates - when start and end days are the same for successive deployments
      row_lookup <- row_lookup[duplicated(row_lookup)==F,]
      
      row_lookup
      
    })
    
  # Independence selection
  output$ind_thresh <- renderText({ input$ind_thresh })
  # Count column
  output$count_col <- renderText({ input$count_col })
  
  
  
# THE CLOSING BRACKET    
}



