#
#

server <- function(input, output, session) {
 
  
  # Upload Data ----------------------------------------------------------------------------------------------------------
  
  # Reactive expression to read in the data files provided by the user
  data_store <- reactiveValues(dfs = list())
  
  observeEvent(input$files, {
    
    # Ensure files exist
    req(input$files)
    
    # List to store dataframes
    dfs <- list()
    
    # Iterate through uploaded files
    for (i in seq_along(input$files$datapath)) {
      file_path <- input$files$datapath[i]
      file_name <- input$files$name[i]
      
      if (grepl("\\.csv$", file_name, ignore.case = TRUE)) {
        
        # Directly read CSV file
        dfs[[file_name]] <- read.csv(file_path, stringsAsFactors = FALSE)
        
      } else if (grepl("\\.zip$", file_name, ignore.case = TRUE)) {
        
        # Unzip to a temporary directory
        temp_dir <- file.path(tempdir(), paste0("unzipped_", i))
        dir.create(temp_dir, showWarnings = FALSE)
        unzip(file_path, exdir = temp_dir)
        
        # List extracted files
        all_extracted <- list.files(temp_dir, full.names = TRUE, recursive = TRUE)
        
        # Filter for CSV files
        csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
        
        if (length(csv_files) == 0) {
          message("No CSV files found in ZIP.")
        }
        
        # Read all extracted CSVs
        for (csv in csv_files) {
          dfs[[basename(csv)]] <- read.csv(csv, stringsAsFactors = FALSE)
        }
      }
    }
    
    # Merge all of the image files included into a single csv
    # split out the image files
    images_data <- lapply(names(dfs)[grepl("^images", names(dfs))], function(name) dfs[[name]])
    
    if (length(images_data) > 0) {
      dfs[["images.csv"]] <- bind_rows(images_data)
      # Remove the un merged files
      dfs <- dfs[!grepl("^images_", names(dfs))]
    }
    
    # HOUSEKEEPING
    # Format appropriate dates and create a species column
    if (length(dfs) > 0) {
      dfs[["images.csv"]]$sp <- paste0(dfs[["images.csv"]]$genus, ".", dfs[["images.csv"]]$species)
      dfs[["images.csv"]]$timestamp <- ymd_hms(dfs[["images.csv"]]$timestamp)
      dfs[["deployments.csv"]]$start_date <- ymd_hms(dfs[["deployments.csv"]]$start_date)
      dfs[["deployments.csv"]]$end_date <- ymd_hms(dfs[["deployments.csv"]]$end_date)
      dfs[["deployments.csv"]]$days <- interval(dfs[["deployments.csv"]]$start_date, dfs[["deployments.csv"]]$end_date)/ddays(1)
      # Add a placename variable to the image data. Its really useful
      dfs[["images.csv"]] <- left_join(dfs[["images.csv"]], dfs[["deployments.csv"]][,c("deployment_id", "placename")])
    } 
    
    # Ensure the reactiveValues object updates properly
    isolate({
      if (length(dfs) > 0) {
        data_store$dfs <- dfs
      } else {
        data_store$dfs <- list()  # Reset if no valid CSVs found
      }
    })
    
  })
  
  observeEvent(input$clear_files, {
    
    data_store$dfs <- list()
    
    shinyjs::reset("files")
    
  })
  
  # Dropdown to select a file to preview
  output$selected_file <- renderUI({
    
    req(length(data_store$dfs) > 0)  # Ensure data exists
    
    selectInput("choice", "Select a File to Preview",
                choices = names(data_store$dfs),
                selected = names(data_store$dfs)[1])
  
    })
  
  # Render a preview of the selected file
  output$custom_data_preview <- renderDT({
    
    req(input$choice, data_store$dfs)
    
    DT::datatable(data_store$dfs[[input$choice]],
                  options = list(scrollX = TRUE))
    
  })
  
   
  # Reactive Map ------------------------------------------------------------------------
  output$map <- renderLeaflet({
    
    #req(data_store())
    deployments<- data_store$dfs[["deployments.csv"]]
    locs <- deployments |>
      select(placename, longitude, latitude) |>
      distinct() |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
    
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
    
    # Call the saved data
    deployments <- data_store$dfs[["deployments.csv"]]
    images <- data_store$dfs[["images.csv"]]
    # format dates
    # start dates
    # deployments$start_date <- ymd_hms(deployments$start_date)
    # end dates
    # deployments$end_date   <- ymd_hms(deployments$end_date)
    # durations
    #deployments$days <- interval(deployments$start_date, deployments$end_date)/ddays(1)
    # image timestamps
    #images$timestamp <- ymd_hms(images$timestamp)
    
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

  # Create waiter instance (for showing users that something is happening when they click on the button)
  w <- Waiter$new(id = "ind_run", html = spin_ellipsis(), color = "grey20")
  
  # When you click run on the ui, the following happens
  observeEvent(input$ind_run, {
    
    # Show spinner when processing starts
    w$show()
    
    # Run the create_ind_detect function on the imported data:
    # Ensure data exists
    req(length(data_store$dfs) > 0)
    
    results <- create_ind_detect(deployments = data_store$dfs[["deployments.csv"]],
                                 images = data_store$dfs[["images.csv"]],
                                 threshold = input$ind_thresh,
                                 count_column = input$ind_count)
    
    # Hide spinner when processing is done
    w$hide()
    
    # Render a preview of the selected dataframe
    
    output$test <- renderDT({
      
      req(length(data_store$dfs) > 0)
      
      DT::datatable(results[["independent_total_observations"]],
                    options = list(scrollX = TRUE))
      
    })

  })
  


}



