#
#

server <- function(input, output, session) {
 
  
  # Upload Data ----------------------------------------------------------------------------------------------------------
  
  # Reactive expression to read in the data files provided by the user
  data_store <- reactiveValues(dfs = list())
  analysis_data_store <- reactiveValues(results = list())
  
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
      tickmode = "array"),
      height=length(levels(deployments$placename))*20)
  })

  #update the count  dropdown dynamically based on available numeric inputs
  observe({
    updateSelectInput(session, "ind_count", choices = names(data_store$dfs[["images.csv"]])[sapply(data_store$dfs[["images.csv"]], function(col) is.numeric(col) && all(col == floor(col)))])
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
    
    
    # Dropdown to select a file to preview
    output$selected_analysis_file <- renderUI({
      
      req(length(results) > 0)  # Ensure data exists
      
      selectInput("analysis_choice", "Select a File to Preview",
                  choices = names(results),
                  selected = names(results)[1])
      
    })
    
    # Render a preview of the selected file
    output$custom_analysis_data_preview <- renderDT({
      
      req(input$analysis_choice, results)
      
      DT::datatable(results[[input$analysis_choice]],
                    options = list(scrollX = TRUE))
    

  })

######################      
# detection summaries -----------------------------------------------------------    
    
    output$capture_summary <- renderPlotly({
      
      # Call the saved data
      deployments <- data_store$dfs[["deployments.csv"]]
      images <- data_store$dfs[["images.csv"]]
      
    long_obs <- results[["independent_total_observations"]] %>% 
      pivot_longer(cols=results[["species_list"]]$sp,  # The columns we want to create into rows - species
                   names_to="sp",       # What we what the number column to be called
                   values_to = "count") # Takes the values in the species columns and calls them `count`
    
    
    # We can them summaries those using dplyr
    tmp <- long_obs %>%                   # Take the long observation data frame `long_obs` 
      group_by(sp) %>%            # Group by species
      summarise(count=sum(count)) # Sum all the independent observations
    
    # Add it to the sp_summary dataframe
    sp_summary <- left_join(results[["species_list"]], tmp)
    
    # Calculate raw occupancy
    # We use the mutate function to mutate the column
    total_binary <-   results[["independent_total_observations"]] %>%    # The total obs dataframe              
      mutate(across(results[["species_list"]]$sp, ~+as.logical(.x)))  # across all of the species columns, make it binary
    
    # Flip the dataframe to longer - as before
    long_bin <- total_binary %>% 
      pivot_longer(cols=results[["species_list"]]$sp, names_to="sp", values_to = "count") # Takes the species names columns, and makes them unique rows with "sp" as the key 
    
    # We can now sum the presence/absences and divide by the number of survey locations
    tmp <- long_bin %>% 
      group_by(sp) %>% 
      summarise(occupancy=sum(count)/nrow(results[["camera_locations"]])) # divided the sum by the number of sites
    
    # add the results to the sp_summary
    sp_summary <- left_join(sp_summary, tmp)
    
    # Order the summaries to something sensible
    sp_summary <- sp_summary[order(sp_summary$count),]
    
    yform <- list(categoryorder = "array",
                  categoryarray = sp_summary$sp)
    
    xform <- list(title="Captures")
    
    # Capture rate
    fig1 <- plot_ly(x = sp_summary$count, y = sp_summary$sp, type = 'bar', orientation = 'h', name="count") %>% 
      layout(yaxis = yform, xaxis=xform, height=nrow(sp_summary)*20) # Try to control the height
    
    yform <- list(categoryorder = "array",
                  categoryarray = sp_summary$sp,
                  showticklabels=F)
    xform <- list(title="Occupancy")
    
    
    # Occupancy
    fig2 <- plot_ly(x = sp_summary$occupancy, y = sp_summary$sp, type = 'bar', orientation = 'h', name="occupancy") %>% 
      layout(yaxis = yform, xaxis=xform)
    
    subplot(nrows=1,fig1, fig2, titleX = T)
    
    })  
    
 
#############################
# Temporal patterns -----------------------------------------------------------    

    
    # Look for the selected option
    observe({
      species_dynamic <- results[["species_list"]]$sp[order(results[["species_list"]]$sp)]
      updateSelectInput(session, "selected_species", choices = species_dynamic, selected = species_dynamic[1])
    })
    
 
  # # Static Plot 1: Number of Active Cameras Over Time
  output$camera_effort_plot <- renderPlotly({

    # Pull in the right data
    mon_obs <- results[["independent_monthly_observations"]]
    sp_summary <- results[["species_list"]]
    
    mon_summary <- mon_obs %>%        # Use the monthly observations dataframe
      group_by(date) %>%              # Group by the date
      summarise(locs_active=n(),      # Count the number of active cameras
                cam_days=sum(days))   # And sum the active days

    mon_summary <- mon_obs %>% 
      group_by(date) %>%  
      summarise(across(sp_summary$sp, sum, na.rm=TRUE)) %>% # summarise across all of 
      # the species columns 
      left_join(x=mon_summary) 
    
     # Update date format
      mon_summary$date <- ym(mon_summary$date)
      
    p1  <- plot_ly(mon_summary, x = ~date, y = ~locs_active, type = "scatter", mode = "lines",
                   line = list(color = "blue"), name = "Active Cameras") %>%
      layout(title = "Active Cameras Over Time",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Active Cameras", rangemode = "tozero"))  # Ensure y-axis starts at 0
    
    
    mon_summary$all.sp <- rowSums(mon_summary[, sp_summary$sp])
    mon_summary$all.cr <- mon_summary$all.sp/(mon_summary$cam_days/100)

    p2 <- plot_ly(mon_summary, x = ~date, y = ~all.cr, type = "scatter", mode = "lines",
                  line = list(color = "darkred"), name = "Capture Rate per 100 days") %>%
      layout(title = "Overall Capture Rates (All Species)",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Capture Rate per 100 days", rangemode = "tozero"))  # Ensure y-axis starts at 0
    
    # Combine plots vertically using subplot()
    subplot(p1, p2, nrows = 2, shareX = TRUE, titleY = TRUE) %>% layout(height = 700)
    
    })

    # Interactive Plot: Capture Rates for Selected Species
    output$species_trends_plot <- renderPlot({

      req(input$selected_species)  # Ensure a species is selected
      
      # Pull in the right data
      mon_obs <- results[["independent_monthly_observations"]]
      sp_summary <- results[["species_list"]]
      
      mon_summary <- mon_obs %>%        # Use the monthly observations dataframe
        group_by(date) %>%              # Group by the date
        summarise(locs_active=n(),      # Count the number of active cameras
                  cam_days=sum(days))   # And sum the active days
      
      mon_summary <- mon_obs %>% 
        group_by(date) %>%  
        summarise(across(sp_summary$sp, sum, na.rm=TRUE)) %>% # summarise across all of 
        # the species columns 
        left_join(x=mon_summary) 
      
      # Update date format
      mon_summary$date <- ym(mon_summary$date)
      
      # Wide to long
      long_df <- mon_summary %>%
        pivot_longer(
          cols = sp_summary$sp,  # All species coilumns will be gathered
          names_to = "sp",  # New column for species names
          values_to = "Count"  # New column for counts
        )
      
      
      
      filtered_data <- long_df %>%
        filter(sp == input$selected_species)

      ggplot(filtered_data, aes(x = date, y = Count)) +
        geom_line(color = "darkgreen", size = 1) +
        labs(title = paste("Capture Rates of", input$selected_species),
             x = "Date", y = "Captures") +
        ylim(0, NA) + 
        theme_minimal()
    })


    
#################
# Spatial patterns 
    
    # Look for the selected option
    observe({
      species_dynamic <- results[["species_list"]]$sp[order(results[["species_list"]]$sp)]
      updateSelectInput(session, "selected_species_map", choices = species_dynamic, selected = species_dynamic[1])
    })
    
    
    output$capture_map <- renderLeaflet({
      
      req(input$selected_species_map) 
      
      wide_obs <- results[["independent_total_observations"]] 
      locs<- results[["camera_locations"]]
      
      total_obs <- left_join(wide_obs, locs)
      
      focal_cr <- total_obs %>% mutate(CaptureRate = total_obs[[input$selected_species_map]] / (days / 100))
      
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addCircleMarkers(
          data = focal_cr,
          lng = ~longitude,
          lat = ~latitude,
          radius = ~(CaptureRate / max(CaptureRate, na.rm = TRUE) * 10) + 1,
          stroke = FALSE,
          fillOpacity = 0.6,
          popup = ~paste(placename, "- Capture Rate:", round(CaptureRate, 2))
        )
    })
    

#################
# Spatial co-ocurance 
    
    
    
    output$corrplot <- renderPlot({
      
      total_obs <- results[["independent_total_observations"]] 
      tmp <- total_obs[,!(colnames(total_obs)%in% c("placename", "days"))]
      M <- cor(tmp[, colSums(tmp)>5])
      
      
      focal_cr <- total_obs %>% mutate(CaptureRate = total_obs[[input$selected_species_map]] / (days / 100))
      
      corrplot(M, method="color", 
               type="upper", 
               order="hclust",
               # addCoef.col = "black", # We suppress the coefs to make a cleaner plot
               tl.col="black", tl.srt=45, #Text label color and rotation
               diag=FALSE)
    })
    
    
    
        
        
###########    


      # If you need code to check an object      
      # 
      # output$test_table <- renderDT({
      #   mon_obs <- results[["independent_monthly_observations"]]
      #   
      #   
      #   mon_summary <- mon_obs %>%        # Use the monthly observations dataframe
      #     group_by(date) %>%              # Group by the date
      #     summarise(locs_active=n(),      # Count the number of active cameras
      #               cam_days=sum(days))   # And sum the active days 
      #   
      #   
      #   datatable(mon_summary, options = list(pageLength = 5, autoWidth = TRUE))
      # })
      
      
      
       
})
  
  
  
  
  
    

  
}

