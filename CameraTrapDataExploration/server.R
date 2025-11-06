#
#

server <- function(input, output, session) {
 
  
  # Upload Data ----------------------------------------------------------------------------------------------------------
  
  # Reactive expression to read in the data files provided by the user
  data_store <- reactiveValues(dfs = list())
  analysis_data_store <- reactiveValues(results = list())
  results_store <- reactiveValues(data = NULL)
  # An a flag to show when the results are ready for appropriate error messages
  results_ready <- reactiveVal(FALSE)
  button_clicked <- reactiveVal(FALSE)
  
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
        dfs[[file_name]] <- read_csv(file_path)
        
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
          dfs[[basename(csv)]] <- read_csv(csv)
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
      
      if ("images.csv" %in% names(dfs)) {
        dfs[["images.csv"]]$sp <- paste0(dfs[["images.csv"]]$genus, ".", dfs[["images.csv"]]$species)
        dfs[["images.csv"]]$timestamp <- ymd_hms(dfs[["images.csv"]]$timestamp)
      }
      
      if ("deployments.csv" %in% names(dfs)) {
        dfs[["deployments.csv"]]$start_date <- ymd(dfs[["deployments.csv"]]$start_date)
        dfs[["deployments.csv"]]$end_date <- ymd(dfs[["deployments.csv"]]$end_date)
        dfs[["deployments.csv"]]$days <- interval(
          dfs[["deployments.csv"]]$start_date,
          dfs[["deployments.csv"]]$end_date
        ) / ddays(1)
      }
      
      # Only join if both exist
      if (all(c("images.csv", "deployments.csv") %in% names(dfs))) {
        dfs[["images.csv"]] <- left_join(
          dfs[["images.csv"]],
          dfs[["deployments.csv"]][, c("deployment_id", "placename")],
          by = "deployment_id"
        )
      }
    }
    
    # WildTrax main reports
    
    if (any(str_detect(names(dfs), "main_report.csv$"))) {
      
      # Find name that ends in "main_report.csv"
      idx <- grep("main_report.csv$", names(dfs))
      
      # Rename that element to main_report.csv
      names(dfs)[idx] <- "main_report.csv"
      
      # Create deployments.csv
      dfs[["deployments.csv"]] <- dfs[["main_report.csv"]] |>
        group_by(project_id, location, latitude, longitude) |>
        summarise(start_date = min(as.Date(image_date_time)),
                  end_date = max(as.Date(image_date_time))) |>
        mutate(deployment_id = paste0(location),   #, "_", start_date),
               feature_type = "") |>
        select(project_id, deployment_id, placename = location, longitude, latitude, start_date, end_date, feature_type) |>
        ungroup() |>
        mutate(days = interval(start_date, end_date) / ddays(1))
      
      # Create images.csv
      dfs[["images.csv"]] <- dfs[["main_report.csv"]] |>
        filter(!species_common_name %in% c("STAFF/SETUP", "Human"),
               !individual_count == "VNA") |>
        group_by(image_date_time) |>
        filter(row_number() == 1) |>
        ungroup() |>
        mutate(is_blank = ifelse(species_common_name == "NONE", 1, 0),
               class = "Mammalia",
               family = "",
               genus = "",
               order = "",
               sp = str_replace_all(species_common_name, " |, ", "."),
               individual_count = as.numeric(individual_count),
               placename = location,
               common_name = species_common_name) |>
        select(project_id,
               deployment_id = location,                      # This nees to change if we want to include multiple deployments
               placename,
               is_blank,
               class,
               species = species_common_name,
               common_name,
               sp,
               timestamp = image_date_time,
               number_of_objects = individual_count,
               age = age_class,
               sex = sex_class,
               family, genus, order)
      
      # Remove main_report.csv
      dfs <- dfs[!grepl("main_report.csv$", names(dfs))]
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
    validate(
      need(data_store$dfs[["deployments.csv"]], 
           "Please upload deployment data to view this content")
    )
    #req(data_store())
    deployments<- data_store$dfs[["deployments.csv"]]
    locs <- deployments |>
      select(placename, longitude, latitude) |>
      distinct() |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 
    #specify the station label 
    locs$label_text <- paste0(
      locs$placename, "<br>",
      "Lat: ", round(st_coordinates(locs)[,1], 5), "<br>",
      "Long: ", round(st_coordinates(locs)[,2], 5)
    )
    
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
      addAwesomeMarkers(label = ~lapply(label_text, HTML),
                        icon = cam) |>
      # add a layer control box to toggle between the layers
      addLayersControl(
        baseGroups = c("Satellite", "Base"),
        options = layersControlOptions(collapsed = FALSE)) |>
      #Add a scale bar
      addScaleBar(position = "topleft")

    
  })
  
  # Deployment check -----------------------------------------------------------
  
  output$deployment_dates <- renderPlotly({
    
    validate(
      need(data_store$dfs[["deployments.csv"]], 
           "Please upload deployment and image data to view this content")
    )
    
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
                       hovertext=paste0(tmp$deployment_id[j]),
                                        #,"<br>",
                                        #,"Start: ",min(tmp_img$timestamp), "<br>",
                                        #,"End: ", max(tmp_img$timestamp)),           # NOT WORKING CURRENTLY
                       color=I("orange"),
                       # Color it all black
                       line=list(
                       width=8,
                       opacity=0.5),
                       # Suppress the legend
                       showlegend = FALSE)
            }
        # Add a black line to 'p' denotting the start and end periods of each deployment + we add one to the end dat as it plots at mignight and looks like some image data fall outside of the deployment window
        p <- add_trace(p, 
                       #Use the start and end date as x coordinates
                       #x = c(tmp$start_date[j], tmp$end_date[j]), 
                       x = c(as.POSIXct(paste(tmp$start_date[j], "00:01:00"), format = "%Y-%m-%d %H:%M:%S"),
                             as.POSIXct(paste(tmp$end_date[j], "23:59:00"), format = "%Y-%m-%d %H:%M:%S")),
                       
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
                       
                       marker = list(
                         symbol = c("circle", "triangle-left"),
                         size = 7,
                         color=c("black", "grey")),
                       
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

  
  # Independent data creation  ---------------------------------------------------
  
  
  # Update the count dropdown dynamically based on available numeric inputs
  observe({
    # Get all integer columns
    integer_cols <- names(data_store$dfs[["images.csv"]])[
      sapply(data_store$dfs[["images.csv"]], function(col) is.numeric(col) && all(col == floor(col)))
    ]
    
    # Exclude specific columns
    integer_cols <- setdiff(integer_cols, c("project_id", "is_blank"))
    
    # Set default selection
    default_selection <- if("number_of_objects" %in% integer_cols) {
      "number_of_objects"
    } else if(length(integer_cols) > 0) {
      integer_cols[1]
    } else {
      NULL
    }
    
    updateSelectInput(session, "ind_count", 
                      choices = integer_cols,
                      selected = default_selection)
  })
  

  # Create waiter instance (for showing users that something is happening when they click on the button)
  w <- Waiter$new(id = "ind_run", html = spin_ellipsis(), color = "grey20")
  
  
  # Track when the button is clicked
  output$button_clicked <- reactive({
    button_clicked()
  })
  outputOptions(output, "button_clicked", suspendWhenHidden = FALSE)
  
  
  # When you click run on the ui, the following happens
  observeEvent(input$ind_run, {
    
    # Set button clicked flag
    button_clicked(TRUE)
    
    # Show spinner when processing starts
    w$show()
    
    # Run the create_ind_detect function on the imported data:
    # Ensure data exists
    req(length(data_store$dfs) > 0)
    
    results <- create_ind_detect(deployments = data_store$dfs[["deployments.csv"]],
                                 images = data_store$dfs[["images.csv"]],
                                 threshold = input$ind_thresh,
                                 count_column = input$ind_count,
                                 include_classes = input$taxa_include,  # Add this
                                 include_humans = input$include_humans)  # Add this
    
    # Store results in reactive value so they are accessible later!
    results_store$data <- results
    
    # Set the Results flag to TRUE
    results_ready(TRUE)

    # Hide spinner when processing is done
    w$hide()
    
    # Dropdown to select a file to preview
    output$selected_analysis_file <- renderUI({
      
      req(length(results_store$data) > 0)  # Ensure data exists
      
      selectInput("analysis_choice", "Select an analysis file to preview:",
                  choices = names(results_store$data),
                  selected = names(results_store$data)[1])
      
    })
    
    # Render a preview of the selected file
    output$custom_analysis_data_preview <- renderDT({
      
      req(input$analysis_choice, results_store$data)
      
      DT::datatable(results_store$data[[input$analysis_choice]],
                    options = list(scrollX = TRUE))
  })
  
    output$results_box <- renderUI({
      req(results_store$data)  # Only render when results exists
      
      box(
        title = "Independent data",
        width = 12,
        downloadButton("download_results", "Download All Datasets (.zip)"),
        p(),
        uiOutput("selected_analysis_file"),
        DTOutput("custom_analysis_data_preview")
        
      )
    })
  
    
    # Download a zipfile of the formatted data
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("ind_data_",  input$ind_thresh,"_mins_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        req(results_store$data)
        
        # Create a temporary directory
        temp_dir <- tempfile()
        dir.create(temp_dir)
        
        # Write each dataframe as a CSV file
        for (i in seq_along(results_store$data)) {
          csv_name <- paste0(names(results_store$data)[i], ".csv")
          csv_path <- file.path(temp_dir, csv_name)
          write.csv(results_store$data[[i]], csv_path, row.names = FALSE)
        }
        
        # Get list of files to zip
        csv_files <- list.files(temp_dir, full.names = TRUE)
        
        # Create the zip file
        zip::zip(zipfile = file, 
                 files = basename(csv_files),
                 root = temp_dir)
      },
      contentType = "application/zip"
    )
    
    

######################      
# detection summaries -----------------------------------------------------------    
    
    
    # Create summary statistics output
    output$summary_stats <- renderUI({
      req(results_ready())
      
      # Calculate statistics
      total_stations <- nrow(results_store$data[["camera_locations"]])
      total_survey_nights <- sum(results_store$data[["independent_total_observations"]]$days)
      total_species <- nrow(results_store$data[["species_list"]])
      total_detections <- sum(results_store$data[["independent_total_observations"]][, results_store$data[["species_list"]]$sp])
      
      # Calculate nights per camera
      avg_nights <- mean(results_store$data[["independent_total_observations"]]$days)
      min_nights <- min(results_store$data[["independent_total_observations"]]$days)
      max_nights <- max(results_store$data[["independent_total_observations"]]$days)
      
      # Calculate date range
      mon_obs <- results_store$data[["independent_monthly_observations"]]
      date_range <- paste(format(min(ym(mon_obs$date)), "%b %Y"), "to", format(max(ym(mon_obs$date)), "%b %Y"))
      
      fluidRow(
        column(12,
               box(
                 title = "Survey Summary Statistics",
                 width = NULL,
                 column(3, 
                        tags$div(style = "text-align: center;",
                                 h3(total_stations, style = "color: #3c8dbc; margin: 5px;"),
                                 p("Unique Stations")
                        )
                 ),
                 column(3,
                        tags$div(style = "text-align: center;",
                                 h3(round(total_survey_nights, 0), style = "color: #3c8dbc; margin: 5px;"),
                                 p("Survey Nights")
                        )
                 ),
                 column(3,
                        tags$div(style = "text-align: center;",
                                 h3(total_species, style = "color: #3c8dbc; margin: 5px;"),
                                 p("Species Detected")
                        )
                 ),
                 column(3,
                        tags$div(style = "text-align: center;",
                                 h3(total_detections, style = "color: #3c8dbc; margin: 5px;"),
                                 p("Total Independent Detections")
                        )
                 ),
                 column(6,
                        tags$div(style = "text-align: center; margin-top: 10px;",
                                 p(strong("Avg Nights per Station: "), round(avg_nights, 1), 
                                   " (min: ", round(min_nights, 1), ", max: ", round(max_nights, 1), ")")
                        )
                 ),
                 column(6,
                        tags$div(style = "text-align: center; margin-top: 10px;",
                                 p(strong("Survey Period: "), date_range)
                        )
                 )
               )
        )
      )
    })
    
    # Set up the error message if the independent data doesnt exist
    output$capture_summary_output <- renderUI({
      if (!results_ready()) {
        box(
          width = 12,
          h4("Independent data not yet created", style = "color: #777; text-align: center; padding: 20px;")
        )
      } else {
        tagList(
          uiOutput("summary_stats"),
          plotlyOutput(outputId = "capture_summary", height = "auto")
        )
      }
    })
    
    # Create the plotly plot
    output$capture_summary <- renderPlotly({
      req(results_ready())
      
      # Call the saved data
      deployments <- data_store$dfs[["deployments.csv"]]
      images <- data_store$dfs[["images.csv"]]
      
      long_obs <- results_store$data[["independent_total_observations"]] %>% 
        pivot_longer(cols=results_store$data[["species_list"]]$sp,
                     names_to="sp",
                     values_to = "count")
      
      tmp <- long_obs %>%
        group_by(sp) %>%
        summarise(count=sum(count))
      
      sp_summary <- left_join(results_store$data[["species_list"]], tmp)
      
      total_binary <- results_store$data[["independent_total_observations"]] %>%
        mutate(across(results_store$data[["species_list"]]$sp, ~+as.logical(.x)))
      
      long_bin <- total_binary %>% 
        pivot_longer(cols=results_store$data[["species_list"]]$sp, names_to="sp", values_to = "count")
      
      tmp <- long_bin %>% 
        group_by(sp) %>% 
        summarise(occupancy=sum(count)/nrow(results_store$data[["camera_locations"]]))
      
      sp_summary <- left_join(sp_summary, tmp)
      sp_summary <- sp_summary[order(sp_summary$count),]
      
      yform <- list(categoryorder = "array", categoryarray = sp_summary$sp)
      xform <- list(title="Independent detections")
      
      fig1 <- plot_ly(x = sp_summary$count, y = sp_summary$sp, type = 'bar', orientation = 'h', name="count") %>% 
        layout(yaxis = yform, xaxis=xform, height=nrow(sp_summary)*20)
      
      yform <- list(categoryorder = "array", categoryarray = sp_summary$sp, showticklabels=F)
      xform <- list(title="Naive occupancy")
      
      fig2 <- plot_ly(x = sp_summary$occupancy, y = sp_summary$sp, type = 'bar', orientation = 'h', name="occupancy") %>% 
        layout(yaxis = yform, xaxis=xform)
      
      subplot(nrows=1, fig1, fig2, titleX = T) %>%
        layout(height = max(400, nrow(sp_summary) * 20))
    }) 
    
  

#############################
# Temporal patterns -----------------------------------------------------------    
    # Code to show error meesage before indenedent data is created
    # Temporal plots output
    output$temporal_plots_output <- renderUI({
      if (!results_ready()) {
        box(
          width = 12,
          h4("Independent data not yet created", style = "color: #777; text-align: center; padding: 20px;")
        )
      } else {
        species_choices <- results_store$data[["species_list"]]$sp[order(results_store$data[["species_list"]]$sp)]
        
        tagList(
          fluidRow(
            column(12, plotlyOutput("camera_effort_plot", height="700px"))  
          ),
          hr(),
          "You can also see the capture rate through time for each of the species included in the species list:",
          p(),
          fluidRow(
            column(4,
                   selectInput("selected_species", "Select Species:",
                               choices = species_choices,
                               selected = species_choices[1])
            ),
            column(8, plotOutput("species_trends_plot"))
          )
        )
      }
    })
    
    
    ########################
    
    
 
  # # Static Plot 1: Number of Active Cameras Over Time
  output$camera_effort_plot <- renderPlotly({

    # Pull in the right data
    mon_obs <- results_store$data[["independent_monthly_observations"]]
    sp_summary <- results_store$data[["species_list"]]
    
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
      mon_obs <- results_store$data[["independent_monthly_observations"]]
      sp_summary <- results_store$data[["species_list"]]
      
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
        ) %>%
        mutate(CaptureRate = Count / (cam_days / 100))  # Calculate capture rate per 100 days
      
      filtered_data <- long_df %>%
        filter(sp == input$selected_species)

      ggplot(filtered_data, aes(x = date, y = CaptureRate)) +
        geom_line(color = "darkgreen", size = 1) +
        labs(title = paste("Capture Rates of", input$selected_species),
             x = "Date", y = "Capture Rate per 100 days") +
        ylim(0, NA) + 
        theme_minimal()
    })


    
#################
# Spatial patterns 
    
   
    # Code to show error meesage before indenedent data is created
    # Spatial plots output
    output$spatial_plots_output <- renderUI({
      if (!results_ready()) {
        box(
          width = 12,
          h4("Independent data not yet created", style = "color: #777; text-align: center; padding: 20px;")
        )
      } else {
        species_choices <- results_store$data[["species_list"]]$sp[order(results_store$data[["species_list"]]$sp)]
        
        tagList(
          fluidRow(
            column(12, 
                   selectInput("selected_species_map", "Select Species:",
                               choices = species_choices,
                               selected = species_choices[1]))
          ),
          fluidRow(
            column(12, leafletOutput("capture_map"))
          ),
          hr(),
          fluidRow(
            column(12, titlePanel("Spatial co-occurences"))
          ),
          "The following plot shows the co-occurance correlations between different species in your survey:",
          p(),
          fluidRow(
            column(12, 
                   plotOutput("corrplot"))
          )
        )
      }
    })
    
 
    output$capture_map <- renderLeaflet({
      
      req(input$selected_species_map) 
      
      wide_obs <- results_store$data[["independent_total_observations"]] 
      locs<- results_store$data[["camera_locations"]]
      
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
      
      total_obs <- results_store$data[["independent_total_observations"]] 
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
    
  
    
        
# End of observeEvent(input$ind_run
})
  

  
}

