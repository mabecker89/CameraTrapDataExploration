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
    
    DT::datatable(data_store$dfs[[input$choice]])
    
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



  # When you click run on the ui, the following happens
  observeEvent(input$ind_run, {
    
    row_lookup <- reactiveVal(NULL)
    independent_data <- reactiveVal(NULL)
    camera_locations <- reactiveVal(NULL)
    species_list <- reactiveVal(NULL)
    site_species_obs <- reactiveVal(NULL)
    site_species_count <- reactiveVal(NULL)
    
    deployments <- data_store$dfs[["deployments.csv"]]
    images <- data_store$dfs[["images.csv"]]
    
    ###############
    # Effort lookup
        tmp <- deployments[is.na(deployments$end_date)==F,]
        daily_lookup <- list()
        # Loop through the deployment dataframe and create a effort for every day the camera is active
        for(i in 1:nrow(tmp))
        {
          if(ymd(tmp$start_date[i])!=ymd(tmp$end_date[i]))
          {
            daily_lookup[[i]] <- data.frame("date"=seq(ymd(tmp$start_date[i]), ymd(tmp$end_date[i]), by="days"), "placename"=tmp$placename[i])
          }
        }
        # Merge the lists into a dataframe
        daily_lookup <- bind_rows(daily_lookup)
        # Remove duplicates - when start and end days are the same for successive deployments
        daily_lookup <- daily_lookup[duplicated(daily_lookup)==F,]

        # Store as the reactive
        row_lookup(daily_lookup)

    ##############
    # Get the data in order
        
        independent  <- input$ind_thresh
        images$animal_count <- images[,input$ind_count] 
       
        
        img_tmp <- images %>%
          arrange(deployment_id) %>%        # Order by deployment_id
          group_by(deployment_id, sp) %>%   # Group species together
          mutate(duration = int_length(timestamp %--% lag(timestamp))) # Calculate the gap between successive detections
    
    ###################
    # Assign event ID's    
        
        # Give a random value to all cells
        img_tmp$event_id <- 9999
        
        # Create a counter
        counter <- 1
        
        # Make a unique code that has one more zero than rows in your dataframe  
        num_code <- as.numeric(paste0(nrow(img_tmp),0))
        
        # Loop through img_tmp - if gap is greater than the threshold -> give it a new event ID
        for (i in 2:nrow(img_tmp)) {
          img_tmp$event_id[i-1]  <- paste0("E", str_pad(counter, nchar(num_code), pad = "0"))
          
          if(is.na(img_tmp$duration[i]) | abs(img_tmp$duration[i]) > (independent * 60))
          {
            counter <- counter + 1
          }
        }
        
        # Update the information for the last row - the loop above always updates the previous row... leaving the last row unchanged
        
        # group ID  for the last row
        if(img_tmp$duration[nrow(img_tmp)] < (independent * 60)|
           is.na(img_tmp$duration[nrow(img_tmp)])){
          img_tmp$event_id[nrow(img_tmp)] <- img_tmp$event_id[nrow(img_tmp)-1]
        } else{
          counter <- counter + 1
          img_tmp$event_id[nrow(img_tmp)] <- paste0("E", str_pad(counter, nchar(num_code), pad = "0"))
        }
        
        # remove the duration column
        img_tmp$duration <- NULL
        
      #####################
      # Add additional data
        # find out the last and the first of the time in the group
        top <- img_tmp %>% group_by(event_id) %>% top_n(1,timestamp) %>% dplyr::select(event_id, timestamp)
        bot <- img_tmp %>% group_by(event_id) %>% top_n(-1,timestamp) %>% dplyr::select(event_id, timestamp)
        names(bot)[2] <- c("timestamp_end")
        
        img_num <- img_tmp %>% group_by(event_id) %>% summarise(event_observations=n()) # number of images in the event
        event_grp <- img_tmp %>% group_by(event_id) %>% summarise(event_groupsize=max(animal_count))
        
        # calculate the duration and add the other elements
        diff <-  top %>% left_join(bot, by="event_id") %>%
          mutate(event_duration=abs(int_length(timestamp %--% timestamp_end))) %>%
          left_join(event_grp, by="event_id")%>%
          left_join(img_num, by="event_id")
        
        # Remove columns you don't need
        diff$timestamp   <-NULL
        diff$timestamp_end <-NULL
        # remove duplicates
        diff <- diff[duplicated(diff)==F,]
        # Merge the img_tmp with the event data
        img_tmp <-  img_tmp %>%
          left_join(diff,by="event_id")
        
        ######################
        # Create independent data
        
        # Remove duplicates and remove dections occuring outside of known activity periods
          ind_dat <- img_tmp[duplicated(img_tmp$event_id)==F,]
        
          # Make a  unique code for ever day and deployment where cameras were functioning
          tmp <- paste(daily_lookup$date, daily_lookup$placename)
        
          #Subset ind_dat to data that matches the unique codes
          ind_dat <- ind_dat[paste(substr(ind_dat$timestamp,1,10), ind_dat$placename) %in% tmp, ]
          
          # make the species column a ‘factor’ - this makes all the data frame building operations much simpler
          ind_dat$sp <- as.factor(ind_dat$sp)
        
        #######################
        # Store the reactive
        independent_data(ind_dat)
        
        ############################################
        # Create and store the site list
          #Subset the columns
          cam_locs <- unique(deployments[, c("project_id", "placename", "longitude", "latitude", "feature_type")])
          # write the file to the reactive
          camera_locations(cam_locs)
          
        ############################################
        # Create a list of species indentified to genus level or higher
          taxonomy_headings <- c("class", "order", "family", "genus", "species", "sp" , "common_name")
          
          # Subset the image data to just those columns
          tmp<- unique(ind_dat[,colnames(ind_dat)%in% taxonomy_headings])
          
          # Create an ordered species list
          sp_list  <- tmp[order(tmp$class, tmp$order, tmp$family, tmp$genus, tmp$species),]
          
          # We will replace the spaces in the species names with dots, this will make things easier for us later (as column headings with spaces in are annoying).
          sp_list$sp <- str_replace(sp_list$sp, " ", ".")
          
          species_list(sp_list)
         
        #############################################
        # site x species

          # Calculate the number of days at each site  
          total_obs <- daily_lookup %>% 
            group_by(placename) %>%
            summarise(days = n())
          
          # Convert to a data frame
          total_obs <- as.data.frame(total_obs)
          
          # Add columns for each species  
          total_obs[, levels(ind_dat$sp)] <- NA
          
          # Duplicate for counts
          total_count <- total_obs
          
          # For each station, count the number of individuals/observations
          for(i in 1:nrow(total_obs))
          {
            tmp <- ind_dat[ind_dat$placename==total_obs$placename[i],]
            
            tmp_stats <- tmp %>%  group_by(sp, .drop=F) %>% summarise(obs=n(), count=sum(animal_count))
            
            total_obs[i,as.character(tmp_stats$sp)] <- tmp_stats$obs
            total_count[i,as.character(tmp_stats$sp)] <- tmp_stats$count
          }
          
          
          # Save them as reactives
          site_species_obs(total_obs) 
          site_species_count(total_count) 
          
          #SHOULD I BE SAVING THEM AS A LIST DATAFRAME
          
          #CHeck output
          output$test <- renderDT({
            
            DT::datatable(total_count)
          })
        

  })

}



