# Functions 

#' create_ind_detect
#'
#' @description Create an independent detections dataframe using the deployments and images data provided 
#'
#' @param deployments The deployments data
#' @param images The images data
#' @param threshold Threshold in minutes
#' @param count_column Desired count column
#' 

create_ind_detect <- function(deployments, images, threshold, count_column, 
                              include_classes = c("Mammalia"), 
                              include_humans = FALSE) {
  
  # Should we do this here instead?
  #images$sp <- paste0(images$genus, ".", images$species)
  #images$timestamp <- ymd_hms(images$timestamp)
  #deployments$start_date <- ymd_hms(deployments$start_date)
  #deployments$end_date <- ymd_hms(deployments$end_date)
  #deployments$days <- interval(deployments$start_date, deployments$end_date)/ddays(1)
  #images <- left_join(images, deployments[,c("deployment_id", "placename")])
  
  # Build the filter
  images_filtered <- images |> 
    filter(is_blank == 0,           # Remove the blanks
           !species == "",          # Remove classifications which don't have species
           class %in% include_classes)  # Filter by selected classes
  
  # Conditionally filter out humans
  if (!include_humans) {
    images_filtered <- images_filtered |>
      filter(species != "sapiens")
  }
  
  # Use images_filtered instead of images for the rest of the function
  images <- images_filtered
  
  # Create list object to store all the dataframes we are creating
  results <- list()
  
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
  row_lookup <- bind_rows(daily_lookup)
  # Remove duplicates - when start and end days are the same for successive deployments
  row_lookup <- row_lookup[duplicated(row_lookup)==F,]
  
  # Save row_lookup
  results[["row_lookup"]] <- row_lookup
  
  # Get the data in order
  
  #independent  <- input$ind_thresh
  independent <- threshold

  #images$animal_count <- images[,input$ind_count]
  # It is often "group_size" or "number_of_objects"
  column <- rlang::sym(count_column)
  images <- images |> mutate(animal_count = !!column)
  
  img_tmp <- images |>
    arrange(deployment_id) |>        # Order by deployment_id
    group_by(deployment_id, sp) |>   # Group species together
    mutate(duration = int_length(timestamp %--% lag(timestamp))) # Calculate the gap between successive detections
  
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
  
  # Update the information for the last row
  # The loop above always updates the previous row... leaving the last row unchanged
  
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
  
  # Add additional data
  
  # find out the last and the first of the time in the group
  top <- img_tmp |>
    group_by(event_id) |>
    top_n(1,timestamp) |>
    dplyr::select(event_id, timestamp)
  
  bot <- img_tmp |>
    group_by(event_id) |>
    top_n(-1,timestamp) |>
    dplyr::select(event_id, timestamp)
  
  names(bot)[2] <- c("timestamp_end")
  
  img_num <- img_tmp |>
    group_by(event_id) |>
    summarise(event_observations=n()) # number of images in the event
  
  event_grp <- img_tmp |>
    group_by(event_id) |>
    summarise(event_groupsize = max(animal_count))
  
  # Calculate the duration and add the other elements
  diff <- top |>
    left_join(bot, by = "event_id") |>
    mutate(event_duration = abs(int_length(timestamp %--% timestamp_end))) |>
    left_join(event_grp, by = "event_id") |>
    left_join(img_num, by = "event_id")
  
  # Remove columns you don't need
  diff$timestamp <- NULL
  diff$timestamp_end <- NULL
  # remove duplicates
  diff <- diff[duplicated(diff)==F,]
  # Merge the img_tmp with the event data
  img_tmp <-  img_tmp |>
    left_join(diff, by = "event_id")
  
  # Create independent data
  
  # Remove duplicates and remove dections occuring outside of known activity periods
  ind_dat <- img_tmp[duplicated(img_tmp$event_id)==F,]
  
  # Make a  unique code for ever day and deployment where cameras were functioning
  tmp <- paste(row_lookup$date, row_lookup$placename)
  
  # Subset ind_dat to data that matches the unique codes
  ind_dat <- ind_dat[paste(substr(ind_dat$timestamp,1,10), ind_dat$placename) %in% tmp, ]
  
  # Make the species column a ‘factor’ - this makes all the data frame building operations much simpler
  ind_dat$sp <- as.factor(ind_dat$sp)
  
  # Store independent data in results
  results[["independent_data"]] <- ind_dat
  
  # Create and store the site list
  # Subset the columns
  cam_locs <- unique(deployments[, c("project_id", "placename", "longitude", "latitude", "feature_type")])
  
  results[["camera_locations"]] <- cam_locs
  
  # Create a list of species identified to genus level or higher
  taxonomy_headings <- c("class", "order", "family", "genus", "species", "sp" , "common_name")
  
  # Subset the image data to just those columns
  tmp<- unique(ind_dat[, colnames(ind_dat) %in% taxonomy_headings])
  
  # Create an ordered species list
  sp_list  <- tmp[order(tmp$class, tmp$order, tmp$family, tmp$genus, tmp$species),]
  
  # We will replace the spaces in the species names with dots, this will make things easier for us later (as column headings with spaces in are annoying).
  sp_list$sp <- str_replace(sp_list$sp, " ", ".")
  
  results[["species_list"]] <- sp_list
  
  # Site x Species
  
  # Calculate the number of days at each site  
  total_obs <- row_lookup |>
    group_by(placename) |>
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
  
  results[["independent_total_observations"]] <- total_obs
  results[["independent_total_counts"]] <- total_count
  
  # Monthly counts
  # Station / Month / days / Covariates / Species      
  tmp <- row_lookup
  # Simplify the date to monthly
  tmp$date <- substr(tmp$date,1,7)
  
  # Calculate the number of days in each month  
  mon_obs <- tmp |>
    group_by(placename, date) |>
    summarise(days = n())
  
  # Convert to a data frame
  mon_obs <- as.data.frame(mon_obs)
  
  mon_obs[, levels(ind_dat$sp)] <- NA
  mon_count <- mon_obs
  # For each month, count the number of individuals/observations
  for(i in 1:nrow(mon_obs))
  {
    tmp <- ind_dat[ind_dat$placename==mon_obs$placename[i] & substr(ind_dat$timestamp,1,7)== mon_obs$date[i],]
    
    tmp_stats <- tmp |> 
      group_by(sp, .drop=F) |>
      summarise(obs=n(), 
                count=sum(animal_count))
    
    mon_obs[i,as.character(tmp_stats$sp)] <- tmp_stats$obs
    mon_count[i,as.character(tmp_stats$sp)] <- tmp_stats$count
    
  }
  
  results[["independent_monthly_observations"]] <- mon_obs
  results[["independent_monthly_counts"]] <- mon_count
  
  # Weekly format
  # Station / Month / days / Covariates / Species      
  tmp <- row_lookup
  # Simplify the date to year-week
  tmp$date <- strftime(tmp$date, format = "%Y-W%U")
  # The way this is coded is the counter W01 starts at the first Sunday of the year, everything before that is W00. Weeks do not roll across years.
  
  # Calculate the number of days in each week  
  week_obs <- tmp |> 
    group_by(placename,date ) |>
    summarise(days = n())
  
  # Convert to a data frame
  week_obs <- as.data.frame(week_obs)
  
  # Add species columns  
  week_obs[, levels(ind_dat$sp)] <- NA
  
  # Duplicate for counts
  week_count <- week_obs
  
  # For each week, count the number of individuals/observations
  for(i in 1:nrow(week_obs))
  {
    tmp <- ind_dat[ind_dat$placename==week_obs$placename[i] & strftime(ind_dat$timestamp, format = "%Y-W%U")== week_obs$date[i],]
    
    tmp_stats <- tmp |>
      group_by(sp, .drop=F) |>
      summarise(obs=n(), 
                count=sum(animal_count))
    
    week_obs[i,as.character(tmp_stats$sp)] <- tmp_stats$obs
    week_count[i,as.character(tmp_stats$sp)] <- tmp_stats$count
    
  }
  
  results[["independent_weekly_observations"]] <- week_obs
  results[["independent_weekly_counts"]] <- week_count
  
  return(results)
  
}









