# Functions 

#' create_ind_detect
#'
#' @description Create an independent detections dataframe using the deployments and images data provided 
#'
#' @param deployments The deployments data
#' @param images The images data
#' @param threshold Threshold in minutes
#' @param count_column Desired count column (character)
#' @param include_classes The genus of species you want to include
#' @param include_humans TRUE/FALSE whether humans are included
#' @param summaries The summaries requested by the user
#'
#' @import dplyr tidyr lubridate stringr rlang
#' @export
create_ind_detect <- function(deployments, images, threshold, count_column, 
                                  include_classes = c("Mammalia"), 
                                  include_humans = FALSE,
                              summaries = c("total", "monthly",
                                            "weekly", "daily")) {
  
  summaries <- intersect(summaries, c("total", "monthly", "weekly", "daily"))
  
  # Build the filter
  images <- images |>
    filter(is_blank == 0,
           !species == "",
           class %in% include_classes)
  
  # Conditionally filter out humans
  if (!include_humans) {
    images <- images |> filter(species != "sapiens")
  }
  
  # Resolve count column once (accepts character name)
  images <- images |>
    mutate(animal_count = .data[[count_column]])
  
  # Effort expansion (row_lookup)
  # Expand each finished deployment into daily rows
  row_lookup <- deployments |>
    filter(!is.na(end_date)) |>
    mutate(start_date = as.Date(start_date),
           end_date   = as.Date(end_date)) |>
    filter(end_date >= start_date) |>
    mutate(date = purrr::map2(start_date, end_date, ~ seq(.x, .y, by = "day"))) |>
    tidyr::unnest(date) |>
    select(date, placename) |>
    distinct()
  
  results <- list()
  results[["row_lookup"]] <- row_lookup
  
  # Event grouping (vectorized)
  # Threshold in seconds
  thresh_sec <- as.numeric(threshold) * 60
  
  img_tmp <- images |>
    arrange(deployment_id, sp, timestamp) |>
    group_by(deployment_id, sp) |>
    mutate(interval_sec = as.numeric(difftime(timestamp, lag(timestamp), units = "secs")),
           new_event    = if_else(is.na(interval_sec) | interval_sec > thresh_sec, 1L, 0L),
           # Sequential event index within each (deployment_id, sp)
           event_idx    = cumsum(replace_na(new_event, 0L))) |>
    ungroup()
  
  # Make global, padded event_id
  # Use a dense rank over the tuple (deployment_id, sp, event_idx)
  pad_width <- nchar(nrow(img_tmp)) + 1
  img_tmp <- img_tmp |>
    mutate(event_gid = dplyr::dense_rank(interaction(deployment_id, sp, event_idx, drop = TRUE)),
           event_id  = paste0("E", stringr::str_pad(event_gid, pad_width, pad = "0"))) |>
    select(-event_gid)
  
  # Event-level summaries (start/end/duration/obs/group size)
  diff <- img_tmp |>
    group_by(event_id) |>
    summarise(
      timestamp_start   = min(timestamp),
      timestamp_end     = max(timestamp),
      event_duration    = as.numeric(difftime(timestamp_end, timestamp_start, units = "secs")),
      event_groupsize   = max(animal_count, na.rm = TRUE),
      event_observations= n(),
      .groups = "drop"
    )
  
  # Merge event summaries back
  img_tmp <- img_tmp |>
    left_join(diff, by = "event_id") |>
    select(-interval_sec, -new_event, -event_idx)
  
  ## Independent data (one row per event)
  ind_dat <- img_tmp |>
    distinct(event_id, .keep_all = TRUE)
  
  # Keep only detections on days cameras were active
  ind_dat <- ind_dat |>
    mutate(day = as.Date(timestamp)) |>
    semi_join(
      row_lookup |> mutate(day = as.Date(date)) |> select(placename, day),
      by = c("placename" = "placename", "day" = "day")
    ) |>
    select(-day)
  
  # Ensure species factor (as in your original)
  ind_dat <- ind_dat |> mutate(sp = as.factor(sp))
  
  # The species columns expected in wide outputs
  sp_levels <- levels(ind_dat$sp)   
  
  results[["independent_data"]] <- ind_dat
  
  # Camera locations 
  cam_locs <- deployments |>
    distinct(project_id, placename, longitude, latitude) #, feature_type) # I have removed feature type as sometimes typos create new stations 
  results[["camera_locations"]] <- cam_locs
  
  # Species list (ordered, sp dotted) 
  taxonomy_headings <- c("class", "order", "family", "genus", "species", "sp", "common_name")
  
  sp_list <- ind_dat |>
    select(any_of(taxonomy_headings)) |>
    distinct() |>
    arrange(class, order, family, genus, species) |>
    mutate(sp = str_replace(sp, " ", "."))
  
  results[["species_list"]] <- sp_list
  
  # Site × Species totals (days + obs / counts)
  # Total days by site (from effort)
  if ("total" %in% summaries) {
  
  total_obs_base <- row_lookup |>
    group_by(placename) |>
    summarise(days = n(), .groups = "drop")
  
  # Summaries by site/species
  ind_by_site <- ind_dat |>
    group_by(placename, sp) |>
    summarise(
      obs   = n(),
      count = sum(animal_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  total_obs <- total_obs_base |>
    left_join(
      ind_by_site |>
        select(placename, sp, obs) |>
        tidyr::pivot_wider(names_from = sp, values_from = obs, values_fill = 0),
      by = "placename"
    )
  sp_cols <- intersect(sp_levels, names(total_obs))
  total_obs <- total_obs |>
    mutate(across(all_of(sp_cols), ~ tidyr::replace_na(.x, 0)))
  
  total_count <- total_obs_base |>
    left_join(
      ind_by_site |>
        select(placename, sp, count) |>
        tidyr::pivot_wider(names_from = sp, values_from = count, values_fill = 0),
      by = "placename"
    )
  sp_cols <- intersect(sp_levels, names(total_count))
  total_count <- total_count |>
    mutate(across(all_of(sp_cols), ~ tidyr::replace_na(.x, 0)))
  
  results[["independent_total_observations"]] <- total_obs
  results[["independent_total_counts"]]      <- total_count
  
  }
  
  # Monthly counts (obs & counts) 
  # Effort per site-month
  
  if ("monthly" %in% summaries) {
  
  mon_obs_base <- row_lookup |>
    mutate(date = as.Date(date),
           month = format(date, "%Y-%m")) |>
    count(placename, month, name = "days") |>
    arrange(placename, month)
  
  ind_by_mon <- ind_dat |>
    mutate(month = format(as.Date(timestamp), "%Y-%m")) |>
    group_by(placename, month, sp) |>
    summarise(
      obs   = n(),
      count = sum(animal_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  mon_obs <- mon_obs_base |>
    rename(date = month) |>  # Rename first
    left_join(
      ind_by_mon |>
        select(placename, month, sp, obs) |>
        tidyr::pivot_wider(names_from = sp, values_from = obs, values_fill = 0),
      by = c("placename", "date" = "month")
    )
  sp_cols <- intersect(sp_levels, names(mon_obs))
  mon_obs <- mon_obs |>
    mutate(across(all_of(sp_cols), ~ tidyr::replace_na(.x, 0)))
  
  mon_count <- mon_obs_base |>
    rename(date = month) |>  # Rename first
    left_join(
      ind_by_mon |>
        select(placename, month, sp, count) |>
        tidyr::pivot_wider(names_from = sp, values_from = count, values_fill = 0),
      by = c("placename", "date" = "month")
    )
  sp_cols <- intersect(sp_levels, names(mon_count))
  mon_count <- mon_count |>
    mutate(across(all_of(sp_cols), ~ tidyr::replace_na(.x, 0)))
  
  results[["independent_monthly_observations"]] <- mon_obs
  results[["independent_monthly_counts"]]      <- mon_count
  
  }
  
  # Weekly counts (obs & counts) 
  # Effort per site-week (YYYY-W##)
  
  if ("weekly" %in% summaries) {
  
  week_obs_base <- row_lookup |>
    mutate(date = as.Date(date),
           week = strftime(date, format = "%Y-W%U"),
           start_date = lubridate::floor_date(date, "week", week_start = 7),  # Sunday
           end_date   = start_date + 6) |>
    count(placename, week, start_date, end_date, name = "days") |>
    arrange(placename, week)
  
  ind_by_week <- ind_dat |>
    mutate(week = strftime(timestamp, format = "%Y-W%U")) |>
    group_by(placename, week, sp) |>
    summarise(
      obs   = n(),
      count = sum(animal_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  week_obs <- week_obs_base |>
    rename(date = week) |>  # Rename BEFORE relocate
    left_join(
      ind_by_week |>
        select(placename, week, sp, obs) |>
        tidyr::pivot_wider(names_from = sp, values_from = obs, values_fill = 0),
      by = c("placename", "date" = "week")
    ) |>
    relocate(start_date, end_date, .after = date)  # Now this works
  sp_cols <- intersect(sp_levels, names(week_obs))
  week_obs <- week_obs |>
    mutate(across(all_of(sp_cols), ~ tidyr::replace_na(.x, 0)))
  
  week_count <- week_obs_base |>
    rename(date = week) |>  # Rename BEFORE relocate
    left_join(
      ind_by_week |>
        select(placename, week, sp, count) |>
        tidyr::pivot_wider(names_from = sp, values_from = count, values_fill = 0),
      by = c("placename", "date" = "week")
    ) |>
    relocate(start_date, end_date, .after = date)  # Now this works
  sp_cols <- intersect(sp_levels, names(week_count))
  week_count <- week_count |>
    mutate(across(all_of(sp_cols), ~ tidyr::replace_na(.x, 0)))
  
  results[["independent_weekly_observations"]] <- week_obs
  results[["independent_weekly_counts"]]      <- week_count
  
  }
  
  # Daily counts (obs & counts)
  
  if ("daily" %in% summaries) {
  
  day_obs_base <- row_lookup |>
    mutate(date = as.Date(date), days = 1L) |>
    arrange(placename, date)
  
  ind_by_day <- ind_dat |>
    mutate(day = as.Date(timestamp)) |>
    group_by(placename, day, sp) |>
    summarise(
      obs   = n(),
      count = sum(animal_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Observations
  day_obs <- day_obs_base |>
    left_join(
      ind_by_day |>
        select(placename, day, sp, obs) |>
        tidyr::pivot_wider(names_from = sp, values_from = obs, values_fill = 0),
      by = c("placename" = "placename", "date" = "day")
    ) 
  
  # Ensure all species columns exist and are 0-filled
  missing_cols <- setdiff(sp_levels, names(day_obs))
  if (length(missing_cols)) {
    day_obs[missing_cols] <- 0L
  }
  
  sp_cols <- intersect(sp_levels, names(day_obs))
  day_obs <- day_obs |>
    mutate(across(all_of(sp_cols), ~ tidyr::replace_na(.x, 0)))
  
  # Counts
  day_count <- day_obs_base |>
    left_join(
      ind_by_day |>
        select(placename, day, sp, count) |>
        tidyr::pivot_wider(names_from = sp, values_from = count, values_fill = 0),
      by = c("placename" = "placename", "date" = "day")
    )
  
  missing_cols <- setdiff(sp_levels, names(day_count))
  if (length(missing_cols)) {
    day_count[missing_cols] <- 0L
  }
  
  sp_cols <- intersect(sp_levels, names(day_count))
  day_count <- day_count |>
    mutate(across(all_of(sp_cols), ~ tidyr::replace_na(.x, 0)))
  
  results[["independent_daily_observations"]] <- day_obs
  results[["independent_daily_counts"]]      <- day_count
  
  }
  
  # Return results
  
  return(results)
  
}


#' create_ind_detect_old
#'
#' @description Create an independent detections dataframe using the deployments and images data provided 
#'
#' @param deployments The deployments data
#' @param images The images data
#' @param threshold Threshold in minutes
#' @param count_column Desired count column
#' @param include_classes The genus of species you want to include
#' @param include_humans TRUE/FALSE whether humans are included

create_ind_detect_old <- function(deployments, images, threshold, count_column, 
                              include_classes = c("Mammalia"), 
                              include_humans = FALSE) {
  
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
  
  # # Daily format
  # # Station / Month / days / Covariates / Species      
  # tmp <- row_lookup
  # tmp$days <- 1
  # # Add species columns  
  # tmp[, levels(ind_dat$sp)] <- NA
  # 
  # day_obs <- tmp
  # day_count <- tmp
  # # For each week, count the number of individuals/observations
  # for(i in 1:nrow(day_obs))
  # {
  #   tmp <- ind_dat[ind_dat$placename==day_obs$placename[i] & strftime(ind_dat$timestamp, format = "%Y-%m-%d")== day_obs$date[i],]
  #   
  #   tmp_stats <- tmp %>%  group_by(sp, .drop=F) %>% summarise(obs=n(), count=sum(animal_count))
  #   
  #   day_obs[i,as.character(tmp_stats$sp)] <- tmp_stats$obs
  #   day_count[i,as.character(tmp_stats$sp)] <- tmp_stats$count
  # 
  # }
  # 
  # results[["independent_daily_observations"]] <- day_obs
  # results[["independent_daily_counts"]] <- day_count
  
  return(results)
  
}









