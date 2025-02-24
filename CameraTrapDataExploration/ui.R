# UI

# Navigation -----------------------------------------------------------------------------------------------------------

header <- dashboardHeader(
  title = title,
  titleWidth = 300
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    # Home tab
    menuItem("Home", tabName = "home", icon = icon("home")),
    # Upload camera trap data
    menuItem("Upload", tabName = "upload", icon = icon("table")),
    # Leaflet map of camera locations
    menuItem("Map", tabName = "map", icon = icon("globe-americas")),
    # Analysis data exploration
    menuItem("Analysis Data Exploration", icon = icon("wpexplorer"),
             menuSubItem("Locations Plot", tabName = "loc_plot"),
             menuSubItem("Independent Detections Summary", tabName = "ind_detect"),
             menuSubItem("Temporal Patterns", tabName = "temporal"),
             menuSubItem("Capture Rates", tabName = "capture"),
             menuSubItem("Spatial Patterns", tabName = "spatial_capture"),
             menuSubItem("Species Co-Occurrences", tabName = "co_occurrences"),
             menuSubItem("Covariate Plots", tabName = "covariate")),
    menuItem("Glossary", tabName = "glossary", icon = icon("table")),
    menuItem("Report", tabName = "report", icon = icon("book"))
  )
)

# Upload Data ----------------------------------------------------------------------------------------------------------

# Match upload button to 'btn-primary' style
custom_style <- HTML("
      .btn {
        font-size: 16px;
      }

      .btn-default.btn-file {
        background-color: #3c8dbc;
        border-color: #367fa9;
        color: white;
      }

      .form-control {
        height: 36px;
        font-size: 16px;
      }

      #custom_upload_progress .progress-bar {
        font-size: 16px;
        color: black;
      }
      #custom_spatial_upload_progress .progress-bar {
        font-size: 16px;
      }
    ")

ui_custom_values <- fluidRow(
  
  column(
    width = 12,
    tags$head(tags$style(custom_style)),
    box(
      
      h2("Upload Camera Trap Data"), width = 12, style = "font-size: 130%",
      
      "Here you may add your own camera trap data to explore using this app.",
      p(),
      "Please make sure the format is the same as Wildlife Insights output.",
      p(),
      "Once the data is uploaded, you can explore the analysis tabs on the left.",
      p(),
      "Please upload your data as either an Excel or csv file.",
      p(),
      
     fileInput("custom_upload", 
               label = NULL,
               multiple = TRUE,
               buttonLabel = "Upload Data",
               placeholder = "No file selected",
               accept = c("csv", "xlsx"),
               width = "50%"),
     
     uiOutput("selected_file")
     
     ),
     
     box(h3("Data Preview"), 
         width = 12,
         DTOutput(outputId = "custom_data_preview"))
    
  )
)



# Map and camera check ----------------------------------------------------------------------------------------------------------

ui_map <- fluidRow(
  
  # Map of camera locations
  column(
      h2("Camera locations"), width = 12, style = "font-size: 130%",
         
         "The following map represents the locations of your cameras in your deployment data.",
         p(),
         "You can click on the individual icons to see the placenames.",
         p(),
         "If a camera is projecting in an incorrect location, you will need to return to your data manager and update it there.",
         p(),
         
         leafletOutput("map", width = "100%", height = map_height),  
         
  
  # Camera activity check
  box(h2("Camera activity"), width = 12, style = "font-size: 130%", 
      
      "The following plot represents the periods of time there was a camera deployed at each placename (black lines).",
      p(),
      "The black dots represent each time a different deployment period starts and ends.",
      p(),
      "The orange blocks represent the period of time between the first and last image for each deployment.",
      p(),
      plotlyOutput(outputId = "deployment_dates", height = "auto"))
  
  )

)

# Independent data creation ----------------------------------------------------------------------------------------------------------

ui_ind_detect <- fluidRow(

  # Map of camera locations
  column(
    h2("Independent data creation"), width = 12, style = "font-size: 130%",

    "This page creates and stores your independent data.",
    p(),
    "You first need to specify your inderpendance interval (minimum = 0.01, max = 1000).",
    p(),
    "Note: Most camera trappers use 30.",
    p(),
    numericInput("ind_thresh", "Minutes:", 10, min = 0.01, max = 1000),
    "You also need to select the column which specifies your count data",
    p(),
    
    # MAKE THIS A DROP DOWN BASED ON COLUMNS IN IMAGE DATA
    
    textInput("count", "group_size"),
    verbatimTextOutput("value")
    
    )

  )



    
# ----------------------------------------------------------------------------------------------------------

body <- dashboardBody(
  autoWaiter(),
  shinyFeedback::useShinyFeedback(),
  shinyjs::useShinyjs(),
  tabItems(
    tabItem("home"),
    tabItem("upload", ui_custom_values),
    tabItem("map", ui_map),
    tabItem("loc_plot"),
    tabItem("ind_detect",ui_ind_detect),
    tabItem("temporal"),
    tabItem("capture"),
    tabItem("spatial_capture"),
    tabItem("co_occurrences"),
    tabItem("covariate"),
    tabItem("glossary"),
    tabItem("report")
  )
)

dashboardPage(
  header = header,
  sidebar = sidebar,
  body =   body,
  title = title,
  skin = skin
)


