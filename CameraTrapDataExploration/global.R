# Global

packages <- c(
  "tidyverse", "shiny", "shinydashboard", "htmltools",
  "leaflet", "leaflet.extras", "sf", "waiter", "tools",
  "readxl", "DT", "plotly", "stringr", "rlang", "viridis",
  "patchwork", "corrplot", "zip", "mapview", "webshot2",
  "officer", "flextable", "ggspatial", "prettymapr",
  "googlesheets4", "ISOweek"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Set max file size to 100 MB
options(shiny.maxRequestSize = 100 * 1024^2)

# Source functions
source("src.R")

# Load example data
cameras <- read_csv("cameras.csv")
common_names <- read_csv("common_names.csv")
deployments <- read_csv("deployments.csv")
images <- read_csv("images.csv")
projects <- read_csv("projects.csv")

# App parameters

# IMPORTANT FOR WILDLIFE INSIGHTS DATA 
images <- left_join(images, deployments[,c("deployment_id", "placename")])

# Parameters
title <- "Camera Trap Exploration Tool"
skin <- "green"
map_height <- 600


