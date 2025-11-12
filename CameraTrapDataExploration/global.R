# Global

# Attach packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(sf)
library(waiter)
library(tools)
library(readxl)
library(DT)
library(plotly)
library(stringr)
library(rlang)
library(viridis)
library(patchwork)
library(corrplot)
library(zip)
library(mapview)
library(webshot2)

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


