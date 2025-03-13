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

# Set max file size to 25 MB
options(shiny.maxRequestSize = 25 * 1024^2)

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
title <- "Camera Trap Data Exploration"
skin <- "green"
map_height <- 600


