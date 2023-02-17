# E537 Lab 5 Lead in Soil
# Author: Bode Hoover (bodehoov@iu.edu)
# Last updated April 22, 2022
# Purpose: Generates interactive map depicting soil samples locations and corresponding [lead]
# If necessary, install necessary packages
# install.packages("package name") 
install.packages("tidyverse")
install.packages("sf")
install.packages("mapview")
install.packages("viridis")
install.packages("lattice")
install.packages("leafpop")
install.packages("gmap")
# Load required packages
library(tidyverse)
library(sf)
library(mapview)
library(viridis)
library(lattice)
library(leafpop)
library(gmap)

# Load data
# on windows need to use C:\\User\\IU username\\Desktop\\Folder\\File.csv
Soil <- read.csv(file = "/Users/bodehoover/Downloads/Courses/SPEA 537 Env Lab/LeadSoil.csv")
# must change directory to your file location

# Create color palette
pal <- inferno(n = 15, direction = -1)

# Create interactive map
mapview(Soil, xcol = "Longitude", 
        ycol = "Latitude", 
        zcol = "Lead_XRF", 
        crs = 4269, # determines what roads are showing
        col.regions = pal,
        layer.name = "[Lead] (ppb)", # renames legend
        grid = FALSE) # removes grid lines

# Clicking on data points will show:
# sample ID,
# sample coordinates, 
# ICPMS [lead], and 
# XRF [lead].
# Can zoom in and out and adjust street views

# Plotting ICPMS data on first layer. Not as useful since multiple NAs
mapview(Soil, xcol = "Longitude", 
        ycol = "Latitude", 
        zcol = "Lead_ICPMS", 
        crs = 4269, 
        col.regions = pal,
        layer.name = "XRF",
        grid = FALSE)


# Using gmap()
mymapkey = "pk-tHVbDiymfUL"
x <- Soil$Latitude
y <- Soil$Longitude
xy <- cbind(x, y)
g <- gmap(xy, type='terrain', map_key = mymapkey)
plot(g, inter=TRUE)
