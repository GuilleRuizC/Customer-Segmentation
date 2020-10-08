
# Import relevant libraries
library(rsconnect)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(plotly)
library(shinyWidgets)
library(RColorBrewer)
library(ColorPalette)

# Set working directory 
setwd("C:/Users/Guillermo/Desktop/ESTUDIOS/Data Science Bootcamp/Projects/Shiny/CustomerSegmentation")
df <- read.csv('supermarket_sales.csv')

# Chage column names to more adequate ones plus make column names lower case
headers <- tolower(colnames(df))
headers[1] = 'id'
headers[6] = 'product.type'
headers[10] = 'spending'
headers[9] = 'tax'
headers[14] = 'cost'
colnames(df) = headers

# Set R output in English
Sys.setlocale("LC_TIME", "C")

