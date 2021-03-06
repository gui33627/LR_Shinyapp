library(shiny)
library(ggplot2)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(htmlwidgets)
library(latex2exp)

map(list.files('R', recursive = TRUE), function(file) source(file.path('R', file)))
map(list.files('UI', recursive = TRUE), function(file) source(file.path('UI', file)))
map(list.files('server', recursive = TRUE), function(file) source(file.path('server', file)))