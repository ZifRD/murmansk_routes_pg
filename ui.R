#Author: Zaporozhtsev I.F.
#Created: May, 2019

library(shiny)
library(leaflet)

shinyUI(
  fluidPage(
    leafletOutput("murmap",height = "580px")
  )
)