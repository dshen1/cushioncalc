################################################################################
## UI code for Shiny app to calculate Cushion diamond expected price
## Author: matt.b.hudson@gmail.com
################################################################################

## Call shiny library
library(shiny)

## Build UI
shinyUI(fluidPage(
  
  titlePanel("Cushion Calculator"),
  
  sidebarLayout(
    sidebarPanel( 
      helpText("Determine the expected value of a Cushion shaped diamond based on its 4C's"),
      selectInput("cut",
                   label = "Choose cut",
                   choices = list( "Ideal","V.Good","Good"),
                   selected = "V.Good"),
      selectInput("color",
                  label = "Choose color",
                  choices = list ("D", "E", "F", "G", "H", "I", "J","K", "L"),
                  selected = "G"),
      selectInput("clarity",
                  label = "Choose clarity",
                  choices = list("IF","VVS1","VVS2", "VS1","VS2", "SI1", "SI2", "I1", "I2", "I3"),
                  selected = "VS2"),
      numericInput("carat",
                  label = "Choose carat",
                  value = 2,
                  min = .1,
                  max = 5,
                  step = .01)),
    mainPanel(
      textOutput("text1"),
      plotOutput("plot1", width = "600px")
      ))
))