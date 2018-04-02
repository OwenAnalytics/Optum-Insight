library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
options(warn = -1)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Anti-coagulant Patterns for Patients After Index VTE"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Please select patient ID's to be plotted."),
      uiOutput('select_patid'),
      # radioButtons("size_copay",
      #              label = "Show size of copay",
      #              choices = c("Yes" = "Copay_sum", "No" = "NULL"),
      #              selected = NULL)
      # # verbatimTextOutput("hover"),
      # verbatimTextOutput("click"),
      # verbatimTextOutput("brush")
      width = 3
    ),
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      # Output: Tabset w/ plot, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                           div(
                             style = "position:relative",
                             plotlyOutput("patternPlot")
                             # , 
                             #            hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                             # uiOutput("hover_info")
                           ),
                           width = 7),
                           # plotlyOutput("patternPlot")), #,  width = "300%", height = "400px"
                  tabPanel("Patient Information", dataTableOutput("select_patid_disp")),
                  tabPanel("INR Information", dataTableOutput("select_inr_disp"))
      ),
      # tableOutput('select_patid_disp'),
      # ggvisOutput('plot1'),
      # plotlyOutput("select_patid_disp"),
      p('You may hover over the points to view more information.')
    )
  )
))
