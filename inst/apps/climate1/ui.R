# Ciro
# # This is the user-interface definition of a Shiny web application.
# # You can find out more about building applications with Shiny here:
# #
# # http://shiny.rstudio.com
# #
# 
# library(shiny)
# 
# shinyUI(fluidPage(
# 
#   # Application title
#   titlePanel("Old Faithful Geyser Data"),
# 
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#     sidebarPanel(
#       sliderInput("bins",
#                   "Number of bins:",
#                   min = 1,
#                   max = 50,
#                   value = 30)
#     ),
# 
#     # Show a plot of the generated distribution
#     mainPanel(
#       plotOutput("distPlot")
#     )
#   )
# ))

#setwd("D:\\apps")
library(stringr)

months = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

data = read.csv(file.path(getwd(),'climate_data_d.csv'), stringsAsFactors = FALSE, sep="\t")
data_M = read.csv(file.path(getwd(),'climate_data_m.csv'), stringsAsFactors = FALSE, sep="\t")

selCountries = sort(unique(data$CNTRY))
selCountries_M = sort(unique(data_M$CNTRY))

shinyUI(pageWithSidebar(
  
  headerPanel("Climate Data: LAC Region"),
  
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     wellPanel(
                       h4("Step 1: Select your location"),
                       br(),
                       selectInput("country", "Country:", selCountries),
                       uiOutput("uiLocality_JD")
                     ),
                     
                     wellPanel(
                       h4("Step 2: Select climate variables"),
                       br(),
                       p(strong("Temperature:")),
                       checkboxInput("tMean", "T. Mean", value = T),
                       checkboxInput("tMin", "T. Minimun", value = T),
                       checkboxInput("tMax", "T. Maximun", value = T),
                       br(),
                       p(strong("Precipitation:")),
                       checkboxInput("prec", "Precipitation", value = T),
                       br(),
                       p(strong("Relative Humidity:")),
                       checkboxInput("rhMean", "RH Mean", value = T),
                       checkboxInput("rhMin", "RH Min", value = T),
                       checkboxInput("rhMax", "RH Max", value = T)
                     ),
                     
                     wellPanel(
                       h4("Step 3: Select your filter months"),
                       br(),
                       selectInput("fromMonth", "From Month", months),
                       uiOutput("uitillMonth")
                     ),
                     
                     wellPanel(
                       uiOutput("uiyear")
                     )
    ),
    
    conditionalPanel(condition="input.conditionedPanels==2",
                     wellPanel(
                       h4("Step 1: Select your location"),
                       br(),
                       selectInput("country_TS", "Country:", selCountries),
                       uiOutput("uiLocality_TS")
                     ),
                     
                     wellPanel(
                       h4("Step 2: Select your date range"),
                       br(),
                       uiOutput("uiDateRange")
                     )
    ),
    
    conditionalPanel(condition="input.conditionedPanels==3",
                     wellPanel(
                       h4("Step 1: Select your location"),
                       br(),
                       selectInput("country_TS_M", "Country:", selCountries_M),
                       uiOutput("uiLocality_TS_M")
                     ),
                     
                     wellPanel(
                       h4("Step 2: Select your date range"),
                       br(),
                       uiOutput("uiDateRange_M"),
                       br(),
                       downloadButton('downloadData', 'Download')
                     )
    )
  ),
  
  mainPanel(
    
    tabsetPanel(
      tabPanel("Julian day", value=1,
               p(strong("Walter-Lieth climate chart")),
               div(plotOutput(outputId = "plot_wl")),
               br(),
               div(plotOutput(outputId = "plot_temp")),
               br(),
               div(plotOutput(outputId = "plot_prec")),
               br(),
               div(plotOutput(outputId = "plot_rh"))
      ),
      
      tabPanel("Time series (daily)", value=2,
               div(plotOutput(outputId = "plot_temp_TS")),
               br(),
               div(plotOutput(outputId = "plot_prec_TS")),
               br(),
               div(plotOutput(outputId = "plot_rh_TS"))
      ),
      
      tabPanel("Time series (monthly)", value=3,
               div(plotOutput(outputId = "plot_temp_TS_M")),
               br(),
               div(plotOutput(outputId = "plot_prec_TS_M")),
               br(),
               div(plotOutput(outputId = "plot_rh_TS_M")),
               br(),
               div(tableOutput(outputId = "plot_table_TS_M"))
      ),
      
      id = "conditionedPanels"
    )
  )
))
