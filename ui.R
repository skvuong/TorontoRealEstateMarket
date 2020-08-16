#-------------------------------------------------------------------------------
#ui.R
#-------------------------------------------------------------------------------

if(!require(shiny))
  install.packages("shiny")
if(!require(shinythemes))
  install.packages("shinythemes")

# Define UI for application
#-------------------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage(
    "Housing Market Analysis",id = "inTabset",
    tabPanel("Analysis", icon = icon("chart-bar"),
      sidebarPanel(
        selectInput(inputId = "visualizeOption",
          label = "Housing Market - Visualization:",
          choices = c("1.Region and House Type effect on Price and Sales Volume",
                      "2.Region and House Type effect on Sold Price Diff-Bubble",
                      "3.Region and House Type effect on Sold Price Diff-Boxplot",
                      "4.Region and House Type effect on Time on Market",
                      "5.Finished / Unfinished Basement effect on Market Share",
                      "6.Finished / Unfinished Basement effect on Sold Price",
                      "7.Age effect on Sales Volume and House Price"
                      ),
          selected =  ""),          
#         sliderInput("numRegions", "No. of Regions in graph:", min=1,max=20,value=10,step=1)
        ), 
    mainPanel(
      plotOutput(outputId = "plot1", height = "600px")
    )
  )
))