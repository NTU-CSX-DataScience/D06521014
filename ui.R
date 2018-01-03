

# Define UI for application that draws a histogram
shinyUI(
       fluidPage(
  
       titlePanel("Taiwan Rainfall-Duration-Frequeny Map "),
  
                  # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                              sidebarPanel(
                                           radioButtons("frequencyi", 
                                                        label = "Frequency",
                                                        choices = list("2y" = 1, "5y" = 2,"10y" = 3, "25y" = 4,"50y" = 5,"100y" = 6, "200y" = 7)),
                                           radioButtons("durationi", 
                                                        label = "Duration",
                                                        choices = list("1hr" = 1, "3hr" = 2,"6hr" = 3, "12hr" = 4,"24hy" = 5)),
                                            sliderInput("years",
                                                        "years:",
                                                         min = 10,
                                                         max = 100,
                                                         value = 30)),#end of sidebarPanel
    
                                                  # Show a plot of the generated distribution
                             mainPanel(
                                       plotOutput("distPlot"),width = "100%"
                                       )
              )#end of sidebarlayout
        
))
