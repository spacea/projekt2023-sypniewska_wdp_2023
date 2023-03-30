install.packages("shiny")
install.packages("shinythemes")

library(shiny)
library(shinythemes)

ui = fluidPage(theme = shinytheme("slate"),
               navbarPage(
                 "Calendar",
                 tabPanel("Calendar",
                          sidebarPanel(
                            tags$h3("Pick a date:"),
                            dateInput("wybrana_data", 
                                      label = NULL,
                                      format = "d MM yyyy",
                                      weekstart = 1,
                                      language = "en",
                                      min = "1970-01-01",
                                      max = "2023-12-31")
                          ),
                          mainPanel(
                            h3("Weather"),
                            hr(),
                            h3("Happy name day to:"),
                            hr(),
                            h3("Holidays"),
                            hr(),
                            h3("Zodiac signs"),
                            tags$div(
                              tags$p(tags$b("Sun sign")),
                              tags$p(tags$b("Moon sign"))
                            ),
                            h4("Horoscope"),
                            hr(),
                            h3("Happy birthday to:")
                          )
                 )
               )
)

server = function(input, output){
}

shinyApp(ui = ui, server = server)
