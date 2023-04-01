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
                            dateInput("picked_date", 
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
                            textOutput("holidays_out"),
                            hr(),
                            h3("Zodiac signs"),
                            tags$div(
                              tags$p(tags$b("Sun sign")),
                              tags$p(tags$b("Moon sign")),
                            ),
                            h4("Horoscope"),
                            hr(),
                            h3("Happy birthday to:"),
                          )
                 )
               )
)

server = function(input, output){
  
  holidays_data = read.csv("holidays.csv", header = TRUE, sep = ";")
  
  holidays = function(picked_date){
    holiday_date = format(input$picked_date, "%m-%d")
    holiday = holidays_data$holiday[holidays_data$day == holiday_date]
  }
  
  output$holidays_out = renderText({
    paste(as.character(holidays()))
  })
}

shinyApp(ui = ui, server = server)
