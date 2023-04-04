install.packages("shiny")
install.packages("shinythemes")
install.packages("lubridate")
install.packages("scales")
install.packages("rjson")

library(shiny)
library(shinythemes)
library(lubridate)
library(scales)
library(rjson)

Sys.setlocale("LC_TIME", 'en')

# ui - funkcja pakietu shiny odpowiadająca za wygląd aplikacji, interfejs użytkownika

ui = fluidPage(theme = shinytheme("slate"), # użyto motywu slate z pakietu shinythemes
               navbarPage(
                 "Calendar.R",
                 tabPanel("Calendar",
                          sidebarPanel(
                            tags$h3("Pick a date:"),
                            dateInput("picked_date", 
                                      label = NULL,
                                      format = "d MM yyyy",
                                      weekstart = 1,
                                      language = "en",
                                      min = "1970-01-01",
                                      max = "2023-12-31"),
                            hr(),
                            h3("Weather"),
                            tags$p("Today is ", tags$b(format(Sys.Date(), "%d %B %Y"))),
                            tags$p(textOutput("weather_out")),
                            tags$p(textOutput("temperature_out")),
                            tags$p(textOutput("wind_out")),
                          ),
                          mainPanel(
                            textOutput("facts_out"), # textOutput użyto do wyświetlania wszelkich outputów z sekcji server
                            hr(),
                            h3("Moon phase"),
                            textOutput("moon_phase_out"),
                            hr(),
                            h3("Happy name day to:"),
                            textOutput("namedays_out"),
                            hr(),
                            h3("Holidays"),
                            textOutput("holidays_out"),
                            hr(),
                            h3("Zodiac signs"),
                            tags$div(
                              tags$p(tags$b("Sun sign")),
                              tags$p(textOutput("sun_out")),
                              tags$p(tags$b("Moon sign")),
                              tags$p(textOutput("moon_out"))
                            ),
                            h4("Horoscope"),
                            textOutput("horoscope_out"),
                            hr(),
                            h3("Born this day in history:"),
                            textOutput("birthdays_out"),
                            hr(),
                          )
                 )
               )
)

# server - w pakiecie shiny tam gromadzi się funkcje, które mają być następnie wyświetlane w ui
# input - wartości wprowadzone przez użytkownika, w tym przypadku data (input$picked_date)
# output - zwracanie funkcji do ui

server = function(input, output){
  
  # odczytanie świąt z pliku csv
  
  holidays_data = read.csv("holidays.csv", header = TRUE, sep = ";") 
  
  holidays = function(picked_date){
    holiday_date = format(input$picked_date, "%m-%d") # sformatowanie wybranej daty do formatu miesiąc-dzień
    holiday = holidays_data$holiday[holidays_data$day == holiday_date] # wydzielenie z pliku csv świąt odpowiadających wybranej dacie
  }
  
  # funkcje output odpowiadają za wykonanie i przekazanie funkcji do ui
  
  output$holidays_out = renderText({
    paste(as.character(holidays()))
  })
  
  namedays_data = read.csv("namedays.csv", header = TRUE, sep = ";")
  
  namedays = function(picked_date){
    nameday_date = format(input$picked_date, "%m-%d")
    nameday = namedays_data$nameday[namedays_data$day == nameday_date]
  }
  
  output$namedays_out = renderText({
    paste(as.character(namedays()))
  })
  
  birthdays_data = read.csv("birthdays.csv", header = TRUE, sep = ";")
  
  birthdays = function(picked_date){
    birthday_date = format(input$picked_date, "%m-%d")
    birthday = birthdays_data$birthdays[birthdays_data$Data == birthday_date]
  }
  
  output$birthdays_out = renderText({
    paste(as.character(birthdays()))
  })
  
  observeEvent(input$picked_date, {
    year = year(input$picked_date)
    month = month(input$picked_date)
    day = day(input$picked_date)
    
    if (month == 1 | month == 2) {
      year = year - 1
      month = month + 12
    }
    
    A = year %/% 100
    B = A %/% 4
    C = 2 - A + B
    E = round(365.25 * (year + 4716), 0)
    F = round(30.6001 * (month + 1), 0)
    
    JulianDay = C + day + E + F - 1524.5
    
    days_since_new_moon = JulianDay - 2451549.5
    
    new_moons = days_since_new_moon / 29.53
    fractional_part = new_moons - floor(new_moons)
    
    days_into_cycle = fractional_part * 29.53
    
    if (days_into_cycle <= 1.84566 | days_into_cycle > 28.1783) {
      moon_phase = "New Moon"
    } else if (days_into_cycle <= 5.53699) {
      moon_phase = "Waxing Crescent"
    } else if (days_into_cycle <= 9.22831) {
      moon_phase = "First Quarter"
    } else if (days_into_cycle <= 12.91963) {
      moon_phase = "Waxing Gibbous"
    } else if (days_into_cycle <= 16.61096) {
      moon_phase = "Full Moon"
    } else if (days_into_cycle <= 20.30228) {
      moon_phase = "Waning Gibbous"
    } else if (days_into_cycle <= 23.9936) {
      moon_phase = "Third Quarter"
    } else if (days_into_cycle <= 27.68493) {
      moon_phase = "Waning Crescent"
    } else {
      moon_phase = "Waning Crescent"
    }
    
    output$moon_phase_out = renderText({
      paste(as.character(moon_phase))
    })
  })
  
  
  
  moon_signs = function(picked_date){
    
    year = year(input$picked_date)
    month = month(input$picked_date)
    day = day(input$picked_date)
    
    day_fix = day + 1 
    
    mod = function(x, y) {
      x - y * floor(x/y)
    }
    
    julian_date = function(year, month, day_fix) {
      a = floor((14 - month)/12)
      y = year + 4800 - a
      m = month + 12*a - 3
      jd = day_fix + floor((153*m + 2)/5) + y*365 + floor(y/4) - floor(y/100) + floor(y/400) - 32045
      return(jd)
    }
    
    zodiac_sign = function(jd) {
      days = jd - 2451545.0
      JulianCenturies = days / 36525
      
      obliquityOfEcliptic = 23.43929111 - 0.013004167 * JulianCenturies - 1.63888889e-7 * JulianCenturies^2 + 5.03611111e-7 * JulianCenturies^3
      
      lunarLongitude = mod(218.32 + 481267.883 * JulianCenturies + 6.29 * 
                             sin((134.9 + 477198.85 * JulianCenturies) * 
                                   pi/180) - 1.27 * sin((259.2 - 413335.38 * JulianCenturies) * 
                                                          pi/180) + 0.66 * sin((235.7 + 890534.23 * JulianCenturies) * 
                                                                                 pi/180) + 0.21 * sin((269.9 + 954397.70 * JulianCenturies) * 
                                                                                                        pi/180) - 0.19 * sin((357.5 + 35999.05 * JulianCenturies) * 
                                                                                                                               pi/180) - 0.11 * sin((186.6 + 966404.05 * JulianCenturies) * pi/180), 360)
      
      zodiac = floor((lunarLongitude + 15) / 30) + 1
      zodiacNames = c("Pisces", "Aries", "Taurus", "Gemini", "Cancer", "Leo", "Virgo", "Libra", "Scorpio", "Sagittarius", "Capricorn", "Aquarius")
      return(zodiacNames[zodiac])
    }
    
    jd = julian_date(year, month, day_fix)
    moon_sign = zodiac_sign(jd)
  }
  
  output$moon_out = renderText({
    moon_sign = moon_signs()
    if (is.na(moon_sign)) {
      moon_sign = "Pisces"
    }
    paste(moon_sign)
  })
  
  output$horoscope_out = renderText({
    paste(switch(moon_signs(),
                 "Aries" = "This is a time to focus on your goals and take action towards them. Trust your instincts and be confident in your abilities. Your energy and determination will help you achieve success.",
                 "Taurus" = "It's important to find balance in all areas of your life. Focus on taking care of yourself and your relationships, both personal and professional. Your patience and persistence will pay off in the long run.",
                 "Gemini" = "You may find yourself feeling more curious and communicative than usual. This is a good time to learn new things and connect with others. Be open to new experiences and ideas.",
                 "Cancer" = "You may be feeling more emotional than usual. It's important to take care of yourself and prioritize your own needs. Trust your intuition and listen to your inner voice.",
                 "Leo" = "This is a time to focus on your creativity and self-expression. Embrace your individuality and let your light shine. Your confidence and charisma will attract positive attention.",
                 "Virgo" = "It's important to focus on the details and take a practical approach to your goals. Pay attention to your health and well-being, and be sure to get enough rest and relaxation.",
                 "Libra" = "This is a time to focus on your relationships and partnerships. Seek balance and harmony in your interactions with others. Your diplomacy and charm will serve you well.",
                 "Scorpio" = "You may find yourself feeling more intense and passionate than usual. Embrace your power and use it wisely. Your intuition and ability to transform will help you achieve your goals.",
                 "Sagittarius" = "This is a time to focus on your sense of adventure and explore new horizons. Take risks and embrace new experiences. Your optimism and enthusiasm will help you find success.",
                 "Capricorn" = "It's important to focus on your goals and take a practical approach to achieving them. Be disciplined and persistent in your efforts. Your hard work will pay off in the long run.",
                 "Aquarius" = "You may find yourself feeling more innovative and unconventional than usual. Embrace your unique perspective and use it to solve problems and create new opportunities. Your vision and creativity will help you succeed.",
                 "This is a time to focus on your intuition and spiritual growth. Connect with your inner self and explore your dreams and visions. Your sensitivity and empathy will help you connect with others on a deeper level.")
    )
  })
  
  # słoneczny znak zodiaku
  
  sun_sign = function(picked_date){
    bday = input$picked_date
    
    day_of_the_month = day(bday) # wydzielenie dnia miesiąca z daty
    month = month(bday) # wydzielenie miesiąca z daty
    
    if ((day_of_the_month >= 22 && month == 12) || (day_of_the_month <= 19 && month == 01)){
      ("Capricorn")
    } else if ((day_of_the_month >= 20 && month == 01) || (day_of_the_month <= 18 && month == 02)){
      ("Aquarius")
    } else if ((day_of_the_month >= 19 && month == 02) || (day_of_the_month <= 20 && month == 03)){
      ("Pisces")
    } else if ((day_of_the_month >= 21 && month == 03) || (day_of_the_month <= 19 && month == 04)){
      ("Aries")
    } else if ((day_of_the_month >= 20 && month == 04) || (day_of_the_month <= 22 && month == 05)){
      ("Taurus")
    } else if ((day_of_the_month >= 23 && month == 05) || (day_of_the_month <= 21 && month == 06)){
      ("Gemini")
    } else if ((day_of_the_month >= 22 && month == 06) || (day_of_the_month <= 22 && month == 07)){
      ("Cancer")
    } else if ((day_of_the_month >= 23 && month == 07) || (day_of_the_month <= 23 && month == 08)){
      ("Leo")
    } else if ((day_of_the_month >= 24 && month == 08) || (day_of_the_month <= 22 && month == 09)){
      ("Virgo")
    } else if ((day_of_the_month >= 23 && month == 09) || (day_of_the_month <= 22 && month == 10)){
      ("Libra")
    } else if ((day_of_the_month >= 23 && month == 10) || (day_of_the_month <= 21 && month == 11)){
      ("Scorpio")
    } else if ((day_of_the_month >= 22 && month == 11) || (day_of_the_month <= 21 && month == 12)){
      ("Sagittarius")
    }
  }
  
  output$sun_out = renderText({
    paste(sun_sign())
  })
  
  # funkcja pokazująca którym dniem roku jest wybrana data i w którym tygodniu roku się znajduje
  
  date_facts = function(picked_date){
    day_of_the_year = ordinal(as.numeric(format(input$picked_date, "%j"))) # dzień roku, funkcja ordinal z pakietu scales odpowiada za dodanie końcówek angielskich liczebników - do jej działania wartość musi być atrybutem numerycznym
    weekday = format(input$picked_date, "%A") # dzień tygodnia
    week = ordinal(as.numeric(format(input$picked_date, "%W"))) # tydzień roku z końcówkami liczebników
    picked_date_formatted = format(input$picked_date, "%d %B %Y") # sformatowana wybrana data z pełną nazwą miesiąca
    
    paste0(weekday, ", ", picked_date_formatted, " is the ", day_of_the_year, " day of the year, in its ",
           week, " week.")
  }
  
  output$facts_out = renderText({
    paste(date_facts())
  })
  
  weather = function(){
    api_key = "70a2bd6f6a99d8e2aa165b7f181eb93f"
    
    city = "Poznan"
    country = "PL"
    
    url = paste0("http://api.openweathermap.org/data/2.5/weather?q=", city, ",", country, "&APPID=", api_key)
    
    json_data = fromJSON(file = url)
    
    paste0("Weather in ", city, " now: ", json_data$weather[[1]]$description)
    
  }
  
  output$weather_out = renderText({
    paste(as.character(weather()))
  })
  
  temperature = function(){
    api_key = "70a2bd6f6a99d8e2aa165b7f181eb93f"
    
    city = "Poznan"
    country = "PL"
    
    url = paste0("http://api.openweathermap.org/data/2.5/weather?q=", city, ",", country, "&APPID=", api_key)
    
    json_data = fromJSON(file = url)
    
    paste0("Temperature: ", round(json_data$main$temp - 273.15, 2), " degrees Celsius")
    
  }
  
  output$temperature_out = renderText({
    paste(as.character(temperature()))
  })
  
  wind = function(){
    api_key = "70a2bd6f6a99d8e2aa165b7f181eb93f"
    
    city = "Poznan"
    country = "PL"
    
    url = paste0("http://api.openweathermap.org/data/2.5/weather?q=", city, ",", country, "&APPID=", api_key)
    
    json_data = fromJSON(file = url)
    
    paste0("Wind speed: ", json_data$wind$speed, " m/s")
    
  }
  
  output$wind_out = renderText({
    paste(as.character(wind()))
  })
  
}

# shinyApp otwiera aplikację

shinyApp(ui = ui, server = server)
