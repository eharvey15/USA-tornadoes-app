#load libraries
library(shiny)
library(plotly)
library(tidyverse)
library(shinyWidgets)
library(RColorBrewer)
library(lubridate)
library(jsonlite)
library(httr)
library(shinythemes)



#load API key
api_KEY <- "XSWCTMEX8WPLAHAYEB95E9A2K"

#define function for accessing weather data
get_weather_data <- function(date, latitude, longitude) {
  
  #error message for yeas prior to 1970
  if (year(date)<1970 | latitude == 0 | longitude == 0){
    warning("Weather data not available prior to 1970")
    
    #error message for erroneous lat/lon
  } else if (latitude == 0 | longitude == 0) {
    warning("Weather data not available for this location")
    
  }
  
  #query for everything else
  else {
    
    query <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",
                    latitude, ",", longitude, "/", date,
                    "?key=", "QMA4VUPHAEZ6PDPCVDWV2SE45")
    
    querydata <- GET(query) %>% content(as = "text", encoding = "UTF-8") %>% fromJSON()
    
    selected_data <- querydata$days[c("temp","tempmax", "tempmin", 
                                      "dew", "humidity", "precip", "precipcover", "precipprob", 
                                      "windgust", "windspeed", "winddir", 
                                      "pressure", "cloudcover", "visibility",
                                      "conditions", "description", "icon")]
    
    return(selected_data)
  }
}


#load the data
df <- read_csv("https://raw.githubusercontent.com/eharvey15/USA-tornadoes-app/main/df_cleaned.csv")



#gather states, years, and force categories
states <- df %>% select(st) %>% distinct() %>% arrange(st)
years <- df %>% select(yr) %>% distinct() %>% arrange(yr)
force_cats <- df %>% select(mag) %>% distinct() %>% arrange(mag)

#get rid of zero values for ending locations so that erroneous storm tracks don't appear
df <- df %>% mutate(elat = ifelse(elat ==0 , NA, elat)) %>% mutate(elon = ifelse(elon ==0, NA, elon))

#give each row number an ID
df$ID <- 1:nrow(df)

#define some orange/red colors
palette <- colorRampPalette(brewer.pal(9, "Oranges"))(5)

#create geo settings
geo <-
  list(
    projection = list(
      type = "mercator",
      rotation = list(lon = -100, lat = 40, roll = 0),
      scale = 8
    ),
    showland = TRUE,
    landcolor = toRGB("gray95"),
    style = "satellite",
    center = list(lat = 39.50, lon =-98.35)
  )

#shiny ui
ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Tornadoes in the US since 1950"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year", min = 1950, max = 2021, value = 1970, animate = TRUE, sep = ""),
      tags$head(tags$style("#tor_map{height:70vh !important;}")),
      tags$head(tags$style("#selected{height:30vh !important;}")),
      textOutput("instructions")
    ),
    
    mainPanel(
      plotlyOutput("tor_map"),
      verbatimTextOutput("selected")
    )
  )
)


#shiny server
server <- function(input, output){

  
  #make the map
  
  output$tor_map <- renderPlotly({
    # plot_geo(color = I("red"), height = 800) #%>%  # the manual height is throwing shiny off
    plot_geo(color = I("red")) %>%
      add_segments(data = df %>% filter(yr==input$year, mag == 0),
                   x = ~slon, xend = ~elon,
                   y = ~slat, yend = ~elat,
                   name = "Tornado track",
                   color = ~I((wid+1)),
                   alpha = 0.3, size = ~I(1),
                   legendgroup = "0",
                   showlegend = FALSE
                   ) %>%
      add_segments(data = df %>% filter(yr==input$year, mag == 1),
                   x = ~slon, xend = ~elon,
                   y = ~slat, yend = ~elat,
                   name = "Tornado track",
                   color = ~I((wid+1)),
                   alpha = 0.3, size = ~I(1),
                   legendgroup = "1",
                   showlegend = FALSE
      ) %>%
      add_segments(data = df %>% filter(yr==input$year, mag == 2),
                   x = ~slon, xend = ~elon,
                   y = ~slat, yend = ~elat,
                   name = "Tornado track",
                   color = ~I((wid+1)),
                   alpha = 0.3, size = ~I(1),
                   legendgroup = "2",
                   showlegend = FALSE
      ) %>%
      add_segments(data = df %>% filter(yr==input$year, mag == 3),
                   x = ~slon, xend = ~elon,
                   y = ~slat, yend = ~elat,
                   name = "Tornado track",
                   color = ~I((wid+1)),
                   alpha = 0.3, size = ~I(1),
                   legendgroup = "3",
                   showlegend = FALSE
      ) %>%
      
      add_segments(data = df %>% filter(yr==input$year, mag == 4),
                   x = ~slon, xend = ~elon,
                   y = ~slat, yend = ~elat,
                   name = "Tornado track",
                   color = ~I((wid+1)),
                   alpha = 0.3, size = ~I(1),
                   legendgroup = "4",
                   showlegend = FALSE
      ) %>%
      add_segments(data = df %>% filter(yr==input$year, mag == 5),
                   x = ~slon, xend = ~elon,
                   y = ~slat, yend = ~elat,
                   name = "Tornado track",
                   color = ~I((wid+1)),
                   alpha = 0.3, size = ~I(1),
                   legendgroup = "5",
                   showlegend = FALSE
      ) %>%
      
      
      add_markers(data = df %>% filter(yr==input$year, mag == 0),
                  customdata = ~ID,
        x = ~slon,
        y = ~slat,
        name = "EF0",
        text = ~paste0("Date: ", date, "<br>",
                       "Force Category: ", mag,"<br>",
                       "Fatalities: ", fat, "<br>",
                       "Injuries: ", inj,"<br>",
                       "Length (miles): ", len, "<br>",
                       "Width (yards): ", wid, "<br>",
                       "State: ", st, "<br>",
                       "Coordinates: ", slat, ", ", slon),
        hoverinfo = "text",
        marker = list(
          color = "#BCBCBC"),
        alpha = 0.5,
        showscale = TRUE,
        showlegend = TRUE,
        # legendgroup = ~mag
        legendgroup = "0"
        ) %>%
      
      add_markers(data = df %>% filter(yr==input$year, mag == 1),
                  customdata = ~ID,
                  x = ~slon,
                  y = ~slat,
                  name = "EF1",
                  text = ~paste0("Date: ", date, "<br>",
                                 "Force Category: ", mag,"<br>",
                                 "Fatalities: ", fat, "<br>",
                                 "Injuries: ", inj,"<br>",
                                 "Length (miles): ", len, "<br>",
                                 "Width (yards): ", wid, "<br>",
                                 "State: ", st, "<br>",
                                 "Coordinates: ", slat, ", ", slon),
                  hoverinfo = "text",
                  marker = list(
                    color = "#ffc100"),
                  alpha = 0.5,
                  showscale = TRUE,
                  showlegend = TRUE,
                  # legendgroup = ~mag
                  legendgroup = "1"
                  ) %>%

      add_markers(data = df %>% filter(yr==input$year, mag == 2),
                  customdata = ~ID,
                  x = ~slon,
                  y = ~slat,
                  name = "EF2",
                  text = ~paste0("Date: ", date, "<br>",
                                 "Force Category: ", mag,"<br>",
                                 "Fatalities: ", fat, "<br>",
                                 "Injuries: ", inj,"<br>",
                                 "Length (miles): ", len, "<br>",
                                 "Width (yards): ", wid, "<br>",
                                 "State: ", st, "<br>",
                                 "Coordinates: ", slat, ", ", slon),
                  hoverinfo = "text",
                  marker = list(
                    color = "#ff9a00"),
                  alpha = 0.5,
                  showscale = TRUE,
                  showlegend = TRUE,
                  # legendgroup = ~mag
                  legendgroup = "2"
                  ) %>%

      add_markers(data = df %>% filter(yr==input$year, mag == 3),
                  customdata = ~ID,
                  x = ~slon,
                  y = ~slat,
                  name = "EF3",
                  text = ~paste0("Date: ", date, "<br>",
                                 "Force Category: ", mag,"<br>",
                                 "Fatalities: ", fat, "<br>",
                                 "Injuries: ", inj,"<br>",
                                 "Length (miles): ", len, "<br>",
                                 "Width (yards): ", wid, "<br>",
                                 "State: ", st, "<br>",
                                 "Coordinates: ", slat, ", ", slon),
                  hoverinfo = "text",
                  marker = list(
                    color = "#ff7400"),
                  alpha = 0.5,
                  showscale = TRUE,
                  showlegend = TRUE,
                  # legendgroup = ~mag
                  legendgroup = "3"
                  ) %>%

      add_markers(data = df %>% filter(yr==input$year, mag == 4),
                  customdata = ~ID,
                  x = ~slon,
                  y = ~slat,
                  name = "EF4",
                  text = ~paste0("Date: ", date, "<br>",
                                 "Force Category: ", mag,"<br>",
                                 "Fatalities: ", fat, "<br>",
                                 "Injuries: ", inj,"<br>",
                                 "Length (miles): ", len, "<br>",
                                 "Width (yards): ", wid, "<br>",
                                 "State: ", st, "<br>",
                                 "Coordinates: ", slat, ", ", slon),
                  hoverinfo = "text",
                  marker = list(
                    color = "#ff4d00"),
                  alpha = 0.5,
                  showscale = TRUE,
                  showlegend = TRUE,
                  # legendgroup = ~mag
                  legendgroup = "4"
                  ) %>%

      add_markers(data = df %>% filter(yr==input$year, mag == 5),
                  customdata = ~ID,
                  x = ~slon,
                  y = ~slat,
                  name = "EF5",
                  text = ~paste0("Date: ", date, "<br>",
                                 "Force Category: ", mag,"<br>",
                                 "Fatalities: ", fat, "<br>",
                                 "Injuries: ", inj,"<br>",
                                 "Length (miles): ", len, "<br>",
                                 "Width (yards): ", wid, "<br>",
                                 "State: ", st, "<br>",
                                 "Coordinates: ", slat, ", ", slon),
                  hoverinfo = "text",
                  marker = list(
                    color = "#922820"),
                  alpha = 0.5,
                  showscale = TRUE,
                  showlegend = TRUE,
                  # legendgroup = ~mag
                  legendgroup = "5"
                  ) %>%
     
      #call the layout
      
      layout(geo = geo)
    
  })
  
 #print the weather data
  output$selected <- renderPrint({
   
    selected_ID <- event_data("plotly_click")$customdata
    if (length(selected_ID) > 0) {
      weather_call <- df %>% filter(ID == selected_ID) %>% select(date, slat, slon)
      print(weather_call)
      weather <- get_weather_data(weather_call$date, weather_call$slat, weather_call$slon)
      print(weather)
    }
  })
  
  output$instructions <- renderText({
    "Click on a storm to see specific weather info for that location and date (no weather data available before 1970)"
  })

}

#run the app

shinyApp(ui = ui, server = server)
