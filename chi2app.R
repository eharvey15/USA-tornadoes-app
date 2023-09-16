
#load packages
library(shiny)
library(plotly)
library(tidyverse)
library(jsonlite)


#load datasets
df_by_decade <- readRDS("df_by_decade.rds")
df <- readRDS("df_cleaned.rds")

#create a chi squared matrix
chi2_matrix <- df_by_decade %>% select(decade, mag, fat) %>%
  pivot_wider(names_from = mag, values_from = fat)

chi2_matrix[is.na(chi2_matrix)] <- 0
decades <- chi2_matrix[,1]
chi2_matrix <- chi2_matrix[,2:7]
chi2_matrix <- as.matrix(chi2_matrix)
rownames(chi2_matrix) <- c("1950s", "1960s","1970s","1980s","1990s","2000s","2010s", "2020s")

#run the test
chi2_test <- chisq.test(chi2_matrix)

#define decades and categories

force_categories <- c("0", "1", "2", "3", "4", "5")
decades <- c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s", "2020s")


#shiny ui

ui <- fluidPage(
  plotlyOutput("heatmap", width = 800, height = 400),
  plotlyOutput("barplot", width = 800,height = 400)
)

#shiny server
server <- function(input, output) {
  
  #make heatmap
  output$heatmap <- renderPlotly({
    plot_ly(
      z = chi2_test$observed,
      x = force_categories,
      y = decades,
      type = "heatmap",
      colorscale = "Viridis"
    ) %>% layout(heatmap, 
                 xaxis = list(title = "Force Category"), 
                 yaxis = list(title = "Decade"),
                 title = "Fatalities")
  })
  
  #make barplot for selected cell in heatmap
  output$barplot <- renderPlotly({
    clickData <- event_data("plotly_click")
    
    if (is.null(clickData)) { return(NULL) }
    
    
    # barplots of fatalities
    plot_ly(data = df %>% filter(mag == clickData$x, decade == clickData$y), 
            x = ~yr,
            y=~fat,
            type = "bar") %>%
      layout(
        xaxis = list(title = paste0("Force Category: ",clickData$x)), 
        yaxis = list(title = clickData$y), 
        title = "Fatalities",
        showlegend = FALSE
      )
  })
  
}

shinyApp(ui, server)



