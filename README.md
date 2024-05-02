# Uber Ride Analysis
## Data Organization
1. First began by uploading and binding all the data into one data frame.
```
uber1 <- read.csv("apr14.csv")
uber2 <- read.csv("aug14.csv")
uber3 <- read.csv("jul14.csv")
uber4 <- read.csv("jun14.csv")
uber5 <- read.csv("may14.csv")
uber6 <- read.csv("sep14.csv")

AllUber <- bind_rows(uber1, uber2, uber3, uber4, uber5, uber6)
```
2. Then added new columns such as date, hour, month, day, and day of week. As well edited the date column. 
```
AllUber$Date.Time <- as.POSIXct(AllUber$Date.Time, format = "%m/%d/%Y %H:%M:%S")
AllUber$Date <- as.Date(AllUber$Date.Time)
AllUber$Hour <- hour(AllUber$Date.Time)
AllUber$Month <- month(AllUber$Date.Time, label = TRUE)
AllUber$Day <- day(AllUber$Date.Time)
AllUber$Day_of_Week <- wday(AllUber$Date.Time, label = TRUE)
```
3. Then created the UI. Each chart has their own panel to click on within the shiny app.
```
ui <- fluidPage(
  titlePanel("Uber Rides Analysis"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Date Range", start = min(loaded_AllUber$Date), end = max(loaded_AllUber$Date))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Pivot Table - Trips by Hour",
                 dataTableOutput("pivot_table_hour")),
        tabPanel("Trips by Hour and Month",
                 plotOutput("trips_by_hour_month")),
        tabPanel("Trips Every Hour",
                 plotOutput("trips_every_hour")),
        tabPanel("Trips by Day of Month",
                 plotOutput("trips_by_day_of_month")),
        tabPanel("Trips Every Day",
                 dataTableOutput("trips_every_day")),
        tabPanel("Trips by Day and Month",
                 plotOutput("trips_by_day_and_month")),
        tabPanel("Trips by Month",
                 plotOutput("trips_by_month")),
        tabPanel("Trips by Bases and Month",
                 plotOutput("trips_by_bases_and_month")),
        tabPanel("Heat Maps",
                 tabsetPanel(
                   tabPanel("Heat Map by Hour and Day",
                            plotOutput("heat_map_hour_day")),
                   tabPanel("Heat Map by Month and Day",
                            plotOutput("heat_map_month_day")),
                   tabPanel("Heat Map by Month and Week",
                            plotOutput("heat_map_month_week")),
                   tabPanel("Heat Map by Bases and Day of Week",
                            plotOutput("heat_map_bases_day_of_week"))
                 )
        )
      )
    )
  )
)
```
4. Then wrote the server. Throught you will see code written ot create pivot tables, histograms, and heat maps.
```
server <- function(input, output) {
  output$pivot_table_hour <- renderDataTable({
    pivot_table <- loaded_AllUber %>%
      group_by(Hour) %>%
      summarise(Trips = n())
    pivot_table
  })
  
  output$trips_by_hour_month <- renderPlot({
    ggplot(loaded_AllUber, aes(x = Hour, fill = Month)) +
      geom_bar(position = "dodge") +
      labs(title = "Trips by Hour and Month", x = "Hour", y = "Trips", fill = "Month")
  })
  
  output$trips_every_hour <- renderPlot({
    ggplot(loaded_AllUber, aes(x = Hour)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
      labs(title = "Trips Every Hour", x = "Hour", y = "Frequency")
  })
  
  output$trips_by_day_of_month <- renderPlot({
    ggplot(loaded_AllUber, aes(x = Day, y = ..count..)) +
      geom_bar() +
      labs(title = "Trips by Day of Month", x = "Day", y = "Trips")
  })
  
  output$trips_every_day <- renderDataTable({
    trips_every_day <- loaded_AllUber %>%
      group_by(Day) %>%
      summarise(Trips = n())
    trips_every_day
  })
  
  output$trips_by_day_and_month <- renderPlot({
    ggplot(loaded_AllUber, aes(x = Day_of_Week, fill = Month)) +
      geom_bar(position = "dodge") +
      labs(title = "Trips by Day and Month", x = "Day of Week", y = "Trips", fill = "Month")
  })
  
  output$trips_by_month <- renderPlot({
    ggplot(loaded_AllUber, aes(x = Month)) +
      geom_bar() +
      labs(title = "Trips by Month", x = "Month", y = "Trips")
  })
  
  output$trips_by_bases_and_month <- renderPlot({
    ggplot(loaded_AllUber, aes(x = Base, fill = Month)) +
      geom_bar(position = "dodge") +
      labs(title = "Trips by Bases and Month", x = "Base", y = "Trips", fill = "Month")
  })
  
  output$heat_map_hour_day <- renderPlot({
    hour_day_counts <- loaded_AllUber %>%
      group_by(Hour, Day_of_Week) %>%
      summarise(Trips = n())
    
    ggplot(hour_day_counts, aes(x = Hour, y = Day_of_Week)) +
      geom_tile(aes(fill = Trips), colour = "white") +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Heat Map by Hour and Day", x = "Hour", y = "Day of Week", fill = "Trips")
  })
  
  output$heat_map_month_day <- renderPlot({
    month_day_counts <- loaded_AllUber %>%
      group_by(Month, Day) %>%
      summarise(Trips = n())
    
    ggplot(month_day_counts, aes(x = Day, y = Month)) +
      geom_tile(aes(fill = Trips), colour = "white") +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Heat Map by Month and Day", x = "Day", y = "Month", fill = "Trips")
  })
  
  output$heat_map_month_week <- renderPlot({
    month_week_counts <- loaded_AllUber %>%
      mutate(Week = as.numeric(format(Date, "%W"))) %>%
      group_by(Month, Week) %>%
      summarise(Trips = n())
    
    ggplot(month_week_counts, aes(x = Week, y = Month)) +
      geom_tile(aes(fill = Trips), colour = "white") +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Heat Map by Month and Week", x = "Week", y = "Month", fill = "Trips")
  })
  
  output$heat_map_bases_day_of_week <- renderPlot({
    base_day_counts <- loaded_AllUber %>%
      group_by(Base, Day_of_Week) %>%
      summarise(Trips = n())
    
    ggplot(base_day_counts, aes(x = Base, y = Day_of_Week)) +
      geom_tile(aes(fill = Trips), colour = "white") +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = "Heat Map by Bases and Day of Week", x = "Base", y = "Day of Week", fill = "Trips")
  })
}
```
5. Finally we run the app to see our results.
```
shinyApp(ui = ui, server = server)
```
