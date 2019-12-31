#
# This is the complete Shiny web application for the eviction lab project.
#

# Import libraries 
library(leaflet)
library(shiny)
library(lubridate)
library(tidyverse)
library(purrr)
library(sf)
library(spatialEco)
library(tigris)
library(sp)

# read in data 
# spatial_data <- "./data_shiny/data.zip"
# unzip(spatial_data, overwrite = FALSE)

eviction_county_2010 <- read_csv("./eviction_county_2010.csv")
eviction_by_state <- read_csv("./eviction_state_2010.csv") 

state_us_geo <- tigris::states(class= "sf")
spatial_state <- as_Spatial(state_us_geo)
state_eviction <- sp::merge(spatial_state, eviction_by_state, by = c("GEOID" = "GEOID"), duplicateGeoms = TRUE)


map_maker <- function(x, y){
  purPal <- colorBin("Reds", domain = y$eviction_filing_rate, bins = c(0, 5, 10, 20, 30, 40, 50, 100, 120))
  blugr <- colorBin("YlGnBu", domain = y$pct_renter_occupied, n = 6)
  
  popup_evic <- paste0("<b>","<center>", y$name,
                       "</b>","</center>",
                       "<br />Median HH Income: $", y$median_household_income,
                       "<br />Population: ", y$population,
                       "<br />Percent Rent Burden: ", y$rent_burden,
                       "<br />Eviction Filing Rate: ", y$eviction_filing_rate)
  
  if (x == "Eviction Filing Rate") {
    leaflet() %>% 
      setView(-98.483330, 38.712046, zoom = 3) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = y, fillColor = ~purPal(y$eviction_filing_rate), 
                  smoothFactor = 0.2, fillOpacity = .9, weight = 0.2,
                  popup = ~popup_evic) %>%
      addLegend(purPal,
                values = y$eviction_filing_rate,
                position = "bottomleft",
                title = "Eviction Filing <br/ > Rates")
  }else{
    leaflet() %>% 
      setView(-98.483330, 38.712046, zoom = 3) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = y, 
                  fillColor = ~blugr(y$pct_renter_occupied),
                  smoothFactor = 0.2, fillOpacity = .9, weight = 0.2,
                  popup = ~popup_evic) %>%
      addLegend(blugr,
                values = y$pct_renter_occupied,
                position = "bottomleft",
                title = "Percent <br/ > Renter Occupied") }
}

ui <- fluidPage(
  titlePanel("Eviction Filings in the United States"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "year",
        label = "Select a Year:",
        choices = unique(eviction_by_state$year)
      ),
      radioButtons(
        inputId = "layer",
        label = "Select a Dataset to View:",
        choices = c("Eviction Filing Rate", 
                    "Percent Renter Occupied")
      ),
      selectInput(
        inputId = "state",
        label = "Select a State:",
        eviction_county_2010$parent_location
      ),
      selectInput(
        inputId = "county",
        label = "Select a County:",
        choices = NULL
      ),
      selectInput(
        'ycol', 'Y Variable', 
        choices = c("Eviction Filing Rate" = 'eviction_filing_rate',
                    "Unemployment Rate" = 'unemployment_rate'
        )),
        actionButton(
          inputId = "graph", "Create a Trendline"
        )

    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map States", leafletOutput("map")),
                  tabPanel("Counties", plotOutput("county_trendlines"), tableOutput("similar_counties"))
      )
    )
  ))

server <- function(input, output, session) {
  observe({
    a <- filter(eviction_county_2010,parent_location == input$state) %>%
      dplyr::select(name)
    updateSelectInput(session,"county","Select a County:",choices = unique(a))
  })
  
  data <- eventReactive(
    input$graph,{
      filter(eviction_county_2010,name==input$county & parent_location == input$state) %>%
        dplyr::select(input$ycol,year)
    }
  )
  
  y <- reactive({
    subset(state_eviction, state_eviction$year == input$year)
  })
  
  x <- reactive({
    input$layer
  })
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addProviderTiles(providers$CartoDB.Positron)
    map_maker(x(), y())
  })
  
  output$county_trendlines <- renderPlot({
    ggplot(data = data(),aes_string(x='year',y=input$ycol)) +
      geom_line(aes(color=input$ycol)) +
      theme_minimal() +
      ggtitle(paste0(input$ycol," trended by year"))+
      scale_fill_brewer(palette = "Spectral") +
      ylab(paste0(input$ycol))
  })
  
  z <- reactive({filter(eviction_county_2010,parent_location == input$state & name == input$county)%>%
      dplyr::select(cluster) %>%unique() %>% pull()})
  
  output$similar_counties <- renderTable({
    eviction_county_2010 %>%
      group_by(county_state) %>%
      filter(cluster==z()) %>%
      summarise_at(c("eviction_filing_rate","population", "cluster", "poverty_rate", "unemployment_rate", "pct_renter_occupied",
                     "Percent_Rural", "median_gross_rent", "median_household_income",
                     "median_property_value", "rent_burden", "pct_white", "pct_nonwhite"), mean, na.rm = TRUE) %>%
      rename(`Eviction Filing Rate`=eviction_filing_rate,Population = population, `Poverty Rate` = poverty_rate,
             `Unemployment Rate` = unemployment_rate, `% Renter Occupied` = pct_renter_occupied,
             `% Rural` = Percent_Rural, `Median Gross Rent` = median_gross_rent, `Median Household Income` = median_household_income,
             `Median Property Value` = median_property_value, `Rent Burden` = rent_burden,
             `% White` = pct_white, `% Non White` = pct_nonwhite) %>% sample_n(4) %>%
      rbind(
        filter(eviction_county_2010,name==input$county & parent_location == input$state) %>%
          group_by(county_state) %>%
          summarise_at(c("eviction_filing_rate","population", "cluster", "poverty_rate", "unemployment_rate", "pct_renter_occupied",
                         "Percent_Rural", "median_gross_rent", "median_household_income",
                         "median_property_value", "rent_burden", "pct_white", "pct_nonwhite"), mean, na.rm = TRUE) %>%
          rename(`Eviction Filing Rate`=eviction_filing_rate,Population = population, `Poverty Rate` = poverty_rate,
                 `Unemployment Rate` = unemployment_rate, `% Renter Occupied` = pct_renter_occupied,
                 `% Rural` = Percent_Rural, `Median Gross Rent` = median_gross_rent, `Median Household Income` = median_household_income,
                 `Median Property Value` = median_property_value, `Rent Burden` = rent_burden,
                 `% White` = pct_white, `% Non White` = pct_nonwhite) %>%
          mutate_if(is.numeric, round)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)