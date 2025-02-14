library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(shinydashboard)
library(DT)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)

# Load Data
df <- read_csv("WBData.csv")
df$year <- as.numeric(df$year)

# Define RECs and their member countries
recs <- list(
  "UMA" = c("Algeria", "Libya", "Mauritania", "Morocco", "Tunisia"),
  "COMESA" = c("Burundi", "Comoros", "Djibouti", "Democratic Republic of the Congo", "Egypt", "Eritrea", 
               "Eswatini", "Ethiopia", "Kenya", "Libya", "Madagascar", "Malawi", "Mauritius", 
               "Rwanda", "Seychelles", "Somalia", "Sudan", "Tunisia", "Uganda", "Zambia", "Zimbabwe"),
  "CEN-SAD" = c("Benin", "Burkina Faso", "Central African Republic", "Chad", "Comoros", 
                "Ivory Coast", "Djibouti", "Egypt", "Eritrea", "Gambia", "Ghana", "Guinea", 
                "Guinea-Bissau", "Kenya", "Libya", "Mali", "Mauritania", "Morocco", "Niger", 
                "Nigeria", "Senegal", "Sierra Leone", "Somalia", "Sudan", "Togo", "Tunisia"),
  "EAC" = c("Burundi", "Democratic Republic of the Congo", "Kenya", "Rwanda", "South Sudan", "Tanzania", "Uganda"),
  "ECCAS" = c("Angola", "Burundi", "Cameroon", "Central African Republic", "Chad", 
              "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Republic of the Congo", "Rwanda", 
              "Sao Tome and Principe"),
  "ECOWAS" = c("Benin", "Burkina Faso", "Cape Verde", "Ivory Coast", "Gambia", 
               "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", 
               "Nigeria", "Senegal", "Sierra Leone", "Togo"),
  "IGAD" = c("Djibouti", "Eritrea", "Ethiopia", "Kenya", "Somalia", "South Sudan", "Sudan", "Uganda"),
  "SADC" = c("Angola", "Botswana", "Comoros", "Democratic Republic of the Congo", "Eswatini", "Lesotho", 
             "Madagascar", "Malawi", "Mauritius", "Mozambique", "Namibia", "Seychelles", 
             "South Africa", "Tanzania", "Zambia", "Zimbabwe")
)

# Load African country boundaries
africa_shapefile <- ne_countries(scale = "medium", type = "countries", returnclass = "sf") %>%
  filter(continent == "Africa")

# Fix name mismatches
country_name_corrections <- c(
  "Côte d'Ivoire" = "Ivory Coast",
  "Cabo Verde" = "Cape Verde",
  "Gambia, The" = "Gambia",
  "Democratic Republic of the Congo" = "Democratic Republic of the Congo",
  "Republic of the Congo" = "Republic of the Congo",
  "Eswatini" = "Eswatini",
  "Egypt, Arab Rep." = "Egypt"
)

africa_shapefile$name <- ifelse(africa_shapefile$name %in% names(country_name_corrections), 
                                country_name_corrections[africa_shapefile$name], 
                                africa_shapefile$name)

# Convert recs list to a dataframe for robust joining
rec_df <- stack(recs) %>% rename(Country = values, REC = ind)

# Ensure all REC countries are present in the shapefile
africa_shapefile <- africa_shapefile %>%
  left_join(rec_df, by = c("name" = "Country"))

# Aggregate data by REC
df$REC <- sapply(df$`Country Name`, function(country) {
  rec <- names(which(sapply(recs, function(countries) country %in% countries)))
  if (length(rec) > 0) return(rec[1]) else return(NA)
})

df <- df %>% filter(!is.na(REC))

df_agg <- df %>% group_by(REC, year) %>% summarise(
  Population = sum(`Population, total`, na.rm = TRUE),
  Electricity_Access = mean(`Access to electricity (% of population)`, na.rm = TRUE),
  GDP_Per_Capita = mean(`GDP per capita (current US$)`, na.rm = TRUE),
  .groups = 'drop'
)

# Handle missing values
df_agg <- df_agg %>%
  mutate(
    Electricity_Access = ifelse(is.na(Electricity_Access), 0, Electricity_Access),
    Population = ifelse(is.na(Population), 0, Population)
  )

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "African RECs Economic Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectizeInput("recs", "Select RECs:", choices = names(recs), 
                     selected = "ECOWAS", multiple = TRUE),
      sliderInput("yearRange", "Select Year Range:", min = 1980, max = 2020.
                  , 
                  value = c(1990, 2018), step = 1)
    )
  ),
  dashboardBody(
    fluidRow(
      box(leafletOutput("mapPlot"), width = 12, title = "Economic Data Map"),
      box(plotlyOutput("gdpPlot"), width = 6, title = "GDP Growth by REC"),
      box(plotlyOutput("electricityPlot"), width = 6, title = "Electricity Access Trends"),
      box(plotlyOutput("populationPlot"), width = 6, title = "Population Trends"),
      box(DTOutput("tableData"), width = 6, title = "Regional Economic Data")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    df_agg %>% filter(REC %in% input$recs & year >= input$yearRange[1] & year <= input$yearRange[2])
  })
  
  output$mapPlot <- renderLeaflet({
    map_data <- africa_shapefile %>% filter(REC %in% input$recs)
    pal <- colorFactor(brewer.pal(8, "Set1"), domain = africa_shapefile$REC)
    
    leaflet(map_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(REC),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        popup = ~paste("<b>Country:</b>", name, "<br><b>REC:</b>", REC)
      )
  })
  
  output$gdpPlot <- renderPlotly({
    ggplotly(ggplot(filtered_data(), aes(x = year, y = GDP_Per_Capita, color = REC)) +
               geom_line(size = 1.2) +
               labs(title = "GDP per Capita Over Time", x = "Year", y = "GDP per capita (USD)", color = "REC") +
               theme_minimal())
  })
  
  
  
  output$electricityPlot <- renderPlotly({
    ggplotly(ggplot(filtered_data(), aes(x = year, y = Electricity_Access, color = REC)) +
               geom_line(size = 1.2) +
               labs(title = "Electricity Access Over Time", x = "Year", y = "Access to Electricity (%)", color = "REC") +
               theme_minimal())
  })
  
  output$populationPlot <- renderPlotly({
    ggplotly(ggplot(filtered_data(), aes(x = year, y = Population / 1e6, color = REC)) +
               geom_line(size = 1.2) +
               labs(title = "Population Trends Over Time", x = "Year", y = "Population (millions)", color = "REC") +
               theme_minimal())
  })
  
  output$tableData <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 5))
  })
}

shinyApp(ui = ui, server = server)
