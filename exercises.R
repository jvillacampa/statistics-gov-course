# Code from day one's exercises of the course on extracting and visualising data
# from statistiscs.gov.scot
# Extracting data manually from the platform, tidying and plotting it in R
# https://github.com/Swirrl/sg-training-material/tree/master/sg-training-r-project 

###############################################.
## Packages ----
###############################################.
library(readr) # for reading csvs
library(ggplot2) # for plotting
library(leaflet) # for mapping
library(rgdal) # for spatial data manipulation
library(dplyr)
library(tidyr)
library(janitor) # to tidy variable names
library(shiny)
library(SPARQL)

###############################################.
## Exercise 1 - download csv manually ----
###############################################.
# pop estimates data extracted manually from website
pop_data <- read_csv("data/population-estimates-current-geographic-boundaries.csv") %>% 
  gather("year", "pop", 3:9) %>%  # moving years from column to rows
  clean_names() %>% #tidying variable names
  filter(reference_area == "Scotland") %>% #selecting only Scotland
  mutate(year = as.numeric(year)) # year as numeric

## Plotting data
ggplot(pop_data, aes(x=year, y=pop))+
  geom_line()+
  geom_point()+
  theme_bw()+
  labs(title= "Scotland's population", x= "Year", y = "Population")

###############################################.
## Exercise 2 - Download csv with data cart ----
###############################################.
# pop and dwelling data for 2017 for council areas extracted manually
# using data cart feature from web
housing_pop_data <- read_csv("data/exercise2_data.csv") %>% 
  clean_names() %>% #tidying variable names
  rename(pop17 = 3, dwelling17 = 4)

## Plotting data
ggplot(housing_pop_data, aes(x=pop17, y=dwelling17, group=feature_name))+
  geom_point()+
  theme_bw()+
  labs(title= "Scotland's population against number of dwellings", 
       x= "Population", y = "Dwellings")

###############################################.
## Exercise 3 - Download csv from R ----
###############################################.
# extracting alcohol stays data with read_csv copying link direction from entire
# dataset download from the website
alcohol_data <- read_csv("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Falcohol-related-discharge") %>% 
  clean_names() %>% #tidying variable names
  # selecting scotland and number of discharges
  filter(feature_code == "S92000003" & units == "Hospital Discharges") 

## Plotting data
ggplot(alcohol_data, aes(x=date_code, y=value, group = feature_code))+
  geom_line()+
  geom_point()+
  theme_bw()+
  theme(axis.text.x=element_text(angle=90)) + # horizontal axis labels
  labs(title= "Alcohol-related hospital discharges, Scotland", 
       x= "Year", y = "Number of hospital stays")

###############################################.
## Exercise 4 - SPARQL query ----
###############################################.
# Extracting data from municipal waste dataset for % recycled using SPARQL

# API  for statistics.gov.scot
endpoint <- "http://statistics.gov.scot/sparql"

# query written in SPARQL
query_recy <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?area ?areatype_name ?areacode ?value
WHERE {
  ?obs qb:dataSet <http://statistics.gov.scot/data/municipal-waste> ;
       <http://statistics.gov.scot/def/measure-properties/ratio> ?value ;
       <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?area_query ;
       <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?period ;
       <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> <http://reference.data.gov.uk/id/government-year/2011-2012> ;
       <http://statistics.gov.scot/def/dimension/indicator(municipalWaste)> <http://statistics.gov.scot/def/concept/indicator-municipal-waste/waste-recycled> .
  ?area_query rdfs:label ?area ;
              <http://statistics.data.gov.uk/def/statistical-entity#code> ?areatype .
  ?areatype rdfs:label ?areatype_name .
  ?area_query skos:notation ?areacode .
}
ORDER BY ?areatype ?area
"

# we use these two variables in conjunction with the SPARQL library to get the data
# this may take some time depending on the query
qd <- SPARQL(endpoint,query_recy)

#  this creates a list with two dataframes inside - ignore the namespace one. We just want results
recy_data <- qd$results %>% 
  mutate(areacode = substr(areacode, 2 , 10))

ggplot(recy_data, aes(x=area, y=value))+
  geom_bar(stat="identity")

###############################################.
## Exercise 5 - mapping ----
###############################################.
# Creating a map of reclycling in Scotland

recy_map <- recy_data %>% filter(area != "Scotland") #excluding Scotland

# now we need to prepare the geodata
boundary <- readOGR(dsn=paste0(getwd(), "/shapefiles"), 
                    "Local_Authority_Districts_December_2018_Boundaries_GB_BGC")

# transforming projection to wGS84
boundary <- spTransform(boundary, 
                        CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# merge the boundary file with the data
spatial_recycling <- merge(boundary, recy_map,
                           by.x = 'lad18cd', by.y = 'areacode', all.x = FALSE)

# define our palette
bins <- c(0, 25, 30, 35, 40, 45, 50, 55, Inf)
pal <- colorBin("YlOrRd", domain = spatial_recycling@data$value, bins = bins)


leaflet(spatial_recycling) %>%
  setView(lng = -3.5, lat = 57.5, zoom = 6) %>%
  addProviderTiles('CartoDB.Positron') %>%
  addPolygons(fillColor = ~pal(value),
              label= (sprintf("<h3>%s</h3>%g",spatial_recycling$lad18nm,
                              spatial_recycling$value) %>% lapply(htmltools::HTML)),
              weight = 1.5,
              fillOpacity = 0.5,
              opacity = 1,
              color = "#555555") %>%
  addLegend(pal = pal, values = ~value, title = "Municipal Recycling Rates 2011/12 (%)")

###############################################.
## Exercise 6 - creating shiny app ----
###############################################.
# with recylcling data

# API  for statistics.gov.scot
endpoint <- "http://statistics.gov.scot/sparql"

# query written in SPARQL
query_recy_shiny <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?area ?areatype_name ?areacode ?value ?period ?indicator
WHERE {
?obs qb:dataSet <http://statistics.gov.scot/data/municipal-waste> ;
    <http://statistics.gov.scot/def/measure-properties/ratio> ?value ;
    <http://purl.org/linked-data/sdmx/2009/dimension#refArea> ?area_query ;
    <http://purl.org/linked-data/sdmx/2009/dimension#refPeriod> ?period_query ;
    <http://statistics.gov.scot/def/dimension/indicator(municipalWaste)> ?indicator_query .
?area_query rdfs:label ?area ;
    <http://statistics.data.gov.uk/def/statistical-entity#code> ?areatype;
    skos:notation ?areacode.
?areatype rdfs:label ?areatype_name .
?period_query rdfs:label ?period .
?indicator_query rdfs:label ?indicator .
}
ORDER BY ?areatype ?area
"

# we use these two variables in conjunction with the SPARQL library to get the data
# this may take some time depending on the query
qd <- SPARQL(endpoint,query_recy_shiny)

#  this creates a list with two dataframes inside - ignore the namespace one. We just want results
recy_shiny <- qd$results %>% 
  mutate(areacode = substr(areacode, 2 , 10))

# now we need to prepare the geodata
boundary <- readOGR(dsn=paste0(getwd(), "/shapefiles"), 
                    "Local_Authority_Districts_December_2018_Boundaries_GB_BGC")

# transforming projection to wGS84
boundary <- spTransform(boundary, 
                        CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


ui <- navbarPage("Scotland Test", id = "nav",
                 
                 tabPanel("Demonstration Shiny",
                          h3("Hello World!"),
                          wellPanel(
                            selectizeInput("year", "Select a year", 
                                           sort(unique(recy_shiny$period)), "2011/2012")
                          ),
                          mainPanel(width=12,
                            h2(textOutput("yearlabel")),
                            column(6,plotOutput("recyclechart")),
                            column(6,leafletOutput("map"))
                            )
                 )
)

server <- function(input, output, session) {

  # draw the chart for the page on load
  output$yearlabel <- renderText(paste0(input$period))
  
  # reactive dataset, filter year based on user selection
  recy_reactive <- reactive({
    recy_shiny %>% filter(period == input$year)
  })
  
  output$recyclechart <- renderPlot({
    
    ggplot(data = recy_reactive(), aes(x = reorder(area, value), y = value)) +
      geom_bar(stat = 'identity') +
      theme_bw() + 
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$map <- renderLeaflet({

    # merge the boundary file with the data
    spatial_recycling <- merge(boundary, recy_reactive(),
                               by.x = 'lad18cd', by.y = 'areacode', all.x = FALSE)
    
    # define our palette
    bins <- c(0, 25, 30, 35, 40, 45, 50, 55, Inf)
    pal <- colorBin("YlOrRd", domain = spatial_recycling@data$value, bins = bins)
    
    leaflet(spatial_recycling) %>%
      setView(lng = -3.5, lat = 57.5, zoom = 6) %>%
      addProviderTiles('CartoDB.Positron') %>%
      addPolygons(fillColor = ~pal(value),
                  label= (sprintf("<h3>%s</h3>%g",spatial_recycling$lad18nm,
                                  spatial_recycling$value) %>% lapply(htmltools::HTML)),
                  weight = 1.5,
                  fillOpacity = 0.5,
                  opacity = 1,
                  color = "#555555") %>%
      addLegend(pal = pal, values = ~value, title = "Municipal Recycling Rates 2011/12 (%)")
  })
  

  
}

shinyApp(ui = ui, server = server)

##END