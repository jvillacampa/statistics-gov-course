# Code for extracting data on delayed discharges and visualising it.

# TODO:
# query can be improved with a couple of things
# definition needs work: total per month is not very insightful,
#   maybe bed days occupied or avergae of patients monthly
# exclude any type or particular delayed discharge
# calculate a crude rate by x pop.

###############################################.
## Packages ----
###############################################.
library(SPARQL)
library(dplyr)
library(shiny)
library(plotly)

###############################################.
## Query and formatting ----
###############################################.

# first we need to create a variable to hold the url of the sparql endpoint
endpoint <- "http://statistics.gov.scot/sparql"

# next we load the SPARQL query string into a variable
query <- "PREFIX dim: <http://statistics.gov.scot/def/dimension/>
PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sdmx: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?areaname ?areacode ?year (AVG(?value) AS ?count)
WHERE {
      ?obs qb:dataSet <http://statistics.gov.scot/data/delayed-discharges-monthly-census> ;
          <http://statistics.gov.scot/def/measure-properties/count> ?value  ;
          dim:delay <http://statistics.gov.scot/def/concept/delay/all-delays> ;
          dim:reasonForDelay <http://statistics.gov.scot/def/concept/reason-for-delay/all> ;
          sdmx:refPeriod ?month_query ;
          sdmx:refArea ?area_query .
      ?area_query rdfs:label ?areaname .
      ?month_query rdfs:label ?month .
      ?area_query skos:notation ?areacode . 
      BIND( SUBSTR(?month, 1, 4) AS ?year ) .
}
GROUP BY ?year ?areaname ?areacode
"
# this didn't work as a case_when
# bind( if ( SUBSTR(?areacode, 1, 3) = "S12",
#            "Council area" ,
#            if ( SUBSTR(?areacode, 1, 3) = "S08",
#                 "Health board",
#                 "unknown type" ))
#   as ?area_type )

# we use these two variables in conjunction with the SPARQL library to get the data
# this may take some time depending on the query
qd <- SPARQL(endpoint,query)

#  this creates a list with two dataframes inside - ignore the namespace one. We just want results
delayed_disch <- qd$results

delayed_disch <- delayed_disch %>% 
  # needed but query works fine in the website but not in R
  mutate(areacode = substr(areacode, 2 , 10),
         # not being able to create areatype in query
         areatype = case_when(substr(areacode, 1, 3) == "S12" ~ "Council area",
                              substr(areacode, 1, 3) == "S08" ~ "Health board"),
         year = as.integer(year)) %>% 
  arrange(year)

###############################################.
## Shiny app ----
###############################################.
###############################################.
# UI 
ui <- fluidPage(
  sidebarPanel(
    selectizeInput('areatype', 'Step 1. Select a geography level', 
              unique(delayed_disch$areatype)),
    uiOutput("areaname_ui")),
  mainPanel(
    plotlyOutput('trend_plot')
  )
)

###############################################.
# Server 
server <- function(input, output) {
  
  ###############################################.
  # reactive control for areas based on their type
  output$areaname_ui <- renderUI({
    areas <- sort(delayed_disch$areaname[delayed_disch$areatype == input$areatype])
    
    selectizeInput("areaname", "Step 2. Select an area/s", choices=areas, 
                   selected = "Health board", multiple = T)
  })
  
  ###############################################.
  # Plot 
  output$trend_plot <- renderPlotly({
    # reactive data
    trend_data <- delayed_disch %>% 
      subset(areaname %in% input$areaname & areatype = input$areatype) %>% 
      droplevels()
    
    #Text for tooltip
    tooltip_trend <- c(paste0(trend_data$areaname, "<br>", trend_data$year,
                              "<br>", trend_data$count))
    
    # Plot
    plot_ly(data=trend_data, x=~year,  y = ~count,
                          color = ~areaname, 
                          text=tooltip_trend, hoverinfo="text", height = 600 ) %>% 
      add_trace(type = 'scatter', mode = 'lines+markers', marker = list(size = 8)) %>% 
      #Layout 
      layout(margin = list(b = 160, t=5), #to avoid labels getting cut out
             yaxis = list(title = "Delayed discharges", rangemode="tozero", fixedrange=TRUE,
                          size = 4, titlefont =list(size=14), tickfont =list(size=14)),
             xaxis = list(title = FALSE, tickfont =list(size=14), 
                          tickangle = 270, fixedrange=TRUE, range = c(2016, 2019)),
             showlegend = TRUE,
             legend = list(orientation = 'h', x = 0, y = 1.18)) %>%  #legend on top
      config(displayModeBar = TRUE, displaylogo = F, collaborate=F, editable =F) # taking out plotly logo and collaborate button
    
  })
}

###############################################.
# Call the app 
# Return a Shiny app object
shinyApp(ui = ui, server = server)


##END