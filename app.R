library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)
library(DT)
library(shiny_dashboard)

setwd("D:/r/shiny_apps/Benephilly_map/data")
benephilly_data <- read.csv("map_data.csv")

philly_shp2 <- readOGR("D:/r/shiny_apps/Benephilly_map/data/Zipcodes_Poly.shp")
philly_shp2@data$CLIENTS_SE <- as.numeric(levels(philly_shp2@data$CLIENTS_SE))[philly_shp2@data$CLIENTS_SE]
pal <- colorQuantile("YlGn", NULL, n = 5)
pal2 <- colorNumeric("YlGn", NULL, n=6)


ui <- dashboardPage(
                header<-dashboardHeader(title="Clients Served"),
                dashboardSidebar(disable = TRUE),
                body<-dashboardBody(
                  fluidRow(
                    column(width = 10,
                           box(width = NULL, solidHeader = TRUE,
                               leafletOutput("leafmap", height=400)
                           ),
                           box(width=NULL,
                               dataTableOutput("results")
                           )
                    ),
                    column(width=2,
                           box(width=NULL, 
                               #uiOutput("yearSelect"),
                               img(src="bdtlogo.png", width="100%", height=100, align="center")
                               
                           )
                    )
                  )
                )
              )

server <- function(input, output, session) {
  
  philly_popup <- paste0("<strong> Zipcodes </strong>", 
                         philly_shp2$CODE, 
                         "<br><strong> Clients served per zipcode </strong>", 
                         philly_shp2$CLIENTS_SE)
  
  output$leafmap <- renderLeaflet({ leaflet(data=philly_shp2) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(fillColor = ~pal(CLIENTS_SE), 
                fillOpacity = 0.7, 
                color = "#BDBDC3", 
                weight = 1,
                popup = philly_popup) %>%
    addLegend("bottomright",pal = pal2, values = ~CLIENTS_SE, bins=6,
              labels= c("0" , "50" , "100" , "150" , "200" , "250+"),
              opacity = 1,
              title = 'Legend')
  })
  
  
  output$results <- renderDataTable(benephilly_data)
  
  
  
  
}

shinyApp(ui = ui, server = server)
