library(dplyr)
library(tidyr)
library(leaflet)
library(rgdal)
library(DT)
library(shinydashboard)

setwd("D:/r/shiny_apps/Benephilly_map/data")

philly_shp3 <- readOGR("D:/r/shiny_apps/Benephilly_map/data/Zipcodes_Poly.shp")
philly_shp3@data$CLIENTS_SE <- NULL

zips <- read.csv("D:/r/Shapefiles_dbfedit.csv",stringsAsFactors = FALSE)
zips[,c(1,3)] <- NULL

philly_shp3@data <- suppressWarnings(left_join(philly_shp3@data, zips, by="CODE"))
philly_shp3@data$clients_served <- as.numeric(philly_shp3@data$clients_served)
head(philly_shp3@data)
str(philly_shp3@data)

bounds<-bbox(philly_shp3)


ui <- dashboardPage(
                header<-dashboardHeader(title="Benephilly Clients Served by Zipcode",
                                        titleWidth = 600),
                dashboardSidebar(disable = TRUE),
                body<-dashboardBody(
                  fluidRow(
                    column(width = 9,
                           box(width = NULL, solidHeader = TRUE,
                               leafletOutput("leafmap", height=400)
                           ),
                           box(width=NULL,
                               dataTableOutput("results")
                           )
                    ),
                    column(width=3,
                           box(width=NULL, 
                               img(src="bdtlogo.png", width="100%", height=100, align="center"),
                               uiOutput("BenefitOutput")
                               
                               
                           )
                    )
                  )
                )
              )

server <- function(input, output, session) {
  
  filtered <- reactive({
    if (is.null(input$BenefitInput)) {
      return(NULL)
    } 
    philly_shp3 %>%
      filter(benefit_key== input$BenefitInput ,
             CODE==CODE,
             clients_served== clients_served)})
  
  colorpal <- reactive({
    colorNumeric("YlGn",filtered()$clients_served)
  })
  
  philly_popup <- reactive({paste0("<strong> Zipcodes </strong>", 
                                            filtered()$CODE, 
                                            "<br><strong> Clients served per zipcode </strong>", 
                                          filtered()$clients_served) })
  
  # Due to use of leafletProxy below, this should only be called once
  output$leafmap<-renderLeaflet({
    
    leaflet(philly_shp3) %>%
      addTiles() %>%
      addPolygons()%>%
      
      # Centre the map in the middle of our co-ordinates
      setView(mean(bounds[1,]),
              mean(bounds[2,]),
              zoom=10 # set to 10 as 9 is a bit too zoomed out
      )       
    
  })
  
  observe({
    pal <- colorpal()
    popup <- philly_popup()
    
    leafletProxy("leafmap", data=filtered()) %>%
      clearShapes()  %>%
      addPolygons(fillColor = ~pal(clients_served), 
                  fillOpacity = 0.7, 
                  color = "#BDBDC3", 
                  weight = 1,
                  popup = popup) %>%
      addLegend("bottomright",pal = pal, values = ~clients_served, bins=6,
                labels= c("0" , "50" , "100" , "150" , "200" , "250+"),
                opacity = 1,
                title = 'Legend') 
      
  })

  
  output$BenefitOutput <- renderUI({
    selectInput("BenefitInput", "Choose a Benefit you want to map:",
                zips$benefit_key,
                selected = "snap")}) 
  
  
  output$results <- renderDataTable(zips)
  
  
  
  
}


shinyApp(ui = ui, server = server)


