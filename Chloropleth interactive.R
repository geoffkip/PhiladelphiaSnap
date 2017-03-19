library(ggmap)
library(maps)
library(ggplot2)
library(dplyr)
library(zipcode)
library(plyr)
library(viridis)
library(maptools)
library(rgdal)
library(foreign)
library(classInt)
library(scales)


setwd("~/Documents/R files/BDT de-identified work/Benephilly_map/data")

data(zipcode)
zipcode$zip <- as.numeric(zipcode$zip)
str(zipcode)



zips <- read.csv("/Users/geoffrey.kip/Documents/R files/BDT de-identified work/Shapefiles_dbfedit.csv",stringsAsFactors = FALSE)
zips[,c(1,3)] <- NULL
zips1 <- zips
colnames(zips1) <- c("zip", "benefit_key" , "clients_served")
zips1<- merge(zips1, zipcode, by='zip')

philly_shp <- readOGR("/Users/geoffrey.kip/Documents/R files/BDT de-identified work/Benephilly_map/data/Zipcodes_Poly.shp")
philly_shp@data$CLIENTS_SE <- NULL
philly_shp@data$id <- rownames(philly_shp@data)
philly_shp.point <- fortify(philly_shp, region="id")
philly_shp.df <- inner_join(philly_shp.point,philly_shp@data, by="id")
philly_shp.df <- suppressWarnings(left_join(philly_shp.df, zips, by="CODE"))
philly_shp.df$clients_served <- as.numeric(philly_shp.df$clients_served)
head(philly_shp.df)
str(philly_shp.df)
philly_shp.df <- philly_shp.df[,c(1,2,7,9,11,12)]
ggplot(philly_shp.df, aes(long, lat, group=group )) + geom_polygon()

zips2 <- zips1[! zips1$zip %in% c(19105,19193, 19102),]
zips1b <- zips1[,1:3]

#bounds<-bbox(philly_shp3)


ui <- dashboardPage(
  header<-dashboardHeader(title="Benephilly Clients Served by Zipcode",
                          titleWidth = 600),
  dashboardSidebar(disable = TRUE),
  body<-dashboardBody(
    fluidRow(
      column(width = 9,
             box(width = NULL, solidHeader = TRUE,
                 plotOutput("map", height=400)
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
  
  output$BenefitOutput <- renderUI({
    selectInput("BenefitInput", "Choose a Benefit you want to map:",
                sort(unique(zips2$benefit_key)),
                selected = "snap")}) 
  
  filtered <- reactive({
    if (is.null(input$BenefitInput)) {
      return(NULL)
    } 
    philly_shp.df %>%
      filter(benefit_key== input$BenefitInput ,
             CODE==CODE,
             clients_served== clients_served,
             group==group,
             long==long,
             lat==lat)})
  
  tabledata <- reactive({
    if (is.null(input$BenefitInput)) {
      return(NULL)
    } 
    zips1b %>%
      filter(benefit_key== input$BenefitInput ,
             zip==zip,
             clients_served== clients_served)})
  
  output$map <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    ggplot() + 
      geom_polygon(data = philly_shp.df, 
                   aes(x = long, y = lat, group = group), 
                   color = "black", size = 0.25) +
      geom_polygon(data = filtered(), 
                   aes(x = long, y = lat, group = group, fill = clients_served), 
                   color = "black", size = 0.25) + 
      scale_fill_distiller(name="Clients Served", palette = "YlGn", breaks = pretty_breaks(n = 6))+
      theme_nothing(legend = TRUE)+
      geom_text(data=zips2, aes(longitude,latitude,label=zip), color="grey",
                size=2,fontface="bold")+
      labs(title="Clients Served by Zipcode")
  })
  

  output$results <- renderDataTable(tabledata())
  
  
  
  
}

shinyApp(ui = ui, server = server)

