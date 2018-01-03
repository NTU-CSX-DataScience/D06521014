#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(gstat) # Use gstat's idw routine
library(sp) 	# provides classes for spatial data in R
library(raster)	# provides classes and methods for raster datasets
library(tmap)
# necessary - but attention for Mac users!
library(rgdal)	# interface to the Geospatial Data Abstraction Library to read and write different spatial file formats
library(leaflet)
library(magrittr)
library(dplyr)

projtwd97 <-CRS("+proj=tmerc +ellps=GRS80 +lon_0=121 +x_0=250000 +k=0.9999 +units=m +no_defs")
#taiwan_county <- readOGR("E:/git/R/003Taiwan_rainfall_Frequency_Duration", "Taiwan_county")
taiwan_county <- readOGR(".", "Taiwan_county")
proj4string(taiwan_county) <-projtwd97
#raintable <- read.csv("https://drive.google.com/file/d/0B9Fxd4oylNx9RjBFYk5BanNrbHM/view?usp=sharing", header=TRUE, sep=",")
library(gsheet)
gsheet2tbl('https://docs.google.com/spreadsheets/d/1ZHPKPoOC8mPVSz29hcc9k_0wyM4XNY7WT0fXF5RRF4Y')
raintable <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1ZHPKPoOC8mPVSz29hcc9k_0wyM4XNY7WT0fXF5RRF4Y')
#raintable <- read.csv(file="E:/git/R/003Taiwan_rainfall_Frequency_Duration/wholeIDFdata104.csv", header=TRUE, sep=",")



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    #  bins <- seq(min(x), max(x), length.out = input$years + 1)
    
    # draw the histogram with the specified number of bins
    # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    ##############################################################
    # Duration
    durationi=0
    
    if(input$durationi==1)
    {
      durationi=1
    }
    if(input$durationi==2)
    {
      durationi=3
    }
    if(input$durationi==3)
    {
      durationi=6
    }
    if(input$durationi==4)
    {
      durationi=12
    }
    if(input$durationi==5)
    {
      durationi=24
    }
    ###################################################################
    rain1<-filter(raintable, duration==durationi & data_years >= input$years)
    #   plot(taiwan_county)
    pt <- SpatialPointsDataFrame(coords=cbind(x=rain1$x_97,y=rain1$y_97), data = rain1,
                                 proj4string = projtwd97)
    # Add P's projection information to the empty grid
    taiwan_county@bbox<-pt@bbox
    # Create an empty grid where n is the total number of cells
    grd              <- as.data.frame(spsample(pt, "regular", n=50000))
    names(grd)       <- c("X", "Y")
    coordinates(grd) <- c("X", "Y")
    gridded(grd)     <- TRUE  # Create SpatialPixel object
    fullgrid(grd)    <- TRUE  # Create SpatialGrid object
    proj4string(grd) <- projtwd97
    frequencyi=0
    if(input$frequencyi==1)
    {
      pt.idw <- gstat::idw(t2y~1,pt,newdata=grd, idp=2.0)
      frequencyi=2
    }
    if(input$frequencyi==2)
    {
      pt.idw <- gstat::idw(t5y~1,pt,newdata=grd, idp=2.0)
      frequencyi=5
    }
    if(input$frequencyi==3)
    {
      pt.idw <- gstat::idw(t10y~1,pt,newdata=grd, idp=2.0)
      frequencyi=10
    }
    if(input$frequencyi==4)
    {
      pt.idw <- gstat::idw(t25y~1,pt,newdata=grd, idp=2.0)
      frequenyi=25
    }
    if(input$frequencyi==5)
    {
      pt.idw <- gstat::idw(t50y~1,pt,newdata=grd, idp=2.0)
      frequencyi=50
    }
    if(input$frequencyi==6)
    {
      pt.idw <- gstat::idw(t10y~1,pt,newdata=grd, idp=2.0)
      frequencyi=100
    }
    if(input$frequencyi==7)
    {
      pt.idw <- gstat::idw(t200y~1,pt,newdata=grd, idp=2.0)
      frequencyi=200
    }
    
    # Convert to raster object then clip to Texas
    r<- raster(pt.idw)
    r.m<- mask(r, taiwan_county)
    # s2<-paste("Return",input$frequencyi,"Duration",IDs[i],"h",".jpg")
    s3<-paste("Return",frequencyi,"Duration",durationi,"h")
    mapplot<-tm_shape(r.m) + 
      tm_raster(n=10,palette = "-RdBu", auto.palette.mapping = FALSE,title="P(mm)")+
      tm_legend(frame =F,title.size = 5,scale = 0.3,legend.text.size=5,bg.color = NA, position =c("left","top"),fontface = "bold")+#,fontface = "bold"
      tm_shape(taiwan_county)+
      tm_polygons(alpha=0,border.col = 'black',border.lwd=20)#+
      #tm_xlab(s3,size=5)
    #tm_layout(title = s3 ,legend.just ="top", scale = 0.2, title.size =10, bg.color = "white",fontfamily="BL")#legend.just ="top",
    #tmap_mode("View")
    mapplot
    
  }, height = 1000, width = 500)
})# end of server <- function(input, output)