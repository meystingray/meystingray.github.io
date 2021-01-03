
# Setup Shiny
setwd("C:/Users/sconroy/Documents/meystingray.github.io/Shiny")
library(shiny)
library(rsconnect)
runApp()
deployApp()


#library(rdrop2)
#token <- drop_auth()
#saveRDS(token, "droptoken.rds")


pkg <- c("data.table","ggplot2","stringr","dplyr","gganimate","RSocrata","ggmap")
suppressWarnings(suppressPackageStartupMessages(
    invisible(lapply(pkg, function(x) require(x, character.only = T, quietly = T)))
))

#PI <- read.socrata("https://www.dallasopendata.com/resource/qv6i-rri7.csv")
#saveRDS(PI,file = "C:/Users/sconroy/Desktop/Debug/PoliceIncidents12-17-20.RDS")
#setDT(PI)

#Murder <- PI[grepl("MURDER",offincident) | grepl("HOMICIDE",offincident),]
Murder <- readRDS("C:/Users/sconroy/Desktop/Debug/Murder.RDS")


# Extract Lat / Long frm geocoded column
Murder[,rowid := 1:.N]
Murder[,Date := as.Date(substr(date1,1,10))]
Murder[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
Murder[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
Murder[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
Murder[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
Murder[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
Murder[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
Murder[,Longitude := as.numeric(Longitude)]
Murder[,Latitude := as.numeric(Latitude)]
Murder[,Year := as.factor(servyr)]
Murder <- Murder[!is.na(Latitude) & !is.na(Longitude) & !is.na(Date) & !is.na(servyr),
                            .(Date,Longitude,Latitude,Year,servyr)]


lats <- c(min(Murder$Latitude),max(Murder$Latitude))
lons <- c(min(Murder$Longitude),max(Murder$Longitude))
bb <- make_bbox(lon=lons,lat=lats,f=0.05)


# Get map of Dallas from Google API
#DallasMapZoom10 <- get_map(location = "Dallas", zoom = 10, maptype = "roadmap",source = "google",messaging = FALSE)
#DallasMapZoom11 <- get_map(location = bb, zoom = 11, maptype = "roadmap",source = "google",messaging = FALSE)
#DallasMapZoom12 <- get_map(location = "Dallas", zoom = 12, source = "google")

#DallasZoom10 <- readRDS("C:/Users/sconroy/Documents/meystingray.github.io/Shiny/Maps/DallasZoom10.RDS")
DallasZoom11 <- readRDS("C:/Users/sconroy/Documents/meystingray.github.io/Shiny/Maps/DallasZoom11.RDS")



input <- list()
input$color <- "watch"

cols <- eval(parse(text = paste0("Murder$",input$color)))

ggmap(DallasZoom11,extent = "device") + 
    geom_point(data = Murder, aes(x = Longitude, y = Latitude,color = as.factor(cols))) +
    labs(color = input$color)

    


