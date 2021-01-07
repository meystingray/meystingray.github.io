library(data.table)
pkg <- c("data.table","ggplot2","stringr","dplyr","gganimate","RSocrata","ggmap","shiny","rdrop2","DT")
suppressWarnings(suppressPackageStartupMessages(
    invisible(lapply(pkg, function(x) require(x, character.only = T, quietly = T)))
))

library(ggmap)

Murder <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/Murder.RDS")
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
Murder <- Murder[!is.na(Latitude) & !is.na(Longitude) & !is.na(Date) & !is.na(servyr),]
ggmap(DallasZoom11,extent = "device") + 
    geom_point(data = Murder, aes(x = Longitude, y = Latitude),#fill = as.factor(cols)),
               size = 2,color = "black",stroke = 1,shape = 21)


DallasZoom11 <- readRDS("C:/Users/sconroy/Documents/meystingray.github.io/Shiny/Maps/DallasZoom11.RDS")

