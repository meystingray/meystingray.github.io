library(rdrop2)

# drop_read_rds <- function(file, dest = tempdir(), dtoken = get_dropbox_token(), ...) {
#     localfile = paste0(dest, "/", basename(file))
#     drop_download(file, local_path = localfile, overwrite = TRUE, dtoken = dtoken)
#     readRDS(localfile, ...)
# }

#token <- readRDS("droptoken.rds")
#Murder <- drop_read_rds("Shiny/Murder.RDS",dtoken = token)
Murder <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/Murder.RDS")

#DallasZoom11 <- drop_read_rds("Shiny/DallasZoom11.RDS",dtoken = token)
DallasZoom11 <- readRDS("C:/Users/sconroy/Documents/meystingray.github.io/Shiny/Maps/DallasZoom11.RDS")
    
library(data.table)

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
Murder <- Murder[!is.na(Latitude) & !is.na(Longitude) & !is.na(Date) & !is.na(servyr),]
