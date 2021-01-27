library(data.table)
library(ggplot2)
library(prophet)


#PI <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/PoliceIncidents1-9-21.RDS")

setDT(PI)

PI[,rowid := 1:.N]

# Date Cleanup
PI[,Date := as.Date(substr(date1,1,10))]
PI <- PI[Date >= as.Date("2014-06-01"),]
setorder(PI,Date)
PI[,MonthDate := as.Date(paste0(format(Date,"%Y-%m"),"-01")),by = Date]
PI[,WeekNum := strftime(Date, format = "%V-%Y"),by = Date]
PI[,WeekDate := min(Date),by = WeekNum]
PI[,Year := as.factor(servyr)]

# Lat / Long Cleabup
PI[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
PI[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
PI[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
PI[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
PI[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
PI[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
PI[,Longitude := as.numeric(Longitude)]
PI[,Latitude := as.numeric(Latitude)]


PI[grepl("SUBSTANCE",offincident) | grepl("MARIJUANA",offincident) | grepl("DRUG",offincident) | 
       grepl("INHALANT PARAPHERNALIA",offincident) | grepl("POSS CONT SUB PEN GRP,",offincident),
   Type := "Illegal_Drugs"]

PI[grepl("MURDER",offincident) | grepl("HOMICIDE",offincident) | grepl("MURDER",ucr_offense) | 
       grepl("HOMICIDE",nibrs_crime_category) | grepl("MURDER",nibrs_crime),Type := "MURDER"]

PI[grepl("KIDNAP",offincident),Type := "KIDNAPPING"]

PI[grepl("ACCIDENT",offincident),Type := "CAR_ACCIDENT"]

PI[grepl("ASSAULT",offincident),Type := "ASSAULT"]

PI[grepl("THEFT",offincident) | grepl("ROBBERY",offincident),Type := "THEFT"]

PI[grepl("FORGERY",offincident),Type := "FORGERY"]

PI[grepl("TRAFFICK",offincident),Type := "TRAFFICKING"]

PI[grepl("DWI",offincident),Type := "DWI"]

PI[grepl("EVADING ARREST",offincident),Type := "EVADING ARREST"]

PI[grepl("HARASSMENT",offincident),Type := "HARASSMENT"]

PI[grepl("ILLEGAL DUMPING",offincident),Type := "ILLEGAL DUMPING"]

PI[grepl("UNAUTHORIZED USE OF MOTOR VEH",offincident),Type := "UNAUTHORIZED USE OF MOTOR VEH"]

PI[grepl("TRAF VIO",offincident),Type := "TRAF VIO"]

PI[grepl("TERRORISTIC THREAT",offincident),Type := "TERRORISTIC THREAT"]

PI[grepl("RECKLESS DAMAGE",offincident),Type := "RECKLESS DAMAGE"]

PI[grepl("PUBLIC INTOXICATION",offincident),Type := "PUBLIC INTOXICATION"]

PI[grepl("BURGLARY",offincident) | grepl("BMV",offincident),Type := "BURGLARY"]

PI[grepl("CRIM MISCHIEF",offincident),Type := "CRIM MISCHIEF"]

PI[grepl("CRIMINAL TRESPASS",offincident),Type := "CRIMINAL TRESPASS"]

PI[grepl("",offincident),Type := ""]

PI[grepl("",offincident),Type := ""]


PI[is.na(Type),Type := "OTHER"]

sort(table(PI[Type == "OTHER",offincident]))

CleanPI <- PI[,.(Year,Date,WeekDate,MonthDate,Latitude,Longitude,Type,offincident,offensecode,signal)]

#saveRDS(CleanPI,"C:/Users/sconroy/Documents/DallasPoliceData/CleanPI.RDS")
