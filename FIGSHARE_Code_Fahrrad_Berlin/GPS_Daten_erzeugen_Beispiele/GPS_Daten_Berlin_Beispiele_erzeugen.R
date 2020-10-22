########################################################################
### library
########################################################################
library(ggmap)
library(XML)
library(stringi)
library(rlist)
########################################################################
### Einlesen
########################################################################
setwd(choose.dir())
setwd("FIGSHARE_Code_Fahrrad_Berlin/GPS_Daten_erzeugen_Beispiele/GPS_Examples/")
########################################################################
###Umwandlung in dataframes
#https://www.google.com/maps/d/   Fahradroute erzeugt und exportiert als .kmz
#https://www.gpsvisualizer.com/convert?output_home von kmz --> .gpx
########################################################################
generate.dataframe <- function(file){
  pfile <- htmlTreeParse(file, useInternalNodes = T)
  coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
  lats <- as.numeric(coords["lat",])
  lons <- as.numeric(coords["lon",])
  geodf <- data.frame(lat = lats, lon = lons)
  return(geodf)
}

df_GPS_Examples1=generate.dataframe("ID1.gpx")
df_GPS_Examples2=generate.dataframe("ID2.gpx")
df_GPS_Examples3=generate.dataframe("ID3.gpx")
df_GPS_Examples4=generate.dataframe("ID4.gpx")
df_GPS_Examples5=generate.dataframe("ID5.gpx")
df_GPS_Examples6=generate.dataframe("ID6.gpx")
df_GPS_Examples7=generate.dataframe("ID7.gpx")
df_GPS_Examples8=generate.dataframe("ID8.gpx")
df_GPS_Examples9=generate.dataframe("ID9.gpx")
df_GPS_Examples10=generate.dataframe("ID10.gpx")

GPS_Examples_list=list("1"=df_GPS_Examples1,
        "2"=df_GPS_Examples2,
        "3"=df_GPS_Examples3,
        "4"=df_GPS_Examples4,
        "5"=df_GPS_Examples5,
        "6"=df_GPS_Examples6,
        "7"=df_GPS_Examples7,
        "8"=df_GPS_Examples8,
        "9"=df_GPS_Examples9,
        "10"=df_GPS_Examples10)
########################################################################
list.save(GPS_Examples_list, file=file.choose())
#"..../FIGSHARE_Code_Fahrrad_Berlin/Listen speichern/result.list.output.rdata"