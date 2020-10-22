###################################################################################################
### Library #######################################################################################
###################################################################################################
library(XML)
library(sp)
library(rlist)
########################################################
#Routen Berlin
#https://www.berlin.de/sen/uvk/verkehr/verkehrsplanung/radverkehr/radverkehrsnetz/radrouten/#gpx_download
########################################################
setwd(choose.dir())
setwd("FIGSHARE_Code_Fahrrad_Berlin/Listen speichern/radrouten_komplett_Berlin/")

generate.dataframe <- function(file){
  pfile <- htmlTreeParse(file, useInternalNodes = T)
  coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
  lats <- as.numeric(coords["lat",])
  lons <- as.numeric(coords["lon",])
  geodf <- data.frame(lat = lats, lon = lons)
  return(geodf)
}

europaradweg_r1_ost=generate.dataframe("europaradweg_r1_ost.gpx")
europaradweg_r1_west=generate.dataframe("europaradweg_r1_west.gpx")
gatowroute_rr2=generate.dataframe("gatowroute_rr2.gpx")
havelradweg=generate.dataframe("havelradweg.gpx")
hellersdorf_route_rr8=generate.dataframe("hellersdorf_route_rr8.gpx")
hohenschoenhausen_route_rr7=generate.dataframe("hohenschoenhausen_route_rr7.gpx")
mauerweg_1=generate.dataframe("mauerweg_1.gpx")
mauerweg_2=generate.dataframe("mauerweg_2.gpx")
mauerweg_3=generate.dataframe("mauerweg_3.gpx")
nordspange_tr2=generate.dataframe("nordspange_tr2.gpx")
ostring_tr7=generate.dataframe("ostring_tr7.gpx")
radfernweg_berlin_kopenhagen=generate.dataframe("radfernweg_berlin_kopenhagen.gpx")
radfernweg_berlin_usedom=generate.dataframe("radfernweg_berlin_usedom.gpx")
reinickendorf_route_rr5=generate.dataframe("reinickendorf_route_rr5.gpx")
spandau_route_rr3=generate.dataframe("spandau_route_rr3.gpx")
suedspange_tr4=generate.dataframe("suedspange_tr4.gpx")
teltow_route_rr12=generate.dataframe("teltow_route_rr12.gpx")
wannseeroute_rr1=generate.dataframe("wannseeroute_rr1.gpx")
westspange_tr1=generate.dataframe("westspange_tr1.gpx")

routen_berlin=list(europaradweg_r1_ost=europaradweg_r1_ost,europaradweg_r1_west=europaradweg_r1_west,
                   gatowroute_rr2=gatowroute_rr2, havelradweg=havelradweg,
                   hellersdorf_route_rr8=hellersdorf_route_rr8,hohenschoenhausen_route_rr7=hohenschoenhausen_route_rr7,
                   mauerweg_1=mauerweg_1,mauerweg_2=mauerweg_2,mauerweg_3=mauerweg_3,
                   nordspange_tr2=nordspange_tr2,ostring_tr7=ostring_tr7,
                   radfernweg_berlin_kopenhagen=radfernweg_berlin_kopenhagen,radfernweg_berlin_usedom=radfernweg_berlin_usedom,
                   reinickendorf_route_rr5=reinickendorf_route_rr5,
                   spandau_route_rr3=spandau_route_rr3,suedspange_tr4=suedspange_tr4,
                   teltow_route_rr12=teltow_route_rr12,wannseeroute_rr1=wannseeroute_rr1,
                   westspange_tr1=westspange_tr1)
########################################################
#FIS_Broker
#Das ist nur der Code zum anschauen, die Daten verändern sich regelmaeßig und man muss händisch die Daten anpassen
#Die Daten für die Arbeit, werden zur Verfuegung gestellt und sind im Ordner als fis_broker vorhanden
########################################################
# library("glue")
# library("httr")
# library("sf")
# library("dplyr")
# 
# get_url <- function(x) {
#   
#   u_data <- glue("http://fbinter.stadt-berlin.de/fb/wfs/data/senstadt/{x}")
#   u_geom <- glue("http://fbinter.stadt-berlin.de/fb/wfs/geometry/senstadt/{x}")
#   
#   query <- glue("?service=wfs&version=2.0.0&request=GetFeature&TYPENAMES={x}")
#   
#   url_data <- paste0(u_data, query)
#   url_geom <- paste0(u_geom, query)
#   
#   if(!http_error(url_data)) {
#     url_data
#   } else {
#     url_geom
#   }
#   
# }
# 
# get_X_Y_coordinates <- function(s) {
#   
#   sftype <- unique(as.character(sf::st_geometry_type(s)))
#   
#   if(sftype == "POINT") {
#     
#     xy <- as.data.frame(sf::st_coordinates(s))
#     dplyr::bind_cols(s, xy)
#     
#   } else {
#     s
#   }
#   
# }
# 
# sf_fisbroker <- function(x) {
#   
#   url <- get_url(x)
#   print(url)
#   s <- sf::read_sf(url)
#   sf::st_crs(s) <- 25833
#   s <- sf::st_transform(s, 4326)
#   s <- get_X_Y_coordinates(s)
#   s
#   
# }
# 
# export_format <- c(
#   "geojson", 
#   # "shp",
#   "sqlite",
#   "xlsx"
# )
# 
# sf_save <- function(z, fname) {
#   
#   ifelse(!dir.exists(fname), dir.create(fname), "Folder exists already")
#   ff <- paste(file.path(fname, fname), export_format, sep = ".")
#   purrr::walk(ff, ~{ sf::st_write(z, .x, delete_dsn = TRUE)})
#   saveRDS(z, paste0(file.path(fname, fname), ".rds"))
#   
# }
# 
# 
# z <- sf_fisbroker("s_vms_radverkehrsanlagen_rva")
# daten=dplyr::glimpse(z)
# ##########################################################
# ## in geeignetes Dataframe erstellen
# ##########################################################
# fis_broker=list()
# new_dataframe=list()
# 
# n=length(daten$geometry)
# 
# del=c(16,472,6877,7939,7687,7963) #diese manuell bearbeiten, da diese mehr Spalten aufweisen
# 
# l=1:n
# l=l[-del]
# 
# for(k in l){
#   
#   new_dataframe[[k]]=as(daten$geometry[[k]], Class = "Spatial")
#   new_dataframe_1=as.data.frame(coordinates(new_dataframe[[k]]))
#   colnames(new_dataframe_1)=c("lon","lat")
#   fis_broker[[k]]=new_dataframe_1
# }
# 
# #Dateien die mehr Spalten als 2 aufweisen in 2 Spalten verpackt
# 
# k=16
# new_dataframe[[k]]=as(daten$geometry[[k]], Class = "Spatial")
# #unlist(coordinates(new_dataframe[[k]]))
# new_dataframe_1=data.frame(lon=c(13.40920, 13.40920,13.40923, 13.40903),lat=c(52.52858,52.52858,52.52861, 52.52832))
# fis_broker[[k]]=new_dataframe_1
# 
# k=472
# new_dataframe[[k]]=as(daten$geometry[[k]], Class = "Spatial")
# #unlist(coordinates(new_dataframe[[k]]))
# new_dataframe_1=data.frame(lon=c(13.22934, 13.22937, 13.22940, 13.22924, 13.23020, 13.23072, 13.23154,
#                                  13.23268, 13.23385, 13.23441, 13.23505, 13.23549, 13.23560, 13.23569),
#                            lat=c(52.62533, 52.62531, 52.62524, 52.62533, 52.62475, 52.62443,
#                                  52.62392, 52.62315, 52.62236, 52.62199, 52.62156, 52.62126, 52.62122, 52.62117))
# fis_broker[[k]]=new_dataframe_1
# 
# k=6877
# new_dataframe[[k]]=as(daten$geometry[[k]], Class = "Spatial")
# #unlist(coordinates(new_dataframe[[k]]))
# new_dataframe_1=data.frame(lon=c(13.33264, 13.33275, 13.33283, 13.33277, 13.33284),
#                            lat=c(52.46883, 52.46894, 52.46903, 52.46894, 52.46899))
# fis_broker[[k]]=new_dataframe_1
# 
# k=7939
# new_dataframe[[k]]=as(daten$geometry[[k]], Class = "Spatial")
# #unlist(coordinates(new_dataframe[[k]]))
# new_dataframe_1=data.frame(lon=c(13.29949, 13.29950, 13.29950, 13.29950, 13.29949),
#                            lat=c(52.51937, 52.51933, 52.51933, 52.51937, 52.51937))
# fis_broker[[k]]=new_dataframe_1
# 
# k=7687
# new_dataframe[[k]]=as(daten$geometry[[k]], Class = "Spatial")
# #unlist(coordinates(new_dataframe[[k]]))
# new_dataframe_1=data.frame(lon=c(13.19495, 13.19446, 13.19494, 13.19655),
#                            lat=c(52.51332, 52.51330, 52.51332, 52.51301))
# fis_broker[[k]]=new_dataframe_1
# 
# k=7963
# new_dataframe[[k]]=as(daten$geometry[[k]], Class = "Spatial")
# #unlist(coordinates(new_dataframe[[k]]))
# new_dataframe_1=data.frame(lon=c(13.43914, 13.43925, 13.43932, 13.43947, 13.43959, 13.43969, 13.43930, 13.43937, 13.43941),
#                            lat=c(52.51297, 52.51314, 52.51322, 52.51341, 52.51358, 52.51373, 52.51319, 52.51322, 52.51325))
# fis_broker[[k]]=new_dataframe_1
# 
# names(fis_broker)=1:n ### ORTSTL



##########################################################################################################
### SAVE #################################################################################################
##########################################################################################################
list.save(routen_berlin, file=file.choose())
#.../FIGSHARE_Code_Fahrrad_Berlin/Listen speichern/routen_berlin.rdata"
#list.save(fis_broker, file=file.choose())
#.../FIGSHARE_Code_Fahrrad_Berlin/Listen speichern/fis_broker.rdata")
