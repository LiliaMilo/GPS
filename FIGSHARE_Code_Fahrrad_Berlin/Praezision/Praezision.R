########################################################################
### library
########################################################################
library(XML)
library(OpenStreetMap)
library(lubridate)
library(ggmap)
library(ggplot2)
library(raster)
library(sp)
###########################################################################################################
###Wie sieht das mit der Präzission in einem GPS Punkt aus?
###########################################################################################################

#lat 111m Breitengrad  zum Äquator oben unten
#lon 70m  Längengrad zum Äquator rechts und links

#berliner Dom 
bsp_berlinerdom=data.frame(lon=13.401111,lat=52.519354)

bsp3.1=data.frame(lon=c(13.401,13.401,13.4019,13.4019),lat=c(52.519,52.5199,52.5199,52.519))
bsp3.2=data.frame(lon=c(13.402,13.401),lat=c(52.519,52.519))

bsp4.1=data.frame(lon=c(13.4011,13.4011,13.40119,13.4011),lat=c(52.5193,52.51939,52.51939,52.5193))
bsp4.2=data.frame(lon=c(13.40119,13.4011),lat=c(52.5193,52.5193))

map <- get_map(c(left = 13.3950001, bottom = 52.517001, right = 13.404909, top = 52.52691))
ggmap(map)+
  geom_point(data = bsp_berlinerdom,
             aes(lon,lat), size=1, alpha=0.7,colour = "red")+
  labs(x = "Longitude", y = "Latitude",title = "Berliner Dom")+
  geom_line(data = bsp3.1,
            aes(lon,lat), size=1, alpha=0.7,colour = "green")+
  geom_line(data = bsp3.2,
            aes(lon,lat), size=1, alpha=0.7,colour = "green")+
  geom_line(data = bsp4.1,
            aes(lon,lat), size=1, alpha=0.7,colour = "black")+
  geom_line(data = bsp4.2,
            aes(lon,lat), size=1, alpha=0.7,colour = "black")+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=10),
        title =element_text(size=12))
  


#Gormannstraße Mulackstraße
bsp_kreuzung=data.frame(lon=13.404667,lat=52.527551)

bsp3.1_kreuzung=data.frame(lon=c(13.404,13.404,13.4049,13.4049),lat=c(52.527,52.5279,52.5279,52.527))
bsp3.2_kreuzung=data.frame(lon=c(13.4049,13.404),lat=c(52.527,52.527))

bsp4.1_kreuzung=data.frame(lon=c(13.4046,13.4046,13.40469,13.40469),lat=c(52.5275,52.52759,52.52759,52.5275))
bsp4.2_kreuzung=data.frame(lon=c(13.40469,13.4046),lat=c(52.5275,52.5275))

map <- get_map(c(left = 13.40200, bottom = 52.52521, right = 13.40846, top = 52.53011),maptype="hybrid")
css_typ_title = list(theme(axis.text=element_text(size=32,face = "bold"),
                           axis.title.x=element_text(size=35,face="plain",hjust = 0.5,vjust=-2),
                           axis.title.y=element_text(size=35,face="plain",hjust = 0.5,vjust=3),
                           title=element_text(size=36,face="plain",hjust = 0.5),
                           plot.margin = (unit(c(.5, .5, 1, 1), "cm"))))



plot= ggmap(map)+
  geom_point(data = bsp_kreuzung,
             aes(lon,lat), size=0.8, alpha=0.7,colour = "red")+
  labs(x = "Longitude", y = "Latitude",title = "Gormann- Mulackstraße",
       colour = "Legend")+
  geom_line(data = bsp3.1_kreuzung,
            aes(lon,lat), size=0.8, alpha=0.7,colour = "darkgreen")+
  geom_line(data = bsp3.2_kreuzung,
            aes(lon,lat), size=0.8, alpha=0.7,colour = "darkgreen")+
  geom_line(data = bsp4.1_kreuzung,
            aes(lon,lat), size=0.8, alpha=0.7,colour = "black")+
  geom_line(data = bsp4.2_kreuzung,
            aes(lon,lat), size=0.8, alpha=0.7,colour = "black")+
  css_typ_title
plot


