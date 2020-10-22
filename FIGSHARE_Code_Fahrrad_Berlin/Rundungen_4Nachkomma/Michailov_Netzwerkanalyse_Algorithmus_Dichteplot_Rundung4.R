########################################################################
### library
########################################################################
library(rlist)
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggmap)
########################################################################
### Einlesen
########################################################################
setwd(choose.dir())
GPS_Tracks=list.load("FIGSHARE_Code_Fahrrad_Berlin/Listen speichern/result.list.output.rdata")
routen_berlin=list.load("FIGSHARE_Code_Fahrrad_Berlin/Listen speichern/routen_berlin.rdata")
fis_broker=list.load("FIGSHARE_Code_Fahrrad_Berlin/Listen speichern/fis_broker.rdata")
########################################################
#Meter berechnen   #https://www.kompf.de/gps/distcalc.html
########################################################
measure.meter <- function(lon1,lat1,lon2,lat2) {
  #mit distance: Entfernung in km 
  lat = (lat1 + lat2) / 2 * 0.01745
  dx = 111.3 * cos(lat) * (lon1 - lon2)
  dy = 111.3 * (lat1 - lat2)
  distance = sqrt(dx^2 + dy^2)
  return(distance*1000)
}
n.length.GPS_Tracks=length(GPS_Tracks)

GPS_Tracks.meter=lapply(1:n.length.GPS_Tracks,FUN = function(x)
  sapply(1:(length(GPS_Tracks[[x]][,1])-1),function(y) 
    measure.meter(GPS_Tracks[[x]][y,2],GPS_Tracks[[x]][y,1],
                  GPS_Tracks[[x]][y+1,2],GPS_Tracks[[x]][y+1,1]))
)

totalgefahren_GPS=sum(sapply(1:n.length.GPS_Tracks,function(x)
       sum(GPS_Tracks.meter[[x]]) ))

########################################################
#CSS Styles
########################################################
css_typhist = list(theme(axis.text=element_text(size=32,face = "bold"),
                         axis.title.x=element_text(size=35,face="plain",hjust = 0.5,vjust=-2),
                         axis.title.y=element_text(size=35,face="plain",hjust = 0.5,vjust=3),
                         plot.margin = (unit(c(.5, .5, 1, 1), "cm"))))

css_typ_title = list(theme(axis.text=element_text(size=32,face = "bold"),
                           axis.title.x=element_text(size=35,face="plain",hjust = 0.5,vjust=-2),
                           axis.title.y=element_text(size=35,face="plain",hjust = 0.5,vjust=3),
                           title=element_text(size=33,face="plain",hjust = 0.5),
                           plot.margin = (unit(c(.5, .5, 1, 1), "cm"))))

css_typ_titleandlegend = list(theme(axis.text=element_text(size=25,face = "bold"),
                                    axis.title.x=element_text(size=28,face="plain",hjust = 0.5,vjust=-2),
                                    axis.title.y=element_text(size=28,face="plain",hjust = 0.5,vjust=3),
                                    title=element_text(size=33,face="plain",hjust = 0.5),
                                    legend.key=element_rect(fill = NA),
                                    legend.title = element_text(size=30,face="plain"),
                                    legend.text = element_text(size=25,face="plain"),
                                    plot.margin = (unit(c(.5, .5, 1, 1), "cm"))))

########################################################
#Histogramme
########################################################
gefahreneStrecke_meter=sapply(GPS_Tracks.meter, sum)
plot=ggplot(data = as.data.frame(gefahreneStrecke_meter),aes(x=gefahreneStrecke_meter,fill=I("#56B4E9")))+ geom_histogram()+
  theme(panel.background = element_blank(),axis.line = element_line(size = 1, colour = "grey80"))+
  xlab("Tracklänge in Meter") + ylab("Häufigkeit")+
  css_typhist+
  xlim(0, 30000)
plot

n.length.result.list.routen_berlin=length(routen_berlin)

result.list.routen_berlin.meter=lapply(1:n.length.result.list.routen_berlin,FUN = function(x)
  sapply(1:(length(routen_berlin[[x]][,1])-1),function(y) 
    measure.meter(routen_berlin[[x]][y,2],routen_berlin[[x]][y,1],
                  routen_berlin[[x]][y+1,2],routen_berlin[[x]][y+1,1]))
)

gefahreneStrecke_meter.routen_berlin=sapply(result.list.routen_berlin.meter, sum)

plot=ggplot(data = as.data.frame(gefahreneStrecke_meter.routen_berlin),aes(x=gefahreneStrecke_meter.routen_berlin,fill=I("#56B4E9")))+ geom_histogram()+
  theme(panel.background = element_blank(),axis.line = element_line(size = 1, colour = "grey80"))+
  xlab("Routenlänge in Meter") + ylab("Häufigkeit")+
  css_typhist+xlim(0, 80000)
plot
########################################################
#Vergleiche   Genauigkeit https://journal.3960.org/posts/2011-07-06-genauigkeit-bei-geo-koordinaten/
# 4 Nachkommastellen Praezission: Latitude: 11 m Longtitude:7m
########################################################
#n=length(GPS_Tracks)#2261
#m=length(routen_berlin)#19
########################################################
####Runden & Dataframes
########################################################
#Input:
#r:Wie viele Nachkommastellen?
#list: Liste welche gerundet werden soll und in ein dataframe transformiert wird
#dis: True: Nur eindeutige/unterscheidbare Zeilen
#    False: Komplette nicht eindeiztige/unterscheidbare Liste
Runden_Melt =function(r,list,dis=T){
  
  if(r>0){
    list.round=lapply(1:length(list), function(x) round(list[[x]],r))
    names(list.round)=names(list)
    list=list.round
  }
    
    data1.all_3=melt(list, id.vars=c("lat","lon"))
    data1.all_3$L1=as.factor(data1.all_3$L1)
  
  if(dis==T){data1.all_3=distinct(data1.all_3)}
  data1_lat_3=range(data1.all_3$lat)
  data1_lon_3=range(data1.all_3$lon)
  
  return(list(list,data1.all_3,data1_lat_3,data1_lon_3))
  
  
  
}
########################################################
###Umwandlung in data.frame's, damit die Funktionen schneller sind
########################################################
########################################################
#Rundung 4 Nachkommastellen 
########################################################
round4_berlin=Runden_Melt(r=4,list=routen_berlin,dis = T)
routen_berlin.round_4=round4_berlin[[1]]
routen_berlin.all_4=round4_berlin[[2]]
range_berlin_lat_4=round4_berlin[[3]]
range_berlin_lon_4=round4_berlin[[4]]

round4_GPS_Tracks=Runden_Melt(r=4,list=GPS_Tracks,dis = T)
GPS_Tracks.round_4=round4_GPS_Tracks[[1]]
routen_GPS_Tracks.all_4=round4_GPS_Tracks[[2]]
range_GPS_Tracks_lat_4=round4_GPS_Tracks[[3]]
range_GPS_Tracks_lon_4=round4_GPS_Tracks[[4]]

round4_fisbroker=Runden_Melt(r=4,list=fis_broker,dis = T)
fisbroker.round_4=round4_fisbroker[[1]]
fisbroker.all_4=round4_fisbroker[[2]]
fisbroker_lat_4=round4_fisbroker[[3]]
fisbroker_lon_4=round4_fisbroker[[4]]
########################################################
########################################################
### Plot Uebersicht Alle
########################################################
lat <- range(range_berlin_lat_4,range_GPS_Tracks_lat_4)
lon <- range(range_berlin_lon_4,range_GPS_Tracks_lon_4)
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")

#Berlin Routen alle
Route_Berlin=ggmap(b1) +  geom_point(data = routen_berlin.all_4,
                                     aes(lon,lat,colour = factor(L1)), size=0.8, alpha=0.9)+
  labs(x = "Longitude", y = "Latitude",color = "Routen",title = "Berlin Routen")+
  theme(legend.position = "none")+
  css_typ_title
Route_Berlin

#GPS Tracks Beispiele
GPS_Tracks.plot=GPS_Tracks
round4_GPS_Tracks.plot=Runden_Melt(r=3,list=GPS_Tracks.plot)
result.list.GPS_Tracks.round_4.plot=round4_GPS_Tracks.plot[[1]]
routen_GPS_Tracks.all_4.plot=round4_GPS_Tracks.plot[[2]]
range_GPS_Tracks_lat_4.plot=round4_GPS_Tracks.plot[[3]]
range_GPS_Tracks_lon_4.plot=round4_GPS_Tracks.plot[[4]]

#GPS Tracks alle farblich nach ID
Routen_GPS_tracks=ggmap(b1) +  geom_point(data = routen_GPS_Tracks.all_4.plot,
                                          aes(lon,lat,colour = factor(L1)), size=0.8, alpha=0.5)+
  labs(x = "Longitude", y = "Latitude",title = "GPS Tracks")+ theme(legend.position = "none")+
  css_typ_title
Routen_GPS_tracks

##Gegenüberstellung Berlin <--> GPS Track
routen_berlin.all_4.col=cbind(routen_berlin.all_4,as.factor("Berlin"))
colnames(routen_berlin.all_4.col)=c("lat","lon","L1","Gruppe")
routen_GPS_Tracks.all_4.col=cbind(routen_GPS_Tracks.all_4,as.factor("Track"))
colnames(routen_GPS_Tracks.all_4.col)=c("lat","lon","L1","Gruppe")
all_date=rbind(routen_GPS_Tracks.all_4.col,routen_berlin.all_4.col)

Routen_Gegenuebergestellt=ggmap(b1) +  geom_point(data = all_date,
                                                  aes(lon,lat,colour = factor(Gruppe)), size=0.8, alpha=0.5) +
  labs(x = "Longitude", y = "Latitude",color = "Typ")+ scale_color_manual(values=c("black","darkgreen"),labels = c("Tracks", "Routen"))+
  guides(col = guide_legend(override.aes = list(shape = 20, size = 10)))+
  css_typ_titleandlegend
Routen_Gegenuebergestellt

#Fis_broker
bbox <- make_bbox(fisbroker_lon_4,fisbroker_lat_4)
b1<- get_map(bbox,maptype="watercolor", source="stamen")

Fis_Broker=ggmap(b1) +  geom_point(data = fisbroker.all_4,
                                   aes(lon,lat), size=0.8, alpha=0.5)+
  labs(x = "Longitude", y = "Latitude",title = "Fis Broker")+
  scale_fill_continuous(guide=FALSE)+
  css_typ_title
Fis_Broker
#####################################################################
###Uebereinstimmungsrate : Tabelle mit mean, max, nKonsens
###Gesamtuebereinstimmungen
#####################################################################
#####Input:
#data1:dataframe mit welchem verglichen werden soll auf Übereinstimmung
#name1:name des dataframes1 
#data2:dataframe mit welchem verglichen werden soll auf Übereinstimmung,
#      diese wird bei der Uebereinstimmungsrate dividiert #Objekt des jeweiligen Faktors
#name2:name des dataframes2
#fis T: FisBroker enthalten [Keine Faktorgruppe dabei]
#    F: FisBroker nicht enthalten [Faktorgruppe dabei]

#Uebereinstimmungsrate:
#1.vergleich  Berlin Routen[data2] <-> GPS Tracks  #n_inn / #n_Route
#Vergleich  [keine Nummer] Fis Broker <-> Berlin Routen[data2] #n_inn / #n_Route
#2.Vergleich  Fis Broker <-> GPS Track[data2] #n_inn / #n_Track 
#3.Vergleich  Radnetz[data2] <-> GPS Track #n_inn / #n_Radnetz

#Gesamtuebereinstimmungen
 #1.vergleich  Berlin Routen[data2] <-> GPS Tracks
 #innerjoin: #innerjoin/ #Tracks
 #outerjoinleft: #out(Route)/#Route
 #outerjoinright: #out(Track)/#Track
 
 #2.Vergleich  Fis Broker <-> GPS Track[data2]
 #innerjoin: #innerjoin/ #Tracks
 #outerjoinleft: #out(Fis Broker)/#Fis Broker
 #outerjoinright: #out(Track)/#Track

 #3.Vergleich  Radnetz[data2] <-> GPS Track
 #innerjoin: #innerjoin/ #Tracks
 #outerjoinleft: #out(Radnetz)/#Radnetz
 #outerjoinright: #out(Track)/#Track

#####Output
##kontante:
# outerjoinleft_abs 
# outerjoinright_abs
# innerjoin_abs
# outerjoinleft_rel
# outerjoinright_rel
# innerjoin_rel
# inn.trackandrouten
# out.left
# out.right
##dataframe:
# Tabelle_Uebereinstimmungsrate [mean, max, nKonsens]
# result_full [fulljoin data1&data2 mit absoluten Uebereinstimmung (#Innerjoin), relative Uebereinstimmung, n(durch was geteilt wurde für die relative Uebereinstimmung,data2!)]
innerJoin=function(data1,name1,data2,name2,fis=F){
  
  #Datenlaenge
  n1=nrow(data1)
  n2=nrow(data2)#durch was geteilt wird
  
  #Anzahl durch was geteilt wird: nameObjekt  n(Anzahl Objekt-GPS-Koordinaten)
  #fulljoin
  full_join_track_routen_4=full_join(data1,data2,by=c("lat","lon"))
  
  if(fis==F){
    colnames(full_join_track_routen_4)=c("lat","lon",name1,name2)
  } else {colnames(full_join_track_routen_4)=c("lat","lon",name2)
  }
  absolutname=dcast(full_join_track_routen_4,reformulate(".",name2),fun.aggregate = length,value.var = name2)
  colnames(absolutname)=c(name2,"n")
  
  #inner_join  :matched multiple Vergleiche (ein Track mit 2 GPS-Punkten Route  --> 2 Innerjoin Punkte) mehrfach
  innerjoin=inner_join(data1,data2,by=c("lat","lon"))
  if(fis==F){colnames(innerjoin)=c("lat","lon",name1,name2)}
  if(fis==T){colnames(innerjoin)=c("lat","lon",name2)}
  
  #Anzahl bestimmen same:   Route Track abs.join (Wie viele GPS Punkte teilen Sie zusammen?)
  #drop:  should missing combinations dropped or kept?
  if(fis==F){
    dcast_innerjoin=dcast(innerjoin,reformulate(".", paste(name1,"+",name2)), fun.agg = function(x) sum(!is.na(x)),drop=F,value.var=name2)
    colnames(dcast_innerjoin)=c(name1,name2,"abs.join")
  }
  if(fis==T){
    dcast_innerjoin=dcast(innerjoin,reformulate(".", paste(name2)), fun.agg = function(x) sum(!is.na(x)),drop=F,value.var=name2)
    colnames(dcast_innerjoin)=c(name2,"abs.join")
  }
  
  #####################################################################
  #Uebereinstimmungsrate
  #####################################################################
  #joinen zusammen, dabei Mean, Max, ntracks berechnen
  result_full_track_routen=full_join(absolutname, dcast_innerjoin, by = c(name2))
  result_full_track_routen$rel=(result_full_track_routen$abs.join/(  result_full_track_routen$n  ))*100
  
  mean_without0=dcast(result_full_track_routen[which(result_full_track_routen$rel!=0),],reformulate(".",name2),fun.aggregate = mean,value.var = "rel")
  max=(dcast(result_full_track_routen[which(result_full_track_routen$rel!=0),],reformulate(".",name2), fun.agg = function(x) max(x),value.var = "rel"))[,2]
  nTracks=(dcast(result_full_track_routen[which(result_full_track_routen$rel!=0),],reformulate(".",name2), fun.agg = function(x) length(x),value.var = "rel"))[,2]
  
  Tabelle_Uebereinstimmungsrate=data.frame(Objekt1=mean_without0[,1],
                                           mean_without0=mean_without0[,2],
                                           max=max,nObjekt2=nTracks)
  #Umsortierung nach maximalen mean
  Tabelle_Uebereinstimmungsrate=arrange(Tabelle_Uebereinstimmungsrate,desc(mean_without0))
  
  
  #####################################################################
  #Gesamtübereinstimmungen
  #Absolut und Relativ
  #####################################################################
  
  ##########################################################
  # Vergleich 1 und 3 
  ##########################################################
  
  if(fis==F){
    
    out=setdiff(full_join_track_routen_4,innerjoin)
    if(name2=="FisRoute"){out.left=out[which(!is.na(out$FisRoute), arr.ind=TRUE),] }
    if(name2=="Route"){out.left=out[which(!is.na(out$Route), arr.ind=TRUE),] }
    out.right=out[which(!is.na(out$Track), arr.ind=TRUE),]
    inn=innerjoin
    
    outerjoinleft_abs=nrow(out.left)
    outerjoinright_abs=nrow(out.right)
    innerjoin_abs=nrow(inn)
    
    outerjoinleft1_rel=outerjoinleft_abs/n2
    outerjoinright1_rel=outerjoinright_abs/n1
    innerjoin1_rel=innerjoin_abs/n1
    
  }
  ##########################################################
  # Vergleich keine  Nummer  Fis<-->Route
  # Vergleich 2
  ##########################################################
  if(fis==T){
  
    out=setdiff(full_join_track_routen_4,innerjoin)
    if(fis==T & name2=="Track"){
      out.left=out[which(is.na(out$Track), arr.ind=TRUE),]
      out.right=out[which(!is.na(out$Track), arr.ind=TRUE),]
    }
    if(fis==T & name2=="Route"){
      out.left=out[which(is.na(out$Route), arr.ind=TRUE),]
      out.right=out[which(!is.na(out$Route), arr.ind=TRUE),]
    }
    inn=innerjoin
    
    outerjoinleft_abs=nrow(out.left)
    outerjoinright_abs=nrow(out.right)
    innerjoin_abs=nrow(inn)
    
    outerjoinleft1_rel=outerjoinleft_abs/ n1
    outerjoinright1_rel=outerjoinright_abs/ n2
    innerjoin1_rel=innerjoin_abs/ n2
    
  }

  
  return(list(outerjoinleft_abs=outerjoinleft_abs,
              outerjoinright_abs=outerjoinright_abs,
              innerjoin_abs=innerjoin_abs,
              outerjoinleft1_rel=outerjoinleft1_rel,
              outerjoinright1_rel=outerjoinright1_rel,
              innerjoin1_rel=innerjoin1_rel,
              inn.trackandrouten=inn,
              out.left=out.left,
              out.right=out.right,
              Tabelle_Uebereinstimmungsrate=Tabelle_Uebereinstimmungsrate,
              result_full_track_routen=result_full_track_routen))
  
}

####Funktion zur Tabelle mit Joins
full_Join_Tabelle=function(datainn,dataleft,dataright){
  full_Join_dataframe=rbind(cbind(datainn,Join=factor("Inn")),
                            cbind(dataleft,Join=factor("Left")),
                            cbind(dataright,Join=factor("Right")))
  
  return(full_Join_dataframe)
}

########################################################
#Plot meisten gefahrenen Strecke
########################################################
###Input:
#k,l: ID's data1 und date2
#data1,data2: Objekt1[listObjekt] und Objekt2[listObjekt]

###Output:
#ggplot mit den jeweiligen GPS Koordinaten
plotVergleichemeistegefahren=function(k,l,data1=result.list.GPS_Tracks.round,data2=routen_berlin.round){
  
  k=which(names(data1)==k)
  l=which(names(data2)==l)
  
  lat <- range(data1[[k]]$lat,data2[[l]]$lat)
  lon <- range(data1[[k]]$lon,data2[[l]]$lon)
  bbox <- make_bbox(lon,lat)
  b1 <- get_map(bbox,maptype="watercolor", source="stamen")
  
  name=names(data2)[l]
  
  ggmap(b1) +  geom_point(data = (as.data.frame(data2[[l]])),
                          aes(lon,lat), size=1, alpha=0.9) + geom_line()+
    geom_point(data = (as.data.frame(data1[[k]])), 
               aes(rev(lon),rev(lat)), size=1, alpha=0.2,col="darkgreen")+
    labs(x = "Longitude", y = "Latitude")+
    xlim(lon)+
    ylim(lat)+css_typ_title
  
}
########################################################
###############################
# Vergleich 1
# Berlin Route <--> GPS Tracks
###############################
innerJoin_Track_Routen=innerJoin(data1=routen_GPS_Tracks.all_4,name1="Track",data2=routen_berlin.all_4,name2="Route",fis=F)
outerjoinleft1_abs=innerJoin_Track_Routen[[1]]
outerjoinright1_abs=innerJoin_Track_Routen[[2]]
innerjoin1_abs=innerJoin_Track_Routen[[3]]
outerjoinleft1_rel=innerJoin_Track_Routen[[4]]
outerjoinright1_rel=innerJoin_Track_Routen[[5]]
innerjoin1_rel=innerJoin_Track_Routen[[6]]
inn.1.trackandrouten=innerJoin_Track_Routen[[7]]
out.1.trackandrouten_Routen_left=innerJoin_Track_Routen[[8]]
out.1.trackandrouten_Track_right=innerJoin_Track_Routen[[9]]
Tabelle_Uebereinstimmungsrate_1=innerJoin_Track_Routen[[10]]
full_join_rel_1=innerJoin_Track_Routen[[11]]
#Welches ist der Track und die Route mit dem maximalen relativen Uebereinstimmungsrate?
full_join_rel_1[which.max(full_join_rel_1$rel),]


full_Join_Tabelle_1=full_Join_Tabelle(inn.1.trackandrouten,out.1.trackandrouten_Routen_left,
                  out.1.trackandrouten_Track_right)

#Examples GPS Track
plotVergleichemeistegefahren(k=full_join_rel_1[which.max(full_join_rel_1$rel),"Track"],
                             l=full_join_rel_1[which.max(full_join_rel_1$rel),"Route"],
                             data1=GPS_Tracks.round_4,data2=routen_berlin.round_4)
########################################################
#### Fis Broker In eine Liste umwandeln
########################################################
fis_broker_1=data.frame(lon=c(),lat=c())
n_fis=length(fis_broker)
for(i in 1:n_fis){
  fis_broker_1=rbind(fis_broker_1,data.frame(lon=data.frame(fis_broker[i])[,1],lat=data.frame(fis_broker[i])[,2]))
}

round4_fisbroker=Runden_Melt(r=4,list=list(fis_broker_1),dis = T)
fisbroker.round_4=round4_fisbroker[[1]]
fisbroker.all_4=round4_fisbroker[[2]]
fisbroker.all_4=fisbroker.all_4[,-3]
fisbroker_lat_4=round4_fisbroker[[3]]
fisbroker_lon_4=round4_fisbroker[[4]]
########################################################
## Vergleich keine Nummer
## Fis Broker <-->routen_berlin
########################################################
innerJoin_fis_route=innerJoin(data1=fisbroker.all_4,name1="Fis_Broker",data2=routen_berlin.all_4,name2="Route",fis=T)
outerjoinleft2_abs=innerJoin_fis_route[[1]]
outerjoinright2_abs=innerJoin_fis_route[[2]]
innerjoin2_abs=innerJoin_fis_route[[3]]
outerjoinleft2_rel=innerJoin_fis_route[[4]]
outerjoinright2_rel=innerJoin_fis_route[[5]]
innerjoin2_rel=innerJoin_fis_route[[6]]
inn.2=innerJoin_fis_route[[7]]
out.2_fisrouten_FIS_left=innerJoin_fis_route[[8]]
out.2_fisrouten_Routen_right=innerJoin_fis_route[[9]]
Tabelle_Uebereinstimmungsrate_2=innerJoin_fis_route[[10]]
full_join_rel_2=innerJoin_fis_route[[11]]
full_join_rel_2[which.max(full_join_rel_2$rel),]

full_Join_Tabelle_2=full_Join_Tabelle(inn.2,out.2_fisrouten_FIS_left,
                                      out.2_fisrouten_Routen_right)
########################################################
###Vergleich 2
## Fis Broker <-->gps tracks
## #inner_join same / # GPS Tracks
########################################################
innerJoin_fis_gps=innerJoin(data1=fisbroker.all_4,name1="Fis_Broker",data2=routen_GPS_Tracks.all_4,name2="Track",fis=T)
outerjoinleft3_abs=innerJoin_fis_gps[[1]]
outerjoinright3_abs=innerJoin_fis_gps[[2]]
innerjoin3_abs=innerJoin_fis_gps[[3]]
outerjoinleft3_rel=innerJoin_fis_gps[[4]]
outerjoinright3_rel=innerJoin_fis_gps[[5]]
innerjoin3_rel=innerJoin_fis_gps[[6]]
inn.3.track=innerJoin_fis_gps[[7]]
out.3.fistrack_FIS_left=innerJoin_fis_gps[[8]]
out.3.fistrack_TRACK_right=innerJoin_fis_gps[[9]]
Tabelle_Uebereinstimmungsrate_3=innerJoin_fis_gps[[10]]
full_join_rel_3=innerJoin_fis_gps[[11]]
#Welche Track ID hat mit Fis_Broker die maximale relative Uebereinstimmungsrate?
full_join_rel_3[which.max(full_join_rel_3$rel),]

full_Join_Tabelle_3= full_Join_Tabelle(inn.3.track,out.3.fistrack_FIS_left,out.3.fistrack_TRACK_right) 

########################################################
####Fis und Routen in eine Liste
########################################################
fis_route.all_4=rbind(data.frame(setdiff(fisbroker.all_4,distinct(routen_berlin.all_4[,-3])),L1=as.factor("Fis")),
                      routen_berlin.all_4)
########################################################
###Vergleich 3
## Radnetz (Fis&Route) <-->gps tracks
########################################################
innerJoin_fisRoute_gps=innerJoin(data1=routen_GPS_Tracks.all_4,name1="Track",data2=fis_route.all_4,name2="FisRoute",fis=F)
outerjoinleft4_abs=innerJoin_fisRoute_gps[[1]]
outerjoinright4_abs=innerJoin_fisRoute_gps[[2]]
innerjoin4_abs=innerJoin_fisRoute_gps[[3]]
outerjoinleft4_rel=innerJoin_fisRoute_gps[[4]]
outerjoinright4_rel=innerJoin_fisRoute_gps[[5]]
innerjoin4_rel=innerJoin_fisRoute_gps[[6]]

inn.4.track.4=innerJoin_fisRoute_gps[[7]]
out.4.routenfistrack_ROUTENFIS_left=innerJoin_fisRoute_gps[[8]]
out.4.routenfistrack_TRACK_right=innerJoin_fisRoute_gps[[9]]
Tabelle_Uebereinstimmungsrate_4=innerJoin_fisRoute_gps[[10]]
full_join_rel_4=innerJoin_fisRoute_gps[[11]]
#Welcher Radnetz Faktor hat mit welchem Track die max relative Uebereinstimmungsrate?
full_join_rel_4[which.max(full_join_rel_4$rel),]

full_Join_Tabelle_4= full_Join_Tabelle(datainn=inn.4.track.4,dataleft=out.4.routenfistrack_ROUTENFIS_left,dataright=out.4.routenfistrack_TRACK_right) 
  
gefahrene_Innerjoin=sapply(1:(nrow(inn.4.track.4)-1),FUN = function(x)
  measure.meter(inn.4.track.4[x,"lon"],inn.4.track.4[x,"lat"],
                inn.4.track.4[x+1,"lon"],inn.4.track.4[x+1,"lat"]))

Total_gefahrene_Innerjoin=sum(gefahrene_Innerjoin)

##############################################################################################
####Tabelle  Fulljoin
#############################################################################################

Join_4_Tabelle_abs=data.frame(Vergleich=c("Berlin Routen <--> GPS Track",
                                          "Fis Broker <--> GPS Track",
                                          "Routen & Fis <--> GPS Track" ),
                              innerjoin=c(innerjoin1_abs,innerjoin3_abs,innerjoin4_abs),
                              outerjoinleft=c(outerjoinleft1_abs,outerjoinleft3_abs,outerjoinleft4_abs),
                              outerjoinright=c(outerjoinright1_abs,outerjoinright3_abs,outerjoinright4_abs)
)

Join_4_Tabelle_rel=data.frame(Vergleich=c("Berlin Routen <--> GPS Track",
                                          "Fis Broker <--> GPS Track",
                                          "Routen & Fis <--> GPS Track" ),
                              innerjoin=c(innerjoin1_rel,innerjoin3_rel,innerjoin4_rel),
                              outerjoinleft=c(outerjoinleft1_rel,outerjoinleft3_rel,outerjoinleft4_rel),
                              outerjoinright=c(outerjoinright1_rel,outerjoinright3_rel,outerjoinright4_rel)
)
Join_4_Tabelle_rel$innerjoin=round(Join_4_Tabelle_rel$innerjoin,4)
Join_4_Tabelle_rel$outerjoinleft=round(Join_4_Tabelle_rel$outerjoinleft,4)
Join_4_Tabelle_rel$outerjoinright=round(Join_4_Tabelle_rel$outerjoinright,4)

Join_4_Tabelle_abs
Join_4_Tabelle_rel
##############################################################################################
### Dichte 
##############################################################################################
#Dichtequotient:
#{gefahrene Meter im Raster [Out_Right(GPS)]/ Total gefahrene [GPS]} /
#{gefahrene Meter [Inn(Objekt <-> GPS)] / gefahrene Meter [Objekt+ GPS]}
########################################################
####Gitter erzeugen
########################################################
#Berlin wird mit lon 13-14 und lat 52-53 vollständig abgebildet
lonseq=seq(13.0000,14.0000,0.0001)
nlonseq=length(lonseq)
latseq=seq(52.0000,53.0000,0.0001)
nlatseq=length(latseq)
exp.grid=expand.grid(lonseq,latseq)
colnames(exp.grid)=c("lon","lat")
exp.grid$Raster=c()


EXP_GRID_FUNC=function(m=100,n=20){
  gridrep=c()
  k=1
  for (l in 1:n){
    fak=c(rep((k:(k+(n-2))),each=m),rep((k+(n-1)),(m+1)))
    fak1=rep(fak,m)
    gridrep=c(gridrep,fak1)
    k=k+n
  }
  
  return(gridrep=c(gridrep,fak))
  
}
gridrep4=EXP_GRID_FUNC(500,20)

exp.grid[,3]=as.factor(gridrep4)
colnames(exp.grid)=c("lon","lat","Raster")

#Hilfsplot für Dichte
df=data.frame(lon=rep(seq(13.025,13.975,0.05),20),lat=rep(seq(52.025,52.975,0.05),each=20),Raster=factor(1:400))
#######################################################################################
######################################################################################
####  Inner_join

#Output: innerjoin: innerjoin(data1,grid)
#        dichte_meter: innerjoin(grid,Raster_Meter)

Inner_join_Dichte=function(data1=full_Join_Tabelle_4,grid=exp.grid){
  
  #Damit die Raster zugeordnet werden
  innerjoin= dplyr::inner_join(data1,grid,by=c("lat","lon"))
  #gefahrene Meter Berechnung
  n=as.numeric(as.character(exp.grid$Raster[nrow(exp.grid)]))
  n1=as.numeric(as.character(exp.grid$Raster[1]))
  Raster_Meter=data.frame("Raster"=as.factor(n1:n),
                          "Meter"=rep(0,length(n1:n)))
  n_Raster=levels(droplevels(innerjoin$Raster))
  
  for( i in n_Raster){
    filter_innerjoin=filter(innerjoin,Raster==i)
    n=nrow(filter_innerjoin)
    if(n>1){
      Raster_Meter[i,2]=sum(sapply(1:(n-1), function(x){
        measure.meter(filter_innerjoin$lon[x],filter_innerjoin$lat[x],
                      filter_innerjoin$lon[(x+1)],filter_innerjoin$lat[(x+1)])
        
      }))
    }
    
  }
   innerjoin_Meter=full_join(innerjoin,Raster_Meter,by="Raster")
  
  return(innerjoin=innerjoin_Meter)
  
}
#######################################################################################
#Grid Erzeugung und Dichtequotientenberechnung
######################################################################################
####CSS Styles
##################################################################################

css_typ1.withoutlegende = list(
  theme_classic(),
  scale_x_continuous(limits = c(13,14), expand = c(0, 0)),
  scale_y_continuous(limits = c(52.25,52.7), expand = c(0, 0)),
  theme(axis.text=element_text(size=32,face = "bold"),
  axis.title.x=element_text(size=35,face="plain",hjust = 0.5,vjust=-2),
  axis.title.y=element_text(size=35,face="plain",hjust = 0.5,vjust=3),
  title=element_text(size=33,face="plain",hjust = 0.5),
  plot.margin = (unit(c(.5, .5, 1, 1), "cm"))))

css_typ1 = list(theme_classic(),
                scale_x_continuous(limits = c(13,14), expand = c(0.01, 0.01)),
                scale_y_continuous(limits = c(52.3,52.6), expand = c(0.01, 0.01)),
                theme(axis.text=element_text(size=28,face = "bold"),
                axis.title.x=element_text(size=35,face="plain",hjust = 0.5,vjust=-2),
                axis.title.y=element_text(size=35,face="plain",hjust = 0.5,vjust=3),
                title=element_text(size=36,face="plain",hjust = 0.5),
                legend.title = element_text(size=35,face="plain"),
                legend.text = element_text(size=27,face="plain"),
                plot.margin = (unit(c(.5, .5, 1, 1), "cm")))
)

css_typ1_withoutscale = list(theme_classic(),
                theme(axis.text=element_text(size=32,face = "bold"),
                      axis.title.x=element_text(size=35,face="plain",hjust = 0.5,vjust=-2),
                      axis.title.y=element_text(size=35,face="plain",hjust = 0.5,vjust=3),
                      title=element_text(size=36,face="plain",hjust = 0.5),
                      legend.title = element_text(size=35,face="plain"),
                      legend.text = element_text(size=27,face="plain"),
                      plot.margin = (unit(c(.5, .5, 1, 1), "cm"))))


#######################################################################################
####Radnetz <--> GPS Tracks
#######################################################################################
#Zaehler
Out_Right_join_Dichte_Radnetz=Inner_join_Dichte(data1=full_Join_Tabelle_4[which(full_Join_Tabelle_4$Join=="Right"),],grid=exp.grid)
#Nenner
Total_join_Dichte_Radnetz=Inner_join_Dichte(data1=full_Join_Tabelle_4,grid=exp.grid)
Inner_join_Dichte_Radnetz=Inner_join_Dichte(data1=full_Join_Tabelle_4[which(full_Join_Tabelle_4$Join=="Inn"),],grid=exp.grid)
#Alles in ein Dataframe
prep=full_join(distinct(Inner_join_Dichte_Radnetz[c("Raster","Meter")]),distinct(Total_join_Dichte_Radnetz[c("Raster","Meter")]),by="Raster",suffix = c(".Inn", ".Total"))
prep=full_join(distinct(Out_Right_join_Dichte_Radnetz[c("Raster","Meter")]),prep,by="Raster")
colnames(prep)=c("Raster","Meter.Out_Right","Meter.Inn","Meter.Total")
#Dichtequotient
prep$Nenner=ifelse(prep$Meter.Inn==0 | prep$Meter.Total==0,0,prep$Meter.Inn/prep$Meter.Total)
prep$Zaehler=ifelse(prep$Meter.Out_Right==0,0,prep$Meter.Out_Right/totalgefahren_GPS)
prep$quotient=ifelse(prep$Zaehler==0 | prep$Nenner==0,0,prep$Zaehler/prep$Nenner)
#Für die Abbildungen
full_Join_Dichtequotient=left_join(exp.grid,prep,by="Raster")
########################################################
### Kasten um das Raster des grössten Dichtequotienten
########################################################
df.box=data.frame(lon=c(13.4,13.4499,13.4499,13.4,13.4),lat=c(52.45,52.45,52.4999,52.4999,52.45))
########################################################
# Dichtequotientenplot
########################################################
plot_merge_Radnetz=inner_join(df,distinct(full_Join_Dichtequotient[,c("Raster","quotient")]), by = "Raster")
plot_merge_Radnetz$quotient=ifelse(is.na(plot_merge_Radnetz$quotient),0,plot_merge_Radnetz$quotient)
plot_merge_Radnetz$quotient=ifelse(plot_merge_Radnetz$quotient==Inf,0,plot_merge_Radnetz$quotient)

plot10=ggplot(plot_merge_Radnetz,aes(x=lon,y=lat)) +
  geom_raster(aes(fill=quotient))+
  geom_path(data=df.box,aes(x=lon,y=lat),col = 'white')+
  labs(x = "Longitude", y = "Latitude",title = "Dichte Radnetz",fill="Quotient")+css_typ1
plot10

###########################################################
###Reinzommen#############################################
###########################################################
zoom=full_Join_Dichtequotient[which(full_Join_Dichtequotient$quotient==max(full_Join_Dichtequotient$quotient)),]#Raster  189 max
zoom=zoom[,c(1:3)]
###
fisRouteOUT_trackIN=inner_join(zoom,out.4.routenfistrack_TRACK_right)
fisRouteIN_trackOUT=inner_join(zoom,out.4.routenfistrack_ROUTENFIS_left)
fisRouteIN_trackIN=inner_join(zoom,inn.4.track.4)
fisRouteIN_trackIN$Raster=as.factor(fisRouteIN_trackIN$Raster)
plo=rbind(data.frame(fisRouteIN_trackIN[,c(1,2,3)],Join="In"),
          data.frame(fisRouteIN_trackOUT[,c(1,2,3)],Join="FisRoute"),
          data.frame(fisRouteOUT_trackIN[,c(1,2,3)],Join="Track")
)

lat <- range(zoom$lat)
lon <- range(zoom$lon)
bbox <- make_bbox(lon,lat)
b1<- get_map(bbox,maptype="watercolor", source="stamen")

plot10=ggmap(b1) +  geom_point(data = plo,
                               aes(lon,lat,colour=Join), size=1.8, alpha=0.5)+
  labs(x = "Longitude", y = "Latitude",color = "Farbcode",title = "Radnetz GPS")+theme(plot.margin=unit(c(0,0,0,0),"mm"))+
  geom_point(data = subset(plo,Join=="FisRoute"), aes(lon,lat,colour=Join), size=1.8, alpha=0.5)+
  geom_point(data = subset(plo,Join=="In"), aes(lon,lat,colour=Join), size=1.8, alpha=0.5)+
  guides(colour=guide_legend(ncol=1,override.aes = list(shape = 20, size = 5)))+
  scale_color_manual(values=c("red", "blue", "yellow1"))+ 
  theme(legend.key=element_rect(fill = NA))+css_typ1_withoutscale

plot10
#############################################################################
####Ende
#############################################################################