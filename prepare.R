##
ogrListLayers("WVL_Scheepvaartongevallen.gdb")
sch_niet_sig<- readOGR("WVL_Scheepvaartongevallen.gdb",layer="Scheepsvoorvallen_niet_significant")
sch_sig<- readOGR("WVL_Scheepvaartongevallen.gdb",layer="Scheepsvoorvallen_significant")

#sluizen<- readShapePoints("WVL_Scheepvaartongevallen.gdb")
#sluizen<- readOGR("WVL_Scheepvaartongevallen.gdb",layer="Sluizen")
# bruggen<- readOGR("WVL_Scheepvaartongevallen.gdb",layer="Bruggen")

##to wgs
rd<- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +no_defs"
wgs<- "+proj=longlat +ellps=WGS84 +datum=WGS84"
proj4string(sch_niet_sig)<- rd #definieer de projectie van shapefile
sch_niet_sig<- spTransform(sch_niet_sig,wgs) #transformeer naar wgs84
proj4string(sch_sig)<- rd #definieer de projectie van shapefile
sch_sig<- spTransform(sch_sig,wgs) #transformeer naar wgs84
# 
# save(sch_niet_sig,file="sch_niet_sig.RData")
# save(sch_sig,file="sch_sig.RData")

ogrListLayers("WVL_Scheepvaartongevallen.gdb")
Veiligh_regios<- readOGR("WVL_Scheepvaartongevallen.gdb",layer="Veiligheidsregios_2015")
proj4string(Veiligh_regios)<- rd #definieer de projectie van shapefile
Veiligh_regios<- spTransform(Veiligh_regios,wgs) #transformeer naar wgs84
save(Veiligh_regios, file="veiligh_regios.RData")


## Raster inladen kan, laad traag
## library(raster)
## AHN<-'r5_37en1.tif' 
## AHN_ras=raster(AHN)
## pal <- colorNumeric(palette = "Spectral", values(AHN_ras),
##                     na.color = "transparent")
## addRasterImage(AHN_ras, colors = pal, opacity = 0.8) %>%




sch_tot<- rbind(sch_niet_sig,sch_sig)
sch_tot$TYPERING_VOORVAL_OMSCHRIJVING<-ifelse(sch_tot$TYPERING_VOORVAL_OMSCHRIJVING == "Significant scheepsongeval", "Significant scheepsongeval","Niet significant scheepsongeval")

# Datum format is nodig voor plotly grafiek, kan geen NL volgorde datum lezen
sch_tot$DATUM_FORMAT <- as.Date(sch_tot$DATUM_VOORVAL , "yy/mm/dd")
sch_tot[order(sch_tot$DATUM_FORMAT ),]

sch_tot$TOTAAL_SLACHTOFFERS <- with(sch_tot, sch_tot$AANTAL_GEWONDEN_LICHT+sch_tot$AANTAL_GEWONDEN_ZWAAR+sch_tot$AANTAL_GEWONDEN_OVERIG+sch_tot$AANTAL_VERMISTEN+sch_tot$AANTAL_DODEN)

save(sch_tot,file="sch_tot.RData")

plot(sch_sig)

library(leaflet)
leaflet() %>% addTiles() %>% addCircles(data=sch_niet_sig)


###explore
load("sch_niet_sig.RData")
load("sch_sig.RData")

library(ggplot2)
df<- as.data.frame(sch_niet_sig)
ggplot(df,aes(x=WEEKDAG_VOORVAL))+geom_histogram(stat="count")
ggplot(df,aes(x=WINDKRACHT_OMSCHRIJVING))+geom_histogram(stat="count")
ggplot(df,aes(x=BEVAARBAARHEIDSKLASSE_OMS))+geom_histogram(stat="count")
ggplot(df,aes(x=X_COORDINAAT,y=Y_COORDINAAT))+geom_point()

df2 <- as.data.frame(sch_sig)
ggplot(df2,aes(x=X_COORDINAAT,y=Y_COORDINAAT))+geom_point()

mymonths <- c("Jan","Feb","Mar",
              "Apr","Mei","Jun",
              "Jul","Aug","Sep",
              "Okt","Nov","Dec")


Gewonden_per_jaar_tot<- aggregate(cbind(AANTAL_GEWONDEN_LICHT,AANTAL_GEWONDEN_ZWAAR,AANTAL_GEWONDEN_OVERIG,AANTAL_VERMISTEN,AANTAL_DODEN) ~ MAAND_VOORVAL + JAAR_VOORVAL + TYPERING_VOORVAL_OMSCHRIJVING,
                                        data=sch_tot,FUN="sum")
colnames(Gewonden_per_jaar_tot)<- c("Maand","Jaar","Typering","Licht gewond","Zwaar gewond","Gewond Overig","Vermist","Dood")
Gewonden_per_jaar_tot$Maand <- mymonths[ Gewonden_per_jaar_tot$Maand ]
Gewonden_per_jaar_tot<- Gewonden_per_jaar_tot[,c(2,1,3,4,5,6,7,8)]
save(Gewonden_per_jaar_tot,file="gewonden_per_jaar_tot.RData")

## Om tabel in mooi formaat te behouden, maar ook te filteren op beheerder onderstaande twee variabelen nodig.
Gewonden_per_beh_tot<- aggregate(cbind(AANTAL_GEWONDEN_LICHT,AANTAL_GEWONDEN_ZWAAR,AANTAL_GEWONDEN_OVERIG,AANTAL_VERMISTEN,AANTAL_DODEN) ~ JAAR_VOORVAL + TYPERING_VOORVAL_OMSCHRIJVING + NAUTISCH_BEHEERDER,
                                  data=sch_tot,FUN="sum")
colnames(Gewonden_per_beh_tot)<- c("Jaar","Typering","Beheerder","Licht gewond","Zwaar gewond","Gewond Overig","Vermist","Dood")
Gewonden_per_beh_tot<- Gewonden_per_beh_tot[,c(3,1,2,4,5,6,7,8)]
save(Gewonden_per_beh_tot,file="gewonden_per_beh_tot.RData")


Gewonden_per_beh_jaar_tot <- aggregate(cbind(AANTAL_GEWONDEN_LICHT,AANTAL_GEWONDEN_ZWAAR,AANTAL_GEWONDEN_OVERIG,AANTAL_VERMISTEN,AANTAL_DODEN) ~ JAAR_VOORVAL + MAAND_VOORVAL + TYPERING_VOORVAL_OMSCHRIJVING + NAUTISCH_BEHEERDER,
                                       data=sch_tot,FUN="sum")
colnames(Gewonden_per_beh_jaar_tot)<- c("Jaar","Maand","Typering","Beheerder","Licht gewond","Zwaar gewond","Gewond Overig","Vermist","Dood")
Gewonden_per_beh_jaar_tot$Maand <- mymonths[Gewonden_per_beh_jaar_tot$Maand ]
Gewonden_per_beh_jaar_tot<- Gewonden_per_beh_jaar_tot[,c(1,4,2,3,5,6,7,8,9)]
save(Gewonden_per_beh_jaar_tot,file="gewonden_per_beh_jaar_tot.RData")
