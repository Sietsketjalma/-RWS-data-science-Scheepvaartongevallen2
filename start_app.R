library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(plotly)
library(DT)
library(rgdal)
library(htmlwidgets)
library(shinyjs)
library(htmltools)
library(leaflet.extras)
library(leaflet.esri)
library(geojsonio)
library(rmapshaper)
library(shinycssloaders)
library(scales)
library(zoo)
library(plyr)
library(devtools)
#library(rmarkdown)

#load("sch_niet_sig.RData")
#load("sch_sig.RData")
load("sch_tot.RData")
# load("gewonden_per_jaar_sign.RData")
# load("gewonden_per_jaar_niet_sign.RData")
load("gewonden_per_jaar_tot.RData")
load("veiligh_regios.Rdata")
load("gewonden_per_beh_tot.RData")
load("gewonden_per_beh_jaar_tot.RData")

beheerders<- c("Alle","Midden Nederland","Noord Nederland","Oost Nederland","RWS Centraal Nautisch Beheer","West Nederland Noord",
               "West Nederland Zuid", "Zee & Delta","Zuid Nederland")
beheerders2<- c("Midden Nederland","Noord Nederland","Oost Nederland","RWS Centraal Nautisch Beheer","West Nederland Noord",
                "West Nederland Zuid", "Zee & Delta","Zuid Nederland")

jaren<- c("Alle",2008:2017)


## workaround. Clickable table begint met tellen bij 0. zelf gemaakte Gewonden_per_jaar_tot bij 1. Mismatch.
cats <- list("Typering"= "AANTAL_GEWONDEN_LICHT",
            "Licht gewond"= "AANTAL_GEWONDEN_ZWAAR",
            "Zwaar gewond"= "AANTAL_GEWONDEN_OVERIG",
            "Gewond Overig"= "AANTAL_VERMISTEN",
            "Vermist" = "AANTAL_DODEN"
            )
## voor jaar aparte list, want daar andere kolomnamen
cats_jaar <- list("Jaar"= "AANTAL_GEWONDEN_LICHT",
                  "Licht gewond"= "AANTAL_GEWONDEN_ZWAAR",
                  "Zwaar gewond"= "AANTAL_GEWONDEN_OVERIG",
                  "Gewond Overig"= "AANTAL_VERMISTEN",
                  "Vermist" = "AANTAL_DODEN"
                  )


maanden <- list("Jan"=1,"Feb"=2,"Mar"=3,
              "Apr"=4,"Mei"=5,"Jun"=6,
              "Jul"=7,"Aug"=8,"Sep"=9,
              "Okt"=10,"Nov"=11,"Dec"=12)


shiny::runApp("dashboard")
