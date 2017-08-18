ui <- dashboardPage(
  dashboardHeader(title = "Scheepvaartongevallen rapportage"),
  
  dashboardSidebar( #disable = TRUE),
    width = 180,
    tags$head(tags$style("#plot1{height:20vh !important;}")),
    tags$head(tags$style("#plot2{height:40vh !important;}")),
    tags$head(tags$style("#plot1_esri{height:20vh !important;}")),
    tags$head(tags$style("#plot2_esri{height:40vh !important;}")),   
    
    sidebarMenu(
       menuItem("Leaflet voorbeeld", tabName = "niet_sign", icon = icon("unlock")),
       menuItem("ESRI voorbeeld", tabName = "esri", icon = icon("lock"))
       
     )
    
  ),
  
  
  dashboardBody(
    
    tabItems(
      # First tab content

      tabItem(tabName = "niet_sign",
              ## downloadButton("report", "Print naar PDF"),
              #h3("Visualisatie"),
              p("Deze tool geeft analyseresultaten van scheepvaartongevallen, interactief weergegeven in grafiekvorm en op een kaart."),
              checkboxGroupInput("type", "Type ongevallen:", choices=c("Niet significant scheepsongeval", "Significant scheepsongeval"), 
                                 selected =c("Niet significant scheepsongeval", "Significant scheepsongeval")),
              
              ##Rapport downloaden, lukt nog niet, internal server error
              # radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
              #              inline = TRUE),
              # downloadButton("downloadFile", "Download"),
              
                
              tabBox(
                title = "", 
                id = "tabset 1", height = "250px",
                tabPanel("Aantal ongevallen", 
                h4("Scheepvaartongevallen (totaal)"),
                plotOutput("plot1") %>% withSpinner(color="#EAF2F8"),  ##withSpinner laat laadteken zien bij laden van output
              
                h4("Scheepvaartongevallen per regio"),
                plotOutput("plot2") %>% withSpinner(color="#EAF2F8")
                ),
                
                tabPanel("Gewonden tabel",
                h4("Gewonden, vermisten en doden"),
                p("Kies evt. jaar en/of beheerder onder de kaart. Klik in de tabel om data op de kaart te filteren."),
                p("Let op: bij deselecteren van de cel kan het enkele seconden duren voor de kaart weer is geladen."),
                dataTableOutput('table1') %>% withSpinner(color="#EAF2F8")
                ),
                
                tabPanel("Gewonden grafiek",
                h4("Gewonden, vermisten en doden"),
                p("Klik op de buttons om een tijdsrange te kiezen, of schuif in de onderstaande balk."),
                p("Hover, klik, of trek een selectie om de punten om deze te zien oplichten in de kaart (best zichtbaar op de donkere basemap), dubbelklik om de selectie weer te verwijderen."),
                plotlyOutput("plot0")  %>% withSpinner(color="#EAF2F8")        
                ),
                
                tabPanel("Ongevallen kalender",
                         h4("Ongevallen overzicht op datum"),
                         checkboxGroupInput("jaar_box", "Jaar:", choices=c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), 
                                            selected =c("2014","2015", "2016"), inline = TRUE),
                         p("Kies een nautisch beheerder onder de kaart om de ongevallenkalender van een bepaalde regio te zien."),
                         plotOutput("plot3")  %>% withSpinner(color="#EAF2F8")
                )
              ),
                

              box(
                #tags$style(type = "text/css", ".box-body {height:80vh}"),
                leafletOutput("map") %>% withSpinner(color="#EAF2F8"), height = 600,
                selectInput("beheerder","Nautisch Beheerder:",choices=beheerders, selected="Alle"),
                p("De jaren 2011 t/m 2014 zijn het best zichtbaar op een donkere basemap."),
                selectInput("jaar","Jaar: ",choices = jaren,selected = "Alle")),
              

              a(href="mailto:martijn.koole@rws.nl","Martijn Koole & Sietske Tjalma",icon("info-circle"), style = "float: right")
        
      ),
      
    
       
      tabItem(tabName = "esri",
             p("Deze tool geeft analyseresultaten van scheepvaartongevallen, interactief weergegeven in grafiekvorm en op een kaart."),
             checkboxGroupInput("type_esri", "Type ongevallen:", choices=c("Niet significant scheepsongeval", "Significant scheepsongeval"), selected =c("Niet significant scheepsongeval", "Significant scheepsongeval")),  
             tabBox(
               title = "", 
               id = "tabset 2", height = "250px",
               tabPanel("Aantal ongevallen", 
                        h4("Scheepvaartongevallen (totaal)"),
                        plotOutput("plot1_esri") %>% withSpinner(color="#EAF2F8"),  ##withSpinner laat laadteken zien bij laden van output
                        
                        h4("Scheepvaartongevallen per regio"),
                        plotOutput("plot2_esri") %>% withSpinner(color="#EAF2F8")
               ),
               
               tabPanel("Gewonden tabel",
                        h4("Gewonden, vermisten en doden"),
                        p("Kies evt. jaar en/of beheerder onder de kaart."),
                        #p("Let op: bij deselecteren van de cel kan het enkele seconden duren voor de kaart weer is geladen"),
                        dataTableOutput('table1_esri') %>% withSpinner(color="#EAF2F8")
               ),
               
               tabPanel("Gewonden grafiek",
                        h4("Gewonden, vermisten en doden"),
                        p("Klik op de buttons om een tijdsrange te kiezen, of schuif in de onderstaande balk."),
                        #p("Hover, klik, of trek een selectie om de punten om deze te zien oplichten in de kaart, dubbelklik om de selectie weer te verwijderen"),
                        plotlyOutput("plot0_esri")  %>% withSpinner(color="#EAF2F8")        
               ),
               
               tabPanel("Ongevallen kalender",
                        h4("Ongevallen overzicht op datum"),
                        checkboxGroupInput("jaar_box_esri", "Jaar:", choices=c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"), 
                                           selected =c("2014","2015", "2016"), inline = TRUE),
                        plotOutput("plot3_esri")  %>% withSpinner(color="#EAF2F8")
               )
             ),
               box(
                 p("De data wordt in deze kaart rechstreeks van ArcGIS online Feature layers of WMS-services geladen."),
                 p("Hierdoor wordt de visualisatie van de RWS layers behouden, er is echter geen interactie tussen de kaart en de tabellen/grafieken."),
                 tags$style(type = "text/css", ".box-body {height:100vh}"),
                 leafletOutput("map2")%>% withSpinner(color="#EAF2F8"), height = 800, 
                 selectInput("beheerder_esri","Nautisch Beheerder:",choices=beheerders, selected="Alle"),
                 selectInput("jaar_esri","Jaar: ",choices = jaren,selected = "Alle")
               ),
               a(href="mailto:sietske.tjalma@rws.nl","Sietske Tjalma & Martijn Koole",icon("info-circle"), style = "float: right")
       )
    )
  )
)

