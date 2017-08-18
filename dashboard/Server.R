

server <- function(input, output) {

  ## Rapport downloaden, lukt nog niet, internal server error
  # output$downloadFile <- downloadHandler(
  #   filename = function() {
  #     paste('my-report', sep = '.', switch(
  #       input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
  #     ))
  #   },
  # 
  #   content = function(file) {
  #     src <- normalizePath('report.Rmd')
  # 
  #     # temporarily switch to the temp dir, in case you do not have write
  #     # permission to the current working directory
  #     owd <- setwd(tempdir())
  #     on.exit(setwd(owd))
  #     file.copy(src, 'report.Rmd', overwrite = TRUE)
  # 
  #     #library(rmarkdown)
  #     out <- rmarkdown::render('report.Rmd', 
  #                              params = list(plot1 = input$plot1),
  #                              switch(input$format,
  #       PDF = pdf_document(), HTML = html_document(), Word = word_document()
  #     ))
  #     file.rename(out, file)
  #   }
  # )

  
  #create reactive dataset to input checkboxes significant niet significant
  data2<- reactive({
    sch_tot[which(sch_tot$TYPERING_VOORVAL_OMSCHRIJVING %in% input$type),]
    })
  

  #create reactive dataset based on dropdown menus beheerder en jaar
  data<- reactive({
    sch_tot2<- sch_tot[which(sch_tot$TYPERING_VOORVAL_OMSCHRIJVING %in% input$type),]
    
    if(input$beheerder == "Alle" & input$jaar == "Alle" ){
      sch_tot2[which(sch_tot2$NAUTISCH_BEHEERDER %in% beheerders2),]
    }
    else if (input$beheerder != "Alle" & input$jaar == "Alle"){
    sch_tot2[which(sch_tot2$NAUTISCH_BEHEERDER %in% input$beheerder),]
      }
    else if (input$beheerder == "Alle" & input$jaar != "Alle"){
      sch_tot2[which(sch_tot2$NAUTISCH_BEHEERDER %in% beheerders2 & 
                           sch_tot2$JAAR_VOORVAL %in% input$jaar),]
      }
    else {
      sch_tot2[which(sch_tot2$NAUTISCH_BEHEERDER %in% input$beheerder & 
                           sch_tot2$JAAR_VOORVAL %in% input$jaar),]
    }
   
  })
    
  
  ## Aparte datasets als er geklikt wordt in de tabel. Anders wacht Leaflet altijd tot de hele tabel geladen is. Nu losstaand.   
  data_click<- reactive ({  
    if (input$jaar != "Alle" & input$beheerder== "Alle"){    ## Jaar & Alle
      cat<- cats[colnames(data3())[input$table1_cells_selected[,2]]][[1]]
      maand<- maanden[data3()[input$table1_cells_selected[,1],2]][[1]]
      type <- data3()[input$table1_cells_selected[,1],3][[1]]
      
      sch_tot[which(sch_tot$JAAR_VOORVAL %in% data3()[input$table1_cells_selected[,1],1] &
                           sch_tot$MAAND_VOORVAL %in% maand &
                           sch_tot$TYPERING_VOORVAL_OMSCHRIJVING %in% type &
                           sch_tot[,cat][[1]]>0),]
    }
    else if (input$jaar == "Alle" & input$beheerder != "Alle"){  ## Alle & Beheerder
      cat2<- cats[colnames(data3())[input$table1_cells_selected[,2]]][[1]]
      
      sch_tot[which(sch_tot$JAAR_VOORVAL %in% data3()[input$table1_cells_selected[,1],2] &
                      sch_tot$NAUTISCH_BEHEERDER %in% data3()[input$table1_cells_selected[,1],1] &
                      sch_tot$TYPERING_VOORVAL_OMSCHRIJVING %in% data3()[input$table1_cells_selected[,1],3][[1]] &
                      sch_tot[,cat2][[1]]>0),]
    }
    else if (input$jaar != "Alle" & input$beheerder != "Alle"){ ## Jaar & Beheerder
      cat3<- cats[colnames(data3())[input$table1_cells_selected[,2]]][[1]]
      maand3<- maanden[data3()[input$table1_cells_selected[,1],3]][[1]]

      sch_tot[which(sch_tot$JAAR_VOORVAL %in% data3()[input$table1_cells_selected[,1],1] &
                      sch_tot$NAUTISCH_BEHEERDER %in% data3()[input$table1_cells_selected[,1],2] &
                      sch_tot$TYPERING_VOORVAL_OMSCHRIJVING %in% data3()[input$table1_cells_selected[,1],4][[1]] &
                      sch_tot$MAAND_VOORVAL %in% maand3 &
                      sch_tot[,cat3][[1]]>0),]

    }
    else if (input$jaar =="Alle" & input$beheerder == "Alle"){ ## Alle Alle
      cat4<- cats_jaar[colnames(data3())[input$table1_cells_selected[,2]]][[1]]

      sch_tot[which(sch_tot$JAAR_VOORVAL %in% data3()[input$table1_cells_selected[,1],1] &

                           sch_tot[,cat4][[1]]>0),]
      
    }
  })

   ##Plot analysetab
 
  
   output$plot1 <- renderPlot({
    dat<-as.data.frame(data2()[which(data2()$NAUTISCH_BEHEERDER %in% beheerders2),])
    ggplot(dat,aes(x=as.factor(JAAR_VOORVAL)))+
      geom_histogram(stat="count")+xlab("Jaar")
  })
    output$plot2 <- renderPlot({
      dat<-as.data.frame(data2()[which(data2()$NAUTISCH_BEHEERDER %in% beheerders),])
      ggplot(dat,aes(x=as.factor(JAAR_VOORVAL),fill=NAUTISCH_BEHEERDER))+
        geom_histogram(stat="count")+facet_wrap(~NAUTISCH_BEHEERDER)+theme(legend.position = "none")+xlab("Jaar")+
       scale_x_discrete(breaks=seq(2008, 2017, 2))
      
    
  })
  
  
  #Optellen van aantal gewonden per jaar als alle jaren in clickable table wordt gekozen
  data3<- reactive({
    Gewonden_per_jaar_tot2<- Gewonden_per_jaar_tot[which(Gewonden_per_jaar_tot$Typering %in% input$type),]
    Gewonden_per_beh_tot2 <- Gewonden_per_beh_tot[which(Gewonden_per_beh_tot$Typering %in% input$type),]
    Gewonden_per_beh_jaar_tot2 <- Gewonden_per_beh_jaar_tot[which(Gewonden_per_beh_jaar_tot$Typering %in% input$type),]
    
    if (input$jaar =="Alle" & input$beheerder == "Alle"){
      aggregate(cbind(`Licht gewond`,`Zwaar gewond`,`Gewond Overig`,Vermist,Dood) ~ Jaar,data=Gewonden_per_jaar_tot2,FUN="sum")
      }
    else if (input$jaar != "Alle" & input$beheerder== "Alle"){
      Gewonden_per_jaar_tot2[which(Gewonden_per_jaar_tot2$Jaar %in% input$jaar),c(1,2,3,4:8)]
      }
    else if (input$jaar == "Alle" & input$beheerder != "Alle"){
      Gewonden_per_beh_tot2[which(Gewonden_per_beh_tot2$Beheerder %in% input$beheerder),c(1,2,3,4:8)]
     }
    else if (input$jaar != "Alle" & input$beheerder != "Alle"){
      Gewonden_per_beh_jaar_tot2[which(Gewonden_per_beh_jaar_tot2$Beheerder %in% input$beheerder & Gewonden_per_beh_jaar_tot2$Jaar %in% input$jaar),c(1,2,3,4:9)]
      }
  })

  
  ##clickable table 
  output$table1 = DT::renderDataTable(
    datatable(data3(), 
    options = list(pageLength = 24,  
                   bLengthChange=0,                       
                   bFilter=0,
                   scrollX = TRUE,
                   scrollY = 500),
    rownames = FALSE,
    selection = list(mode = 'single', 
                     target = 'cell'))%>%
    
     ## Alle cellen met 0 lichtgrijs kleuren, de rest zwart.  
     formatStyle(
         columns = c(1:9), 
         target = "cell",
         color = styleInterval(0, c('lightgrey', 'black'))
     )
  )
    
 
  output$plot0 <- renderPlotly ({
    
    ## Alleen waardes boven nul laten zien in grafiek, en NA waardes er uithalen. 
    ## Moet voor allemaal apart, anders error indirect number of dimensions
    zw_gewond <- data2()[((data2()$AANTAL_GEWONDEN_ZWAAR)>0) & (!is.na(data2()$AANTAL_GEWONDEN_ZWAAR)),] 
    li_gewond <- data2()[((data2()$AANTAL_GEWONDEN_LICHT)>0) & (!is.na(data2()$AANTAL_GEWONDEN_LICHT)),] 
    vermist <- data2()[((data2()$AANTAL_VERMISTEN)>0) & (!is.na(data2()$AANTAL_VERMISTEN)),]
    doden <- data2()[((data2()$AANTAL_DODEN)>0) & (!is.na(data2()$AANTAL_DODEN)),]
    ov_gewond <- data2()[((data2()$AANTAL_GEWONDEN_OVERIG)>0) & (!is.na(data2()$AANTAL_GEWONDEN_OVERIG)),]
    
    ##If else nodig, want bij niet significante checkbox is er geen data in doden, zwaar gewonden en vermisten, dan error in plotly.
    if (sum(doden$AANTAL_DODEN)>0){
      p <- plot_ly() %>%
        add_markers(x = zw_gewond$DATUM_FORMAT, y = zw_gewond$AANTAL_GEWONDEN_ZWAAR , key = zw_gewond$VOORVALNUMMER, name = "Zwaar gewond") %>%
        add_markers(x = li_gewond$DATUM_FORMAT, y = li_gewond$AANTAL_GEWONDEN_LICHT , key = li_gewond$VOORVALNUMMER, name = "Licht gewond") %>%
        add_markers(x = vermist$DATUM_FORMAT, y = vermist$AANTAL_VERMISTEN, key = vermist$VOORVALNUMMER,name = "Vermist") %>%
        add_markers(x = doden$DATUM_FORMAT, y = doden$AANTAL_DODEN, key = doden$VOORVALNUMMER,name = "Doden") %>%
        add_markers(x = ov_gewond$DATUM_FORMAT, y = ov_gewond$AANTAL_GEWONDEN_OVERIG, key = ov_gewond$VOORVALNUMMER, name = "Overig gewond") %>%
        layout(
          title = "",
          hovermode = "closest",
          dragmode = "lasso",
          xaxis = list(
            
            ## define range bij het openen van de dataset, in miliseconden
            range = c(as.numeric(max(data2()$DATUM_FORMAT) - 360)*86400000,
                      as.numeric(max(data2()$DATUM_FORMAT))*86400000),
            
            rangeselector = list(
              buttons = list(
                list(
                  count = 10,
                  label = "10 jr",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 5,
                  label = "5 jr",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 3,
                  label = "3 jr",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 jr",
                  step = "year",
                  stepmode = "backward"),
                # list(
                #   count = 1,
                #   label = "YTD",
                #   step = "year",
                #   stepmode = "todate"),
                list(step = "all"))),
            
            rangeslider = list(type = "date")),
          
          yaxis = list(title = "Aantal"))
    }
    else{
      p <- plot_ly() %>%
        add_markers(x = li_gewond$DATUM_FORMAT, y = li_gewond$AANTAL_GEWONDEN_LICHT, key = li_gewond$VOORVALNUMMER, name = "Licht gewond") %>%
        add_markers(x = ov_gewond$DATUM_FORMAT, y = ov_gewond$AANTAL_GEWONDEN_OVERIG, key = ov_gewond$VOORVALNUMMER,  name = "Overig gewond") %>%
        layout(
          title = "",
          hovermode = "closest",
          dragmode = "lasso",
          xaxis = list(
            
            ## define range bij het openen van de dataset, in miliseconden
            range = c(as.numeric(max(data2()$DATUM_FORMAT) - 360)*86400000,
                      as.numeric(max(data2()$DATUM_FORMAT))*86400000),
            
            rangeselector = list(
              buttons = list(
                list(
                  count = 10,
                  label = "10 jr",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 5,
                  label = "5 jr",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 3,
                  label = "3 jr",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 jr",
                  step = "year",
                  stepmode = "backward"),
                # list(
                #   count = 1,
                #   label = "YTD",
                #   step = "year",
                #   stepmode = "todate"),
                list(step = "all"))),
            
            rangeslider = list(type = "date")),
          
          yaxis = list(title = "Aantal"))
    }
  })
  
  
  ## Create een dataset van de geselecteerde punten in Plotly, worden later in Leaflet opgeroepen. 
  selection <- reactive({
    selected_plotly <- event_data("plotly_selected")
    if (is.null(selected_plotly)) { #Als niks geselcteerd, doe niks
      return(NULL)
    } else {
      selected_data <- selected_plotly[['key']]  #Key is in dit geval Voorvalnummer
      if (length(selected_data) == 0) {
        selected_data <- 'abcdefg'    # a hack but it's working - set to something that can't be selected
      }
      
      if(!(selected_data %in% data2()$VOORVALNUMMER)) {
        return(NULL) # if there is not a match, do nothing as well
      } else {
        selection <- data2()[data2()$VOORVALNUMMER %in% selected_data,]
        return(selection)
      }
    }
  })
  
  ## Create een dataset van de punt bij hover in Plotly, worden later in Leaflet opgeroepen. 
  hover <- reactive({
    hover_plotly <- event_data("plotly_hover")
    if (is.null(hover_plotly)){return(NULL)}
    else{
      hover_data <- hover_plotly[['key']]
      if (length(hover_data) == 0) {
        hover_data <- 'abcdefg'
      }
      if(!(hover_data %in% data2()$VOORVALNUMMER)){
        return(NULL)
      } else {
        hover<- data2()[data2()$VOORVALNUMMER %in% hover_data,]
        return(hover)
      }
    }
  })
  
  ## Create een dataset van de punt bij click in Plotly, worden later in Leaflet opgeroepen.
  click <- reactive({
    click_plotly <- event_data("plotly_click")
    if (is.null(click_plotly)){return(NULL)}
    else{
      click_data <- click_plotly[['key']]
      if (length(click_data) == 0) {
        click_data <- 'abcdefg'
      }
      if(!(click_data %in% data2()$VOORVALNUMMER)){
        return(NULL)
      } else {
        click<- data2()[data2()$VOORVALNUMMER %in% click_data,]
        return(click)
      }
    }
  })
  
  
  output$plot3 <- renderPlot({
    if(input$beheerder== "Alle"){
    ## tel het aantal dubbel voorkomende datums op en maak nieuwe kolom, aantal ongevallen per dag
    df<- as.data.frame(table(data2()$DATUM_FORMAT)) 
    colnames(df)<- c("date", "Aantal_ongevallen")
    df$date <- as.Date(df$date)
    
    # # Create Month Week
    df$yearmonth <- as.yearmon(df$date)
    df$yearmonthf <- factor(df$yearmonth)
    
    df$week <- as.numeric(format(df$date, format="%U"))
    
    df$year <- format(df$date, format="%Y")
    
    df$weekday <- weekdays(as.Date(df$date,'%d-%m-%Y'))
    df$weekday <- strtrim(df$weekday, 3) ## trim naar eerste drie letters
    
    df$month <- months(as.Date(df$date, '%d-%m-%Y'))
    df$month <- strtrim(df$month, 3) ## trim naar eerste drie letters
    
    df$monthweek <- df$week
    df <- ddply(df,.(yearmonthf), transform, monthweek=1+week-min(week))  # compute week number of month
    df <- df[which(df$year %in% input$jaar_box), ]
    }
    
    else if(input$beheerder != "Alle"){
      df <- data2()[which(data2()$NAUTISCH_BEHEERDER %in% input$beheerder),]
      df<- as.data.frame(table(df$DATUM_FORMAT)) 
      colnames(df)<- c("date", "Aantal_ongevallen")
      df$date <- as.Date(df$date)
      
      # # Create Month Week
      df$yearmonth <- as.yearmon(df$date)
      df$yearmonthf <- factor(df$yearmonth)
      
      df$week <- as.numeric(format(df$date, format="%U"))
      
      df$year <- format(df$date, format="%Y")
      
      df$weekday <- weekdays(as.Date(df$date,'%d-%m-%Y'))
      df$weekday <- strtrim(df$weekday, 3) ## trim naar eerste drie letters
      
      df$month <- months(as.Date(df$date, '%d-%m-%Y'))
      df$month <- strtrim(df$month, 3) ## trim naar eerste drie letters
      
      df$monthweek <- df$week
      df <- ddply(df,.(yearmonthf), transform, monthweek=1+week-min(week))  # compute week number of month
      df <- df[which(df$year %in% input$jaar_box), ]
    }
    
  
    # # Plot
     ggplot(df, aes(monthweek, factor(weekday, c("zon", "zat", "vri", "don", "woe", "din", "maa")),
                                      fill = Aantal_ongevallen)) +
       geom_tile( color = "white") +
       facet_grid(year~factor(month, c("jan", "feb", "maa", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))) +
       scale_fill_gradient(low="yellow", high="red") +
       labs(x="Week van de maand",
            y="",
            title = "",
            subtitle="Aantal scheepsongevallen per dag",
            fill="")


}) 
  
   
  ##########Leaflet###############
  ###############################
  
  output$map <- renderLeaflet({

    pal <- colorNumeric(
      palette = "Spectral",
      c(2008:2017)
    )
    
    col <- colorFactor(
      palette = "Set3",
      domain = Veiligh_regios$Veiligheidsregio
    )
    
    dat<- data()[order(data()$JAAR_VOORVAL),]
    leaflet('map') %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Basemap licht") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Basemap donker") %>% 
      
      
      ###basemap en layer opties clickable maken
      addLayersControl(
        baseGroups = c("Basemap licht", "Basemap donker"),
        overlayGroups = c("Scheepsongevallen", "Veiligheid regios"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      

      
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom naar Nederland",
        onClick=JS("function(btn, map){ var center = [52.2,5.8];
                                        map.setView(center, zoom =7);}"))) %>%
      
      

      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)%>%
      
      setView(lng = 5.8, lat = 52.2, zoom = 7) %>%
      clearShapes() %>%

      addPolygons(data = Veiligh_regios,  
                  fill = T, weight = 0.5, color = ~col(Veiligheidsregio), opacity=0.9, fillOpacity = 0.7, group = "Veiligheid regios"
                  , label = ~Veiligheidsregio)%>%

      
      addCircles(data=dat, radius=60,color =  ~pal(JAAR_VOORVAL),#"#777777",
                 fillColor = ~pal(JAAR_VOORVAL), fillOpacity = 0.9,opacity=0.7 , group = "Scheepsongevallen", popup = ~paste("<b>Voorvalnummer:</b>",VOORVALNUMMER, "<br>",
                                                                                   "<b>Datum ongeval</b>:",paste(DATUM_VOORVAL, TIJDSTIP_VOORVAL), "<br>",
                                                                                 "<b>Typering:</b>", TYPERING_VOORVAL_OMSCHRIJVING, "<br>",
                                                                                   "<b>Aantal schepen:</b>",AANTAL_SCHEPEN,"<br>",
                                                                                 "<b>Aantal zwaar gewond:</b>", AANTAL_GEWONDEN_ZWAAR, "<br>",
                                                                                 "<b>Aantal licht gewond:</b>",AANTAL_GEWONDEN_LICHT, "<br>",
                                                                                 "<b>Aantal vermist:</b>",AANTAL_VERMISTEN, "<br>",
                                                                                 "<b>Aantal doden:</b>",AANTAL_DODEN, "<br>",
                                                                                 "<b>Aantal overig gewond:</b>",AANTAL_GEWONDEN_OVERIG, "<br>",
                                                                                   "<b>Aard voorval:</b>",AARD_VOORVAL_OMSCHRIJVING, "<br>",
                                                                                   "<b>Betrokken vaart:</b>", BETROKKEN_VAART, "<br>",
                                                                                   "<b>Vaarweg (nwb):</b>", VAARWEG_NAAM_NWB, "<br>",
                                                                                   "<b>Specifieke locatie: </b>", NADERE_SPECIFICATIE_LOCATIE 
                                                                                  ),
                 highlightOptions = highlightOptions(color = "white", weight = 3,
                                                     bringToFront = TRUE)
                 ) %>%
    
            
      addDrawToolbar(targetLayerId = NULL, targetGroup = NULL,
                     position = c("topleft", "topright", "bottomleft", "bottomright"),
                     polylineOptions = drawPolylineOptions(),
                     polygonOptions = drawPolygonOptions(),
                     circleOptions = drawCircleOptions(),
                     rectangleOptions = FALSE,
                     markerOptions = drawMarkerOptions(), editOptions = editToolbarOptions(),
                     singleFeature = FALSE)%>%
    
      hideGroup("Veiligheid regios") %>%

   
      leaflet::addLegend("bottomright", pal = pal, values = c(2008:2017),
                title = "Jaar voorval",
                labFormat = labelFormat(big.mark=""),
                opacity = 1

      )
    
  })

  observe({
    
    req(selection()) # Do this if selection() is not null
    
    proxy <- leafletProxy('map')
    
    # Clear old selection on map, and add new selection
    proxy %>%
      clearGroup(group = 'selection') %>%
      addCircles(data = selection(), color = '#FFFFFF', fillColor = '#FFFFFF', fillOpacity = 1, radius = 1000,
                  opacity = 1, popup = ~paste("<b>Voorvalnummer:</b>",VOORVALNUMMER, "<br>",
                                             "<b>Datum ongeval</b>:",paste(DATUM_VOORVAL, TIJDSTIP_VOORVAL), "<br>",
                                             "<b>Typering:</b>", TYPERING_VOORVAL_OMSCHRIJVING, "<br>",
                                             "<b>Aantal schepen:</b>",AANTAL_SCHEPEN,"<br>",
                                             "<b>Aantal zwaar gewond:</b>", AANTAL_GEWONDEN_ZWAAR, "<br>",
                                             "<b>Aantal licht gewond:</b>",AANTAL_GEWONDEN_LICHT, "<br>",
                                             "<b>Aantal vermist:</b>",AANTAL_VERMISTEN, "<br>",
                                             "<b>Aantal doden:</b>",AANTAL_DODEN, "<br>",
                                             "<b>Aantal overig gewond:</b>",AANTAL_GEWONDEN_OVERIG, "<br>",
                                             "<b>Aard voorval:</b>",AARD_VOORVAL_OMSCHRIJVING, "<br>",
                                             "<b>Betrokken vaart:</b>", BETROKKEN_VAART, "<br>",
                                             "<b>Vaarweg (nwb):</b>", VAARWEG_NAAM_NWB, "<br>",
                                             "<b>Specifieke locatie: </b>", NADERE_SPECIFICATIE_LOCATIE 
                  ), group = 'selection')
  })
  
  ## Als selectie in Plolty wordt verwijderd, ook weer in Leaflet verwijderen
  observe({
    req(is.null(selection()))
    proxy <- leafletProxy('map')
    proxy %>%
      clearGroup(group = 'selection')
  })
  
  observe({
    req(hover()) # Do this if selection() is not null
    proxy <- leafletProxy('map')
    
    # Clear old selection on map, and add new selection
    proxy %>%
      clearGroup(group = 'hover') %>%
      addCircles(data = hover(), color = '#FFFFFF', fillColor = '#FFFFFF', fillOpacity = 1, radius = 1000,
                 opacity = 1, group = 'hover') 
  }) 
  
  ## Als hover in Plolty wordt verwijderd, ook weer in Leaflet verwijderen
  observe({
    req(is.null(hover()))
    proxy <- leafletProxy('map')
    proxy %>%
      clearGroup(group = 'hover')
  })
  
  observe({
    
    req(click()) # Do this if selection() is not null
    
    proxy <- leafletProxy('map')
    
    # Clear old selection on map, and add new selection
    proxy %>%
      clearGroup(group = 'click') %>%
      addCircles(data = click(), color = '#FFFFFF', fillColor = '#FFFFFF', fillOpacity = 1, radius = 1000,
                 opacity = 1, popup = ~paste("<b>Voorvalnummer:</b>",VOORVALNUMMER, "<br>",
                                             "<b>Datum ongeval</b>:",paste(DATUM_VOORVAL, TIJDSTIP_VOORVAL), "<br>",
                                             "<b>Typering:</b>", TYPERING_VOORVAL_OMSCHRIJVING, "<br>",
                                             "<b>Aantal schepen:</b>",AANTAL_SCHEPEN,"<br>",
                                             "<b>Aantal zwaar gewond:</b>", AANTAL_GEWONDEN_ZWAAR, "<br>",
                                             "<b>Aantal licht gewond:</b>",AANTAL_GEWONDEN_LICHT, "<br>",
                                             "<b>Aantal vermist:</b>",AANTAL_VERMISTEN, "<br>",
                                             "<b>Aantal doden:</b>",AANTAL_DODEN, "<br>",
                                             "<b>Aantal overig gewond:</b>",AANTAL_GEWONDEN_OVERIG, "<br>",
                                             "<b>Aard voorval:</b>",AARD_VOORVAL_OMSCHRIJVING, "<br>",
                                             "<b>Betrokken vaart:</b>", BETROKKEN_VAART, "<br>",
                                             "<b>Vaarweg (nwb):</b>", VAARWEG_NAAM_NWB, "<br>",
                                             "<b>Specifieke locatie: </b>", NADERE_SPECIFICATIE_LOCATIE 
                 ), group = 'click')
  })
  
  ## Als selectie in Plolty wordt verwijderd, ook weer in Leaflet verwijderen
  observe({
    req(is.null(click()))
    proxy <- leafletProxy('map')
    proxy %>%
      clearGroup(group = 'click')
  })    
  
  
  ## Als selectie in tabel wordt geklikt: alleen deze punten op de kaart laten zien
  observe({
    req(nrow(input$table1_cells_selected)!=0)
    proxy <- leafletProxy('map')
    
    pal <- colorNumeric(
      palette = "Spectral",
      c(2008:2017)
    )
    
    proxy %>%
      clearGroup(group = 'Scheepsongevallen') %>%
      clearGroup(group = 'table_click') %>%
      addCircles(data = data_click(), color = ~pal(JAAR_VOORVAL), fillColor = ~pal(JAAR_VOORVAL), fillOpacity = 0.9, radius = 60,
               opacity = 0.7, popup = ~paste("<b>Voorvalnummer:</b>",VOORVALNUMMER, "<br>",
                                           "<b>Datum ongeval</b>:",paste(DATUM_VOORVAL, TIJDSTIP_VOORVAL), "<br>",
                                           "<b>Typering:</b>", TYPERING_VOORVAL_OMSCHRIJVING, "<br>",
                                           "<b>Aantal schepen:</b>",AANTAL_SCHEPEN,"<br>",
                                           "<b>Aantal zwaar gewond:</b>", AANTAL_GEWONDEN_ZWAAR, "<br>",
                                           "<b>Aantal licht gewond:</b>",AANTAL_GEWONDEN_LICHT, "<br>",
                                           "<b>Aantal vermist:</b>",AANTAL_VERMISTEN, "<br>",
                                           "<b>Aantal doden:</b>",AANTAL_DODEN, "<br>",
                                           "<b>Aantal overig gewond:</b>",AANTAL_GEWONDEN_OVERIG, "<br>",
                                           "<b>Aard voorval:</b>",AARD_VOORVAL_OMSCHRIJVING, "<br>",
                                           "<b>Betrokken vaart:</b>", BETROKKEN_VAART, "<br>",
                                           "<b>Vaarweg (nwb):</b>", VAARWEG_NAAM_NWB, "<br>",
                                           "<b>Specifieke locatie: </b>", NADERE_SPECIFICATIE_LOCATIE 
               ), group = 'table_click')
  })    
  
  ## Als selectie in de tabel wordt uitgeklikt weer de originele punten laten zien. 
  observe({
    req(nrow(input$table1_cells_selected)==0)
    proxy <- leafletProxy('map')
    
    pal <- colorNumeric(
      palette = "Spectral",
      c(2008:2017)
    )
    
    dat<- data()[order(data()$JAAR_VOORVAL),]
    
    proxy %>%
      clearGroup(group = 'table_click') %>%
      addCircles(data=dat, radius=60,color =  ~pal(JAAR_VOORVAL),#"#777777",
                 fillColor = ~pal(JAAR_VOORVAL), fillOpacity = 0.9,opacity=0.7 , group = "Scheepsongevallen", popup = ~paste("<b>Voorvalnummer:</b>",VOORVALNUMMER, "<br>",
                                                                                                                             "<b>Datum ongeval</b>:",paste(DATUM_VOORVAL, TIJDSTIP_VOORVAL), "<br>",
                                                                                                                             "<b>Typering:</b>", TYPERING_VOORVAL_OMSCHRIJVING, "<br>",
                                                                                                                             "<b>Aantal schepen:</b>",AANTAL_SCHEPEN,"<br>",
                                                                                                                             "<b>Aantal zwaar gewond:</b>", AANTAL_GEWONDEN_ZWAAR, "<br>",
                                                                                                                             "<b>Aantal licht gewond:</b>",AANTAL_GEWONDEN_LICHT, "<br>",
                                                                                                                             "<b>Aantal vermist:</b>",AANTAL_VERMISTEN, "<br>",
                                                                                                                             "<b>Aantal doden:</b>",AANTAL_DODEN, "<br>",
                                                                                                                             "<b>Aantal overig gewond:</b>",AANTAL_GEWONDEN_OVERIG, "<br>",
                                                                                                                             "<b>Aard voorval:</b>",AARD_VOORVAL_OMSCHRIJVING, "<br>",
                                                                                                                             "<b>Betrokken vaart:</b>", BETROKKEN_VAART, "<br>",
                                                                                                                             "<b>Vaarweg (nwb):</b>", VAARWEG_NAAM_NWB, "<br>",
                                                                                                                             "<b>Specifieke locatie: </b>", NADERE_SPECIFICATIE_LOCATIE 
                 ),
                 highlightOptions = highlightOptions(color = "white", weight = 3,
                                                     bringToFront = TRUE)
      )
    })    

  
  

  #################################
  #################################
#   ######Shiny 2 tab###### (ESRI)
  


  
    output$map2 <- renderLeaflet({
    
    leaflet('map2') %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Basemap licht")%>%
      addEsriBasemapLayer("DarkGray", autoLabels = FALSE, group = "Basemap donker")%>%
      

      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom naar Nederland",
        onClick=JS("function(btn, map){ var center = [52.2,5.8];
                   map.setView(center, zoom =7);}"))) %>%
        
      addFullscreenControl(position = "topleft", pseudoFullscreen = TRUE)%>%

      
      setView(lng = 7.8, lat = 52.2, zoom = 6) %>%
      clearShapes() %>%
      
      hideGroup(c("Gemeenten WMS", "Grote havens", "Passages")) %>%
        
      addWMSTiles(
        "https://geodata.nationaalgeoregister.nl/bestuurlijkegrenzen/wms",
        layers = "gemeenten",
        options = WMSTileOptions(format = "image/png", transparent = TRUE),
        attribution = "PDOK",
        group = "Gemeenten WMS"
      )%>%
      

      ## WFS laag kan ook, zelfde manier als WMS  
      # addWMSTiles(
      #     "https://geodata.nationaalgeoregister.nl/bag/wfs",
      #     layers = "bag:pand",
      #     options = WMSTileOptions(format = "image/png", transparent = TRUE),
      #     attribution = "PDOK",
      #     group = "BAG panden WFS (zoom ver in)"
      #   )%>%
        
     
      addEsriFeatureLayer("http://services1.arcgis.com/P1Z2KnOs8ZgdyKMH/arcgis/rest/services/Regiogebieden/FeatureServer/0", 
                          useServiceSymbology = TRUE, stroke = TRUE, fill = TRUE, group = "RWS Regiogebieden",
                          labelProperty='DISTRICTNA')%>%  
      
        
      addEsriFeatureLayer("http://services1.arcgis.com/P1Z2KnOs8ZgdyKMH/arcgis/rest/services/Scheepsongevallen/FeatureServer/0",
                          useServiceSymbology = TRUE, stroke = TRUE, fill = TRUE, group ="Significante scheepsongevallen",
                          popupProperty = propstoHTMLTable(
                            props = c('VOORVALNUMMER', 'DATUM_VOORVAL', 'TYPERING_VOORVAL_OMSCHRIJVING', 'AANTAL_SCHEPEN', 'AARD_VOORVAL_OMSCHRIJVING', 'BETROKKEN_VAART',
                                      'AANTAL_GEWONDEN_LICHT', 'AANTAL_GEWONDEN_ZWAAR', 'AANTAL_GEWONDEN_OVERIG', 'AANTAL_DODEN', 'VAARWEG_NAAM_NWB', 'NADERE_SPECIFICATIE_LOCATIE'),
                            table.attrs = list(class='table table-striped table-bordered'),drop.na = T))%>%
                                
                          
      addEsriFeatureLayer("http://services1.arcgis.com/P1Z2KnOs8ZgdyKMH/arcgis/rest/services/Scheepsongevallen/FeatureServer/1", 
                          useServiceSymbology = TRUE, stroke = TRUE, fill = TRUE, group ="Niet significante scheepsongevallen",
                          popupProperty = propstoHTMLTable(
                          props = c('VOORVALNUMMER', 'DATUM_VOORVAL', 'TYPERING_VOORVAL_OMSCHRIJVING', 'AANTAL_SCHEPEN', 'AARD_VOORVAL_OMSCHRIJVING', 'BETROKKEN_VAART', 
                                    'AANTAL_GEWONDEN_LICHT', 'AANTAL_GEWONDEN_ZWAAR', 'AANTAL_GEWONDEN_OVERIG', 'AANTAL_DODEN', 'VAARWEG_NAAM_NWB', 'NADERE_SPECIFICATIE_LOCATIE'),
                          table.attrs = list(class='table table-striped table-bordered'),drop.na = T),
                          color='#95959', weight=1, fillOpacity = 0.7)%>%
      
      
      addEsriFeatureLayer("https://services.arcgis.com/JmSyAMvJJkgxqun6/arcgis/rest/services/NL_Scheepvaart/FeatureServer/3",group = "Passages", useServiceSymbology = TRUE, 
                          stroke = TRUE, fill = TRUE, popupProperty = propstoHTMLTable(
                            props = c('VRT_NAAM', 'Passages'),
                            table.attrs = list(class='table table-striped table-bordered'),drop.na = T))%>% 
                          
      
      addEsriFeatureLayer("https://services.arcgis.com/JmSyAMvJJkgxqun6/arcgis/rest/services/NL_Scheepvaart/FeatureServer/0", 
                          group = "Grote havens", useServiceSymbology = TRUE, stroke = TRUE, fill = TRUE,  popupProperty = propstoHTMLTable(
                            props = c('PORT_NAME'),
                            table.attrs = list(class='table table-striped table-bordered'),drop.na = T))%>% 
      
        
      ##addEsriFeatureLayer("https://services.arcgis.com/JmSyAMvJJkgxqun6/arcgis/rest/services/NL_Scheepvaart/FeatureServer/1", useServiceSymbology = TRUE, stroke = TRUE, fill = TRUE, group = "Overige havens")%>%
      
      
      ###basemap en layer opties clickable maken
      addLayersControl(
        baseGroups = c("Basemap licht", "Basemap donker"), 
        overlayGroups = c("RWS Regiogebieden", "Gemeenten WMS", "Significante scheepsongevallen", "Niet significante scheepsongevallen", "Grote havens"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
        
      
      addDrawToolbar(targetLayerId = NULL, targetGroup = NULL,
                      position = c("topleft", "topright", "bottomleft", "bottomright"),
                      polylineOptions = drawPolylineOptions(),
                      polygonOptions = drawPolygonOptions(),
                      circleOptions = drawCircleOptions(),
                      rectangleOptions = FALSE,
                      markerOptions = drawMarkerOptions(), editOptions = editToolbarOptions(),
                      singleFeature = FALSE)
  
      
     })

    
    ## Plot analyse tab
  
  #create reactive dataset to input checkboxes significant niet significant
  data2_esri<- reactive({
    sch_tot[which(sch_tot$TYPERING_VOORVAL_OMSCHRIJVING %in% input$type_esri),]
  })
  
  #create reactive dataset based on dropdown menus beheerder en jaar
  data_esri<- reactive({
    sch_tot2_esri<- sch_tot[which(sch_tot$TYPERING_VOORVAL_OMSCHRIJVING %in% input$type_esri),]
    
    if(input$beheerder_esri == "Alle" & input$jaar_esri == "Alle" ){
      sch_tot2_esri[which(sch_tot2_esri$NAUTISCH_BEHEERDER %in% beheerders2),]
    }
    else if (input$beheerder_esri != "Alle" & input$jaar_esri == "Alle"){
      sch_tot2_esri[which(sch_tot2_esri$NAUTISCH_BEHEERDER %in% input$beheerder_esri),]
    }
    else if (input$beheerder_esri == "Alle" & input$jaar_esri != "Alle"){
      sch_tot2_esri[which(sch_tot2_esri$NAUTISCH_BEHEERDER %in% beheerders2 & 
                       sch_tot2_esri$JAAR_VOORVAL %in% input$jaar_esri),]
    }
    else {
      sch_tot2_esri[which(sch_tot2_esri$NAUTISCH_BEHEERDER %in% input$beheerder_esri & 
                       sch_tot2_esri$JAAR_VOORVAL %in% input$jaar_esri),]
    }
    
  })
  
  
  
      output$plot1_esri <- renderPlot({
        dat<-as.data.frame(data2_esri()[which(data2_esri()$NAUTISCH_BEHEERDER %in% beheerders2),])
        ggplot(dat,aes(x=as.factor(JAAR_VOORVAL)))+
          geom_histogram(stat="count")+xlab("Jaar")
      })
      output$plot2_esri <- renderPlot({
        dat<-as.data.frame(data2_esri()[which(data2_esri()$NAUTISCH_BEHEERDER %in% beheerders),])
        ggplot(dat,aes(x=as.factor(JAAR_VOORVAL),fill=NAUTISCH_BEHEERDER))+
          geom_histogram(stat="count")+facet_wrap(~NAUTISCH_BEHEERDER)+theme(legend.position = "none")+xlab("Jaar")
       
      })
      
      
      #Optellen van aantal gewonden per jaar als alle jaren in clickable table wordt gekozen
      data3_esri<- reactive({
        Gewonden_per_jaar_tot2_esri<- Gewonden_per_jaar_tot[which(Gewonden_per_jaar_tot$Typering %in% input$type_esri),]
        Gewonden_per_beh_tot2_esri <- Gewonden_per_beh_tot[which(Gewonden_per_beh_tot$Typering %in% input$type_esri),]
        Gewonden_per_beh_jaar_tot2_esri <- Gewonden_per_beh_jaar_tot[which(Gewonden_per_beh_jaar_tot$Typering %in% input$type_esri),]
        
        if (input$jaar_esri =="Alle" & input$beheerder_esri == "Alle"){
          aggregate(cbind(`Licht gewond`,`Zwaar gewond`,`Gewond Overig`,Vermist,Dood) ~ Jaar,data=Gewonden_per_jaar_tot2_esri,FUN="sum")
        }
        else if (input$jaar_esri != "Alle" & input$beheerder_esri== "Alle"){
          Gewonden_per_jaar_tot2_esri[which(Gewonden_per_jaar_tot2_esri$Jaar %in% input$jaar_esri),c(1,2,3,4:8)]
        }
        else if (input$jaar_esri == "Alle" & input$beheerder_esri != "Alle"){
          Gewonden_per_beh_tot2_esri[which(Gewonden_per_beh_tot2_esri$Beheerder %in% input$beheerder_esri),c(1,2,3,4:8)]
        }
        else if (input$jaar_esri != "Alle" & input$beheerder_esri != "Alle"){
          Gewonden_per_beh_jaar_tot2_esri[which(Gewonden_per_beh_jaar_tot2_esri$Beheerder %in% input$beheerder_esri & Gewonden_per_beh_jaar_tot2_esri$Jaar %in% input$jaar_esri),c(1,2,3,4:9)]
        }
      })
      
      
      output$table1_esri = DT::renderDataTable(
        data3_esri(), rownames = FALSE,
        selection = list(mode = 'single', 
                         target = 'cell'),
        options = list(pageLength = 24,  
                       bLengthChange=0,                       
                       bFilter=0,
                       scrollX = TRUE)
        
      )
      
      output$plot0_esri <- renderPlotly ({
        
        ## Alleen waardes boven nul laten zien in grafiek, en NA waardes er uithalen. 
        ## Moet voor allemaal apart, anders error indirect number of dimensions
        
        zw_gewond_esri <- data2_esri()[((data2_esri()$AANTAL_GEWONDEN_ZWAAR)>0) & (!is.na(data2_esri()$AANTAL_GEWONDEN_ZWAAR)),] 
        li_gewond_esri <- data2_esri()[((data2_esri()$AANTAL_GEWONDEN_LICHT)>0) & (!is.na(data2_esri()$AANTAL_GEWONDEN_LICHT)),] 
        vermist_esri <- data2_esri()[((data2_esri()$AANTAL_VERMISTEN)>0) & (!is.na(data2_esri()$AANTAL_VERMISTEN)),]
        doden_esri <- data2_esri()[((data2_esri()$AANTAL_DODEN)>0) & (!is.na(data2_esri()$AANTAL_DODEN)),]
        ov_gewond_esri <- data2_esri()[((data2_esri()$AANTAL_GEWONDEN_OVERIG)>0) & (!is.na(data2_esri()$AANTAL_GEWONDEN_OVERIG)),]
        
        ##If else nodig, want bij niet significante checkbox is er geen data in doden, zwaar gewonden en vermisten, dan error in plotly.
        
        if (sum(doden_esri$AANTAL_DODEN)>0){
          p <- plot_ly() %>%
            add_markers(x = zw_gewond_esri$DATUM_FORMAT, y = zw_gewond_esri$AANTAL_GEWONDEN_ZWAAR , key = zw_gewond_esri$VOORVALNUMMER, name = "Zwaar gewond") %>%
            add_markers(x = li_gewond_esri$DATUM_FORMAT, y = li_gewond_esri$AANTAL_GEWONDEN_LICHT , key = li_gewond_esri$VOORVALNUMMER, name = "Licht gewond") %>%
            add_markers(x = vermist_esri$DATUM_FORMAT, y = vermist_esri$AANTAL_VERMISTEN, key = vermist_esri$VOORVALNUMMER,name = "Vermist") %>%
            add_markers(x = doden_esri$DATUM_FORMAT, y = doden_esri$AANTAL_DODEN, key = doden_esri$VOORVALNUMMER,name = "Doden") %>%
            add_markers(x = ov_gewond_esri$DATUM_FORMAT, y = ov_gewond_esri$AANTAL_GEWONDEN_OVERIG, key = ov_gewond_esri$VOORVALNUMMER, name = "Overig gewond") %>%
            layout(
              title = "",
              hovermode = "closest",
              dragmode = "lasso",
              xaxis = list(
                
                ## define range bij het openen van de dataset, in miliseconden
                range = c(as.numeric(max(data2_esri()$DATUM_FORMAT) - 360)*86400000,
                          as.numeric(max(data2_esri()$DATUM_FORMAT))*86400000),
                
                rangeselector = list(
                  buttons = list(
                    list(
                      count = 10,
                      label = "10 jr",
                      step = "year",
                      stepmode = "backward"),
                    list(
                      count = 5,
                      label = "5 jr",
                      step = "year",
                      stepmode = "backward"),
                    list(
                      count = 3,
                      label = "3 jr",
                      step = "year",
                      stepmode = "backward"),
                    list(
                      count = 1,
                      label = "1 jr",
                      step = "year",
                      stepmode = "backward"),
                    # list(
                    #   count = 1,
                    #   label = "YTD",
                    #   step = "year",
                    #   stepmode = "todate"),
                    list(step = "all"))),
                
                rangeslider = list(type = "date")),
              
              yaxis = list(title = "Aantal"))
        }
        else{
          p <- plot_ly() %>%
            add_markers(x = li_gewond_esri$DATUM_FORMAT, y = li_gewond_esri$AANTAL_GEWONDEN_LICHT, key = li_gewond_esri$VOORVALNUMMER, name = "Licht gewond") %>%
            add_markers(x = ov_gewond_esri$DATUM_FORMAT, y = ov_gewond_esri$AANTAL_GEWONDEN_OVERIG, key = ov_gewond_esri$VOORVALNUMMER,  name = "Overig gewond") %>%
            layout(
              title = "",
              hovermode = "closest",
              dragmode = "lasso",
              xaxis = list(
                
                ## define range bij het openen van de dataset, in miliseconden
                range = c(as.numeric(max(data2_esri()$DATUM_FORMAT) - 360)*86400000,
                          as.numeric(max(data2_esri()$DATUM_FORMAT))*86400000),
                
                rangeselector = list(
                  buttons = list(
                    list(
                      count = 10,
                      label = "10 jr",
                      step = "year",
                      stepmode = "backward"),
                    list(
                      count = 5,
                      label = "5 jr",
                      step = "year",
                      stepmode = "backward"),
                    list(
                      count = 3,
                      label = "3 jr",
                      step = "year",
                      stepmode = "backward"),
                    list(
                      count = 1,
                      label = "1 jr",
                      step = "year",
                      stepmode = "backward"),
                    # list(
                    #   count = 1,
                    #   label = "YTD",
                    #   step = "year",
                    #   stepmode = "todate"),
                    list(step = "all"))),
                
                rangeslider = list(type = "date")),
              
              yaxis = list(title = "Aantal"))
        }
      })
      
      output$plot3_esri <- renderPlot({
        ## tel het aantal dubbel voorkomende datums op en maak nieuwe kolom, aantal ongevallen per dag
        df<- as.data.frame(table(data2_esri()$DATUM_FORMAT)) 
        colnames(df)<- c("date", "Aantal_ongevallen")
        df$date <- as.Date(df$date)
        
        # # Create Month Week
        df$yearmonth <- as.yearmon(df$date)
        df$yearmonthf <- factor(df$yearmonth)
        
        df$week <- as.numeric(format(df$date, format="%U"))
        
        df$year <- format(df$date, format="%Y")
        
        df$weekday <- weekdays(as.Date(df$date,'%d-%m-%Y'))
        df$weekday <- strtrim(df$weekday, 3) ## trim naar eerste drie letters
        
        df$month <- months(as.Date(df$date, '%d-%m-%Y'))
        df$month <- strtrim(df$month, 3) ## trim naar eerste drie letters
        
        df$monthweek <- df$week
        df <- ddply(df,.(yearmonthf), transform, monthweek=1+week-min(week))  # compute week number of month
        df <- df[which(df$year %in% input$jaar_box_esri), ]
        
        # # Plot
        ggplot(df, aes(monthweek, factor(weekday, c("zon", "zat", "vri", "don", "woe", "din", "maa")),
                       fill = Aantal_ongevallen)) +
          geom_tile( color = "white") +
          facet_grid(year~factor(month, c("jan", "feb", "maa", "apr", "mei", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))) +
          scale_fill_gradient(low="yellow", high="red") +
          labs(x="Week van de maand",
               y="",
               title = "",
               subtitle="Aantal scheepsongevallen per dag",
               fill="")
        
        
      }) 
      
      
  

}  ##Afsluitende function(input,output) haakje 



