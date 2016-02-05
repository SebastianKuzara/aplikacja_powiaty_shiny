for(package in (c("shiny", "dplyr", "tidyr", "leaflet", "rgdal", "DT", "ggplot2", "plotly"))) {
  if(!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  } else
    library(package, character.only = TRUE)
}


load("./data/dane.Rdata")


shinyServer(function(input, output) {

  #=============ZAKLADKA 1 - DANE===========================
  
  ### Wygrenerowanie tabeli dostosowanej do wyboru użytkownika
  tabela <- reactive({
    
    temp <- dataset_gather %>%
            filter(Rodzaj.wskaznika %in% "bezr" & rok %in% input$years_bezr |   
                   Rodzaj.wskaznika %in% "wyn" & rok %in% input$years_wyn |
                   Rodzaj.wskaznika %in% "firmy" & rok %in% input$years_firmy) 
    
    if(input$woj != "---POLSKA---") {
      temp <- temp %>%
        filter(Wojewodztwo %in% input$woj)
    }
    
    temp <- temp %>%
      select(-rok, -Rodzaj.wskaznika, -Wojewodztwo, -Kod_woj) %>%
      spread(wskaznik, wartosc)
    
    
    datatable(temp, filter = "top")
    
  })
  
  ### Wynik tabela
  output$table <- renderDataTable({
    tabela()
  })
  
  #==============ZAKŁADKA 2 - WYKRESY==============================
  
    ## ---------------Histogram------------------
  
  ### Generowanie histogramu
  plotHist <- reactive({
    
    temp <- dataset_gather %>%
      filter(Rodzaj.wskaznika == input$hist_var,
             rok == input$hist_year) %>%
      select(Powiat, wartosc)
    
    ggplot(data = temp, aes(wartosc)) +
      geom_histogram(fill = "grey40", color = "white", bins = input$hist_bins) +
      ggtitle(paste("Histogram -", switch(input$hist_var,
                                          "bezr" = "Bezrobocie",
                                          "wyn" = "Wynagrodzenia",
                                          "firmy" = "Liczba przedsiębiorstw na 10 tys. mieszkańców"), 
                    "w", input$hist_year, sep = " ")) +
      ylab("Liczba obserwacji") +
      xlab("Wartości") +
      theme_bw()
  })

  ### Wynik histogram
  output$hist <- renderPlot({
    plotHist()
  })
  
  ## ------------Boxplot----------------
  
  ### Dane do boxplota
  dataBoxplot <- reactive({
    
    dataset_gather %>%
      filter(Rodzaj.wskaznika == input$boxplot_var,
             rok %in% input$boxplot_years[1]:input$boxplot_years[2]) %>%
      select(Powiat, rok, wartosc)
    
  })
  
  ### Generowanie boxplota
  plotBoxplot <- reactive({
    
    ggplot(data = dataBoxplot(), aes(y = wartosc, x = rok)) +
      geom_boxplot(fill = "grey40", outlier.shape = ifelse(input$boxplot_outliers, 19, NA)) +
      labs(x = "Rok", y = switch(input$boxplot_var,
                                 "bezr" = "Bezrobocie [%]",
                                 "wyn" = "Wynagordzenia [zł]",
                                 "firmy" = "Liczba firm na 10 tys. mieszk."))
    
  })
    
   ### Wartości odstające boxplot i edycja osi OY
  outliersBoxplot <- reactive({
    
    temp <- dataBoxplot() %>%
      group_by(rok) %>%
      summarise(
        Q1 = quantile(wartosc, probs = 0.25, na.rm = TRUE),
        Q3 = quantile(wartosc, probs = 0.75, na.rm = TRUE),
        roznica = abs(Q1 - Q3),
        dolnyWas = Q1 - 1.5*roznica,
        gornyWas = Q3 + 1.5*roznica,
        najmniej = max(dolnyWas, min(wartosc, na.rm = TRUE)),
        najwiecej = min(gornyWas, max(wartosc, na.rm = TRUE))
      )
    
    if(!input$boxplot_outliers) { 
      scale_y_continuous(limits = c(min(temp$najmniej), max(temp$najwiecej)))
    } else{
      return()
    }
    
  })
  
  ### Wynik boxplot
  output$boxplot <- renderPlot({
    plotBoxplot() +
      outliersBoxplot() +
      theme_bw()
  })
  
  
      ## ------------Wykres rozrzutu--------------
  
  ### Generowanie wykresu punktowego
  scatterPlot <- reactive({
    
    temp <- dataset_gather %>%
      select(Kod, Powiat, wartosc, wskaznik) %>%
      spread(key = wskaznik, value = wartosc)
    
    name1 <- paste(input$scatter_var1, input$scatter_year, sep = "_")
    name2 <- paste(input$scatter_var2, input$scatter_year, sep = "_")

    ggplot(data = temp, aes_string(name1, name2)) + 
      geom_point() +
      xlab(paste(switch(input$scatter_var1,
                        "bezr" = "Bezrobocie [%]",
                        "wyn" = "Wynagordzenia [zł]",
                        "firmy" = "Liczba firm na 10 tys. mieszk."),
                 "w", input$scatter_year, "roku", sep = " ")) +
      ylab(paste(switch(input$scatter_var1,
                        "bezr" = "Bezrobocie [%]",
                        "wyn" = "Wynagordzenia [zł]",
                        "firmy" = "Liczba firm na 10 tys. mieszk."),
                 "w", input$scatter_year, "roku", sep = " "))
    
  })
  
  ### GEnerowanie linii trnedu
  scatterModel <- reactive({
    
    if(!input$model){
      return()
    } else if(input$model_type == "lm") {
      geom_smooth(method = "lm", se = FALSE)
    } else if(input$model_type == "loess") {
      geom_smooth(method = "loess", se = FALSE)
    }
    
  })
  
  ### Wyniki wykresu punktowy
  output$scatter <- renderPlot({
    scatterPlot() +
      scatterModel() +
      theme_bw()
  })
  
 ## --------------Porównanie-------------------
  
  ### Utworzenie obiektu typu reacive z trzema zmiennymi
  v <- reactiveValues(pow1 = FALSE, pow2 = FALSE, go = FALSE)
  
  ### Funkcja reakcji na użycie przycisku "Losuj"
  observeEvent(input$random, {
    v$pow1 <- sample(dataset$Powiat, 1)
    v$pow2 <- sample(dataset$Powiat, 1)
    v$go <- input$random
  })
  
  ### Wygenreowanei części UI
  ### Wybór powiatów lub ich zmiana po wylosowaniu
  output$ui <- renderUI({
    if(v$go == FALSE){
      
      inputPanel(
        selectInput("powiat1", "Wybierz powiat:",
                    choices = sort(dataset$Powiat),
                    selected = "aleksandrowski"),
        selectInput("powiat2", "Wybierz powiat", 
                    choices = sort(dataset$Powiat),
                    selected = "augustowski"))
      
    } else {
      
      inputPanel(
        selectInput("powiat1", "Wybierz powiat:",
                    choices = sort(dataset$Powiat),
                    selected = v$pow1),
        selectInput("powiat2", "Wybierz powiat", 
                    choices = sort(dataset$Powiat),
                    selected = v$pow2))
      
    }
    
  })
  
  ### Wygeneorwnaie wykresu porównania
  plotPor <- reactive({
    
    temp <- dataset_gather %>%
      dplyr::filter(Powiat %in% c(input$powiat1, input$powiat2),
             Rodzaj.wskaznika %in% input$por_var)
    
    gg <- ggplot(data = temp, aes(y = wartosc, x = rok, group = Powiat)) +
      
      geom_line(aes(color = Powiat), lwd = 0.2) +
      geom_point(aes(color = Powiat), size = 2) +
      ylab(switch(input$por_var,
                        "bezr" = "Bezrobocie [%]",
                        "wyn" = "Wynagordzenia [zł]",
                        "firmy" = "Liczba firm na 10 tys. mieszk.")) +
      scale_color_manual(values = c("#e41a1c", "#377eb8")) +
      theme_bw()
    
   
    ggplotly(gg)
    
  })
  
  ### Wyniki porównanie
  output$por <- renderPlotly({
    plotPor()
  })
  
  
 #========================== MAPA =================================
  ### Dane do mapy
  dataMap <- reactive({
    
    name <- paste(input$map_var, input$map_year, sep = "_")
    
    mapaPowiaty@data <- mapaPowiaty@data %>%
      select_("Kod", "Powiat", "Wojewodztwo", "wskaznik" = name)
    
   mapaPowiaty
    
  })
  
  ### Wyskakujące okienka
  mapPopup <- reactive({
    
    paste0(
      "<strong>Powiat ", dataMap()$Powiat, "</strong>",
      "<br>Kod: ", dataMap()$Kod,
      "<br>Województwo ", dataMap()$Wojewodztwo,
      "<br><br><strong>Wartość wskaźnika: ", dataMap()$wskaznik, "</strong>"
    )
    
  })
  
  ### Generowanie mapy
  mapa <- reactive({
    
    pal <- switch(input$map_breaks_type,
                  "colorBin" = colorBin("PuBu", dataMap()$wskaznik, bins = input$map_number_breaks, pretty = FALSE),
                  "colorQuantile" = colorQuantile("YlOrRd", dataMap()$wskaznik, n = input$map_number_breaks))
    
    leaflet(dataMap()) %>%
      addPolygons(color = ~pal(wskaznik) , stroke = FALSE, popup = mapPopup(), fillOpacity = 1) %>%
      addPolylines(weight = 0.5, color= "black", opacity = 1) %>%
      addLegend("bottomright", pal = pal, 
                value = ~wskaznik, opacity = 1, 
                title = "Legenda", layerId = "legenda")
 
  })
  
  ### Wynik mapa
  output$map <- renderLeaflet({
    mapa()
  })

})
