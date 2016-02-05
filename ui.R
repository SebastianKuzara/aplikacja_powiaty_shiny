for(package in (c("shiny", "leaflet", "DT", "plotly"))) {
  if(!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  } else
    library(package, character.only = TRUE)
}


load("./data/kody.woj.Rdata")
kody_woj <- kody_woj[order(kody_woj$Kod),]

load("./data/dane.Rdata")

shinyUI(navbarPage(title = "Powiaty",
                   
#======================ZAKŁADKA 1 - DANE=============================================
                   
                   tabPanel("Dane",
                            
                            sidebarPanel(
                              
                              selectInput("woj", "Wybierz województwo:",
                                          choices = c("---POLSKA---", as.character(kody_woj$Wojewodztwo))),
                              
                              checkboxGroupInput("var_table", "Wybierz jakie dane wyświetlić:",
                                          choices = list("Bezrobocie" = "bezr",
                                                         "Wynagrodzenia" = "wyn",
                                                         "Liczba firm/10 tys. osób" = "firmy"),
                                          selected = c("bezr")),
                              
                              conditionalPanel(
                                condition = "input.var_table.indexOf('bezr') > -1",
                                hr(),
                                h4("Bezrobocie:"),
                                selectInput("years_bezr", "Dla jakich lat wyświetlić dane?",
                                            choices = 2004:2014, selected = 2014, multiple = TRUE)
                              ),
                              
                              conditionalPanel(
                                condition = "input.var_table.indexOf('wyn') > -1",
                                hr(),
                                h4("Wynagrodzenia:"),
                                selectInput("years_wyn", "Dla jakich lat wyświetlić dane?",
                                            choices = 2004:2014, selected = NULL, multiple = TRUE)
                              ),
                              
                              conditionalPanel(
                                condition = "input.var_table.indexOf('firmy') > -1",
                                hr(),
                                h4("Liczba przedsiębiorstw na 10 tys. mieszkańców:"),
                                selectInput("years_firmy", "Dla jakich lat wyświetlić dane?",
                                            choices = 2004:2014, selected = NULL, multiple = TRUE)
                              )
                              
                            ),
                            
                            mainPanel(
                              dataTableOutput("table"))
                            ),

#============================ZAKŁADKA 2 - WYKRESY======================================
                   


                   tabPanel("Wykresy",
                        inputPanel(id = "plot_choice",  
                            #tags$style(type = "text/css", "#plot {align: left;"),
                            radioButtons("plot", "Wybierz typ wykresu:",
                                         choices = list("Histogram" = "hist",
                                                        "Wykresu pudełkowy" = "boxplot",
                                                        "Wykres rozrzutu" = "scatter",
                                                        "Porównanie" = "por"),
                                         width = "400%", inline = TRUE),
                            hr(),
                            align = "left"
                        ),
                            
  ##  --------------Histogram------------------                          
                            
                            conditionalPanel(
                              condition = "input.plot == 'hist'",
                              
                              sidebarPanel(
                              selectInput("hist_var", "Wybierz zmienną do prezentacji na histogramie:",
                                          choices = list("Bezrobocie" = "bezr",
                                                         "Wynagrodzenia" = "wyn",
                                                         "Liczba firm/10 tys. osób" = "firmy"),
                                          selected = c("bezr")),
                             selectInput("hist_year", "Wybierz rok dla zmiennej",
                                           choices = 2004:2014, selected = 2014),
                             sliderInput("hist_bins", "Ile przedziałów zaprezentować na histogramie",
                                         min = 5, max = 50, value = 20)),
                             
                             mainPanel(plotOutput("hist"))
                            ),
  
  ##  ------------Boxplot-----------------------
  
                          conditionalPanel(
                            condition = "input.plot == 'boxplot'",
                            sidebarPanel(
                            selectInput("boxplot_var", "Wybierz zmienną do prezentacji na wykresie pudełkowym",
                                        choices = list("Bezrobocie" = "bezr",
                                                       "Wynagrodzenia" = "wyn",
                                                       "Liczba firm/10 tys. osób" = "firmy"),
                                        selected = c("bezr")),
                            sliderInput("boxplot_years", "Wybierz lata do prezentacji na wykresie pudełkowym",
                                        min = 2004, max = 2014, value = c(2012, 2014), step = 1, sep = ""),
                            br(),
                            checkboxInput("boxplot_outliers", "Czy wyświetlić wartości odstające?", value = TRUE)
                          ),
                          
                          mainPanel(plotOutput("boxplot"))
                          
                          ),

  ## ------------Wykres punktowy-----------------
                   

                        conditionalPanel(
                          condition = "input.plot == 'scatter'",
                          sidebarPanel(
                          selectInput("scatter_var1", "Wybierz pierwszą zmienną (oś OX)",
                                      choices = list("Bezrobocie" = "bezr",
                                                     "Wynagrodzenia" = "wyn",
                                                     "Liczba firm/10 tys. osób" = "firmy"),
                                      selected = c("bezr")),
                          selectInput("scatter_var2", "Wybierz drugą zmienną (oś OY)",
                                      choices = list("Bezrobocie" = "bezr",
                                                     "Wynagrodzenia" = "wyn",
                                                     "Liczba firm/10 tys. osób" = "firmy"),
                                      selected = c("wyn")),
                          selectInput("scatter_year", "Wybierz rok dla którego mają być prezentowane dane",
                                      choices = 2004:2014, selected = 2014),
                          hr(),
                          checkboxInput("model", "Czy wyświerlić liniię trendu?", value = FALSE),
                          
                          conditionalPanel(
                            condition = "input.model",
                            br(),
                            radioButtons("model_type", "Jaki model wyświetlić?", choices = list("Liniowy" = "lm", 
                                                                                "Nieliniowy" = "loess"))
                          )
                    
                          ),
                          
                          mainPanel(plotOutput("scatter"))
                          
                        ),

  ##  ---------------Porównanie------------------

                        conditionalPanel(
                          condition = "input.plot == 'por'",
                          sidebarPanel(
                          uiOutput("ui"),
                          hr(),
                          radioButtons("por_var", "Jakie dane przedstawić na wykresie?",
                                       choices = list("Bezrobocie" = "bezr",
                                                      "Wynagrodzenia" = "wyn",
                                                      "Liczba firm/10 tys. osób" = "firmy"),
                                       selected = c("wyn")),
                          br(),
                          actionButton("random", "Losuj")
                        ),
                        
                        mainPanel(plotlyOutput("por"))
                        )
  ),
#====================== MAPA===========================
                   tabPanel("Mapa",
                            
                            div(class="outer",
                                tags$head(
                                  
                                  includeCSS("./settings/styles.css"),
                                  includeScript("./settings/gomap.js")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%"),
                                
                                absolutePanel(id = "absPanel", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 80, left = 20, right = "auto", bootom = "auto",
                                              width = 330,
                                              
                                              selectInput("map_var", "Wybierz zmienną do prezentacji na mapie",
                                                          choices = list("Bezrobocie" = "bezr",
                                                                         "Wynagrodzenia" = "wyn",
                                                                         "Liczba firm/10 tys. osób" = "firmy"),
                                                          selected = c("bezr")),
                                              selectInput("map_year", "Wybierz rok dla prezentowanej zmiennej",
                                                          choices = 2004:2014, selected = 2014),
                                              radioButtons("map_breaks_type", "W jaki sposób dzielić dane?",
                                                           choices = list("Równe przedziały" = "colorBin",
                                                                          "Kwantyle" = "colorQuantile")),
                                              sliderInput("map_number_breaks", "Na ile przedziałow podzielić dane?",
                                                          min = 2, max = 8, value = 4)
                                              )
                            )
                            )

 
  
  
))

