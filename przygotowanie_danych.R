library(dplyr)
library(tidyr)
library(rgdal)


# DANE - SZEROKI ZAKRES
dataset <- read.table("./data/dane.csv", na.strings = "-", sep = ";", header = TRUE, dec = ",", colClasses = c("factor", "character", rep("numeric", len = 33)))
dataset <- dataset[order(dataset$Kod),]  ## Zmiana kolejności wyświetlania tabeli wg kodu powiatu
dataset$Powiat <-  gsub("^Powiat ", "", dataset$Powiat) ## usunięcie słowa poiat z nazw powiatów
dataset$Powiat <- gsub("[*]$", "", dataset$Powiat) ## usunięcie gwiazdek z końca nazw niektórych powiatów
dataset <- dataset %>% mutate(Kod_woj = substr(x = dataset$Kod, start = 1, stop = 2)) ## Utworzenie nowej kolumny z kodem województwa

# WCZYTANIE KODÓW I NAZW WOJEWÓDZTW
load("./data/kody.woj.Rdata")
names(kody_woj)[1] <- "Kod_woj" ## zmiana nazwy kolumny kodów woj. w celu ujednolicenia z główną tabelą danych
kody_woj[c(5, 9, 12, 14),1] <- c("06", "08", "02", "04") ## Poprawienie kodów województw

# PRZYPISANIE POSZCZEGÓLNYM POWIATOM WOJEWÓDZTW W KTÓRYCH SĄ POLOŻONE
dataset <- left_join(x = dataset, y = kody_woj, by = "Kod_woj")

#DANE - WĄSKI ZAKRES
dataset_gather <- gather(dataset, wskaznik, wartosc, bezr_2004:firmy_2014)
dataset_gather <- dataset_gather %>%
mutate(Rodzaj.wskaznika = gsub("_[0-9]{4}$", "", dataset_gather$wskaznik)) %>% ##utworzenie zmiennej określającej rodzaj wskaźnika (bez roku)
mutate(rok = gsub("^[a-z]+_", "", dataset_gather$wskaznik)) ## utworzenie zmiennej określającej (tylko) rok

###Wczytanie mapy
mapaPowiaty <- readOGR("data", "powiaty4")

### Dodanie zmiennych do mapy
mapaPowiaty@data <- merge(mapaPowiaty@data, dataset, by.x = "Kod", by.y = "Kod", sort = FALSE)

save(dataset, dataset_gather, mapaPowiaty, file = "./data/dane.Rdata")
