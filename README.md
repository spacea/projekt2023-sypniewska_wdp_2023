# Calendar.R

Aplikacja Calendar.R powie Ci wszystko, co musisz wiedzieć o danej dacie. Wystarczy wybrać ją z panelu bocznego – w zamian dowiesz się, jakie danego dnia są święta i imieniny, kto znany ma urodziny, oraz jaki wtedy wypada znak zodiaku (zarówno ten najbardziej znany, słoneczny, jak i księżycowy), wraz z horoskopem. Dodatkowo aplikacja wyświetla pogodę dla Poznania w czasie rzeczywistym.

## Wymagania

- zainstalowana [aktualna wersja R](https://cran.rstudio.com/)
- zainstalowana aktualna wersja dowolnego zintegrowanego środowiska programistycznego (IDE) przeznaczonego do pracy z R, np. [RStudio](https://posit.co/download/rstudio-desktop/)
- znajomość języka angielskiego

## Instalacja
Poniższy kod zawiera pakiety wymagane do działania aplikacji. Należy go wkleić do wybranego IDE przeznazonego do pracy z R, np. RStudio, a następnie wykonać przy pomocy skrótu klawiszowego **Ctrl+Enter**.
```
install.packages(c("shiny", "shinythemes", "lubridate", "scales", "rjson"))
```
Instalację pakietów wystarczy wykonać tylko raz, przed pierwszym użyciem aplikacji.

## Użycie
### Bez pobierania repozytorium
1. Poniższy kod należy wkleić do wybranego IDE przeznaczonego do pracy z R, a następnie wykonać każdą kolejną linijkę przy pomocy skrótu klawiszowego **Ctrl+Enter**. Ostatnia linijka otworzy aplikację.
```
library(shiny)
runGitHub("projekt2023-sypniewska_wdp_2023", "spacea")
```
2. Z panelu bocznego wybrać datę.

### Z pobranym repozytorium
1. Pobrać wszystkie pliki z repozytorium do jednego folderu.
2. Otworzyć plik **app.R** w wybranym zintegrowanym środowisku programistycznym przeznaczonym do pracy z R, np. RStudio.
3. Wczytać wszystkie pakiety zawarte na górze skryptu za pomocą **Ctrl+Enter** (komendy *library*).
4. Używając tej samej kombinacji klawiszy wykonać wszystkie kolejne funkcje. Ostatnia - *shinyApp* - otworzy aplikację.
5. Z panelu bocznego wybrać datę.

## Źródła
- [OpenWeatherMap](https://openweathermap.org/) – pogoda
- [Kalbi.pl](https://www.kalbi.pl/), [NationalToday.com](https://nationaltoday.com/), [Wikipedia](https://pl.wikipedia.org/wiki/Kalendarium_dzie%C5%84_po_dniu) – święta 
- [On This Day](https://www.onthisday.com/), [Famous Birthdays](https://www.famousbirthdays.com/) – urodziny
- [Kalendarz Świąt](https://www.kalendarzswiat.pl/), [Wikipedia](https://pl.wikipedia.org/wiki/Kalendarium_dzie%C5%84_po_dniu) – imieniny

## Autorzy
- Adam Olejniczak
- Nina Roszkiewicz
- Justyna Sypniewska

