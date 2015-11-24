# Analiza podatkov s programom R, 2015/16

Avtor: Katarina Kromar

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2015/16.

## Analiza umrljivosti otrok do 5. leta starosti

Analizirala bom podatke o umrljivosti otrok po svetu do 5. leta starosti. Najprej bom analizirala podhranjenost otrok glede na spol od leta 2010 do 2014, nato pa še nerazvitost otrok ter delež cepljenih do 5. leta starosti.

Skozi analizo želim izvedeti kolikšen delež tistih, ki umrejo, je podhranjenih ali nerazvitih, ter ali ima cepljenje vpliv na umrljivost otrok.

Podatke sem pridobila na spletu:

* http://apps.who.int/gho/data/node.main.ChildMort-1?lang=en
* http://apps.who.int/gho/data/node.main.1098?lang=en
* http://apps.who.int/gho/data/view.main.90200
* http://apps.who.int/gho/data/node.main.HE-1588?lang=en

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Spletni vmesnik

Spletni vmesnik se nahaja v datotekah v mapi `shiny/`. Poženemo ga tako, da v
RStudiu odpremo datoteko `server.R` ali `ui.R` ter kliknemo na gumb *Run App*.
Alternativno ga lahko poženemo tudi tako, da poženemo program `shiny.r`.

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `httr` - za pobiranje spletnih strani
* `XML` - za branje spletnih strani
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
