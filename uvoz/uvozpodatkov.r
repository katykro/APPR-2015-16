# Uvoz podatkov

## Funkcija, ki uvozi podatke iz datotek podatki-umrljivost.csv in podatki-cepljenost.csv
require(dplyr)

stolpci <- c("Drzava","Leto","Smrtnost do 5.leta starosti", "Smrtnost dojenckov")
uvozi.umrljivost <- function(){
  return(read.table("podatki/podatki-umrljivost.csv", sep=",",
                    as.is = TRUE,
                    nrow=(195-1),header=FALSE,strip.white=TRUE, col.names=stolpci,na.strings="0 [0-0]",
                    fileEncoding = "UTF-8",skip=1))
}
umrljivost <- uvozi.umrljivost()
umrljivost$Leto <- NULL
umrljivost$Smrtnost.do.5.leta.starosti <- umrljivost$Smrtnost.do.5.leta.starosti %>% gsub("^([0-9. ]+).*", "\\1", .) %>% gsub(" ","", .) %>% as.numeric()
umrljivost$Smrtnost.dojenckov <- umrljivost$Smrtnost.dojenckov %>% gsub("^([0-9. ]+).*", "\\1", .) %>% gsub(" ","", .) %>% as.numeric()
umrljivost <- umrljivost[!is.na(umrljivost$Smrtnost.do.5.leta.starosti),]
umrljivost <- umrljivost[!is.na(umrljivost$Smrtnost.dojenckov),]
write.csv(umrljivost,"umrljivost.csv",row.names=FALSE)

# cepljenost
stolpci <- c("Drzava","Leto","k","Cepljenost-Z","Cepljenost-M")
uvozi.cepljenost <- function(){
  return(read.table("podatki/podatki-cepljenost.csv", sep=",",as.is = TRUE,
                    nrow=(65-2),header=FALSE,strip.white=TRUE, col.names=stolpci,na.strings="No data",
                    fileEncoding = "UTF-8",skip=2))
}
cepljenost <- uvozi.cepljenost()
cepljenost$k <- NULL
cepljenost$Leto <- NULL

cepljenost$Cepljenost.Z <- cepljenost$Cepljenost.Z %>% gsub("^([0-9. ]+).*", "\\1", .) %>% gsub(" ","", .) %>% as.numeric()
cepljenost$Cepljenost.M <- cepljenost$Cepljenost.M %>% gsub("^([0-9. ]+).*", "\\1", .) %>% gsub(" ","", .) %>% as.numeric()
cepljenost <- cepljenost[!is.na(cepljenost$Cepljenost.Z),]
cepljenost <- cepljenost[!is.na(cepljenost$Cepljenost.M),]
write.csv(cepljenost,"cepljenost.csv",row.names=FALSE)


# združitev obeh tabel
zdruzena1 <- full_join(umrljivost, cepljenost, "Drzava"="Drzava", copy=TRUE)
write.csv(zdruzena1, "zdruzeni1.csv",row.names=FALSE)


## Funkcija, ki uvozi podatke iz datotek podatki-nerazvitost.xml in podatki-podhranjenost.xml
 
require(dplyr)
require(httr)
require(jsonlite)
library(rjson)
library(RCurl)
 
nerazviti <- fromJSON(file="podatki/podatki-nerazvitost.xml", method="C")
r <- GET("http://apps.who.int/gho/athena/data/GHO/MDG_0000000027.json?filter=COUNTRY:*;REGION:*;SEX:*")
text <- content(r, "text")
data <- fromJSON(content(r, "text"))

nerazvitost<- nerazviti$fact %>% sapply(unlist) %>% t() %>%
data.frame()
nerazvitost$dims.DATASOURCE <- NULL
nerazvitost$dims.GHO <- NULL
nerazvitost$dims.SEX <- NULL
stolpci <- c("Drzava","Leto","Odstotek nerazvitih otrok")
colnames(nerazvitost) <- stolpci
nerazvitost$Drzava <- as.character(nerazvitost$Drzava)
nerazvitost$`Odstotek nerazvitih otrok` <- as.numeric(levels(nerazvitost$`Odstotek nerazvitih otrok`))[nerazvitost$`Odstotek nerazvitih otrok`]
write.csv(nerazvitost,"nerazvitost.csv",row.names=FALSE)

# podhranjenost

podhranjeni <- fromJSON(file="podatki/podatki-podhranjenost.xml", method="C")
s <- GET("http://apps.who.int/gho/athena/data/GHO/WHOSIS_000008.json?profile=simple&filter=COUNTRY:*;REGION:*;SEX:*")
text <- content(s,"text")
podatki <- fromJSON(content(s,"text"))

podhranjenost <- podhranjeni$fact %>% sapply(unlist) %>% t() %>% data.frame()
podhranjenost$dims.DATASOURCE <- NULL
podhranjenost$dims.GHO <- NULL
podhranjenost$dims.SEX <- NULL
stolpcipo <- c("Drzava","Leto","Odstotek podhranjenih otrok")
colnames(podhranjenost) <- stolpcipo
podhranjenost$Drzava <- as.character(podhranjenost$Drzava)
podhranjenost$`Odstotek podhranjenih otrok` <- as.numeric(levels(podhranjenost$`Odstotek podhranjenih otrok`))[podhranjenost$`Odstotek podhranjenih otrok`]
write.csv(podhranjenost,"podhranjenost.csv",row.names=FALSE)

# združitev obeh  tabel iz JSON
zdruzena2 <- full_join(nerazvitost, podhranjenost, "Drzava"="Drzava", copy=TRUE)
write.csv(zdruzena2, "zdruzeni2.csv",row.names=FALSE)

# celotna združitev vseh štirih tabel
vsi_zdruzeni <- full_join(zdruzena1, zdruzena2, "Drzava"="Drzava", copy=TRUE)
write.csv(vsi_zdruzeni, "vsi_zdruzeni.csv", row.names = FALSE)

# Dodane tabele za lažjo vizualizacijo
pogoste_umrljivost <- filter(umrljivost, Smrtnost.do.5.leta.starosti > 100)
pogoste_umrljivost[4,1] <- "DRC"

pogoste_dojencki <- filter(umrljivost, Smrtnost.dojenckov > 100)
pogoste_dojencki[3,1] <- "DRC"

pogoste_cepljenost <- filter(cepljenost, Cepljenost.M < 45)
pogoste_cepljenost[1,1] <- "C. African R."
pogoste_cepljenost[5,1] <- "LPDR"
miniM <- cepljenost$Drzava[min(cepljenost$Cepljenost.M)]
miniZ <- cepljenost$Drzava[min(cepljenost$Cepljenost.Z)]

pogoste_nerazvitost <- filter(nerazvitost,`Odstotek nerazvitih otrok` > 44)
pogoste_nerazvitost[6,1] <- "PN Guinea"

pogoste_pod <- filter(podhranjenost, `Odstotek podhranjenih otrok` > 30)
podh_b <- pogoste_pod[1,]
podh_p <- pogoste_pod[9,]
pogostebrez_bp <- filter(pogoste_pod, Drzava!="Bangladesh" & Drzava!= "Pakistan")
pogoste_pod1 <- full_join(pogostebrez_bp, podh_b)
pogoste_podhranjenost <- full_join(pogoste_pod1, podh_p)
pogoste_podhranjenost <- arrange(pogoste_podhranjenost, Drzava)

pogoste_cepljenostMZ <- melt(pogoste_cepljenost,id.vars= "Drzava")
