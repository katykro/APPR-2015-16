# Uvoz podatkov

## Funkcija, ki uvozi podatke iz datotek podatki-umrljivost.csv in podatki-cepljenost.csv
require(dplyr)

stolpci <- c("Drzava","Leto","Smrtnost do 5.leta starosti", "Smrtnost dojenckov")
uvozi.umrljivost <- function(){
  return(read.table("podatki/podatki-umrljivost.csv", sep=",",
                    as.is = TRUE,
                    nrow=(5045-1),header=FALSE,strip.white=TRUE, col.names=stolpci,na.strings="0 [0-0]",
                    fileEncoding = "UTF-8",skip=1))
}
umrljivost <- uvozi.umrljivost()
# odstranila sem vse v oklepajih ter spremenila v numeric
umrljivost$Smrtnost.do.5.leta.starosti <- umrljivost$Smrtnost.do.5.leta.starosti %>% gsub("^([0-9. ]+).*", "\\1", .) %>% gsub(" ","", .) %>% as.numeric()
umrljivost$Smrtnost.dojenckov <- umrljivost$Smrtnost.dojenckov %>% gsub("^([0-9. ]+).*", "\\1", .) %>% gsub(" ","", .) %>% as.numeric()
umrljivost$Leto <- umrljivost$Leto %>% as.numeric()
# izbrisala sem vrstice z NA
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

# odstranitev oklepajev, pretvorba v numeric ter izbris vrstic z NA
cepljenost$Cepljenost.Z <- cepljenost$Cepljenost.Z %>% gsub("^([0-9. ]+).*", "\\1", .) %>% gsub(" ","", .) %>% as.numeric()
cepljenost$Cepljenost.M <- cepljenost$Cepljenost.M %>% gsub("^([0-9. ]+).*", "\\1", .) %>% gsub(" ","", .) %>% as.numeric()
cepljenost <- cepljenost[!is.na(cepljenost$Cepljenost.Z),]
cepljenost <- cepljenost[!is.na(cepljenost$Cepljenost.M),]

write.csv(cepljenost,"cepljenost.csv",row.names=FALSE)


# združitev obeh tabel
zdruzenaUC <- inner_join(umrljivost, cepljenost, "Drzava"="Drzava","Leto"="Leto", copy=TRUE)

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
# izbrisani nepotrebni stolpci
nerazvitost$dims.DATASOURCE <- NULL
nerazvitost$dims.GHO <- NULL
nerazvitost$dims.SEX <- NULL

stolpci <- c("Drzava","Leto","Odstotek nerazvitih otrok")
colnames(nerazvitost) <- stolpci

nerazvitost$Drzava <- as.character(nerazvitost$Drzava)
nerazvitost$`Odstotek nerazvitih otrok` <- as.numeric(levels(nerazvitost$`Odstotek nerazvitih otrok`))[nerazvitost$`Odstotek nerazvitih otrok`]
nerazvitost$Leto <- as.numeric(levels(nerazvitost$Leto))[nerazvitost$Leto]
# različna podatka za isto leto
nerazvitost[79,] <- NA
nerazvitost[78,] <- NA
nerazvitost <- nerazvitost[!is.na(nerazvitost$Leto),]

write.csv(nerazvitost,"nerazvitost.csv",row.names=FALSE)

# združitev tabel o umrljivosti in nerazvitosti
zdruzenaUN <- inner_join(umrljivost, nerazvitost, "Drzava"="Drzava","Leto"="Leto", copy=TRUE)

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
podhranjenost$Leto <- as.numeric(levels(podhranjenost$Leto))[podhranjenost$Leto]
# različna podatka za isto leto
podhranjenost[79,] <- NA
podhranjenost[78,] <- NA
podhranjenost <- podhranjenost[!is.na(podhranjenost$Leto),]

write.csv(podhranjenost,"podhranjenost.csv",row.names=FALSE)

# združitev obeh  tabel iz JSON
zdruzenaNP <- inner_join(nerazvitost, podhranjenost, "Drzava"="Drzava","Leto"="Leto", copy=TRUE)

#združitev umrljivosti in podhranjenosti
zdruzenaUP <- inner_join(umrljivost, podhranjenost, "Drzava"="Drzava", "Leto"="Leto",copy=TRUE )

# celotna združitev vseh štirih tabel
vsi_zdruzeni <- inner_join(zdruzenaNP, zdruzenaUC, "Drzava"="Drzava", "Leto"="Leto",copy=TRUE)
write.csv(vsi_zdruzeni, "vsi_zdruzeni.csv", row.names = FALSE)

# Dodane tabele za lažjo vizualizacijo
# največja smrtnost do 5. leta starosti (nad 95 000 na leto)
pogoste_umrljivost <- filter(zdruzenaUC, Smrtnost.do.5.leta.starosti > 95)
pogoste_umrljivost[3,] <- NA
pogoste_umrljivost[7,] <- NA
pogoste_umrljivost[2,1] <- "DR Congo"
pogoste_umrljivost[10,1] <- "UR Tanzania"
pogoste_umrljivost <- pogoste_umrljivost[!is.na(pogoste_umrljivost$Leto),]

pogoste_cepljenost <- filter(cepljenost, Cepljenost.M < 45)
pogoste_cepljenost[1,1] <- "C. African R."
pogoste_cepljenost[5,1] <- "LPDR"
miniM <- min(cepljenost$Cepljenost.M)
miniZ <- min(cepljenost$Cepljenost.Z)

pogoste_nerazvitost <- filter(nerazvitost,`Odstotek nerazvitih otrok` > 42)
pogoste_nerazvitost[1,1] <- "DR Congo"

pogoste_pod <- filter(podhranjenost, `Odstotek podhranjenih otrok` > 30)
podh_b <- pogoste_pod[1,]
podh_p <- pogoste_pod[9,]
pogostebrez_bp <- filter(pogoste_pod, Drzava!="Bangladesh" & Drzava!= "Pakistan")
pogoste_pod1 <- full_join(pogostebrez_bp, podh_b)
pogoste_podhranjenost <- full_join(pogoste_pod1, podh_p)
pogoste_podhranjenost <- arrange(pogoste_podhranjenost, Drzava)

analiza <- filter(vsi_zdruzeni, Smrtnost.do.5.leta.starosti > 50)
analiza[4,1] <- "DR Congo"
analiza[1,1] <- "Banglad."
analiza[2,1] <-"Burk. Faso"
analiza[9,] <- NA
analiza <- analiza[!is.na(analiza$Leto),]
analiza$Delez_dojenckov <- (analiza$Smrtnost.dojenckov/analiza$Smrtnost.do.5.leta.starosti)*100
write.csv(analiza, "analiza.csv", row.names = FALSE)

require(reshape2)

pogoste_cepljenostMZ <- melt(analiza %>% select(Drzava,Cepljenost.M,Cepljenost.Z),id.vars= "Drzava")
