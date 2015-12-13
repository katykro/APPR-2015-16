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
zdruzena <- full_join(umrljivost, cepljenost, "Drzava"="Drzava", copy=TRUE)
write.csv(zdruzena, "zdruzeni.csv",row.names=FALSE)

## Funkcija, ki uvozi podatke iz datotek podatki-nerazvitost.xml in podatki-podhranjenost.xml

require(dplyr)
require(httr)
require(jsonlite)
library(rjson)
library(RCurl)

html <- file("podatki/podatki-nerazvitost.html") %>% read_html()
tabela1 <- html %>% html_nodes(xpath="//table") %>% .[[1]] %>% html_table(fill = TRUE)
Encoding(tabela[[1]]) <- "UTF-8"


nerazvitost <- fromJSON(file="podatki/data (12).xml", method="C")
r <- GET("http://apps.who.int/gho/athena/data/GHO/MDG_0000000027.json?filter=COUNTRY:*;REGION:*;SEX:*")
text <- content(r, "text")
data <- fromJSON(content(r, "text"))
data$dataset %>% names() %>% print()

# tabela2 <- data.frame(data$dataset$data, stringsAsFactors=FALSE)
# names(tabela) <- data$dataset$column_name
# print(sapply(tabela, class))
