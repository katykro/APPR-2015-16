# Uvoz podatkov

# Funkcija, ki uvozi podatke iz datotek podatki umrljivost.csv in podatki cepljenost.csv
require(dplyr)

stolpci <- c("Drzava","Leto","Smrtnost do 5.leta starosti", "Smrtnost dojenckov")
uvozi.umrljivost <- function(){
  return(read.table("podatki-umrljivost.csv", sep=",",
                    quote="",as.is = TRUE,
                    nrow=(195-1),header=FALSE,strip.white=TRUE, col.names=stolpci,na.strings="0 [0-0]",
                    fileEncoding = "UTF-8",skip=1))
}
umrljivost <- uvozi.umrljivost()
umrljivost$Leto <- NULL


stolpci <- c("Drzava","Leto","k","Cepljenost-Z","Cepljenost-M")
uvozi.cepljenost <- function(){
  return(read.table("podatki-cepljenost.csv", sep=",",quote="",as.is = NA,
                    nrow=(65-2),header=FALSE,strip.white=TRUE, col.names=stolpci,na.strings="No data",
                    fileEncoding = "UTF-8",skip=2))
}
cepljenost <- uvozi.cepljenost()
cepljenost$k <- NULL
cepljenost$Leto <- NULL
z <-strsplit(cepljenost$Cepljenost.Z, split = " ")[[1]]
as.numeric(z[1])
m <-strsplit(cepljenost$Cepljenost.M, split = " ")[[1]]
as.numeric(m[1])
#cepljenost$Cepljenost.Z <- as.numeric(cepljenost$Cepljenost.Z)
#cepljenost$Cepljenost.M <- as.numeric(cepljenost$Cepljenost.M)
cepljenost$Cepljenost.Z[cepljenost$Cepljenost.Z == "No data"] <- NA

z <-strsplit("91.7 [83.5-96.0]", split = " ")[[1]]
as.numeric(z[1])


