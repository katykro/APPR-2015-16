# Uvoz podatkov

# Funkcija, ki uvozi podatke iz datotek podatki umrljivost.csv in podatki cepljenost.csv

stolpci <- c("Drzava","Leto","Smrtnost do 5.leta starosti", "Smrtnost dojenckov")
uvozi.umrljivost <- function(){
  return(read.table("podatki-umrljivost.csv", sep=",",
                    quote="",as.is = TRUE,
                    nrow=(195-1),header=FALSE,strip.white=TRUE, col.names=stolpci,na.strings="0 [0-0]",
                    fileEncoding = "UTF-8",skip=2))
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
cepljenost$Cepljenost.Z <- as.factor(cepljenost$Cepljenost.Z)
cepljenost$Cepljenost.M <- as.factor(cepljenost$Cepljenost.M)
cepljenost$Cepljenost.Z[cepljenost$Cepljenost.Z == "No data"] <- NA
