# Uvoz podatkov

# Funkcija, ki uvozi podatke iz datotek podatki umrljivost.csv in podatki cepljenost.csv

stolpci <- c("Drž+zava","Leto","Smrtnost do 5.leta starosti", "Smrtnost dojenckov")
uvozi.umrljivost <- function(){
  return(read.table("podatki umrljivost.csv", sep=",",
                    quote="",as.is = TRUE,
                    nrow=(195-1),header=FALSE,strip.white=TRUE, col.names=stolpci,
                    fileEncoding = "UTF-8",skip=2))
}
umrljivost <- uvozi.umrljivost()



stolpci <- c("Drzava","Leto","k","Cepljenost-Z","Cepljenost-M")
uvozi.cepljenost <- function(){
  return(read.table("podatki cepljenost.csv", sep=",",quote="",as.is = TRUE,
                    nrow=(65-2),header=FALSE,strip.white=TRUE, col.names=stolpci,na.string=NA,
                    fileEncoding = "UTF-8",skip=2))
}
cepljenost <- uvozi.cepljenost()

