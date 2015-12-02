# Uvoz podatkov

# Funkcija, ki uvozi podatke iz datotek podatki umrljivost.csv in podatki cepljenost.csv
uvozi.umrljivost <- function(){
  return(read.table("podatki umrljivost.csv", sep=",",
                    header=TRUE,
                    fileEncoding = "UTF-8"))
}
umrljivost <- uvozi.umrljivost()

# read.csv(file= "podatki umrljivost.csv")
# stolpci <- c("Država","Leto","Smrtnost do 5.leta starosti", "Smrtnost dojenčkov")
# podatki <- read.csv2(file = "podatki umrljivost.csv", skip=1, 
#+                      nrow=(195-1), header=FALSE, strip.white=TRUE, col.names=stolpci, 
#+                      fileEncoding="Windows-1250")
#