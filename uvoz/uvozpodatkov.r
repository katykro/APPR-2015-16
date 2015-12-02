# Uvoz podatkov

# Funkcija, ki uvozi podatke iz datotek podatki umrljivost.csv in podatki cepljenost.csv
uvozi.umrljivost <- function(){
  return(read.table("podatki umrljivost.csv", sep=",", as.is=TRUE,
                    col.names = c("Country", "Year", "Number of under-five deaths", "Number of infant deaths"),
                    fileEncoding = "UTF-8"))
}
umrljivost <- uvozi.umrljivost()

