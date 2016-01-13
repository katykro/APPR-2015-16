# Uvoz potrebnih knjižnic
library(sp)
library(maptools)
library(digest)
gpclibPermit()
library(ggplot2)
library(dplyr)

pretvori.zemljevid <- function(zemljevid,pogoj=TRUE) {
  fo <- fortify(zemljevid[pogoj,])
  data <- zemljevid@data
  data$id <- as.character(0:(nrow(data)-1))
  return(inner_join(fo, data, by="id"))
}

svet<-uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                      "ne_110m_admin_0_countries",force=FALSE)
cep <- select(tidy_analiza, Drzava, Spol, Cepljenost)
m <- match(svet$name_long,cep$Drzava)
svet$Cepljenost <- tidy_analiza$Cepljenost[m]
svet$Spol <- tidy_analiza$Spol[m]
afrika <- pretvori.zemljevid(svet, svet$continent == "Africa")
map1 <- ggplot() + geom_polygon(data = afrika, aes(x=long,y=lat, group=group,
                                                   fill= Cepljenost), color="grey35")+ xlab("") + ylab("") +
  scale_fill_gradient(low = "#132B43", high = "#56B1F7") +
  guides(fill = guide_colorbar(title = "Cepljenost v odstotkih v Afriki"))
print(map1)

smrt <- select(analiza, Drzava, Smrtnost.do.5.leta.starosti)
m1 <- match(svet$name_long, smrt$Drzava)
svet$Smrtnost <- analiza$Smrtnost.do.5.leta.starosti[m1]
svet1 <- pretvori.zemljevid(svet)
map2 <- ggplot() + geom_polygon(data = svet1, aes(x=long,y=lat, group=group,
                                                   fill= Smrtnost), color="grey35")+ xlab("") + ylab("") +
  scale_fill_gradient(low = "#132B43", high = "#56B1F7") +
  guides(fill = guide_colorbar(title = "Smrtnost do 5. leta starosti"))
print(map2)
