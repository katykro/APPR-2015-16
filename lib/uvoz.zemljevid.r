# Uvoz potrebnih knjižnic
library(sp)
library(maptools)
library(digest)
gpclibPermit()
library(ggplot2)
library(dplyr)
source("lib/uvozi.zemljevid.r", encoding = "UTF-8")

pretvori.zemljevid <- function(zemljevid,pogoj=TRUE) {
  fo <- fortify(zemljevid[pogoj,])
  data <- zemljevid@data
  data$id <- as.character(0:(nrow(data)-1))
  return(inner_join(fo, data, by="id"))
}

# zemljevid podhranjenost

svet<-uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                      "ne_110m_admin_0_countries",force=FALSE)
podh <- select(vsi_zdruzeni, Drzava,`Odstotek podhranjenih otrok`)
m <- match(svet$name_long,podh$Drzava)
svet$Podhranjenost <- vsi_zdruzeni$`Odstotek podhranjenih otrok`[m]
svet1 <- pretvori.zemljevid(svet)
map1 <- ggplot() + geom_polygon(data = svet1, aes(x=long,y=lat, group=group,
                                                   fill= Podhranjenost), color="grey35")+ xlab("") + ylab("") +
  scale_fill_gradient(low = "darkgreen", high = "red") +
  guides(fill = guide_colorbar(title = "Odstotek podhranjenih otrok - svet"))
print(map1)

raz <- select(vsi_zdruzeni, Drzava,`Odstotek nerazvitih otrok`)
m2 <- match(svet$name_long, raz$Drzava)
svet$Nerazvitost <- vsi_zdruzeni$`Odstotek nerazvitih otrok`[m2]
afrika <- pretvori.zemljevid(svet, svet$continent == "Africa")
map2 <- ggplot() + geom_polygon(data = afrika, aes(x=long,y=lat, group=group,
                                                   fill= Nerazvitost), color="grey35")+ xlab("") + ylab("") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  guides(fill = guide_colorbar(title = "Odstotek nerazvitih otrok - Afrika"))
print(map2)

#  zem3 <- map2 + geom_text(data = vsi_zdruzeni %>% filter(`Odstotek nerazvitih otrok` > 40),
#                           aes(x=long, y=lat, label = Drzava),size = 3, vjust = 2)
#  print(zem3)


cep <- select(tidy_analiza,Spol, Drzava, Cepljenost)
m3 <- match(svet$name_long, cep$Drzava)
svet$Cepljenost <- tidy_analiza$Cepljenost[m3]
svet$Spol <- tidy_analiza$Spol[m3]
afrika <- pretvori.zemljevid(svet, svet$continent == "Africa")
map3 <- ggplot() + geom_polygon(data = afrika, aes(x=long,y=lat, group=group,
                                                   fill= Cepljenost), color="grey35")+ xlab("") + ylab("") +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  guides(fill = guide_colorbar(title = "Odstotek cepljenih otrok - Afrika"))
print(map3)

# ta del ne deluje...
# map4 <- map3 + geom_point(data = tidy_analiza, aes(x =long , y =lat, color = Spol)) +
#   scale_color_manual(name="Spol", breaks = c("Z", "M"),
#                      labels = c("Ženski", "Moški"),
#                      values = c("red", "blue"))
# print(map4)

# slovenija <- slovenija + geom_text(data = obcine@data %>% 
#                                      filter(procent.obsojenih > 1 | procent.obsojenih < 0.1),
#                                    aes(x = Y_C, y = X_C, label = OB_UIME),
#                                    size = 3, vjust = 2)

  