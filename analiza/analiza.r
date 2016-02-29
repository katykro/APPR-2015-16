# 4. faza: Analiza podatkov

#ločila bom države glede na cepljenost in umrljivost

pod_analiza <- analiza
rownames(pod_analiza) <- pod_analiza$Drzava
pod_analiza <- pod_analiza[c("Cepljenost.Z","Smrtnost.do.5.leta.starosti")]
analiza.norm <- scale(pod_analiza)

k <- kmeans(analiza.norm, 5, nstart =1000)
table(k$cluster)

analiza.skupine <- data.frame(Drzava = names(k$cluster),skupina = factor(k$cluster))

# dendrogram z ward.D
razporeditev <- dist(as.matrix(analiza.norm))
hc <- hclust(razporeditev, method = "ward.D") 

n <- 5
dend <- as.dendrogram(hc, main = "Razporeditev držav",sub = "",  hang = -1)
sk <- cutree(hc, k = n)
labels_colors(dend) <- rainbow(n)[sk][order.dendrogram(dend)]

plot(dend)


# dendrogram z ward.D 26 držav na 5 skupin

pod_analiza2 <- vsi_zdruzeni
pod_analiza2[6,1] <-"DR Congo"
pod_analiza2[24,1] <- "Maced."
pod_analiza2[21,1] <- "S. Leone"
pod_analiza2[17,1] <- "Mozam."
pod_analiza2[3,1] <- "Burk. Faso"
pod_analiza2[2,1] <- "Banglad."
rownames(pod_analiza2) <- pod_analiza2$Drzava
pod_analiza2 <- pod_analiza2[c("Cepljenost.Z","Smrtnost.do.5.leta.starosti")]
analiza.norm2 <- scale(pod_analiza2)

k <- kmeans(analiza.norm2, 5, nstart =1000)
table(k$cluster)
analiza.skupine2 <- data.frame(Drzava = names(k$cluster),skupina = factor(k$cluster))


razporeditev2 <- dist(as.matrix(analiza.norm2))
hc2 <- hclust(razporeditev2, method = "ward.D") 

n <- 5
dend2 <- as.dendrogram(hc2, main = "Razporeditev držav", sub = "", hang = -1)
sk2 <- cutree(hc2, k = n)
labels_colors(dend2) <- rainbow(n)[sk2][order.dendrogram(dend2)]
plot(dend2)


# razporeditev 26 držav v 5 skupin - zemljevid

pod_analiza3 <- vsi_zdruzeni
rownames(pod_analiza3) <- pod_analiza3$Drzava
pod_analiza3 <- pod_analiza3[c("Cepljenost.Z","Smrtnost.do.5.leta.starosti")]
analiza.norm3 <- scale(pod_analiza3)

k <- kmeans(analiza.norm3,5)
table(k$cluster)
k <- kmeans(analiza.norm3,5, nstart = 10000)
analiza.skupine3 <- data.frame(Drzava = names(k$cluster), skupina = factor(k$cluster))
skupine3 <- analiza.skupine3
dobro.slabo3 <- skupine3[c("Togo", "The former Yugoslav republic of Macedonia","Bangladesh","Burkina Faso","Nigeria"), "skupina"]

m3 <- match(svet$name_long , skupine3$Drzava)
svet$skupine <- analiza.skupine3$skupina[m3]
svet3 <- pretvori.zemljevid(svet)
zem3 <- ggplot() + geom_polygon(data = svet3, aes(x=long, y=lat, 
                                                   group = group, fill = skupine),
                                color = "grey") + xlab("") + ylab("") +
  scale_fill_manual(values = setNames(c("orange","green3","yellow2","blue","red2"), dobro.slabo3),
                    labels = setNames(c("C ~, U ~","C >>, U <<","C >, U <"," C >, U >","C <<, U >>"), dobro.slabo3),
                    na.value = "#7f7f7f")

plot(zem3)  