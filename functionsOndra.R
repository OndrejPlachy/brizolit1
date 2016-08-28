setwd("C:/Users/Pavouk/Dropbox/R Studio dokuments/BRIZOLIT BEZPECNOST")

# ggplot stat_count 2 variables
gg1 <- function(x,y) {
  ggplot(svl2, aes(x=x, fill = y))+
    stat_count(width = 0.8)
  }
gg1(svl2$Kraj, svl2$B.2)
gg1(svl2$Kraj, svl2$L.8)

# summary of each variable with percent

Perc.summary<-function(x){
  x1<- summary(x)
  x2<- getPercentString(summary(x), NROW(x))
  x.compl <- data.frame(unclass(summary(x)), check.names = F, stringsAsFactors = F)
  colnames(x.compl) <- c("Cetnost")
  x.compl$Procent <- x2
  x.compl
}

Perc.summary(svl2$Kraj)
Perc.summary(svl2$L.8)
a<- data.frame(Perc.summary(factor(svl2$Pocet.incidencnich.zaznamu)))

y4 <- setNames(cbind(rownames(y4), y4, row.names = NULL), c("kategorie", "Vloupani"))

# data frame s kategorii, cetnosti a procenty
Perc.summary2 <- function(x){
  x1<- summary(x)
  x2<- getPercentString(summary(x), NROW(x))
  x.compl <- data.frame(unclass(summary(x)), check.names = F, stringsAsFactors = F)
  x.compl <- setNames(cbind(rownames(x.compl), x.compl, x2, row.names=NULL), c("kategorie", "cetnost", "procent"))
  x.compl
}

Perc.summary2(svl2$B.2)

