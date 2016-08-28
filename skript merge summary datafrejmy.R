svl <- gs_title("dotaznikySVLbrizolit")
svl %>% gs_download(to = "svl.csv")
svl <- read.csv("svl.csv", header = T)

# VIKTIMIZACE DOMACNOSTI

svl$E.1.1 <- factor(svl$E.1.1)
y <- data.frame(unclass(summary(svl$E.1.1)), check.names = FALSE, stringsAsFactors = FALSE)
y <- setNames(cbind(rownames(y), y, row.names = NULL), c("kategorie", "Kradez auta"))
y

svl$E.1.5 <- factor(svl$E.1.5)
y2 <- data.frame(unclass(summary(svl$E.1.5)), check.names = FALSE, stringsAsFactors = FALSE)
y2 <- setNames(cbind(rownames(y2), y2, row.names = NULL), c("kategorie", "Kradez veci z auta"))
y2

svl$E.2.1 <- factor(svl$E.2.1)
y3 <- data.frame(unclass(summary(svl$E.2.1)), check.names = FALSE, stringsAsFactors = FALSE)
y3 <- setNames(cbind(rownames(y3), y3, row.names = NULL), c("kategorie", "Kradez kola, motorky, mopedu"))
y3

svl$E.3 <- factor(svl$E.3)
y4 <- data.frame(unclass(summary(svl$E.3)), check.names = FALSE, stringsAsFactors = FALSE)
y4 <- setNames(cbind(rownames(y4), y4, row.names = NULL), c("kategorie", "Vloupani"))
y4

svl$E.4 <- factor(svl$E.4)
y5 <- data.frame(unclass(summary(svl$E.4)), check.names = FALSE, stringsAsFactors = FALSE)
y5 <- setNames(cbind(rownames(y5), y5, row.names = NULL), c("kategorie", "Vandalismus, poskozovani cizi veci"))
y5

svl$E.5 <- factor(svl$E.5)
y6 <- data.frame(unclass(summary(svl$E.5)), check.names = FALSE, stringsAsFactors = FALSE)
y6 <- setNames(cbind(rownames(y6), y6, row.names = NULL), c("kategorie", "Pokus o vloupani"))
y6

svl$E.6 <- factor(svl$E.6)
y7 <- data.frame(unclass(summary(svl$E.6)), check.names = FALSE, stringsAsFactors = FALSE)
y7 <- setNames(cbind(rownames(y7), y7, row.names = NULL), c("kategorie", "Zharstvi"))
y7

# funkce na merge vice datafrejmu
MyMerge <- function(x, y){
  df <- merge(x, y, by = "kategorie", all.x= TRUE, all.y= TRUE)
  return(df)
}
prehledviktimizacedomacnosti <- Reduce(MyMerge, list(y,y2,y3,y4,y5,y6,y7))
# nahradim NA nulou
prehledviktimizacedomacnosti[is.na(prehledviktimizacedomacnosti)] <- 0
prehledviktimizacedomacnosti


# VIKTIMIZACE OSOBY + HATECRIME
svl$F.1 <- factor(svl$F.1)
x <- data.frame(unclass(summary(svl$F.1)), check.names = FALSE, stringsAsFactors = FALSE)
x <- setNames(cbind(rownames(x), x, row.names = NULL), c("kategorie", "Kradez prosta"))
x

svl$F.2 <- factor(svl$F.2)
x2 <- data.frame(unclass(summary(svl$F.2)), check.names = FALSE, stringsAsFactors = FALSE)
x2 <- setNames(cbind(rownames(x2), x2, row.names = NULL), c("kategorie", "Loupezne prepadeni"))
x2

svl$F.3 <- factor(svl$F.3)
x3 <- data.frame(unclass(summary(svl$F.3)), check.names = FALSE, stringsAsFactors = FALSE)
x3 <- setNames(cbind(rownames(x3), x3, row.names = NULL), c("kategorie", "Vyhrozovani nasilim"))
x3

svl$F.4 <- factor(svl$F.4)
x4 <- data.frame(unclass(summary(svl$F.4)), check.names = FALSE, stringsAsFactors = FALSE)
x4 <- setNames(cbind(rownames(x4), x4, row.names = NULL), c("kategorie", "Fyzicke napadeni"))
x4

svl$F.5 <- factor(svl$F.5)
x5 <- data.frame(unclass(summary(svl$F.5)), check.names = FALSE, stringsAsFactors = FALSE)
x5 <- setNames(cbind(rownames(x5), x5, row.names = NULL), c("kategorie", "Sexualni obtezovani"))
x5

svl$F.6 <- factor(svl$F.6)
x6 <- data.frame(unclass(summary(svl$F.6)), check.names = FALSE, stringsAsFactors = FALSE)
x6 <- setNames(cbind(rownames(x6), x6, row.names = NULL), c("kategorie", "Sexualni zneuziti"))
x6

svl$F.7 <- factor(svl$F.7)
x7 <- data.frame(unclass(summary(svl$F.7)), check.names = FALSE, stringsAsFactors = FALSE)
x7 <- setNames(cbind(rownames(x7), x7, row.names = NULL), c("kategorie", "Podvod"))
x7

svl$F.8 <- factor(svl$F.8)
x8 <- data.frame(unclass(summary(svl$F.8)), check.names = FALSE, stringsAsFactors = FALSE)
x8 <- setNames(cbind(rownames(x8), x8, row.names = NULL), c("kategorie", "Lichva"))
x8

svl$F.9 <- factor(svl$F.9)
x9 <- data.frame(unclass(summary(svl$F.9)), check.names = FALSE, stringsAsFactors = FALSE)
x9 <- setNames(cbind(rownames(x9), x9, row.names = NULL), c("kategorie", "Vydirani, vyhrozovani"))
x9

svl$F.10 <- factor(svl$F.10)
x10 <- data.frame(unclass(summary(svl$F.10)), check.names = FALSE, stringsAsFactors = FALSE)
x10 <- setNames(cbind(rownames(x10), x10, row.names = NULL), c("kategorie", "Psychicke nasili"))
x10

svl$F.11 <- factor(svl$F.11)
x11 <- data.frame(unclass(summary(svl$F.11)), check.names = FALSE, stringsAsFactors = FALSE)
x11 <- setNames(cbind(rownames(x11), x11, row.names = NULL), c("kategorie", "Sikana, tyrani"))
x11

svl$F.12 <- factor(svl$F.12)
x12 <- data.frame(unclass(summary(svl$F.12)), check.names = FALSE, stringsAsFactors = FALSE)
x12 <- setNames(cbind(rownames(x12), x12, row.names = NULL), c("kategorie", "Korupce"))
x12

svl$F.13 <- factor(svl$F.13)
x13 <- data.frame(unclass(summary(svl$F.13)), check.names = FALSE, stringsAsFactors = FALSE)
x13 <- setNames(cbind(rownames(x13), x13, row.names = NULL), c("kategorie", "Vystaveni nelegalnim drogam"))
x13

svl$F.14 <- factor(svl$F.14)
x14 <- data.frame(unclass(summary(svl$F.14)), check.names = FALSE, stringsAsFactors = FALSE)
x14 <- setNames(cbind(rownames(x14), x14, row.names = NULL), c("kategorie", "Obchod s lidmi, sex. vykoristovani"))
x14

svl$F.15 <- factor(svl$F.15)
x15 <- data.frame(unclass(summary(svl$F.15)), check.names = FALSE, stringsAsFactors = FALSE)
x15 <- setNames(cbind(rownames(x15), x15, row.names = NULL), c("kategorie", "Obchod s lidmi, prac. vykoristovani"))
x15

svl$G.1 <- factor(svl$G.1)
x16 <- data.frame(unclass(summary(svl$G.1)), check.names = FALSE, stringsAsFactors = FALSE)
x16 <- setNames(cbind(rownames(x16), x16, row.names = NULL), c("kategorie", "Hate crime"))
x16

# funkce na merge vice datafrejmu
MyMerge <- function(x, y){
  df <- merge(x, y, by = "kategorie", all.x= TRUE, all.y= TRUE)
  return(df)
}
prehledviktimizaceosoby <- Reduce(MyMerge, list(x, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16))
prehledviktimizaceosoby
# nahradim NA nulou
prehledviktimizaceosoby[is.na(prehledviktimizaceosoby)] <- 0

