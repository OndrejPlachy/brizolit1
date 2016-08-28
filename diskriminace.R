diskrim <- select(svl, K.1.A.lastyear:K.1.G.whenever)
summary(diskrim)

# nejdriv si predelam ty hodnoty
svl$K.1.A.lastyear <- factor(svl$K.1.A.lastyear, levels = c(1,2,3,4,5), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat"))
summary(svl$K.1.A.lastyear)

# vytvorim si tabulku
K.1.A.lastyear <- data.frame(unclass(summary(diskrim$K.1.A.lastyear)), check.names = FALSE, stringsAsFactors = FALSE)
K.1.A.lastyear <- setNames(cbind(rownames(K.1.A.lastyear), y, row.names = NULL), c("kategorie", "diskriminace v praci"))
K.1.A.lastyear
