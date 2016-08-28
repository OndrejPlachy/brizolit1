require(dplyr)
require(ggplot2)
require(psych)
require(reshape)
require(reshape2)
require(likert)
require(knitr)
require(plyr)

getwd()
setwd("C:/Users/Pavouk/Dropbox/R Studio dokuments/BRIZOLIT BEZPECNOST")

#stahnout data rovnou z googlespreadsheets
install.packages("googlesheets")
require(googlesheets)
#ukaze mi, jaky tam jsou ulozeny soubory
gs_ls()
#stahnu si to k sobe
svl <- gs_title("dotaznikySVLbrizolit")
svl %>% gs_download(to = "svl.csv")

nesvl <- gs_title("dotaznikyNESVLbrizolit")
nesvl %>% gs_download(to="nesvl.csv")

#nactu to
svl <- read.csv("svl.csv", header = T)

nesvl <- read.csv("inci.csv", header = T)

# SCREENOVACI CAST
# B.2 - pohlavi
svl$B.2 <- factor(svl$B.2, levels = c(1,2), labels = c("Male", "Female"))
summary(svl$B.2)
# B.5 - vekove kohorty
svl$B.5b <- factor(svl$B.5b, levels = c(2:6), labels = c("15-19", "20-29", "30-44", "45-59", "60+"))
summary(svl$B.5b)
summary(svl$B.5a)

# STRACH Z KRIMINALITY
# C.2.1 - mira kriminality v CR
svl$C.2.1 <- factor(svl$C.2.1, levels = c(1,2,3,98), labels = c("snizuje", "zvysuje", "je stejna", "nevi (zije v CR mene nez 5 let)"))
summary(svl$C.2.1)
# C.2.2 - mira kriminality v obci XY
svl$C.2.2 <- factor(svl$C.2.2, levels = c(1,2,3,98), labels = c("snizuje", "zvysuje", "je stejna", "nevi (zije v obci mene nez 5 let)"))
summary(svl$C.2.2)
# C.3 - znepokojeni mirou kriminality
summary(svl$C.3)
# C.4 - strach v noci na ulici
summary(svl$C.4)

# DUVERA PREVENCE PRAVNI VEDOMI
# D.1 - spokojenost s PCR
summary(svl$D.1)
# D.2 - duvera PCR
summary(svl$D.2)
# D.3 - spokojenost s MP
summary(svl$D.3)
# D.4 - duvera MP
summary(svl$D.4)
# D.5
summary(svl$D.5)
# D.6 - znate prevent akce?
summary(svl$D.6)
# D.7 - spokojenost se samospravou
summary(svl$D.7)
# D.8 - na koho byste se obratili v pripade ohrozeni
svl$D.8 <- factor(svl$D.8, levels = c(1,2,3,4,5,6,7,8,9,98), labels = c("Policie CR", "Mestska Policie", "Mestsky urad", "konkretni prislusnik PCR", "konkretni straznik MP", "zastupitel mesta", "starosta", "jina osoba", "svepomoc", "nevim"))
summary(svl$D.8)

# VIKTIMIZACE DOMACNOSTI
# E.1 - Automobil - vlastnictvi
svl$E.1 <- factor(svl$E.1, levels = c(1,2), labels = c("Ano", "Ne"))
summary(svl$E.1)
# Automobil - kradez
svl$E.1.1 <- factor(svl$E.1.1, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$E.1.1)
# E.1.2 - Automobil - kradez, kdy naposledy?
svl$E.1.2 <- factor(svl$E.1.2, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$E.1.2)
# E.1.3 - kolikrat za posl. rok kradez auta
svl$E.1.3 <- as.integer(svl$E.1.3)
summary(svl$E.1.3)
# E.1.4 - Kradez veci z motoroveho prostredku
svl$E.1.4 <- factor(svl$E.1.4, levels = c(1,2,3), labels = c("Ano, mne osobne", "Ano, nekomu ze clenu domacnosti", "Ne"))
summary(svl$E.1.4)
# E.1.5 - kolikrat kradez veci z motoroveho prostredku
svl$E.1.5 <- factor(svl$E.1.5, levels = c(1:5), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat"))
summary(svl$E.1.5)
# E.1.6 - kdy k tomu naposledy doslo ke kradezi veci z motoroveho prostredku
svl$E.1.6 <- factor(svl$E.1.6, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$E.1.6)
# E.1.7 - kolikrat za posl. rok kradez veci z motoroveho prostredku
summary(svl$E.1.7)
# E.2 - kradez kola, motocyklu, mopedu
svl$E.2 <- factor(svl$E.2, levels = c(1,2,3), labels = c("Ano, mne osobne", "Ano, nekomu ze clenu domacnosti", "Ne"))
summary(svl$E.2)
# E.2.1 - kdy naposledy kradez kola, motorky, mopedu
svl$E.2.1 <- factor(svl$E.2.1, levels = c(1:5), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat"))
summary(svl$E.2.1)
# E.2.2 - kradez kola, motorky, mopedu, kdy k tomu naposledy doslo
svl$E.2.2 <- factor(svl$E.2.2, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$E.2.2)
# E.2.3 kolikrat za posl. rok kradez kola, motorky, mopedu
summary(svl$E.2.3)
# E.3 - vloupani
svl$E.3 <- factor(svl$E.3, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$E.3)
# E.3.1 - vloupani, kdy naposled
svl$E.3.1 <- factor(svl$E.3.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$E.3.1)
# E.3.2 - kolikrat za posl. rok vloupani 
summary(svl$E.3.2)
# E.4 - Vandalismus
svl$E.4 <- factor(svl$E.4, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$E.4)
# E.4.1 - Vandalismus, kdy naposled
svl$E.4.1 <- factor(svl$E.4.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$E.4.1)
# E.4.2 - kolikrat za posl. rok vandalismus
summary(svl$E.4.2)
# E.5 - pokus o vloupani
svl$E.5 <- factor(svl$E.5, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$E.5)
# E.5.1 - pokus o vloupani, kdy naposled
svl$E.5.1 <- factor(svl$E.5.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$E.5.1)
# E.5.2 - kolikrat za posl. rok pokus o vloupani
summary(svl$E.5.2)
# E.6 - zharstvi
svl$E.6 <- factor(svl$E.6, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$E.6)
# E.6.1 - zharstvi, kdy naposled
svl$E.6.1 <- factor(svl$E.6.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$E.6.1)
# E.6.2 - kolikrat za posl. rok zharstvi
summary(svl$E.6.2)

# VIKTIMIZACE OSOBY
# F.1 - kradez prosta
svl$F.1 <- factor(svl$F.1, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.1)
# F.1.1 - kradez prosta, kdy naposled
svl$F.1.1 <- factor(svl$F.1.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.1.1)
# F.1.2 - kolikrat za posl. rok kradez prosta
summary(svl$F.1.2)
# F.2 - Loupezne prepadeni
svl$F.2 <- factor(svl$F.2, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.2)
# F.2.1 - Loupezne prepadeni, kdy naposled
svl$F.2.1 <- factor(svl$F.2.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.2.1)
# F.2.2 - kolikrat za posl. rok loupezne prepadeni
summary(svl$F.2.2)
# F.3 - vyhrozovani nasilim
svl$F.3 <- factor(svl$F.3, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.3)
# F.3.1 - vyhrozovani nasilim, kdy naposled
svl$F.3.1 <- factor(svl$F.3.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.3.1)
# F.3.2 - kolikrat za posl rok vyhrozovani nasilim
summary(svl$F.3.2)
# F.4 - Fyzicke napadeni
svl$F.4 <- factor(svl$F.4, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.4)
# F.4.1 - Fyzicke napadeni, kdy naposled
svl$F.4.1 <- factor(svl$F.4.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.4.1)
# F.4.2 - kolikrat za posl. rok fyzicke napadeni
summary(svl$F.4.2)
# F.5 - sexualni obtezovani
svl$F.5 <- factor(svl$F.5, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.5)
# F.5.1 - sexualni obtezovani, kdy naposled
svl$F.5.1 <- factor(svl$F.5.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.5.1)
# F.5.2 - kolikrat za posl. rok sexualni obtezovani
summary(svl$F.5.2)
# F.6 - sexualni zneuziti, znasilneni
svl$F.6 <- factor(svl$F.6, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.6)
# F.6.1 - sexualni zneuziti, znasilneni, kdy naposled
svl$F.6.1 <- factor(svl$F.6.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.6.1)
# F.6.2 - kolikrat za posl. rok znasilneni, zneuziti
summary(svl$F.6.2)
# F.7 - podvod
svl$F.7 <- factor(svl$F.7, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.7)
# F.7.1 - podvod  kdy naposled
svl$F.7.1 <- factor(svl$F.7.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.7.1)
# F.7.2 kolikrat za posl. rok podvod
summary(svl$F.7.2)
# F.8 - lichva
svl$F.8 <- factor(svl$F.8, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.8)
# F.8.1 - lichva kdy naposled
svl$F.8.1 <- factor(svl$F.8.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.8.1)
# F.8.2 - kolikrat za posl. rok lichva
summary(svl$F.8.2)
# F.9 - vydirani, vyhrozovani
svl$F.9 <- factor(svl$F.9, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.9)
# F.9.1 - vydirani, vyhrozovani, kdy naposled
svl$F.9.1 <- factor(svl$F.9.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.9.1)
# F.9.2 - kolikrat za posl. rok vydirani vyhrozovani
summary(svl$F.9.2)
# F.10 - Psychicke nasili 
svl$F.10 <- factor(svl$F.10, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.10)
# F.10.1 - Psychicke nasili, kdy naposled
svl$F.10.1 <- factor(svl$F.10.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.10.1)
# F.10.2 - kolikrat za posl. rok psychicke nasili, vyhrozovani
summary(svl$F.10.2)
# F.11 - sikana, tyrani
svl$F.11 <- factor(svl$F.11, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.11)
# F.11.1 - sikana, tyrani, kdy naposled
svl$F.11.1 <- factor(svl$F.11.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.11.1)
# F.11.2 - kolikrat za posl. rok sikana, tyrani
summary(svl$F.11.2)
# F.12 - korupce
svl$F.12 <- factor(svl$F.12, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.12)
# F.12.1 - korupce, kdy naposled
svl$F.12.1 <- factor(svl$F.12.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.12.1)
# F.12.2 - kolikrat za posl. rok korupce
summary(svl$F.12.2)
# F.13 - vystaveni nelegalnim drogam
svl$F.13 <- factor(svl$F.13, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.13)
# F.13.1 - vystaveni nelegalnim drogam, kdy naposled
svl$F.13.1 <- factor(svl$F.13.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.13.1)
# F.13.2 - kolikrat za posl. rok vystaveni nelegalnim drogam
summary(svl$F.13.2)
# F.14 - obchodovani s lidmi za ucelem sex. vykoristovani
svl$F.14 <- factor(svl$F.14, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.14)
# F.14.1 - obchodovani s lidmi za ucelem sex. vykoristovani, kdy naposled
svl$F.14.1 <- factor(svl$F.14.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.14.1)
# F.14.2 - kolikrat za posl. rok obchodovani s lidmi za ucelem sex. vykoristovani
summary(svl$F.14.2)
# F.15 - obchodovani s lidmi, pracovni vykoristovani
svl$F.15 <- factor(svl$F.15, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$F.15)
# F.15.1 - obchodovni s lidmi, pracovni vykoristovani, kdy naposled
svl$F.15.1 <- factor(svl$F.15.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$F.15.1)
# F.15.2 - kolikrat za posl. rok obchodovani s lidmi, pracovni vykoristovani
summary(svl$F.15.2)

# NASILI Z NENAVISTI
# G.1 - nasili z nenavisti
svl$G.1 <- factor(svl$G.1, levels = c(1:6), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Nikdy"))
summary(svl$G.1)
# G.1.1 - nasili z nenavisti, kdy naposled
svl$G.1.1 <- factor(svl$G.1.1, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$G.1.1)
# G.1.2 - kolikrat za posl. rok nasili z nenavisti
summary(svl$G.1.2)

# JINA KRIMINALITA
# H.1 -stal jste se obeti jineho typu kriminality
svl$H.1 <- factor(svl$H.1, levels = c(1,2), labels = c("Ano", "Ne"))
summary(svl$H.1)
# H.1.2 - jina kriminalita kolikrat
svl$H.1.2 <- factor(svl$H.1.2, levels = c(1:5), labels = c("Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat"))
summary(svl$H.1.2)
# H.1.3 - jina kriminalita kdy naposledy
svl$H.1.3 <- factor(svl$H.1.3, levels = c(1,2,3), labels = c("V poslednich dvanacti mesicich", "V poslednich peti letech", "Vice nez pred peti lety"))
summary(svl$H.1.3)

# KRIMINALIZACE A STIGMATIZACE
# I.1 - jak casto Policie v okoli bydliste
svl$I.1 <- factor(svl$I.1, levels = c(1,2,3,4,5,6,7), labels = c("Jednou denne", "Dvakrat a vicekrat za den", "Dvakrat, trikrat za tyden", "Jednou za tyden", "Jednou za dva tydny", "Jednou za mesic", "Mene nez jednou za mesic"))
summary(svl$I.1)
# I.2 - kolikrat zjistovala totoznost policie
svl$I.2 <- factor(svl$I.2, levels = c(1,2,3,4,5,6), labels = c("Nikdy", "Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat"))
summary(svl$I.2)
# I.3 - kolikrat dostal blokovou pokutu
svl$I.3 <- factor(svl$I.3, levels = c(1,2,3,4,5,6), labels = c("Nikdy", "Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat"))
summary(svl$I.3)
# I.4 - jak casto socialka
svl$I.4 <- factor(svl$I.4, levels = c(1,2,3,4,5,6), labels = c("Nikdy", "Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat"))
summary(svl$I.4)
# I.5 - odebrani ditete
svl$I.5 <- factor(svl$I.5, levels = c(1,2), labels = c("Ano", "Ne"))
summary(svl$I.5)
# I.6 - policie zameruje vic na misto bydliste
svl$I.6 <- factor(svl$I.6, levels = c(1,2), labels = c("Ano", "Ne"))
summary(svl$I.6)
# I.6.1 - je spravne ze se PCR zameruje na vase misto bydliste
svl$I.6.1 <- factor(svl$I.6.1, levels = c(1,2,3,4,5,6,7), labels = c("1", "2", "3", "4", "5", "6", "7"))
summary(svl$I.6.1)

# SELF-REPORT KRIMINALITA
# J.1 - byl jste nekdy ve vezeni
svl$J.1 <- factor(svl$J.1, levels = c(1,2), labels = c("Ano", "Ne"))
summary(svl$J.1)

# J.2 <- spachal jste nekdy nejaky trestny cin
svl$J.2 <- factor(svl$J.2, levels = c(1,2), labels = c("Ano", "Ne"))
summary(svl$J.2)
# J.2.1 - spachal jste nekdy nejaky trestny cin za posledni rok
# vyhodit odpoved nikdy (0) a nechat tam jenom NA
svl$J.2.1 <- factor(svl$J.2.1, levels = c(0,1,2,3,4,5,99), labels = c("Nikdy", "Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Odmitl/a odpovedet"))
summary(svl$J.2.1)

# J.3 - uzival/a jste nekdy drogy
svl$J.3 <- factor(svl$J.3, levels = c(1,2,3), labels = c("Ano, uzivam", "Ano, uzival/a", "Ne"))
summary(svl$J.3)


# DISKRIMINACE
# K.1 - kolikrat jste byli diskriminovani
svl$K.1 <- factor(svl$K.1, levels = c(0,1,2,3,4,5,99), labels = c("Nikdy", "Jednou", "Dvakrat", "Trikrat", "Ctyrikrat", "Petkrat a vicekrat", "Odmitl/a odpovedet"))
summary(svl$K.1)
# K.1.1 - nahlasili jste diskriminaci
svl$K.1.1 <- factor(svl$K.1.1, levels = c(1,2,3), labels = c("Ano, vsechny pripady", "Ano, nektere pripady", "Ne"))
summary(svl$K.1.1)

# DEMOGRAFICKE UDAJE
# L.1 - Status
svl$L.1 <- factor(svl$L.1, levels = c(1,2,3,4,5), labels = c("Svobodny/a", "Zenaty/a", "Nesezdany/a, v partnerskem vztahu", "Rozvedeny/a", "Vdovec/vdova"))
summary(svl$L.1)
# L.2 - Vzdelani
svl$L.2 <- factor(svl$L.2, levels = c(1,2,3,4,5), labels = c("Zakladni", "Stredni bez maturity", "Stredni s maturitou", "Vyssi odborne", "Vysokoskolske"))
summary(svl$L.2)
# L.3 - delka bydleni na adrese
svl$L.3 <- factor(svl$L.3, levels = c(1,2,3,4,5,6,7,8), labels = c("Mene nez 1 mesic", "1 mesic az 6 mesicu", "6 mesicu az 1 rok", "1 rok az 5 let", "6-10 let", "11 az 20 let", "Vice nez 20 let", "Od narozeni"))
summary(svl$L.3)
# L.4 - delka bydleni v obci
svl$L.4 <- factor(svl$L.4, levels = c(1,2,3,4,5,6,7,8), labels = c("Mene nez 1 mesic", "1 mesic az 6 mesicu", "6 mesicu az 1 rok", "1 rok az 5 let", "6-10 let", "11 az 20 let", "Vice nez 20 let", "Od narozeni"))
summary(svl$L.4)
# L.4.1 - predchozi bydliste
summary(svl$L.4.1)
# L.5 - kategorie bydleni
svl$L.5 <- factor(svl$L.5, levels = c(1,2,3,4,5,6,7,8), labels = c("Najemni bytovy dum obecny", "Najemni bytovy dum soukromy", "Byt v osobnim/druzstevnim vlastnictvi", "Rodinny dum soukromy", "Rodinny dum najemni", "Ubytovna", "Jiny dum k bydleni bez najemni smlouvy", "Objekt neurcen k bydleni"))
summary(svl$L.5)
# L.6 - vyse najmu
svl$L.6 <- factor(svl$L.6, levels = c(1,2,3,4,5,6,7,8,9,98,99), 
                  labels = c("do 2.000,- Kc", "2.001 - 3.000,- Kc", "3.001 - 4.000,- Kc", "4.001 - 5.000,- Kc", "5.001 - 7.000,- Kc", "7.001 - 9000,- Kc", "9.001 - 10.000,- Kc", "10.001 - 15.000 Kc", "15.001 a vice", "Nevi", "Odmitl/a uvest"))
summary(svl$L.6)
# L.8 - ekonomicky status
svl$L.8 <- factor(svl$L.8, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
                  labels = c("Zena v domacnosti", 
                             "Na materske, rodicovske dovolene", 
                             "Starobni duchodce", 
                             "Invalidni duchodce",
                             "Studujici", 
                             "Nezamestnany", 
                             "Delnik nekvalifikovany", 
                             "Delnik kvalifikovany", 
                             "Nizsi administrativni pracovnik", 
                             "Nizsi odborny pracovnik", 
                             "Vedouci administrativne-technicky pracovnik", 
                             "VS odborny a vedecky pracovnik (ucitel, lekar, pravnik ap.)", 
                             "OSVC", 
                             "Ostatni"))
summary(svl$L.8)
# L.8.1 - mate legalni smlouvu
svl$L.8.1 <- factor(svl$L.8.1, levels = c(1,2), labels = c("Ano", "Ne"))
summary(svl$L.8.1)

# L.8.2 - prijem domacnosti
svl$L.8.2 <- factor(svl$L.8.2, levels = c(1,2,3,4,5,6,7,8,9,10,11,98,99), 
                    labels = c("do 6.000,- Kc",
                               "6.001 - 8.000,- Kc",
                               "8.001 - 10.000,- Kc",
                               "10.001 - 12.000,- Kc",
                               "12.001 - 14.000,- Kc",
                               "14.001 - 17.000,- Kc",
                               "17.001 - 20.000,- Kc",
                               "20.001 - 25.000,- Kc",
                               "25.001 - 35.001,- Kc",
                               "35.001 - 50.000,- Kc",
                               "50.001 a vice",
                               "Nevi",
                               "Odmitl/a uvest"))
summary(svl$L.8.2)
# L.8.3 - obcanstvi
svl$L.8.3 <- factor(svl$L.8.3, level = c(1,2,3), labels = c("Ceske", "Slovenske", "Jine"))
summary(svl$L.8.3)


write.csv(svl, file = "svl2.csv")

