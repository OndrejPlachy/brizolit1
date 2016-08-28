require(dplyr)
require(ggplot2)
require(psych)
require(reshape)
require(reshape2)
require(likert)
require(knitr)

getwd()
setwd("C:/Users/Pavouk/Dropbox/R Studio dokuments/BRIZOLIT BEZPECNOST")

#stahnout data rovnou z googlespreadsheets
install.packages("googlesheets")
library(googlesheets)
#ukaze mi, jaky tam jsou ulozeny soubory
gs_ls()
#stahnu si to k sobe
inci <- gs_title("incidence")
inci %>% gs_download(to="inci.csv")

inci <- read.csv("inci.csv", header = T)

# UVOD
# A.1 - kategorie viktimizace
inci$A.1 <- factor(inci$A.1, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                   labels = c("Fyzicke napadeni",
                              "Korupce",
                              "Kradez automobilu",
                              "Kradez kola, motocyklu",
                              "Kradez prosta",
                              "Kradez veci z automobilu",
                              "Lichva",
                              "Loupezne prepadeni",
                              "Nasili z nenavisti",
                              "Obchodovani s lidmi",
                              "Podvod",
                              "Pokus o vloupani",
                              "Psychicke nasili",
                              "Sexualni obtezovani",
                              "Sexualni zneuziti, znasilneni",
                              "Pracovni vykoristovani",
                              "Sikana",
                              "Vandalismus",
                              "Vloupani",
                              "Vydirani",
                              "Vyhrozovani nasilim",
                              "Vystaveni nelegalnim drogam",
                              "Zharstvi"))
summary(inci$A.1)
# A.3 - jak hodnotite z hlediska zavaznosti, co se prihodilo
summary(inci$A.3)

# MISTO A CAS
# B.1 - v jakem mesici se to stalo
inci$B.1 <- factor(inci$B.1, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,98),
                   labels = c("Cervenec 2016",
                              "Cerven 2016",
                              "Kveten 2016",
                              "Duben 2016",
                              "Brezen 2016",
                              "Unor 2016",
                              "Leden 2016",
                              "Prosinec 2015",
                              "Listopad 2015",
                              "Rijen 2015",
                              "Zari 2015",
                              "Srpen 2015",
                              "Cervenec 2015",
                              "Cerven 2015",
                              "Kveten 2015",
                              "Duben 2015",
                              "Srpen 2016",
                              "Nevi"))
summary(inci$B.1)

# B.2 - co to bylo za den
inci$B.2 <- factor(inci$B.2, levels = c(1,2,3,4,5,6,7,98),
                   labels = c("Pondeli",
                              "Utery",
                              "Streda",
                              "Ctvrtek",
                              "Patek",
                              "Sobota",
                              "Nedele",
                              "Nevi"))
summary(inci$B.2)

# B.3 - v kolik to bylo hodin
inci$B.3 <- factor(inci$B.3, levels = c(1,2,3,4,5,6,98),
                   labels = c("Rano (4:00 az 8:00)",
                              "Dopoledne (8:00 az 12:00)",
                              "Odpoledne (12:00 az 16:00)",
                              "Navecer (16:00 az 20:00)",
                              "Vecer (20:00 az 24:00)",
                              "V noci (24:00 az 4:00)",
                              "Nevi"))
summary(inci$B.3)

# B.4 - stalo se to zde v obci
inci$B.4 <- factor(inci$B.4, levels = c(1,2,3,4),
                   labels = c("Ano",
                              "Ne, v jine obci ve stejnem kraji",
                              "Ne, v jine obci v CR",
                              "Ne, v zahranici"))
summary(inci$B.4)

# B.5 - na jakem miste se to presne stalo
inci$.B.5 <- factor(inci$B.5, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21),
                    labels = c("Doma",
                               "Doma na navsteve u partnera/partnerky",
                               "Doma u pribuznych",
                               "Doma u pratel, znamych",
                               "Doma u nekoho jineho",
                               "Na ulici v blizkosti/sousedstvi domova (v lokalite)",
                               "Na ulici",
                               "Ve skole",
                               "V praci",
                               "V parku",
                               "Na zahrade",
                               "Sklepni prostory, verejne prostory domu",
                               "Na nadrazi (vlak, autobus)",
                               "V restauraci, hospode, baru",
                               "Na diskotece, zabave, koncerte",
                               "V nocnim klubu, herne",
                               "V obchode",
                               "V nakupnim centru",
                               "Na parkovisti",
                               "Na ceste z prace, ze skoly",
                               "Jinde"))
summary(inci$B.5)

# MAJETKOVA SKODA
# C.1 - byla vam zpusobena nejaka skoda na majetku
inci$C.1 <- factor(inci$C.1, levels = c(1,2),
                   labels = c("Ano", "Ne"))
summary(inci$C.1)

# C.1.1 - kolik Kc byla skoda
summary(inci$C.1.1)

# C.1.2 - jak skodu hodnotite z hlediska zavaznosti
summary(inci$C.1.2)

# SKODA NA ZDRAVI
# D.1 - utrpel/a jste ujmu na zdravi
inci$D.1 <- factor(inci$D.1, levels = c(1,2),
                   labels = c("Ano", "Ne"))
summary(inci$D.1)

# D.1.1 - vyhledal/a jste lekarske osetreni
inci$D.1.1 <- factor(inci$D.1.1, levels = c(1,2,3),
                   labels = c("Ano ambulantne", "Ano, byl/a jsem hospitalizovany/a", "Ne"))
summary(inci$D.1.1)

# D.1.2 - jak dlouho trvalo leceni
inci$D.1.2 <- factor(inci$D.1.2, levels = c(1,2,3,4,5,6,7,8),
                     labels = c("Nebyl/a lecena",
                                "Mene nez tyden",
                                "Tyden",
                                "Dva tydny",
                                "Dva tydny az mesic",
                                "Mesic az ctvrt roku",
                                "Ctvrt roku az pul roku",
                                "Vice nez pul roku"))
summary(inci$D.1.2)                       

# D.1.3 - mate nejake trvale nasledky 
inci$D.1.3 <- factor(inci$D.1.3, levels = c(1,2,3),
                     labels = c("Ano, fyzicke", "Ano, psychicke", "Ne"))
summary(inci$D.1.3)

# D.1.4 - jak hodnotite ujmu na zdravi, ktera vam byla zpusobena
summary(inci$D.1.4)

# PACHATELE
# E.1 - kolik bylo pachatelu
inci$E.1 <- factor(inci$E.1, levels = c(1,2,3,4,5,6,7,99),
                   labels = c("Jeden",
                              "Dva",
                              "Tri",
                              "Ctyri",
                              "Pet a vic",
                              "Videl/a pachatele, ale neni si jist/a",
                              "Nevi, nevidel/a pachatele",
                              "Odmitl/a"))
summary(inci$E.1)                     

# E.1.1 - pohlavi pachatelu
inci$E.1.1 <- factor(inci$E.1.1, levels = c(1,2,3,98,99),
                   labels = c("Muz",
                              "Zena",
                              "Obojiho pohlavi",
                              "Nevi",
                              "Odmitl/a"))
summary(inci$E.1.1)

# E.1.2 - kolik bylo muzu a zen
inci$E.1.2 <- factor(inci$E.1.2, levels = c(1,2,3,4,98,99),
                     labels = c("Pocet muzu",
                                "Pocet zen",
                                "Nevi, ale prevazovali muzi",
                                "Nevi, ale prevazovaly zeny",
                                "Nevi",
                                "Odmitl/a"))
summary(inci$E.1.2)
# E.1.2male - odpoved Pocet muzu 
summary(inci$E.1.2male)
# E.1.2female - odpoved Pocet zen 
summary(inci$E.1.2female)
                                
# E.1.3 - jak stari byli pachatele
inci$E.1.3 <- factor(inci$E.1.3, levels = c(1,2,3,4,5,6,7,8,98,99),
                   labels = c("Dite, deti (12 let a mladsi)",
                              "Nactilety, nactileti (13-17)",
                              "Mlady dospely, mladi dospeli (18-25)",
                              "Dospely, dospeli (26-35)",
                              "Dospely, dospeli (36-45)",
                              "Starsi dospely, dospeli (46-65)",
                              "Duchodce, duchodci (65 a vice)",
                              "Ruzneho veku",
                              "Nevi",
                              "Odmitl/a"))
summary(inci$E.1.3)                              
# E.1.3a - vypis veku pachatelu
summary(inci$E.1.3a)

# E.1.4 - kdo byl pachatelem
# multiple choice answers

# E.1.5a - jake byl pachatel etnicity
summary(inci$E.1.5a)

# ZBRAN
# F.1 - mel u sebe nejakou zbran/zbrane
inci$F.1 <- factor(inci$F.1, levels = c(1,2),
                   labels = c("Ano", "Ne"))
summary(inci$F.1)
# F.1.1 - o jakou zbran se jednalo
inci$F.1.1 <- factor(inci$F.1.1, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,98,99),
                   labels = c("Baseballova palka",
                              "Kamen, cihla",
                              "Klacek",
                              "Kyselina",
                              "Lahev",
                              "Maceta",
                              "Nuz",
                              "Obranny sprej",
                              "Obusek, tonfa",
                              "Paralyzer",
                              "Plynova pistole",
                              "Strelna zbran dlouha",
                              "Strelna zbran kratka",
                              "Tyc",
                              "Jina",
                              "Nevi",
                              "Odmitl/a"))
summary(inci$F.1.1)

# F.1.2 - vyhrozovali vam pouzitim teto zbrane
inci$F.1.2 <- factor(inci$F.1.2, levels = c(1,2),
                    labels = c("Ano",
                               "Ne"))
summary(inci$F.1.2)

# F.1.3 - pouzili tyto zbrane
inci$F.1.3 <- factor(inci$F.1.3, levels = c(1,2),
                     labels = c("Ano",
                                "Ne"))
summary(inci$F.1.3)

# MOTIVACE, HLASENI
# G.1 - motivace etnickou, narodnostni, rasovou atd. identitou
inci$G.1 <- factor(inci$G.1, levels = c(1,2),
                   labels = c("Ano",
                              "Ne"))
summary(inci$G.1)

# G.2 - nahlasila jste udalost na PCR
inci$G.2 <- factor(inci$G.2, levels = c(1,2),
                   labels = c("Ano",
                              "Ne"))
summary(inci$G.2)

# G.2.1 - oznamil jste danou udalost nekomu jinemu nez policii
inci$G.2.1 <- factor(inci$G.2.1, levels = c(1,2,3),
                   labels = c("Ano, zastupci jine organizace/instituce",
                              "Ne, jenom svym nejblizsim",
                              "Ne, vubec nikomu"))
summary(inci$G.2.1)

# G.2.3 - proc jste udalost nenahlasil/a policii
inci$G.2.3 <- factor(inci$G.2.3, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
                     labels = c("Nekdo jiny to ohlasil policii",
                                "Nestalo to za to",
                                "Mel/a jsem strach z pachatelu",
                                "Mel/a jsem strach z rodiny/pratel pachatelu",
                                "Chranil/a jsem tim pachatele",
                                "Neduveruji Policii",
                                "Policie by s tim nic neudelala",
                                "Nejsem udavac",
                                "Dokazu si to vyridit sam/sama",
                                "Nechtel/a jsem se dostat do problemu",
                                "Nechtel/a jsem, aby se to nekdo dozvedel",
                                "Stydel jsem se",
                                "Domnivam se, ze sam jsem jednal/a nezakonne",
                                "Neni to moje starost (ma to resit nekdo jiny - ucitel, majitel domu)",
                                "Jina",
                                "Nevim na koho jsem se mel/a obratit",
                                "Odmitl/a"
                                ))
summary(inci$G.2.3)

# REAKCE POLICIE
# H.2 - byla policie ochotna cin resit
inci$H.2 <- factor(inci$H.2, levels = c(1,2),
                   labels = c("Ano",
                              "Ne"))
summary(inci$H.2)

# H.3 - byla udalost posouzena jako trestny cin
inci$H.3 <- factor(inci$H.3, levels = c(1,2,3,4),
                   labels = c("Ano",
                              "Ne, jako prestupek",
                              "Ne",
                              "Nevim"))
summary(inci$H.3)

# H.4 - byl pachatel/e dopaden/i
inci$H.4 <- factor(inci$H.4, levels = c(1,2,3,4),
                   labels = c("Ano",
                              "Ano, ale ne vsichni",
                              "Ne",
                              "Nevim"))
summary(inci$H.4)

# H.5 - byl pachatel/e souzen/i
inci$H.5 <- factor(inci$H.5, levels = c(1,2,3,4),
                   labels = c("Ano",
                              "Ano, ale ne vsichni",
                              "Ne",
                              "Nevim"))
summary(inci$H.5)

# H.6 - jak hodnotite postup policie?
summary(inci$H.6)
