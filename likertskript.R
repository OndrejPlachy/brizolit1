svl <- read.csv("svl.csv", header = T)

# SOCIALNI A SPOLECENSKE PROBLEMY
socprobl <- select(svl2, C.1.1:C.1.8)
title.socprobl <- "Jak moc jste znepokojeni nasledujicimi spolecenskymi problemy?"


factorize <- function(df, colNames) {
  dl <- lapply(df, factor)
  return(as.data.frame(dl))
}

socprobl <- factorize(socprobl, C.1.1:C.1.8)
colnames(socprobl) <- c("zdravotnictvi", 
                        "ekonomika, zivotni uroven",
                        "zivotni prostredi",
                        "kriminalita", 
                        "nezamestnanost",
                        "politicka situace",
                        "pristehovalectvi",
                        "skolstvi")

socprobl.lik <- likert(socprobl, nlevels = 7)
summary(socprobl.lik)

plot(socprobl.lik, centered =T, wrap = 30) + ggtitle(title.socprobl)

plot(socprobl.lik, type = "heat")

# ZNEPOKOJENI MIROU KRIMINALITY
mira.kriminality <- select(svl2, C.3)
mira.kriminality <- factorize(mira.kriminality, C.3)
colnames(mira.kriminality) <- c("Znepokojeni mirou kriminality")
mira.kriminality.lik <- likert(mira.kriminality, nlevels = 7)
plot(mira.kriminality.lik, centered = T, wrap = 10)
plot(mira.kriminality.lik, type = "heat")

# DUVERA PREVENCE A PRAVNI VEDOMI
d.s <- select(svl2, B.2, B.5b, D.1:D.5)
colnames(d.s) <- c("pohlavi",
                   "vekova skupina",
  "Spokojenost s praci PCR",
  "Duvera v PCR",
  "Spokojenost s Mestskou policii",
  "Duvera v Mestskou policii",
  "Duvera v APK"
)
factorize(d.s)
d.s.lik <- likert(d.s[,c(3:7)], grouping = d.s$pohlavi)
plot(d.s.lik, centered =T, wrap = 30)


# vyreseno i s groupings
sp.prob <- svl2 %>% select(B.2, C.1.1:C.1.8)
sp.prob <- factorize(sp.prob)
colnames(sp.prob) <- c("pohlavi",
                       "vekova kohorta",
                       "zdravotnictvi", 
                        "ekonomika, zivotni uroven",
                        "zivotni prostredi",
                        "kriminalita", 
                        "nezamestnanost",
                        "politicka situace",
                        "pristehovalectvi",
                        "skolstvi")

sp.prob.lik <- likert(sp.prob[,c(3:10)], grouping = sp.prob$pohlavi)
plot(sp.prob.lik, centered =T, wrap = 30) + ggtitle(title.socprobl)
