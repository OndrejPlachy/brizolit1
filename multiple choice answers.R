setwd("C:/Users/Pavouk/Dropbox/R Studio dokuments/BRIZOLIT BEZPECNOST")
## SVL

# J.3.1 - jake drogy
# POZOR MULTIPLE CHOICE
drogy <- select(svl2, J.3.1)
drogy

library(stringr)
drogy <- str_split_fixed(drogy$J.3.1, ",", 5)
apply(drogy, 2, summary)

# J.2.2 - jake zlociny jste spachal/a
# POZOR MULTIPLE CHOICE
summary(svl$J.2.2)

# J.1.1 - za co jste byl/a odsouzen/a
# POZOR MULTIPLE CHOICE
summary(svl$J.1.1)

# K.2 - na koho byste se obratila v budoucnu pokud byste byl/a diskriminovan/a
# POZOR MULTIPLE CHOICE

# L.8.4 - matersky jazyk

# L.8.5 - narodnost, etnicita

## INCIDENCE

# E.1.4 - kdo byl/i pachatele