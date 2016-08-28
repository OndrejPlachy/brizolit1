# download file from Google Spreadsheet
install.packages("googlesheets")
library(googlesheets)

setwd("C:/Users/Pavouk/Dropbox/R Studio dokuments/BRIZOLIT BEZPECNOST")

svl <- gs_title("dotaznikySVLbrizolit")
svl %>% gs_download(to = "svl.csv")

# 
svl <- read.csv("svl.csv", header = T)

# select
domacnost <- select(svl, L.7.1:L.7.15)

# jak to vyresit? 
# rozdelit F/M a cislo zamenit to za hodnoty Male Female a hodnoty z COHORT
# spocitat kolik jich je v kazde domacnosti - domacnost = radek


