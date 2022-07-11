
options(stringsAsFactors = FALSE)

wardList <- read.csv("data-raw/mimupcode_ward.csv")
villageList <- read.csv("data-raw/mimupcode_village.csv")

townshipList <- data.frame()

for(i in unique(wardList$ts_pcode)) {
  x <- subset(wardList, ts_pcode == i)[1, ]
  townshipList <- rbind(townshipList, x)
}

townshipList <- townshipList[ , 1:4]
usethis::use_data(townshipList, compress = "xz", overwrite = TRUE)


