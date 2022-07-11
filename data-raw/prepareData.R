################################################################################
#
# load libraries
#
################################################################################

if(!require(openxlsx)) install.packages("openxlsx")
if(!require(remotes)) install.packages("remotes")
if(!require(myanmarMCCTdata)) remotes::install_github("validmeasures/myanmarMCCTdata")

################################################################################
#
# retrieve raw dataset with all repeats - main form
#
################################################################################

rep.names <- c("grp_hh",              ## HH members dataset
               "support_gov_rep",     ## HH dataset
               "support_ngo_rep",     ## HH dataset
               "support_cso_rep",     ## HH dateset
               "support_ro_rep",      ## HH dataset
               "support_ho_rep",      ## HH dataset
               "support_orgoth_rep",  ## HH dataset
               "child_vc_rep",        ## Vaccination and child illness dataset
               "grp_q2_5_to_q2_7",    ## IYCF dataset
               "ancnow_rep",          ## ANC dataset
               "ancpast_rep")         ## ANC dataset

surveyData <- get_mcct_data(id = "baseline_mcct_final",
                            start = "2019-08-06",
                            username = "validmeasures",
                            password = "6Y2-8yK-Nmk-Lbf",
                            filename = "main_form",
                            rep = TRUE,
                            rep.name = rep.names)

## Create household data object
hh <- surveyData[[1]]

hh <- clean_hh(df = hh, checks = checks)

## Save as CSV for submission purposes
write.csv(hh, "data-raw/raw/household.csv", row.names = FALSE)

## Create HH xlsx file
rawData <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb = rawData, sheetName = "household")
openxlsx::writeData(wb = rawData, sheet = "household", x = hh)

## convert non-ASCII text
for(i in names(hh)) {
  hh[ , i][stringi::stri_enc_mark(str = hh[ , i]) != "ASCII"] <- "other for translation"
}

usethis::use_data(hh, compress = "xz", overwrite = TRUE)

## Create household data
#hh <- merge_repeats(x = surveyData, rep.name = "support_gov_rep")
#hh <- merge(hh, surveyData[[5]], by.x = "KEY", by.y = "PARENT_KEY", all.x = TRUE)
#hh <- merge(hh, surveyData[[4]], by.x = "KEY", by.y = "PARENT_KEY", all.x = TRUE)
#hh <- merge(hh, surveyData[[7]], by.x = "KEY", by.y = "PARENT_KEY", all.x = TRUE)
#hh <- merge(hh, surveyData[[6]], by.x = "KEY", by.y = "PARENT_KEY", all.x = TRUE)
#hh <- merge(hh, surveyData[[8]], by.x = "KEY", by.y = "PARENT_KEY", all.x = TRUE)

## convert non-ASCII text
#for(i in names(hh)) {
#  hh[ , i][stringi::stri_enc_mark(str = hh[ , i]) != "ASCII"] <- "other for translation"
#}

#usethis::use_data(hh, compress = "xz", overwrite = TRUE)

## Create household members dataset
hhMembers <- surveyData[[2]]

## Write as CSV
write.csv(hhMembers, "data-raw/raw/roster.csv", row.names = FALSE)

## Add to rawData.xlsx
openxlsx::addWorksheet(wb = rawData, sheet = "roster")
openxlsx::writeData(wb = rawData, sheet = "roster", x = hhMembers)

## convert non-ASCII text
for(i in names(hhMembers)) {
  hhMembers[ , i][stringi::stri_enc_mark(str = hhMembers[ , i]) != "ASCII"] <- "other for translation"
}

#hhMembers <- merge(hh, hhMembers, by.x = "KEY", by.y = "PARENT_KEY")
usethis::use_data(hhMembers, compress = "xz", overwrite = TRUE)

## Get raw child health dataset
childHealth <- surveyData[[9]]

## Write as CSV
write.csv(childHealth, "data-raw/raw/childHealth.csv", row.names = FALSE)

## Save into xlsx
openxlsx::addWorksheet(wb = rawData, sheet = "childHealth")
openxlsx::writeData(wb = rawData, sheet = "childHealth", x = childHealth)

## convert non-ASCII text
for(i in names(childHealth)) {
  childHealth[ , i][stringi::stri_enc_mark(str = childHealth[ , i]) != "ASCII"] <- "other for translation"
}

#childHealth <- merge(hh, childHealth, by.x = "KEY", by.y = "PARENT_KEY")
usethis::use_data(childHealth, compress = "xz", overwrite = TRUE)

## Create IYCF dataset
iycf <- surveyData[[10]]

## Write to CSV
write.csv(iycf, "data-raw/raw/iycf.csv", row.names = FALSE)

## Write to xlsx
openxlsx::addWorksheet(wb = rawData, sheetName = "iycf")
openxlsx::writeData(wb = rawData, sheet = "iycf", x = iycf)

## convert non-ASCII text
for(i in names(iycf)) {
  iycf[ , i][stringi::stri_enc_mark(str = iycf[ , i]) != "ASCII"] <- "other for translation"
}

usethis::use_data(iycf, compress = "xz", overwrite = TRUE)

## Create anc dataset - current pregnancy
anc1 <- surveyData[[11]]

## Write as CSV
write.csv(anc1, "data-raw/raw/anc1.csv", row.names = FALSE)

## Write as xlsx
openxlsx::addWorksheet(wb = rawData, sheetName = "anc1")
openxlsx::writeData(wb = rawData, sheet = "anc1", x = anc1)

## convert non-ASCII text
for(i in names(anc1)) {
  anc1[ , i][stringi::stri_enc_mark(str = anc1[ , i]) != "ASCII"] <- "other for translation"
}

usethis::use_data(anc1, compress = "xz", overwrite = TRUE)

## ANC for most recent pregnancy

anc2 <- surveyData[[12]]

## Write as CSV
write.csv(anc2, "data-raw/raw/anc2.csv", row.names = FALSE)

## Write as xlsx
openxlsx::addWorksheet(wb = rawData, sheetName = "anc2")
openxlsx::writeData(wb = rawData, sheet = "anc2", x = anc2)

## convert non-ASCII text
for(i in names(anc2)) {
  anc2[ , i][stringi::stri_enc_mark(str = anc2[ , i]) != "ASCII"] <- "other for translation"
}

usethis::use_data(anc2, compress = "xz", overwrite = TRUE)

###################### Get and process anthropometric dataset ##################

rep.names <- c("grp_family",
               "grp_hh",
               "childanthro_rep",
               "mom_anthro_rep")

anthroData <- get_mcct_data(id = "baseline_mcct_anthro_final",
                            start = "2019-08-06",
                            username = "validmeasures",
                            password = "6Y2-8yK-Nmk-Lbf",
                            filename = "anthro_form",
                            rep = TRUE,
                            rep.name = rep.names)

anthroDF <- anthroData[[1]]
anthroDF <- clean_anthro(df = anthroDF, checks = anthroChecks)

## convert non-ASCII text
for(i in names(anthroDF)) {
  anthroDF[ , i][stringi::stri_enc_mark(str = anthroDF[ , i]) != "ASCII"] <- "Burmese for translation"
}

## Write as CSV
write.csv(anthroDF, "data-raw/raw/anthroDF.csv", row.names = FALSE)

## Write as xlsx
openxlsx::addWorksheet(wb = rawData, sheetName = "anthroDF")
openxlsx::writeData(wb = rawData, sheet = "anthroDF", x = anthroDF)

usethis::use_data(anthroDF, compress = "xz", overwrite = TRUE)

#childAnthro <- merge_repeats(x = anthroData, rep.name = "childanthro_rep")
childAnthro <- anthroData[[4]]

## Write as CSV
write.csv(childAnthro, "data-raw/raw/childAnthro.csv", row.names = FALSE)

## Write as xlsx
openxlsx::addWorksheet(wb = rawData, sheetName = "childAnthro")
openxlsx::writeData(wb = rawData, sheet = "childAnthro", x = childAnthro)


## convert non-ASCII text
for(i in names(childAnthro)) {
  childAnthro[ , i][stringi::stri_enc_mark(str = childAnthro[ , i]) != "ASCII"] <- "Burmese for translation"
}

#childAnthro <- merge(hhMembers, childAnthro, by = "PARENT_KEY", all.y = TRUE)
#childAnthro <- merge(hh, childAnthro, by = "KEY", all = TRUE)
usethis::use_data(childAnthro, compress = "xz", overwrite = TRUE)

## Mother anthropometry

#motherAnthro <- merge_repeats(x = anthroData, rep.name = "mom_anthro_rep")
motherAnthro <- anthroData[[5]]

## Write as CSV
write.csv(motherAnthro, "data-raw/raw/motherAnthro.csv", row.names = FALSE)

## Write as xlsx
openxlsx::addWorksheet(wb = rawData, sheetName = "motherAnthro")
openxlsx::writeData(wb = rawData, sheet = "motherAnthro", x = motherAnthro)

## convert non-ASCII text
for(i in names(motherAnthro)) {
  motherAnthro[ , i][stringi::stri_enc_mark(str = motherAnthro[ , i]) != "ASCII"] <- "Burmese for translation"
}

#motherAnthro <- merge(hh, motherAnthro, by = "KEY", all.y = TRUE)
usethis::use_data(motherAnthro, compress = "xz", overwrite = TRUE)


############################ Village profile ###################################

rep.names <- c("respondent_rpt",
               "cbo_yes_grp",
               "credit_rep")

villageData <- get_mcct_data(id = "baseline_villprofile_final",
                             start = "2019-08-06",
                             username = "validmeasures",
                             password = "6Y2-8yK-Nmk-Lbf",
                             filename = "village_form",
                             rep = TRUE,
                             rep.name = rep.names)

villageData1 <- merge_repeats(x = villageData, rep.name = "respondent_rpt")
villageData2 <- merge_repeats(x = villageData, rep.name = "cbo_yes_grp")
villageData3 <- merge_repeats(x = villageData, rep.name = "credit_rep")

villageData <- merge(villageData1, villageData2, by = "KEY")
villageData <- merge(villageData, villageData3, by = "KEY")

villageData1 <- merge(townshipList, villageData1, by.x = "ts_pcode", by.y = "geo_town")
villageData2 <- merge(townshipList, villageData2, by.x = "ts_pcode", by.y = "geo_town")
villageData3 <- merge(townshipList, villageData3, by.x = "ts_pcode", by.y = "geo_town")

## Write as CSV
write.csv(villageData, "data-raw/raw/village.csv", row.names = FALSE)
write.csv(villageData1, "data-raw/raw/village1.csv", row.names = FALSE)
write.csv(villageData2, "data-raw/raw/village2.csv", row.names = FALSE)
write.csv(villageData3, "data-raw/raw/village3.csv", row.names = FALSE)

## Write as xlsx
openxlsx::addWorksheet(wb = rawData, sheetName = "village")
openxlsx::writeData(wb = rawData, sheet = "village", x = villageData)

## convert non-ASCII text
for(i in names(villageData)) {
  villageData[ , i][stringi::stri_enc_mark(str = villageData[ , i]) != "ASCII"] <- "Burmese for translation"
}

for(i in names(villageData1)) {
  villageData1[ , i][stringi::stri_enc_mark(str = villageData1[ , i]) != "ASCII"] <- "Burmese for translation"
}

for(i in names(villageData2)) {
  villageData2[ , i][stringi::stri_enc_mark(str = villageData2[ , i]) != "ASCII"] <- "Burmese for translation"
}

for(i in names(villageData3)) {
  villageData3[ , i][stringi::stri_enc_mark(str = villageData3[ , i]) != "ASCII"] <- "Burmese for translation"
}

usethis::use_data(villageData, compress = "xz", overwrite = TRUE)
usethis::use_data(villageData1, compress = "xz", overwrite = TRUE)
usethis::use_data(villageData2, compress = "xz", overwrite = TRUE)
usethis::use_data(villageData3, compress = "xz", overwrite = TRUE)


############################ Township profile ##################################

townshipData <- get_mcct_data(id = "baseline_townshipprofile_final",
                              start = "2019-08-06",
                              username = "validmeasures",
                              password = "6Y2-8yK-Nmk-Lbf",
                              filename = "township_form")

## Write as CSV
write.csv(townshipData, "data-raw/raw/township.csv", row.names = FALSE)

## Write as xlsx
openxlsx::addWorksheet(wb = rawData, sheetName = "township")
openxlsx::writeData(wb = rawData, sheet = "township", x = townshipData)

townshipData <- read.csv("data-raw/raw/townshipV2.csv")

## convert non-ASCII text
for(i in names(townshipData)) {
  townshipData[ , i][stringi::stri_enc_mark(str = townshipData[ , i]) != "ASCII"] <- "Burmese for translation"
}

## Merge townshipData with townshipList to add township name
townshipData <- merge(townshipList, townshipData,
                      by.x = "ts_pcode", by.y = "geo_town",
                      all.y = TRUE)

usethis::use_data(townshipData, compress = "xz", overwrite = TRUE)

## Save workbook
openxlsx::saveWorkbook(wb = rawData, file = "data-raw/raw/rawData.xlsx", overwrite = TRUE)





