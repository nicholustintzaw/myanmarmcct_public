library(myanmarMCCTdata)
library(openxlsx)
library(magrittr)

## Create XLSX object to save various results in

resultsTable <- openxlsx::createWorkbook()

## MCCT - individual

x <- create_mcct(df = hh, x = hhMembers) %>%
  recode_mcct() %>%
  create_weighted_table(vars = c("mcct1", "mcct2",
                                 paste("mcct3", letters[1:7], sep = ""),
                                 paste("mcct5", letters[1:4], sep = ""),
                                 paste("mcct6", letters[1:3], sep = "")),
                        labs = c("MCCT coverage",
                                 "Mean number of cash transfers received",
                                 "Cash transfer received through mobile programme",
                                 "Cash transfer received through wave money",
                                 "Cash transfer received through village head/GAD",
                                 "Cash transfer received through EHO",
                                 "Cash transfer received through CBO",
                                 "Cash transfer received through midwife",
                                 "Cash transfer received through health staff",
                                 "Cash transfer used for food",
                                 "Cash transfer used for education",
                                 "Cash transfer used for housing",
                                 "Cash transfer used for clothes",
                                 "Usage of cash transfer decided by mother",
                                 "Usage of cash transfer decided by husband",
                                 "Usage of cash transfer decided by other household head/member"),
                        state = "Kayah")

y <- create_mcct(df = hh, x = hhMembers) %>%
  recode_mcct() %>%
  create_weighted_table(vars = c("mcct1", "mcct2",
                                 paste("mcct3", letters[1:7], sep = ""),
                                 paste("mcct5", letters[1:4], sep = ""),
                                 paste("mcct6", letters[1:3], sep = "")),
                        labs = c("MCCT coverage",
                                 "Mean number of cash transfers received",
                                 "Cash transfer received through mobile programme",
                                 "Cash transfer received through wave money",
                                 "Cash transfer received through village head/GAD",
                                 "Cash transfer received through EHO",
                                 "Cash transfer received through CBO",
                                 "Cash transfer received through midwife",
                                 "Cash transfer received through health staff",
                                 "Cash transfer used for food",
                                 "Cash transfer used for education",
                                 "Cash transfer used for housing",
                                 "Cash transfer used for clothes",
                                 "Usage of cash transfer decided by mother",
                                 "Usage of cash transfer decided by husband",
                                 "Usage of cash transfer decided by other household head/member"),
                        state = "Kayin")

mcctResults <- data.frame(rbind(x, y))

##
usethis::use_data(mcctResults, compress = "xz", overwrite = TRUE)

## Add mcctResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "MCCT")
openxlsx::writeData(wb = resultsTable, sheet = "MCCT", mcctResults)

## Food consumption score

x <- recode_fcs(df = hh) %>%
  create_weighted_table(vars = c("fcs", "poor", "borderline", "acceptable"),
                        sex = TRUE,
                        labs = c("Mean food consumption score",
                                 "Poor food consumption",
                                 "Borderline food consumption",
                                 "Acceptable food consumption"),
                        state = "Kayah")

y <- recode_fcs(df = hh) %>%
  create_weighted_table(vars = c("fcs", "poor", "borderline", "acceptable"),
                        sex = TRUE,
                        labs = c("Mean food consumption score",
                                 "Poor food consumption",
                                 "Borderline food consumption",
                                 "Acceptable food consumption"),
                        state = "Kayin")

fcsResults <- data.frame(rbind(x, y))

##
usethis::use_data(fcsResults, compress = "xz", overwrite = TRUE)

## Add fcsResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "FCS")
openxlsx::writeData(wb = resultsTable, sheet = "FCS", fcsResults)


## Food consumption score - nutrition

x <- recode_fcs_nutrition(df = hh) %>%
  create_weighted_table(vars = c("vita1", "vita2", "vita3",
                                 "protein1", "protein2", "protein3",
                                 "iron1", "iron2", "iron3"),
                        sex = TRUE,
                        labs = c("Never consumes vitamin A-rich foods",
                                 "Sometimes consumes vitamin A-rich foods",
                                 "At least daily consumption of vitamin A-rich foods",
                                 "Never consumes protein-rich foods",
                                 "Sometimes consumes protein-rich foods",
                                 "At least daily consumption of protein-rich foods",
                                 "Never consumes haeme iron-rich foods",
                                 "Sometimes consumes haeme iron-rich foods",
                                 "At least daily consumption of haeme iron-rich foods"),
                        state = "Kayah")

y <- recode_fcs_nutrition(df = hh) %>%
  create_weighted_table(vars = c("vita1", "vita2", "vita3",
                                 "protein1", "protein2", "protein3",
                                 "iron1", "iron2", "iron3"),
                        sex = TRUE,
                        labs = c("Never consumes vitamin A-rich foods",
                                 "Sometimes consumes vitamin A-rich foods",
                                 "At least daily consumption of vitamin A-rich foods",
                                 "Never consumes protein-rich foods",
                                 "Sometimes consumes protein-rich foods",
                                 "At least daily consumption of protein-rich foods",
                                 "Never consumes haeme iron-rich foods",
                                 "Sometimes consumes haeme iron-rich foods",
                                 "At least daily consumption of haeme iron-rich foods"),
                        state = "Kayin")

fcsnResults <- data.frame(rbind(x, y))

##
usethis::use_data(fcsnResults, compress = "xz", overwrite = TRUE)

## Add fcsnResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "FCS-N")
openxlsx::writeData(wb = resultsTable, sheet = "FCS-N", fcsnResults)


## Consumption-based coping strategies index

x <- recode_csi_consumption(df = hh) %>%
  create_weighted_table(vars = "bTotalWeighted",
                        labs = "Mean coping strategies index",
                        state = "Kayah")

y <- recode_csi_consumption(df = hh) %>%
  create_weighted_table(vars = "bTotalWeighted",
                        labs = "Mean coping strategies index",
                        state = "Kayin")

ccsiResults <- data.frame(rbind(x, y))

##
usethis::use_data(ccsiResults, compress = "xz", overwrite = TRUE)

## Add ccsiResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "CCSI")
openxlsx::writeData(wb = resultsTable, sheet = "CCSI", ccsiResults)


## Livelihood coping strategies index

x <- recode_csi_livelihoods(df = hh) %>%
  create_weighted_table(vars = c("secure", "stress", "crisis", "emergency"),
                        labs = c("Household is food secure",
                                 "Household using stress coping strategies",
                                 "Household using crisis coping strategies",
                                 "Household using emergency coping strategies"),
                        state = "Kayah")

y <- recode_csi_livelihoods(df = hh) %>%
  create_weighted_table(vars = c("secure", "stress", "crisis", "emergency"),
                        labs = c("Household is food secure",
                                 "Household using stress coping strategies",
                                 "Household using crisis coping strategies",
                                 "Household using emergency coping strategies"),
                        state = "Kayin")

lcsiResults <- data.frame(rbind(x, y))

##
usethis::use_data(lcsiResults, compress = "xz", overwrite = TRUE)

## Add lcsiResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "LCSI")
openxlsx::writeData(wb = resultsTable, sheet = "LCSI", lcsiResults)

## Household food expenditure share

x <- recode_hfes(df = hh) %>%
  create_weighted_table(vars = c("vulnerable", "high", "medium", "low"),
                        labs = c("Vulnerable household",
                                 "High food insecurity household",
                                 "Moderate food insecurity household",
                                 "Low food insecurity household"),
                        state = "Kayah")

y <- recode_hfes(df = hh) %>%
  create_weighted_table(vars = c("vulnerable", "high", "medium", "low"),
                        labs = c("Vulnerable household",
                                 "High food insecurity household",
                                 "Moderate food insecurity household",
                                 "Low food insecurity household"),
                        state = "Kayin")

hfesResults <- data.frame(rbind(x, y))

##
usethis::use_data(hfesResults, compress = "xz", overwrite = TRUE)

## Add hfesResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "HFES")
openxlsx::writeData(wb = resultsTable, sheet = "HFES", hfesResults)

## WASH

x <- recode_wash(df = hh) %>%
  create_weighted_table(vars = c("summer1", "summer6", "summer5",
                                 "rain1", "rain6", "rain5",
                                 "winter1", "winter6", "winter5",
                                 "san3", "san4", "san5", "san6",
                                 "hw14", "hw15", "hw16"),
                        labs = c("At least limited water source in summer season",
                                 "Unimproved water source in summer season",
                                 "Surface water in summer season",
                                 "At least limited water source in rainy season",
                                 "Unimproved water source in rainy season",
                                 "Surface water in rainy season",
                                 "At least limited water source in winter season",
                                 "Unimproved water source in winter season",
                                 "Surface water in winter season",
                                 "Basic sanitation facility",
                                 "Limited sanitation facility",
                                 "Unimproved sanitation facility",
                                 "Open defecation",
                                 "Basic handwashing facility",
                                 "Limited handwashing facility",
                                 "No handwashing facility"),
                        state = "Kayah")

y <- recode_wash(df = hh) %>%
  create_weighted_table(vars = c("summer1", "summer6", "summer5",
                                 "rain1", "rain6", "rain5",
                                 "winter1", "winter6", "winter5",
                                 "san3", "san4", "san5", "san6",
                                 "hw14", "hw15", "hw16"),
                        labs = c("At least limited water source in summer season",
                                 "Unimproved water source in summer season",
                                 "Surface water in summer season",
                                 "At least limited water source in rainy season",
                                 "Unimproved water source in rainy season",
                                 "Surface water in rainy season",
                                 "At least limited water source in winter season",
                                 "Unimproved water source in winter season",
                                 "Surface water in winter season",
                                 "Basic sanitation facility",
                                 "Limited sanitation facility",
                                 "Unimproved sanitation facility",
                                 "Open defecation",
                                 "Basic handwashing facility",
                                 "Limited handwashing facility",
                                 "No handwashing facility"),
                        state = "Kayin")

washResults <- data.frame(rbind(x, y))

##
usethis::use_data(washResults, compress = "xz", overwrite = TRUE)

## Add washResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "WASH")
openxlsx::writeData(wb = resultsTable, sheet = "WASH", washResults)


## Immunisation

x <- recode_chealth(df = create_chealth(df = childHealth, x = hh, y = hhMembers)) %>%
  create_weighted_table(vars = c("vac1", "vac2", "vac3", "vac4", "vac5a", "vac6",
                                 "bcg", "hepb", "penta1", "penta2", "penta3",
                                 "polio1", "polio2", "polio3", "polioinj",
                                 "measles1", "measles2", "rubella", "enc",
                                 "vita", "worm"),
                        labs = c("Ever vaccinated",
                                 "Vaccination card retention rate",
                                 "Immunisation access",
                                 "Immunisation utilisation",
                                 "Full immunisation coverage",
                                 "Hepatitis B immunisation given within first 24 hours",
                                 "Received BCG vaccine",
                                 "Received Hepatitis B vaccine",
                                 "Received pentavalent vaccine first dose",
                                 "Received pentavalent vaccine second dose",
                                 "Received pentavalent vaccine third dose",
                                 "Received oral polio vaccine first dose",
                                 "Received oral polio vaccine second dose",
                                 "Received oral polio vaccine third dose",
                                 "Received injectable polio vaccine",
                                 "Received measles vaccine first dose",
                                 "Received measles vaccine second dose",
                                 "Received rubella vaccine",
                                 "Received pneumococcal vaccine",
                                 "Received vitamin A in the past 6 months",
                                 "Received deworming tablets in the past 6 months"),
                        state = "Kayah")

y <- recode_chealth(df = create_chealth(df = childHealth, x = hh, y = hhMembers)) %>%
  create_weighted_table(vars = c("vac1", "vac2", "vac3", "vac4", "vac5a", "vac6",
                                 "bcg", "hepb", "penta1", "penta2", "penta3",
                                 "polio1", "polio2", "polio3", "polioinj",
                                 "measles1", "measles2", "rubella", "enc",
                                 "vita", "worm"),
                        labs = c("Ever vaccinated",
                                 "Vaccination card retention rate",
                                 "Immunisation access",
                                 "Immunisation utilisation",
                                 "Full immunisation coverage",
                                 "Hepatitis B immunisation given within first 24 hours",
                                 "Received BCG vaccine",
                                 "Received Hepatitis B vaccine",
                                 "Received pentavalent vaccine first dose",
                                 "Received pentavalent vaccine second dose",
                                 "Received pentavalent vaccine third dose",
                                 "Received oral polio vaccine first dose",
                                 "Received oral polio vaccine second dose",
                                 "Received oral polio vaccine third dose",
                                 "Received injectable polio vaccine",
                                 "Received measles vaccine first dose",
                                 "Received measles vaccine second dose",
                                 "Received rubella vaccine",
                                 "Received pneumococcal vaccine",
                                 "Received vitamin A in the past 6 months",
                                 "Received deworming tablets in the past 6 months"),
                        state = "Kayin")

immunisationResults <- data.frame(rbind(x, y))

##
usethis::use_data(immunisationResults, compress = "xz", overwrite = TRUE)

## Add immunisationResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "Immunisation")
openxlsx::writeData(wb = resultsTable, sheet = "Immunisation", immunisationResults)

## Birthweight

x <- create_chealth(df = childHealth, x = hh, y = hhMembers) %>%
  recode_chealth() %>%
  create_weighted_table(vars = c("bwt", "bwtCard", "lbw1", "lbw2"),
                        labs = c("Mean birthweight (grams)",
                                 "Mean birthweight based on health card (grams)",
                                 "Low birthweight prevalence",
                                 "Low birthweight prevalence based on health card"),
                        state = "Kayah")

y <- create_chealth(df = childHealth, x = hh, y = hhMembers) %>%
  recode_chealth() %>%
  create_weighted_table(vars = c("bwt", "bwtCard", "lbw1", "lbw2"),
                        labs = c("Mean birthweight (grams)",
                                 "Mean birthweight based on health card (grams)",
                                 "Low birthweight prevalence",
                                 "Low birthweight prevalence based on health card"),
                        state = "Kayin")

lbwResults <- data.frame(rbind(x, y))

##
usethis::use_data(lbwResults, compress = "xz", overwrite = TRUE)

## Add lbwResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "LBW")
openxlsx::writeData(wb = resultsTable, sheet = "LBW", lbwResults)

## Period prevalence of childhood illnesses

x <- recode_chealth(df = create_chealth(df = childHealth, x = hh, y = hhMembers)) %>%
  create_weighted_table(vars = c("dia1", "ari1", "fev1"),
                         labs = c("Period prevalence of diarrhoea",
                                  "Period prevalence of cough/difficulty in breathing",
                                  "Period prevalence of fever"),
                         state = "Kayah")

y <- recode_chealth(df = create_chealth(df = childHealth, x = hh, y = hhMembers)) %>%
  create_weighted_table(vars = c("dia1", "ari1", "fev1"),
                         labs = c("Period prevalence of diarrhoea",
                                  "Period prevalence of cough/difficulty in breathing",
                                  "Period prevalence of fever"),
                         state = "Kayin")

illResults <- data.frame(rbind(x, y))

##
usethis::use_data(illResults, compress = "xz", overwrite = TRUE)

## Add illResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "Childhood illness")
openxlsx::writeData(wb = resultsTable, sheet = "Childhood illness", illResults)

## Treatement-seeking diarrhoea

x <- recode_chealth(df = create_chealth(df = childHealth, x = hh, y = hhMembers)) %>%
  create_weighted_table(vars = c("dia2", "dia4", "dia3a", "dia3b", "dia3c", "dia3d",
                                 "dia3f", "dia3g", "dia3h",
                                 "dia5a", "dia5b", "dia5c", "dia5d", "dia5e",
                                 "dia5f", "dia5g", "dia5h", "dia5i", "dia5j",
                                 "dia5k", "dia5l", "dia5m",
                                 "dia7", "dia8a", "dia8b", "dia8c", "dia8d",
                                 "dia8e", "dia8f", "dia9"),
                        labs = c("Sought treatment for diarrhoea",
                                 "Mean time to treatment (days)",
                                 "Did not seek treatment for diarrhoea because there was no facility",
                                 "Did not seek treatment for diarrhoea because health facility was inaccessible",
                                 "Did not seek treatment for diarrhoea because it was expensive",
                                 "Did not seek treatment for diarrhoea because it was not necessary",
                                 "Did not seek treatment for diarrhoea because advised not to",
                                 "Did not seek treatment for diarrhoea because alternative treatment was sought",
                                 "Did not seek treatment for diarrhoea because treatment not known",
                                 "Sought treatment from township hospital",
                                 "Sought treatment from station hospital",
                                 "Sought treatment from RHC/health assistant",
                                 "Sought treatment from SRHC/midwife",
                                 "Sought treatment from private clinic/doctor",
                                 "Sought treatment from community health worker",
                                 "Sought treatment from traditional healer",
                                 "Sought treatment from untrained health worker",
                                 "Sought treatment by buying drug from shop",
                                 "Sought treatment from EHO clinic/volunteer",
                                 "Sought treatment from family member",
                                 "Sought treatment from NGOs/clinic",
                                 "Sought treatment from auxiliary midwife",
                                 "Mean cost for treatment/service (MMK)",
                                 "Cost for transportation",
                                 "Cost for registration",
                                 "Cost for medicine",
                                 "Cost for laboratory fees",
                                 "Cost for provider fees",
                                 "Cost for gifts",
                                 "Took loan to cover treatment/service costs"),
                        state = "Kayah")

y <- recode_chealth(df = create_chealth(df = childHealth, x = hh, y = hhMembers)) %>%
  create_weighted_table(vars = c("dia2", "dia4", "dia3a", "dia3b", "dia3c", "dia3d",
                                 "dia3f", "dia3g", "dia3h",
                                 "dia5a", "dia5b", "dia5c", "dia5d", "dia5e",
                                 "dia5f", "dia5g", "dia5h", "dia5i", "dia5j",
                                 "dia5k", "dia5l", "dia5m",
                                 "dia7", "dia8a", "dia8b", "dia8c", "dia8d",
                                 "dia8e", "dia8f", "dia9"),
                        labs = c("Sought treatment for diarrhoea",
                                 "Mean time to treatment (days)",
                                 "Did not seek treatment for diarrhoea because there was no facility",
                                 "Did not seek treatment for diarrhoea because health facility was inaccessible",
                                 "Did not seek treatment for diarrhoea because it was expensive",
                                 "Did not seek treatment for diarrhoea because it was not necessary",
                                 "Did not seek treatment for diarrhoea because advised not to",
                                 "Did not seek treatment for diarrhoea because alternative treatment was sought",
                                 "Did not seek treatment for diarrhoea because treatment not known",
                                 "Sought treatment from township hospital",
                                 "Sought treatment from station hospital",
                                 "Sought treatment from RHC/health assistant",
                                 "Sought treatment from SRHC/midwife",
                                 "Sought treatment from private clinic/doctor",
                                 "Sought treatment from community health worker",
                                 "Sought treatment from traditional healer",
                                 "Sought treatment from untrained health worker",
                                 "Sought treatment by buying drug from shop",
                                 "Sought treatment from EHO clinic/volunteer",
                                 "Sought treatment from family member",
                                 "Sought treatment from NGOs/clinic",
                                 "Sought treatment from auxiliary midwife",
                                 "Mean cost for treatment/service (MMK)",
                                 "Cost for transportation",
                                 "Cost for registration",
                                 "Cost for medicine",
                                 "Cost for laboratory fees",
                                 "Cost for provider fees",
                                 "Cost for gifts",
                                 "Took loan to cover treatment/service costs"),
                        state = "Kayin")

diarrhoeaResults <- data.frame(rbind(x, y))

##
usethis::use_data(diarrhoeaResults, compress = "xz", overwrite = TRUE)

## Add diarrhoeaResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "Diarrhoea treatment")
openxlsx::writeData(wb = resultsTable, sheet = "Diarrhoea treatment", diarrhoeaResults)

## Treatement-seeking cough

x <- recode_chealth(df = create_chealth(df = childHealth, x = hh, y = hhMembers)) %>%
  create_weighted_table(vars = c("ari2", "ari4", "ari3a", "ari3b", "ari3c", "ari3d",
                                 "ari3f", "ari3g", "ari3h",
                                 "ari5a", "ari5b", "ari5c", "ari5d", "ari5e",
                                 "ari5f", "ari5g", "ari5h", "ari5i", "ari5j",
                                 "ari5k", "ari5l", "ari5m",
                                 "ari7", "ari8a", "ari8b", "ari8c", "ari8d",
                                 "ari8e", "ari8f", "ari9"),
                        labs = c("Sought treatment for cough",
                                 "Mean time to treatment (days)",
                                 "Did not seek treatment for cough because there was no facility",
                                 "Did not seek treatment for cough because health facility was inaccessible",
                                 "Did not seek treatment for cough because it was expensive",
                                 "Did not seek treatment for cough because it was not necessary",
                                 "Did not seek treatment for cough because advised not to",
                                 "Did not seek treatment for cough because alternative treatment was sought",
                                 "Did not seek treatment for cough because treatment not known",
                                 "Sought treatment from township hospital",
                                 "Sought treatment from station hospital",
                                 "Sought treatment from RHC/health assistant",
                                 "Sought treatment from SRHC/midwife",
                                 "Sought treatment from private clinic/doctor",
                                 "Sought treatment from community health worker",
                                 "Sought treatment from traditional healer",
                                 "Sought treatment from untrained health worker",
                                 "Sought treatment by buying drug from shop",
                                 "Sought treatment from EHO clinic/volunteer",
                                 "Sought treatment from family member",
                                 "Sought treatment from NGOs/clinic",
                                 "Sought treatment from auxiliary midwife",
                                 "Mean cost for treatment/service (MMK)",
                                 "Cost for transportation",
                                 "Cost for registration",
                                 "Cost for medicine",
                                 "Cost for laboratory fees",
                                 "Cost for provider fees",
                                 "Cost for gifts",
                                 "Took loan to cover treatment/service costs"),
                        state = "Kayah")

y <- recode_chealth(df = create_chealth(df = childHealth, x = hh, y = hhMembers)) %>%
  create_weighted_table(vars = c("ari2", "ari4", "ari3a", "ari3b", "ari3c", "ari3d",
                                 "ari3f", "ari3g", "ari3h",
                                 "ari5a", "ari5b", "ari5c", "ari5d", "ari5e",
                                 "ari5f", "ari5g", "ari5h", "ari5i", "ari5j",
                                 "ari5k", "ari5l", "ari5m",
                                 "ari7", "ari8a", "ari8b", "ari8c", "ari8d",
                                 "ari8e", "ari8f", "ari9"),
                        labs = c("Sought treatment for cough",
                                 "Mean time to treatment (days)",
                                 "Did not seek treatment for cough because there was no facility",
                                 "Did not seek treatment for cough because health facility was inaccessible",
                                 "Did not seek treatment for cough because it was expensive",
                                 "Did not seek treatment for cough because it was not necessary",
                                 "Did not seek treatment for cough because advised not to",
                                 "Did not seek treatment for cough because alternative treatment was sought",
                                 "Did not seek treatment for cough because treatment not known",
                                 "Sought treatment from township hospital",
                                 "Sought treatment from station hospital",
                                 "Sought treatment from RHC/health assistant",
                                 "Sought treatment from SRHC/midwife",
                                 "Sought treatment from private clinic/doctor",
                                 "Sought treatment from community health worker",
                                 "Sought treatment from traditional healer",
                                 "Sought treatment from untrained health worker",
                                 "Sought treatment by buying drug from shop",
                                 "Sought treatment from EHO clinic/volunteer",
                                 "Sought treatment from family member",
                                 "Sought treatment from NGOs/clinic",
                                 "Sought treatment from auxiliary midwife",
                                 "Mean cost for treatment/service (MMK)",
                                 "Cost for transportation",
                                 "Cost for registration",
                                 "Cost for medicine",
                                 "Cost for laboratory fees",
                                 "Cost for provider fees",
                                 "Cost for gifts",
                                 "Took loan to cover treatment/service costs"),
                        state = "Kayin")

coughResults <- data.frame(rbind(x, y))

##
usethis::use_data(coughResults, compress = "xz", overwrite = TRUE)

## Add coughResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "Cough treatment")
openxlsx::writeData(wb = resultsTable, sheet = "Cough treatment", coughResults)

## Treatment for fever

x <- recode_chealth(df = create_chealth(df = childHealth, x = hh, y = hhMembers)) %>%
  create_weighted_table(vars = c("fev2", "fev4", "fev3a", "fev3b", "fev3c", "fev3d",
                                 "fev3f", "fev3g", "fev3h",
                                 "fev5a", "fev5b", "fev5c", "fev5d", "fev5e",
                                 "fev5f", "fev5g", "fev5h", "fev5i", "fev5j",
                                 "fev5k", "fev5l", "fev5m",
                                 "fev7", "fev8a", "fev8b", "fev8c", "fev8d",
                                 "fev8e", "fev8f", "fev9"),
                        labs = c("Sought treatment for fever",
                                 "Mean time to treatment (days)",
                                 "Did not seek treatment for fever because there was no facility",
                                 "Did not seek treatment for fever because health facility was inaccessible",
                                 "Did not seek treatment for fever because it was expensive",
                                 "Did not seek treatment for fever because it was not necessary",
                                 "Did not seek treatment for fever because advised not to",
                                 "Did not seek treatment for fever because alternative treatment was sought",
                                 "Did not seek treatment for fever because treatment not known",
                                 "Sought treatment from township hospital",
                                 "Sought treatment from station hospital",
                                 "Sought treatment from RHC/health assistant",
                                 "Sought treatment from SRHC/midwife",
                                 "Sought treatment from private clinic/doctor",
                                 "Sought treatment from community health worker",
                                 "Sought treatment from traditional healer",
                                 "Sought treatment from untrained health worker",
                                 "Sought treatment by buying drug from shop",
                                 "Sought treatment from EHO clinic/volunteer",
                                 "Sought treatment from family member",
                                 "Sought treatment from NGOs/clinic",
                                 "Sought treatment from auxiliary midwife",
                                 "Mean cost for treatment/service (MMK)",
                                 "Cost for transportation",
                                 "Cost for registration",
                                 "Cost for medicine",
                                 "Cost for laboratory fees",
                                 "Cost for provider fees",
                                 "Cost for gifts",
                                 "Took loan to cover treatment/service costs"),
                        state = "Kayah")

y <- recode_chealth(df = create_chealth(df = childHealth, x = hh, y = hhMembers)) %>%
  create_weighted_table(vars = c("fev2", "fev4", "fev3a", "fev3b", "fev3c", "fev3d",
                                 "fev3f", "fev3g", "fev3h",
                                 "fev5a", "fev5b", "fev5c", "fev5d", "fev5e",
                                 "fev5f", "fev5g", "fev5h", "fev5i", "fev5j",
                                 "fev5k", "fev5l", "fev5m",
                                 "fev7", "fev8a", "fev8b", "fev8c", "fev8d",
                                 "fev8e", "fev8f", "fev9"),
                        labs = c("Sought treatment for fever",
                                 "Mean time to treatment (days)",
                                 "Did not seek treatment for fever because there was no facility",
                                 "Did not seek treatment for fever because health facility was inaccessible",
                                 "Did not seek treatment for fever because it was expensive",
                                 "Did not seek treatment for fever because it was not necessary",
                                 "Did not seek treatment for fever because advised not to",
                                 "Did not seek treatment for fever because alternative treatment was sought",
                                 "Did not seek treatment for fever because treatment not known",
                                 "Sought treatment from township hospital",
                                 "Sought treatment from station hospital",
                                 "Sought treatment from RHC/health assistant",
                                 "Sought treatment from SRHC/midwife",
                                 "Sought treatment from private clinic/doctor",
                                 "Sought treatment from community health worker",
                                 "Sought treatment from traditional healer",
                                 "Sought treatment from untrained health worker",
                                 "Sought treatment by buying drug from shop",
                                 "Sought treatment from EHO clinic/volunteer",
                                 "Sought treatment from family member",
                                 "Sought treatment from NGOs/clinic",
                                 "Sought treatment from auxiliary midwife",
                                 "Mean cost for treatment/service (MMK)",
                                 "Cost for transportation",
                                 "Cost for registration",
                                 "Cost for medicine",
                                 "Cost for laboratory fees",
                                 "Cost for provider fees",
                                 "Cost for gifts",
                                 "Took loan to cover treatment/service costs"),
                        state = "Kayin")

feverResults <- data.frame(rbind(x, y))

##
usethis::use_data(feverResults, compress = "xz", overwrite = TRUE)

## Add feverResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "Fever treatment")
openxlsx::writeData(wb = resultsTable, sheet = "Fever treatment", feverResults)


## Child nutrition

x <- create_canthro(df = childAnthro, x = anthroDF) %>%
  recode_anthro() %>%
  create_weighted_anthro(vars = c("haz", "global.haz", "moderate.haz", "severe.haz",
                                  "waz", "global.waz", "moderate.waz", "severe.waz",
                                  "whz", "gam.whz", "mam.whz", "sam.whz",
                                  "muac", "gam.muac", "mam.muac", "sam.muac"),
                         sex = TRUE,
                         labs <- c("Mean height-for-age z-score",
                                   "Global stunting/stuntedness prevalence",
                                   "Moderate stunting/stuntedness prevalence",
                                   "Severe stunting/stuntedness prevalence",
                                   "Mean weight-for-age z-score",
                                   "Global underweight prevalence",
                                   "Moderate underweight prevalence",
                                   "Severe underweight prevalence",
                                   "Mean weight-for-age z-score",
                                   "Global wasting prevalence by weight-for-height z-score",
                                   "Moderate wasting prevalence by weight-for-height z-score",
                                   "Severe wasting prevalence by weight-for-height z-score",
                                   "Mean mid-upper arm circumference",
                                   "Global wasting prevalence by mid-upper arm circumfererence",
                                   "Moderate wasting prevalence by mid-upper arm circumfererence",
                                   "Severe wasting prevalence by mid-upper arm circumfererence"),
                         state = "Kayah")

y <- create_canthro(df = childAnthro, x = anthroDF) %>%
  recode_anthro() %>%
  create_weighted_anthro(vars = c("haz", "global.haz", "moderate.haz", "severe.haz",
                                  "waz", "global.waz", "moderate.waz", "severe.waz",
                                  "whz", "gam.whz", "mam.whz", "sam.whz",
                                  "muac", "gam.muac", "mam.muac", "sam.muac"),
                         sex = TRUE,
                         labs <- c("Mean height-for-age z-score",
                                   "Global stunting/stuntedness prevalence",
                                   "Moderate stunting/stuntedness prevalence",
                                   "Severe stunting/stuntedness prevalence",
                                   "Mean weight-for-age z-score",
                                   "Global underweight prevalence",
                                   "Moderate underweight prevalence",
                                   "Severe underweight prevalence",
                                   "Mean weight-for-age z-score",
                                   "Global wasting prevalence by weight-for-height z-score",
                                   "Moderate wasting prevalence by weight-for-height z-score",
                                   "Severe wasting prevalence by weight-for-height z-score",
                                   "Mean mid-upper arm circumference",
                                   "Global wasting prevalence by mid-upper arm circumfererence",
                                   "Moderate wasting prevalence by mid-upper arm circumfererence",
                                   "Severe wasting prevalence by mid-upper arm circumfererence"),
                         state = "Kayin")

cnutResults <- data.frame(rbind(x, y))

##
usethis::use_data(cnutResults, compress = "xz", overwrite = TRUE)

## Add cnutResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "Child nutrition")
openxlsx::writeData(wb = resultsTable, sheet = "Child nutrition", cnutResults)

## IYCF

x <- create_iycf(df = iycf, x = hh, y = hhMembers) %>%
  recode_iycf() %>%
  create_weighted_table(vars = c("eibf", "ebf", "contBF1", "contBF2",
                                 "compFeeding",
                                 "mmf1", "mmf2", "mmf3", "mmf",
                                 "fgscore", "mdd",
                                 "mad1", "mad2", "mad"),
                        sex = TRUE,
                        labs = c("Early initation of breastfeeding",
                                 "Exclusive breastfeeding",
                                 "Continued breastfeeding at 1 year",
                                 "Continued breastfeeding at 2 years",
                                 "Timely introduction of complementary foods",
                                 "Minimum meal frequency - breastfed 6-8 months old",
                                 "Minimum meal frequency - breastfed 9-23 months old",
                                 "Minimum meal frequency - non-breastfed 6-23 months old",
                                 "Minimum meal frequency",
                                 "Mean number of food groups",
                                 "Minimum dietary diversity",
                                 "Minimum acceptable diet - breastfed",
                                 "Minimum acceptable diet - non-breastfed",
                                 "Minimum acceptable diet"),
                        state = "Kayah")

y <- create_iycf(df = iycf, x = hh, y = hhMembers) %>%
  recode_iycf() %>%
  create_weighted_table(vars = c("eibf", "ebf", "contBF1", "contBF2",
                                 "compFeeding",
                                 "mmf1", "mmf2", "mmf3", "mmf",
                                 "fgscore", "mdd",
                                 "mad1", "mad2", "mad"),
                        sex = TRUE,
                        labs = c("Early initation of breastfeeding",
                                 "Exclusive breastfeeding",
                                 "Continued breastfeeding at 1 year",
                                 "Continued breastfeeding at 2 years",
                                 "Timely introduction of complementary foods",
                                 "Minimum meal frequency - breastfed 6-8 months old",
                                 "Minimum meal frequency - breastfed 9-23 months old",
                                 "Minimum meal frequency - non-breastfed 6-23 months old",
                                 "Minimum meal frequency",
                                 "Mean number of food groups",
                                 "Minimum dietary diversity",
                                 "Minimum acceptable diet - breastfed",
                                 "Minimum acceptable diet - non-breastfed",
                                 "Minimum acceptable diet"),
                        state = "Kayin")

iycfResults <- data.frame(rbind(x, y))

##
usethis::use_data(iycfResults, compress = "xz", overwrite = TRUE)

## Add iycfResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "IYCF")
openxlsx::writeData(wb = resultsTable, sheet = "IYCF", iycfResults)


## Family planning

x <- recode_fplan(df = hh) %>%
  create_weighted_table(vars = c("fp1", "fp2", "fp3",
                                 "fp4", "fp5", "fp6a", "fp6b", "fp6c", "fp6d",
                                 "fp6e", "fp6f", "fp6g", "fp6h", "fp6i", "fp6j",
                                 "fp6k", "fp6l", "fp6m", "fp6n",
                                 "fp7a", "fp7b", "fp7c", "fp7d", "fp7e",
                                 "fp7f", "fp7g", "fp7h", "fp7i", "fp7j",
                                 "fp7k",
                                 "fp8",
                                 "fp9a", "fp9b", "fp9c", "fp9d", "fp9e",
                                 "fp9f", "fp9g", "fp9h", "fp9i", "fp9j",
                                 "fp9k", "fp10"),
                        labs = c("Mean time to wait for next child (months)",
                                 "Mean time to wait for next child for currently pregnant (months)",
                                 "Mean time to wait for next child according to husband (months)",
                                 "Using contraceptives now",
                                 "Used contraceptives before",
                                 "Type of contraceptive used: female sterilisation",
                                 "Type of contraceptive used: male sterilisation",
                                 "Type of contraceptive used: intrauterine device",
                                 "Type of contraceptive used: implant",
                                 "Type of contraceptive used: injectable",
                                 "Type of contraceptive used: pills",
                                 "Type of contraceptive used: male condom",
                                 "Type of contraceptive used: female condom",
                                 "Type of contraceptive used: lactational amenorrhea",
                                 "Type of contraceptive used: rhythm method",
                                 "Type of contraceptive used: withdrawal",
                                 "Type of contraceptive used: emergency contraception",
                                 "Type of contraceptive used: diaphragm",
                                 "Type of contraceptive used: foam/jelly",
                                 "Source of contraceptive: government hospital",
                                 "Source of contraceptive: government health centre",
                                 "Source of contraceptive: government health worker",
                                 "Source of contraceptive: UHC/MCH centre",
                                 "Source of contraceptive: private hospital",
                                 "Source of contraceptive: private doctor",
                                 "Source of contraceptive: pharmacy",
                                 "Source of contraceptive: NGO",
                                 "Source of contraceptive: EHO clinic",
                                 "Source of contraceptive: Auxiliary midwife",
                                 "Source of contraceptive: midwife",
                                 "Received family planning information",
                                 "Source of family planning information: government hospital",
                                 "Source of family planning information: government health centre",
                                 "Source of family planning information: government health worker",
                                 "Source of family planning information: UHC/MCH centre",
                                 "Source of family planning information: private hospital",
                                 "Source of family planning information: private doctor",
                                 "Source of family planning information: pharmacy",
                                 "Source of family planning information: NGO",
                                 "Source of family planning information: EHO clinic",
                                 "Source of family planning information: auxiliary midwife",
                                 "Source of family planning information: midwife",
                                 "Appropriate family planning knowledge"),
                        state = "Kayah")

y <- recode_fplan(df = hh) %>%
  create_weighted_table(vars = c("fp1", "fp2", "fp3",
                                 "fp4", "fp5", "fp6a", "fp6b", "fp6c", "fp6d",
                                 "fp6e", "fp6f", "fp6g", "fp6h", "fp6i", "fp6j",
                                 "fp6k", "fp6l", "fp6m", "fp6n",
                                 "fp7a", "fp7b", "fp7c", "fp7d", "fp7e",
                                 "fp7f", "fp7g", "fp7h", "fp7i", "fp7j",
                                 "fp7k",
                                 "fp8",
                                 "fp9a", "fp9b", "fp9c", "fp9d", "fp9e",
                                 "fp9f", "fp9g", "fp9h", "fp9i", "fp9j",
                                 "fp9k", "fp10"),
                        labs = c("Mean time to wait for next child (months)",
                                 "Mean time to wait for next child for currently pregnant (months)",
                                 "Mean time to wait for next child according to husband (months)",
                                 "Using contraceptives now",
                                 "Used contraceptives before",
                                 "Type of contraceptive used: female sterilisation",
                                 "Type of contraceptive used: male sterilisation",
                                 "Type of contraceptive used: intrauterine device",
                                 "Type of contraceptive used: implant",
                                 "Type of contraceptive used: injectable",
                                 "Type of contraceptive used: pills",
                                 "Type of contraceptive used: male condom",
                                 "Type of contraceptive used: female condom",
                                 "Type of contraceptive used: lactational amenorrhea",
                                 "Type of contraceptive used: rhythm method",
                                 "Type of contraceptive used: withdrawal",
                                 "Type of contraceptive used: emergency contraception",
                                 "Type of contraceptive used: diaphragm",
                                 "Type of contraceptive used: foam/jelly",
                                 "Source of contraceptive: government hospital",
                                 "Source of contraceptive: government health centre",
                                 "Source of contraceptive: government health worker",
                                 "Source of contraceptive: UHC/MCH centre",
                                 "Source of contraceptive: private hospital",
                                 "Source of contraceptive: private doctor",
                                 "Source of contraceptive: pharmacy",
                                 "Source of contraceptive: NGO",
                                 "Source of contraceptive: EHO clinic",
                                 "Source of contraceptive: Auxiliary midwife",
                                 "Source of contraceptive: midwife",
                                 "Received family planning information",
                                 "Source of family planning information: government hospital",
                                 "Source of family planning information: government health centre",
                                 "Source of family planning information: government health worker",
                                 "Source of family planning information: UHC/MCH centre",
                                 "Source of family planning information: private hospital",
                                 "Source of family planning information: private doctor",
                                 "Source of family planning information: pharmacy",
                                 "Source of family planning information: NGO",
                                 "Source of family planning information: EHO clinic",
                                 "Source of family planning information: auxiliary midwife",
                                 "Source of family planning information: midwife",
                                 "Appropriate family planning knowledge"),
                        state = "Kayin")

fpResults <- data.frame(rbind(x, y))

##
usethis::use_data(fpResults, compress = "xz", overwrite = TRUE)

## Add fpResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "Family planning")
openxlsx::writeData(wb = resultsTable, sheet = "Family planning", fpResults)

## Antenatal care

x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past") %>%
  recode_anc(status = "past") %>%
  create_weighted_table(vars = c("anc1", "anc4", "incurCost", "amountCost",
                                 "transportCost", "registrationCost",
                                 "medicineCost", "labCost", "providerCost",
                                 "giftCost", "borrow",
                                 "specialist", "doctor", "nurse", "ha",
                                 "pdoc", "lhv", "midwife", "amw", "tba",
                                 "chw", "ehw",
                                 "counsel", "restrict", "restrictVeg",
                                 "restrictFruits", "restrictGrains",
                                 "restrictMeat", "restrictFish",
                                 "restrictDairy",
                                 "b1", "ifa1", "ifa2", "ifa3",
                                 "ifa4a", "ifa4b", "ifa4c", "ifa4d", "ifa4e",
                                 "test1", "test2a", "test2b", "test2c", "test2d",
                                 "test3a", "test3b", "test3c", "test3d", "test3e",
                                 "test4", "test5"),
                        labs = c("At least one ANC visit with trained health worker",
                                 "At least four ANC visits with any service provider",
                                 "Incur cost for ANC",
                                 "Mean cost of ANC (MMK)",
                                 "Cost of ANC for transport",
                                 "Cost of ANC for registration",
                                 "Cost of ANC for medicine",
                                 "Cost of ANC for laboratory",
                                 "Cost of ANC for provider",
                                 "Cost of ANC for gifts",
                                 "Took loan for ANC costs",
                                 "ANC provider: specialist",
                                 "ANC provider: doctor",
                                 "ANC provider: nurse",
                                 "ANC provider: health assistant",
                                 "ANC provider: private doctor",
                                 "ANC provider: lady health visitor",
                                 "ANC provider: midwife",
                                 "ANC provider: auxiliary midwife",
                                 "ANC provider: traditional birth attendant",
                                 "ANC provider: community health worker",
                                 "ANC provider: ethnic health workers",
                                 "Attended ANC counselling",
                                 "Restricted eating certain foods",
                                 "Restrict vegetables",
                                 "Restrict fruits",
                                 "Restrict grains",
                                 "Restrict meat",
                                 "Restrict fish",
                                 "Restrict dairy",
                                 "Received vitamin B1 supplementation",
                                 "Received iron-folic acid supplementation",
                                 "Mean number of days iron-folic acid taken",
                                 "Cost of iron-folic acid supplementation (MMK)",
                                 "Source of IFA: government hospital",
                                 "Source of IFA: EHO clinic",
                                 "Source of IFA: private doctor/clinic",
                                 "Source of IFA: SRHC/RHC",
                                 "Source of IFA: Routine ANC location in village/ward",
                                 "ANC tests done",
                                 "ANC test: hepatitis B",
                                 "ANC test: hepatitis C",
                                 "ANC test: HIV/AIDS",
                                 "ANC test: syphillis",
                                 "ANC test provider: government hospital",
                                 "ANC test provider: EHO clinic",
                                 "ANC test provider: private doctor/clinic",
                                 "ANC test provider: SRHC/RHC",
                                 "ANC test provider: routine ANC location in village/ward",
                                 "Incur costs for ANC test",
                                 "Cost of ANC test (MMK)"),
                        state = "Kayah")

y <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past") %>%
  recode_anc(status = "past") %>%
  create_weighted_table(vars = c("anc1", "anc4", "incurCost", "amountCost",
                                 "transportCost", "registrationCost",
                                 "medicineCost", "labCost", "providerCost",
                                 "giftCost", "borrow",
                                 "specialist", "doctor", "nurse", "ha",
                                 "pdoc", "lhv", "midwife", "amw", "tba",
                                 "chw", "ehw",
                                 "counsel", "restrict", "restrictVeg",
                                 "restrictFruits", "restrictGrains",
                                 "restrictMeat", "restrictFish",
                                 "restrictDairy",
                                 "b1", "ifa1", "ifa2", "ifa3",
                                 "ifa4a", "ifa4b", "ifa4c", "ifa4d", "ifa4e",
                                 "test1", "test2a", "test2b", "test2c", "test2d",
                                 "test3a", "test3b", "test3c", "test3d", "test3e",
                                 "test4", "test5"),
                        labs = c("At least one ANC visit with trained health worker",
                                 "At least four ANC visits with any service provider",
                                 "Incur cost for ANC",
                                 "Mean cost of ANC (MMK)",
                                 "Cost of ANC for transport",
                                 "Cost of ANC for registration",
                                 "Cost of ANC for medicine",
                                 "Cost of ANC for laboratory",
                                 "Cost of ANC for provider",
                                 "Cost of ANC for gifts",
                                 "Took loan for ANC costs",
                                 "ANC provider: specialist",
                                 "ANC provider: doctor",
                                 "ANC provider: nurse",
                                 "ANC provider: health assistant",
                                 "ANC provider: private doctor",
                                 "ANC provider: lady health visitor",
                                 "ANC provider: midwife",
                                 "ANC provider: auxiliary midwife",
                                 "ANC provider: traditional birth attendant",
                                 "ANC provider: community health worker",
                                 "ANC provider: ethnic health workers",
                                 "Attended ANC counselling",
                                 "Restricted eating certain foods",
                                 "Restrict vegetables",
                                 "Restrict fruits",
                                 "Restrict grains",
                                 "Restrict meat",
                                 "Restrict fish",
                                 "Restrict dairy",
                                 "Received vitamin B1 supplementation",
                                 "Received iron-folic acid supplementation",
                                 "Mean number of days iron-folic acid taken",
                                 "Cost of iron-folic acid supplementation (MMK)",
                                 "Source of IFA: government hospital",
                                 "Source of IFA: EHO clinic",
                                 "Source of IFA: private doctor/clinic",
                                 "Source of IFA: SRHC/RHC",
                                 "Source of IFA: Routine ANC location in village/ward",
                                 "ANC tests done",
                                 "ANC test: hepatitis B",
                                 "ANC test: hepatitis C",
                                 "ANC test: HIV/AIDS",
                                 "ANC test: syphillis",
                                 "ANC test provider: government hospital",
                                 "ANC test provider: EHO clinic",
                                 "ANC test provider: private doctor/clinic",
                                 "ANC test provider: SRHC/RHC",
                                 "ANC test provider: routine ANC location in village/ward",
                                 "Incur costs for ANC test",
                                 "Cost of ANC test (MMK)"),
                        state = "Kayin")

ancResults <- data.frame(rbind(x, y))

##
usethis::use_data(ancResults, compress = "xz", overwrite = TRUE)

## Add ancResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "ANC")
openxlsx::writeData(wb = resultsTable, sheet = "ANC", ancResults)

## Birth/delivery

x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past") %>%
  recode_birth() %>%
  create_weighted_table(vars = c("birth1a", "birth1b", "birth1c", "birth1d",
                                 "birth1e", "birth1f",
                                 "birth2a", "birth2b", "birth2c", "birth2d", "birth2e",
                                 "birth3",
                                 "birth3a", "birth3b", "birth3c", "birth3d",
                                 "birth3e", "birth3f", "birth3g", "birth3h",
                                 "birth3i",
                                 "birth4a", "birth4b", "birth4c", "birth4d",
                                 "birth5", "birth6", "birth7"),
                        labs = c("Birth/delivery location: home",
                                 "Birth/delivery location: government hospital",
                                 "Birth/delivery location: private doctor/clinic",
                                 "Birth/delivery location: SRHC/RHC",
                                 "Birth/delivery location: routine ANC location in village/ward",
                                 "Birth/delivery location: EHO clinic",
                                 "Reason for choice of birth/delivery location: convenience",
                                 "Reason for choice of birth/delivery location: tradition",
                                 "Reason for choice of birth/delivery location: close distance",
                                 "Reason for choice of birth/delivery location: safety for mother/baby",
                                 "Reason for choice of birth/delivery location: affordable cost",
                                 "Birth/delivery attended by a skilled birth attendant",
                                 "Birth/delivery attendant: doctor",
                                 "Birth/delivery attendant: nurse",
                                 "Birth/delivery attendant: lady health visitor",
                                 "Birth/delivery attendant: midwife",
                                 "Birth/delivery attendant: auxiliary midwife",
                                 "Birth/delivery attendant: traditional birth attendant",
                                 "Birth/delivery attendant: none",
                                 "Birth/delivery attendant: relatives",
                                 "Birth/delivery attendant: EHO cadres",
                                 "Birth/delivery type: normal",
                                 "Birth/delivery type: caesarian",
                                 "Birth/delivery type: vacuum",
                                 "Birth/delivery type: forcepts",
                                 "Incurred costs for birth/delivery",
                                 "Cost of birth/delivery (MMK)",
                                 "Took loan for cost of birth/delivery"),
                        state = "Kayah")

y <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past") %>%
  recode_birth() %>%
  create_weighted_table(vars = c("birth1a", "birth1b", "birth1c", "birth1d",
                                 "birth1e", "birth1f",
                                 "birth2a", "birth2b", "birth2c", "birth2d", "birth2e",
                                 "birth3",
                                 "birth3a", "birth3b", "birth3c", "birth3d",
                                 "birth3e", "birth3f", "birth3g", "birth3h",
                                 "birth3i",
                                 "birth4a", "birth4b", "birth4c", "birth4d",
                                 "birth5", "birth6", "birth7"),
                        labs = c("Birth/delivery location: home",
                                 "Birth/delivery location: government hospital",
                                 "Birth/delivery location: private doctor/clinic",
                                 "Birth/delivery location: SRHC/RHC",
                                 "Birth/delivery location: routine ANC location in village/ward",
                                 "Birth/delivery location: EHO clinic",
                                 "Reason for choice of birth/delivery location: convenience",
                                 "Reason for choice of birth/delivery location: tradition",
                                 "Reason for choice of birth/delivery location: close distance",
                                 "Reason for choice of birth/delivery location: safety for mother/baby",
                                 "Reason for choice of birth/delivery location: affordable cost",
                                 "Birth/delivery attended by a skilled birth attendant",
                                 "Birth/delivery attendant: doctor",
                                 "Birth/delivery attendant: nurse",
                                 "Birth/delivery attendant: lady health visitor",
                                 "Birth/delivery attendant: midwife",
                                 "Birth/delivery attendant: auxiliary midwife",
                                 "Birth/delivery attendant: traditional birth attendant",
                                 "Birth/delivery attendant: none",
                                 "Birth/delivery attendant: relatives",
                                 "Birth/delivery attendant: EHO cadres",
                                 "Birth/delivery type: normal",
                                 "Birth/delivery type: caesarian",
                                 "Birth/delivery type: vacuum",
                                 "Birth/delivery type: forcepts",
                                 "Incurred costs for birth/delivery",
                                 "Cost of birth/delivery (MMK)",
                                 "Took loan for cost of birth/delivery"),
                        state = "Kayin")

birthResults <- data.frame(rbind(x, y))

##
usethis::use_data(birthResults, compress = "xz", overwrite = TRUE)

## Add birthResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "Birth")
openxlsx::writeData(wb = resultsTable, sheet = "Birth", birthResults)


## Postnatal care

x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past") %>%
  recode_pnc() %>%
  create_weighted_table(vars = c("pnc1", "pnc2", "pnc2a",
                                 "pnc3a", "pnc3b", "pnc3c", "pnc3d", "pnc3e",
                                 "pnc3f", "pnc3g", "pnc3h",
                                 "pnc5",
                                 "pnc6",
                                 "pnc7a", "pnc7b", "pnc7c",
                                 "pnc7d", "pnc7e", "pnc7f"),
                        labs = c("Received postnatal care",
                                 "Mean time to postnatal care (hours)",
                                 "Received postnatal care within 48 hours",
                                 "Postnatal care provider: doctor",
                                 "Postnatal care provider: nurse",
                                 "Postnatal care provider: lady health visitor",
                                 "Postnatal care provider: midwife",
                                 "Postnatal care provider: auxiliary midwife",
                                 "Postnatal care provider: traditional birth attendant",
                                 "Postnatal care provider: relatives",
                                 "Postnatal care provider: EHO cadres",
                                 "Vitamin B1 supplementation",
                                 "Incurred costs for postnatal care",
                                 "Postnatal care costs: transportation",
                                 "Postnatal care costs: registration",
                                 "Postnatal care costs: medicine",
                                 "Postnatal care costs: laboratory fees",
                                 "Postnatal care costs: provider fees",
                                 "Postnatal care costs: gifts"),
                        state = "Kayah")

y <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past") %>%
  recode_pnc() %>%
  create_weighted_table(vars = c("pnc1", "pnc2", "pnc2a",
                                 "pnc3a", "pnc3b", "pnc3c", "pnc3d", "pnc3e",
                                 "pnc3f", "pnc3g", "pnc3h",
                                 "pnc5",
                                 "pnc6",
                                 "pnc7a", "pnc7b", "pnc7c",
                                 "pnc7d", "pnc7e", "pnc7f"),
                        labs = c("Received postnatal care",
                                 "Mean time to postnatal care (hours)",
                                 "Received postnatal care within 48 hours",
                                 "Postnatal care provider: doctor",
                                 "Postnatal care provider: nurse",
                                 "Postnatal care provider: lady health visitor",
                                 "Postnatal care provider: midwife",
                                 "Postnatal care provider: auxiliary midwife",
                                 "Postnatal care provider: traditional birth attendant",
                                 "Postnatal care provider: relatives",
                                 "Postnatal care provider: EHO cadres",
                                 "Vitamin B1 supplementation",
                                 "Incurred costs for postnatal care",
                                 "Postnatal care costs: transportation",
                                 "Postnatal care costs: registration",
                                 "Postnatal care costs: medicine",
                                 "Postnatal care costs: laboratory fees",
                                 "Postnatal care costs: provider fees",
                                 "Postnatal care costs: gifts"),
                        state = "Kayin")

pncResults <- data.frame(rbind(x, y))

##
usethis::use_data(pncResults, compress = "xz", overwrite = TRUE)

## Add pncResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "PNC")
openxlsx::writeData(wb = resultsTable, sheet = "PNC", pncResults)


## Newborn care

x <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past") %>%
  recode_nbc() %>%
  create_weighted_table(vars = c("nbc1", "nbc2",
                                 "nbc3a", "nbc3b", "nbc3c", "nbc3d",
                                 "nbc3e", "nbc3f", "nbc3g", "nbc3h",
                                 "nbc5", "nbc6",
                                 "nbc7a", "nbc7b", "nbc7c", "nbc7d",
                                 "nbc7e", "nbc7f", "nbc8",
                                 "nbc9",
                                 "nbc10a", "nbc10b", "nbc10c", "nbc10d", "nbc10e",
                                 "nbc10f", "nbc10g", "nbc11",
                                 "nbc13", "nbc15"),
                        labs = c("Newborn care within 24 hours",
                                 "Newborn care within 48 hours",
                                 "Newborn care provider: doctor",
                                 "Newborn care provider: nurse",
                                 "Newborn care provider: lady health visitor",
                                 "Newborn care provider: midwife",
                                 "Newborn care provider: auxiliary midwife",
                                 "Newborn care provider: traditional birth attendant",
                                 "Newborn care provider: relatives",
                                 "Newborn care provider: EHO cadres",
                                 "Incurred costs for newborn care",
                                 "Cost of newborn care (MMK)",
                                 "Newborn cost: transportation",
                                 "Newborn cost: registration",
                                 "Newborn cost: medicine",
                                 "Newborn cost: laboratory fees",
                                 "Newborn cost: provider fees",
                                 "Newborn cost: gifts",
                                 "Took loan for newborn care costs",
                                 "Fed colostrum to newborn",
                                 "Danger signs: feeding less",
                                 "Danger signs: convulsion",
                                 "Danger signs: high/low temperature",
                                 "Danger signs: local infection",
                                 "Danger signs: no/less movement",
                                 "Danger signs: fast/difficult breathing",
                                 "Danger signs: yellow skin",
                                 "Does not know any danger sign",
                                 "Inappropriate cord care",
                                 "Innappropriate wound care"),
                        state = "Kayah")

y <- create_anc(df = anc2, x = hh, y = hhMembers, status = "past") %>%
  recode_nbc() %>%
  create_weighted_table(vars = c("nbc1", "nbc2",
                                 "nbc3a", "nbc3b", "nbc3c", "nbc3d",
                                 "nbc3e", "nbc3f", "nbc3g", "nbc3h",
                                 "nbc5", "nbc6",
                                 "nbc7a", "nbc7b", "nbc7c", "nbc7d",
                                 "nbc7e", "nbc7f", "nbc8",
                                 "nbc9",
                                 "nbc10a", "nbc10b", "nbc10c", "nbc10d", "nbc10e",
                                 "nbc10f", "nbc10g", "nbc11",
                                 "nbc13", "nbc15"),
                        labs = c("Newborn care within 24 hours",
                                 "Newborn care within 48 hours",
                                 "Newborn care provider: doctor",
                                 "Newborn care provider: nurse",
                                 "Newborn care provider: lady health visitor",
                                 "Newborn care provider: midwife",
                                 "Newborn care provider: auxiliary midwife",
                                 "Newborn care provider: traditional birth attendant",
                                 "Newborn care provider: relatives",
                                 "Newborn care provider: EHO cadres",
                                 "Incurred costs for newborn care",
                                 "Cost of newborn care (MMK)",
                                 "Newborn cost: transportation",
                                 "Newborn cost: registration",
                                 "Newborn cost: medicine",
                                 "Newborn cost: laboratory fees",
                                 "Newborn cost: provider fees",
                                 "Newborn cost: gifts",
                                 "Took loan for newborn care costs",
                                 "Fed colostrum to newborn",
                                 "Danger signs: feeding less",
                                 "Danger signs: convulsion",
                                 "Danger signs: high/low temperature",
                                 "Danger signs: local infection",
                                 "Danger signs: no/less movement",
                                 "Danger signs: fast/difficult breathing",
                                 "Danger signs: yellow skin",
                                 "Does not know any danger sign",
                                 "Inappropriate cord care",
                                 "Innappropriate wound care"),
                        state = "Kayin")

nbcResults <- data.frame(rbind(x, y))

##
usethis::use_data(nbcResults, compress = "xz", overwrite = TRUE)

## Add nbcResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "NBC")
openxlsx::writeData(wb = resultsTable, sheet = "NBC", nbcResults)


## Maternal nutrition

x <- recode_maternal_anthro(df = motherAnthro, x = anthroDF, y = childAnthro) %>%
  create_weighted_anthro(vars = c("muac", "gam", "mam", "sam"),
                         labs = c("Mean maternal MUAC (cm)",
                                  "Maternal global wasting - MUAC less than 21.0 cms",
                                  "Maternal moderate wasting - MUAC less than 21.0 cms and greater than or equal to 18.5 cms",
                                  "Maternal severe wasting - MUAC less than 18.5 cms"),
                         state = "Kayah")

y <- recode_maternal_anthro(df = motherAnthro, x = anthroDF, y = childAnthro) %>%
  create_weighted_anthro(vars = c("muac", "gam", "mam", "sam"),
                         labs = c("Mean maternal MUAC (cm)",
                                  "Maternal global wasting - MUAC less than 21.0 cms",
                                  "Maternal moderate wasting - MUAC less than 21.0 cms and greater than or equal to 18.5 cms",
                                  "Maternal severe wasting - MUAC less than 18.5 cms"),
                         state = "Kayin")

mnutResults <- data.frame(rbind(x, y))

##
usethis::use_data(mnutResults, compress = "xz", overwrite = TRUE)

## Add mnutResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "Maternal nutrition")
openxlsx::writeData(wb = resultsTable, sheet = "Maternal nutrition", mnutResults)


## MDDW

x <- recode_mddw(df = hh) %>%
  create_weighted_table(vars = "mddw",
                        labs = "Minimum dietary diversity for women",
                        state = "Kayah")

y <- recode_mddw(df = hh) %>%
  create_weighted_table(vars = "mddw",
                        labs = "Minimum dietary diversity for women",
                        state = "Kayin")

mddwResults <- data.frame(rbind(x, y))

##
usethis::use_data(mddwResults, compress = "xz", overwrite = TRUE)

## Add mddwResults to a worksheet in resultsTable workbook
openxlsx::addWorksheet(wb = resultsTable, sheetName = "MDDW")
openxlsx::writeData(wb = resultsTable, sheet = "MDDW", mddwResults)

openxlsx::saveWorkbook(resultsTable, file = "data-raw/resultsTable.xlsx", overwrite = TRUE)
