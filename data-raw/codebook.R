################################################################################
#
# Prepare codebook
#
################################################################################

library(readxl)
library(openxlsx)

## Read survey sheet of xlsform
survey <- read_xlsx(path = "data-raw/forms/baseline_mcct_final.xls", sheet = "survey")

## Select rows with type that contains data
survey <- survey[!survey$type %in% c("begin group", "begin repeat",
                                     "end group", "end repeat", "note"), ]

survey <- survey[!is.na(survey$type), ]

## Read choices sheet of xlsform
choices <- read_xlsx(path = "data-raw/forms/baseline_mcct_final.xls", sheet = "choices")

## Remove blanks
choices <- choices[!is.na(choices$list_name), ]

get_choices <- function(survey, choices) {

  responses <- vector(mode = "character", length = nrow(survey))

  for(i in 1:nrow(survey)) {

    if(stringr::str_detect(string = survey$type[i], pattern = "select_one ")) {
      choice.set <- stringr::str_remove_all(string = survey$type[i],
                                            pattern = "select_one ")

      responses[i] <- paste(choices$value[choices$list_name == choice.set],
                            choices$`label:: english`[choices$list_name == choice.set],
                            sep = "=",
                            collapse = "; ")
    }

    if(stringr::str_detect(string = survey$type[i],
                           pattern = "select_multiple ")) {
      choice.set <- stringr::str_remove_all(string = survey$type[i],
                                            pattern = "select_multiple ")

      responses[i] <- paste(choices$value[choices$list_name == choice.set],
                            choices$`label:: english`[choices$list_name == choice.set],
                            sep = "=",
                            collapse = "; ")
    }
  }
  return(responses)
}

responses <- get_choices(survey = survey, choices = choices)

codebook <- data.frame(variable = as.character(survey$name),
                       question = as.character(survey$`label:: english`),
                       choices = as.character(responses))

codebook <- tibble::as_tibble(codebook)
usethis::use_data(codebook, overwrite = TRUE)

################################################################################

## Create HH codebook
householdCodebook <- codebook[codebook$variable %in% names(hh), ]

householdCodebook$variable <- as.character(householdCodebook$variable)
householdCodebook$question <- as.character(householdCodebook$question)
householdCodebook$choices <- as.character(householdCodebook$choices)

householdCodebook[is.na(householdCodebook$question), "question"] <- c("Survey start time",
                                                        "Survey end time",
                                                        "Device unique identifier",
                                                        "Subscriber unique identifier",
                                                        "SIM unique identifier",
                                                        "Device phone number",
                                                        "Village/ward code/name",
                                                        "Enumerator name",
                                                        "Calculated respondent identifier",
                                                        "Calculated number of married women in household",
                                                        "Calculated number or pregnant women in household",
                                                        "Calculated number of mothers with under 5 children in household",
                                                        "Calculated number of children under 5 in household")

write.csv(householdCodebook, "data-raw/codebook/householdCodebook.csv", row.names = FALSE)

## Create hhMembers codebook
rosterCodebook <- codebook[codebook$variable %in% names(hhMembers), ]

rosterCodebook$variable <- as.character(rosterCodebook$variable)
rosterCodebook$question <- as.character(rosterCodebook$question)
rosterCodebook$choices <- as.character(rosterCodebook$choices)

rosterCodebook[is.na(rosterCodebook$question), "question"] <- c("Household member counting variable",
                                                                "Calculated age of household member in months",
                                                                "Calculated age of household member in years",
                                                                "Calculated age of household member in years with no missing values",
                                                                "Calculated age of household member in years final calculation",
                                                                "Calculated age of household member in months with no missing values",
                                                                "Calcualted age of household member in months final calculation",
                                                                "Calculated variable to indicate that household member is married or not; 0 = not married; else = married",
                                                                "Calculated variable to indicate that household member is married or not; 1 = married; NA = not married",
                                                                "Calculated variable to indicate that household member is pregnant or not; 0 = not pregnant; 1 = pregnant",
                                                                "Calculated variable to indicate that household member is a mother of under 5 child/children; 0 = NO; else = YES",
                                                                "Calculated variable to indicate that household member is a mother of under 2 child/children; 0 = NO; else = YES",
                                                                "Calculated variable to indicate that household member is a pregnant or lactating women (PLW); 1 = YES; NA = NO",
                                                                "Calculated variable to indicate that household member is an under 5 child; 1 = YES; NA = NO")

rosterCodebook$question <- stringr::str_remove_all(string = rosterCodebook$question,
                                               pattern = '<span style=\"color:blue\"|>|\\$|\\{|\\}</span>')

rosterCodebook$question <- stringr::str_replace_all(string = rosterCodebook$question,
                                                pattern = "\\(hh_mem_name\\)",
                                                replacement = "[HH_MEM_NAME]")

write.csv(rosterCodebook, "data-raw/codebook/rosterCodebook.csv", row.names = FALSE)

## childHealth codebook

childHealthCodebook <- codebook[codebook$variable %in% names(childHealth), ]
write.csv(childHealthCodebook, "data-raw/codebook/childHealthCodebook.csv", row.names = FALSE)

## iycf codebook

iycfCodebook <- codebook[codebook$variable %in% names(iycf), ]
write.csv(iycfCodebook, "data-raw/codebook/iycfCodebook.csv", row.names = FALSE)

## anc1 codebook

anc1Codebook <- codebook[codebook$variable %in% names(anc1), ]
write.csv(anc1Codebook, "data-raw/codebook/anc1Codebook.csv", row.names = FALSE)

## anc2 codebook

anc2Codebook <- codebook[codebook$variable %in% names(anc2), ]
write.csv(anc2Codebook, "data-raw/codebook/anc2Codebook.csv", row.names = FALSE)

## Save codebooks into a single xlsx
codebook <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb = codebook, sheetName = "household")
openxlsx::writeData(wb = codebook, sheet = "household", x = householdCodebook)
openxlsx::addWorksheet(wb = codebook, sheetName = "roster")
openxlsx::writeData(wb = codebook, sheet = "roster", x = rosterCodebook)
openxlsx::addWorksheet(wb = codebook, sheetName = "childHealth")
openxlsx::writeData(wb = codebook, sheet = "childHealth", x = childHealthCodebook)
openxlsx::addWorksheet(wb = codebook, sheetName = "iycf")
openxlsx::writeData(wb = codebook, sheet = "iycf", x = iycfCodebook)
openxlsx::addWorksheet(wb = codebook, sheetName = "anc1")
openxlsx::writeData(wb = codebook, sheet = "anc1", x = anc1Codebook)
openxlsx::addWorksheet(wb = codebook, sheetName = "anc2")
openxlsx::writeData(wb = codebook, sheet = "anc2", x = anc2Codebook)


################################################################################

## Read survey sheet of xlsform
survey <- read_xlsx(path = "data-raw/forms/baseline_mcct_anthro_final.xls", sheet = "survey")

## Select rows with type that contains data
survey <- survey[!survey$type %in% c("begin group", "begin repeat",
                                     "end group", "end repeat", "note"), ]

survey <- survey[!is.na(survey$type), ]

## Read choices sheet of xlsform
choices <- read_xlsx(path = "data-raw/forms/baseline_mcct_anthro_final.xls", sheet = "choices")

## Remove blanks
choices <- choices[!is.na(choices$list_name), ]

responses <- get_choices(survey = survey, choices = choices)

codebookAnthro <- data.frame(variable = as.character(survey$name),
                             question = as.character(survey$`label:: english`),
                             choices = as.character(responses))

codebookAnthro <- tibble::as.tibble(codebookAnthro)
write.csv(codebookAnthro, "data-raw/codebook/codebookAnthro.csv", row.names = FALSE)
openxlsx::addWorksheet(wb = codebook, sheetName = "anthro")
openxlsx::writeData(wb = codebook, sheet = "anthro", x = codebookAnthro)

usethis::use_data(codebookAnthro, overwrite = TRUE)

################################################################################

## Read survey sheet of xlsform
survey <- read_xlsx(path = "data-raw/forms/baseline_townshipprofile_final.xls", sheet = "survey")

## Select rows with type that contains data
survey <- survey[!survey$type %in% c("begin group", "begin repeat",
                                     "end group", "end repeat", "note"), ]

survey <- survey[!is.na(survey$type), ]

## Read choices sheet of xlsform
choices <- read_xlsx(path = "data-raw/forms/baseline_townshipprofile_final.xls", sheet = "choices")

## Remove blanks
choices <- choices[!is.na(choices$list_name), ]

responses <- get_choices(survey = survey, choices = choices)

codebookTownship <- data.frame(variable = as.character(survey$name),
                               question = as.character(survey$`label:: english`),
                               choices = as.character(responses))

codebookTownship <- tibble::as.tibble(codebookTownship)

## Write as CSV
write.csv(codebookTownship, "data-raw/codebook/codebookTownship.csv", row.names = FALSE)

## Write as xlsx
openxlsx::addWorksheet(wb = codebook, sheetName = "township")
openxlsx::writeData(wb = codebook, sheet = "township", x = codebookTownship)

usethis::use_data(codebookTownship, overwrite = TRUE)

################################################################################

## Read survey sheet of xlsform
survey <- read_xlsx(path = "data-raw/forms/baseline_villprofile_final.xls", sheet = "survey")

## Select rows with type that contains data
survey <- survey[!survey$type %in% c("begin group", "begin repeat",
                                     "end group", "end repeat", "note"), ]

survey <- survey[!is.na(survey$type), ]

## Read choices sheet of xlsform
choices <- read_xlsx(path = "data-raw/forms/baseline_villprofile_final.xls", sheet = "choices")

## Remove blanks
choices <- choices[!is.na(choices$list_name), ]

responses <- get_choices(survey = survey, choices = choices)

codebookVillage <- data.frame(variable = as.character(survey$name),
                              question = as.character(survey$`label:: english`),
                              choices = as.character(responses))

codebookVillage <- tibble::as.tibble(codebookVillage)

## Write as CSV
write.csv(codebookVillage, "data-raw/codebook/codebookVillage.csv", row.names = FALSE)

## Write as xlsx
openxlsx::addWorksheet(wb = codebook, sheetName = "village")
openxlsx::writeData(wb = codebook, sheet = "village", x = codebookVillage)

usethis::use_data(codebookVillage, overwrite = TRUE)


## output XLSX codebook
openxlsx::saveWorkbook(wb = codebook,
                       file = "data-raw/codebook/codebook.xlsx",
                       overwrite = TRUE)
