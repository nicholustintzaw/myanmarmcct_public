if(!require(openxlsx)) install.packages("openxlsx")

x <- read.xlsx(xlsxFile = "data-raw/checks/mcct_baseline_correction.xlsx", sheet = 1)
x <- x[ , c("_index", "variable", "newvalue")]
names(x) <- c("index", "variable", "newvalue")

y <- read.xlsx(xlsxFile = "data-raw/checks/mcct_baseline_correction_additional.xlsx", sheet = 1)
y <- y[ , c("_index", "variable", "newvalue")]
names(y) <- c("index", "variable", "newvalue")

checks <- data.frame(rbind(x, y))

id <- 1:nrow(checks)
checks <- data.frame(id, checks)

usethis::use_data(checks, overwrite = TRUE)


x <- read.xlsx(xlsxFile = "data-raw/checks/mcct_baseline_anthro_correction.xlsx", sheet = 1)
x <- x[ , c("_index", "variable", "newvalue")]
names(x) <- c("index", "variable", "newvalue")

y <- read.xlsx(xlsxFile = "data-raw/checks/mcct_baseline_anthro_id_matching.xlsx", sheet = 1)
y <- y[ , c("_index", "variable", "newvalue")]
names(y) <- c("index", "variable", "newvalue")

anthroChecks <- data.frame(rbind(x, y))

usethis::use_data(anthroChecks, overwrite = TRUE)
