################################################################################
#
# Libraries
#
################################################################################

options(stringsAsFactors = FALSE)

pop <- read.csv("data-raw/pop/popMyanmar.csv")

x <- pop[ , c("geo_ward_vt_eho", "population")]
y <- hh[hh$sample_component == 1, c("geo_state", "geo_rural", "geo_villward")]
hhWeight <- hh[hh$sample_component == 1, ]
z <- merge(x, y, by.x = "geo_ward_vt_eho", by.y = "geo_villward", all.y = TRUE)
hhWeight <- merge(x, hhWeight, by.x = "geo_ward_vt_eho", by.y = "geo_villward", all.y = TRUE)

### get median population size of clusters in a state

medianPop <- tapply(X = z$population, INDEX = z$geo_state, FUN = median, na.rm = TRUE)

z$population[is.na(z$population) & z$geo_state == "MMR002"] <- medianPop[1]
z$population[is.na(z$population) & z$geo_state == "MMR003"] <- medianPop[2]

### get weights for MMR002 and geo_rural == 0 (Rural)

z1 <- z[z$geo_state == "MMR002" & z$geo_rural == "0", ]

z1 <- aggregate(x = z1[ , c("geo_state", "geo_rural", "geo_ward_vt_eho", "population")],
                by = list(z1$geo_ward_vt_eho), FUN = "unique")

z1$totalPop <- sum(z1$population)

### get weights for MMR002 and geo_rural == 1 (Urban)

z2 <- z[z$geo_state == "MMR002" & z$geo_rural == "1", ]

z2 <- aggregate(x = z2[ , c("geo_state", "geo_rural", "geo_ward_vt_eho", "population")],
                by = list(z2$geo_ward_vt_eho), FUN = "unique")

z2$totalPop <- sum(z2$population)


### get weights for MMR002 and geo_rural == 2 (EHO)

z3 <- z[z$geo_state == "MMR002" & z$geo_rural == "2", ]

z3 <- aggregate(x = z3[ , c("geo_state", "geo_rural", "geo_ward_vt_eho", "population")],
                by = list(z3$geo_ward_vt_eho), FUN = "unique")

z3$totalPop <- sum(z3$population)


### get weights for MMR003 and geo_rural == 0 (Rural)

z4 <- z[z$geo_state == "MMR003" & z$geo_rural == "0", ]

z4 <- aggregate(x = z4[ , c("geo_state", "geo_rural", "geo_ward_vt_eho", "population")],
                by = list(z4$geo_ward_vt_eho), FUN = "unique")

z4$totalPop <- sum(z4$population)

### get weights for MMR003 and geo_rural == 1 (Urban)

z5 <- z[z$geo_state == "MMR003" & z$geo_rural == "1", ]

z5 <- aggregate(x = z5[ , c("geo_state", "geo_rural", "geo_ward_vt_eho", "population")],
                by = list(z5$geo_ward_vt_eho), FUN = "unique")

z5$totalPop <- sum(z5$population)


### get weights for MMR003 and geo_rural == 2 (EHO)

z6 <- z[z$geo_state == "MMR003" & z$geo_rural == "2", ]

z6 <- aggregate(x = z6[ , c("geo_state", "geo_rural", "geo_ward_vt_eho", "population")],
                by = list(z6$geo_ward_vt_eho), FUN = "unique")

z6$totalPop <- sum(z6$population)


zz <- data.frame(rbind(z1, z2, z3, z4, z5, z6))

### Kayin pop - 1055359; Kayah pop - 286627

nClusters <- vector(mode = "numeric", length = nrow(zz))
nClusters[zz$geo_state == "MMR002" & zz$geo_rural == "0"] <- 24
nClusters[zz$geo_state == "MMR002" & zz$geo_rural == "1"] <- 24
nClusters[zz$geo_state == "MMR002" & zz$geo_rural == "2"] <- 26
nClusters[zz$geo_state == "MMR003" & zz$geo_rural == "0"] <- 27
nClusters[zz$geo_state == "MMR003" & zz$geo_rural == "1"] <- 17
nClusters[zz$geo_state == "MMR003" & zz$geo_rural == "2"] <- 27

zz$nClusters <- nClusters

clusterSize <- data.frame(table(z$geo_ward_vt_eho))
names(clusterSize) <- c("geo_villward", "size")

zz <- merge(zz, clusterSize,
            by.x = "geo_ward_vt_eho", by.y = "geo_villward",
            all.x = TRUE)

zz$weights <- get_weights(n = zz$population, N = zz$totalPop, m = zz$nClusters, c = zz$size)


hhWeight <- merge(zz[ c("geo_ward_vt_eho", "weights")], hhWeight, by.x = "geo_ward_vt_eho", by.y = "geo_ward_vt_eho", all.y = TRUE)


################################################################################

cluster <- paste(hh$geo_vt, hh$geo_ward, hh$geo_ehovill, sep = "")

clusterList <- data.frame(hh$geo_vt, hh$geo_ward, hh$geo_ehovill, cluster)

uniqueClusters <- data.frame(unique(cluster))

write.csv(uniqueClusters, "data-raw/uniqueClusters.csv", row.names = FALSE)




