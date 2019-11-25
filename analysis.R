################################################################################################################
# Obtains current occurrence data in GBIF for a taxonomic list of spiders and two .csv files
# containing literature based records for spiders of the Iberian Penninsula and elsewhere.
# Calculates extent of occurrence (an IUCN metric for Red Lists) using these records and compares the putative
# IUCN Red List designations of the litereture, GBIF, and combined datasets.
#
# Author: Vaughn M. Shirey
################################################################################################################

# load required libraries
library(red); library(reshape2); library(tidyverse)

# load Iberian dataset & endemic taxon list
idat <- read.csv("iberian_occur.txt", sep = "\t", header = TRUE)
itax <- read.csv("iberian_endem.csv", sep = ",", header = FALSE)

# load global dataset
gdat <- read.csv("srli_occur.txt", sep = "\t", header = TRUE)

# load GBIF data
gbif <- read.csv("gbif_occur.csv", sep = "\t", header = TRUE, stringsAsFactors = FALSE) %>%
  filter(datasetKey != "8655c292-f762-11e1-a439-00145eb45e9a" & datasetKey !="c47e7a5b-692d-4e26-a32f-74b0188eb594" &
           datasetKey != "8655c292-f762-11e1-a439-00145eb45e9a") %>%
  select(species, decimalLongitude, decimalLatitude) # remove contributed literature data from gbif occurrences

# reformat both Iberian and global literature data to match Red-package GBIF output, grab unique taxa
idat <- (data.frame(long = idat[, 17], lat = idat[, 16], V1 = paste(idat$genus, idat$specificEpithet)))
gdat <- (data.frame(long = gdat[, 9], lat = gdat[, 8], V1 = paste(gdat$genus, gdat$specificEpithet)))

idat <- idat[!is.na(idat$long),]
gdat <- gdat[!is.na(gdat$long),]

idat <- idat[(idat$V1 %in% itax$V1),] # use only Iberian endemics

itaxa <- as.vector(unique(idat$V1))
gtaxa <- as.vector(unique(gdat$V1))

# grab GBIF records for all unique taxa in the Iberian dataset, remove records from contributed literature dataset
idat_gbif <- gbif %>% filter(species %in% itaxa) %>%
  mutate(long = as.numeric(decimalLongitude), lat = round(as.numeric(decimalLatitude), 5), V1 = species) %>%
  select(long, lat, V1)

# grab GBIF records for all unique taxa in the global dataset, remove records from contributed literature dataset
gdat_gbif <- gbif %>% filter(species %in% gtaxa) %>%
  mutate(long = as.numeric(decimalLongitude), lat = round(as.numeric(decimalLatitude), 5), V1 = species) %>%
  select(long, lat, V1)

# calculate EOO from literature only data set for both Iberian and global lists
n <- length(itaxa)
idat_eoo <- data.frame()

for(i in 1:n){
  taxon <- itaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  idat_eoo[i, 1] <- taxon
  idat_eoo[i, 2] <- eoo(idat[which(idat[, 3]==taxon), c(1,2)])
}

n <- length(gtaxa)
gdat_eoo <- data.frame()

for(i in 1:n){
  taxon <- gtaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  gdat_eoo[i, 1] <- taxon
  gdat_eoo[i, 2] <- eoo(gdat[which(gdat[, 3]==taxon), c(1,2)])
}

# calculate EOO from GBIF only dataset for both Iberian and global lists
n <- length(itaxa)
idat_gbif_eoo <- data.frame()

for(i in 1:n){
  taxon <- itaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  idat_gbif_eoo[i, 1] <- taxon
  idat_gbif_eoo[i, 2] <- eoo(na.omit(idat_gbif[which(idat_gbif[, 3]==taxon), c(1,2)]))
}

n <- length(gtaxa)
gdat_gbif_eoo <- data.frame()

for(i in 1:n){
  taxon <- gtaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  gdat_gbif_eoo[i, 1] <- taxon
  gdat_gbif_eoo[i, 2] <- eoo(na.omit(gdat_gbif[which(gdat_gbif[, 3]==taxon), c(1,2)]))
}

# calculate EOO for the combined dataset for both Iberian and global lists
idat_merge <- rbind(idat, idat_gbif)
gdat_merge <- rbind(gdat, gdat_gbif)

n <- length(itaxa)
idat_merge_eoo <- data.frame()

for(i in 1:n){
  taxon <- itaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  idat_merge_eoo[i, 1] <- taxon
  idat_merge_eoo[i, 2] <- eoo(na.omit(idat_merge[which(idat_merge[, 3]==taxon), c(1,2)]))
}

n <- length(gtaxa)
gdat_merge_eoo <- data.frame()

for(i in 1:n){
  taxon <- gtaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  gdat_merge_eoo[i, 1] <- taxon
  gdat_merge_eoo[i, 2] <- eoo(na.omit(gdat_merge[which(gdat_merge[, 3]==taxon), c(1,2)]))
}

# cbind and write results to .csv files
idat_all_eoo <- cbind(idat_eoo, idat_gbif_eoo, idat_merge_eoo)
idat_all_eoo <- idat_all_eoo[, c(1,2,4,6)]
colnames(idat_all_eoo) <- c("ScientificName", "Literature", "GBIF", "Combined")

gdat_all_eoo <- cbind(gdat_eoo, gdat_gbif_eoo, gdat_merge_eoo)
gdat_all_eoo <- gdat_all_eoo[, c(1,2,4,6)]
colnames(gdat_all_eoo) <- c("ScientificName", "Literature", "GBIF", "Combined")

write.csv(idat_all_eoo, "Iberian_results.csv")
write.csv(gdat_all_eoo, "Global_results.csv")