# Obtains current occurrence data in GBIF for a taxonomic list of spiders and two .csv files
# containing literature based records for spiders of the Iberian Penninsula and elsewhere.
# Calculates extent of occurrence (an IUCN metric for Red Lists) using these records and compares the putative
# IUCN Red List designations of the litereture, GBIF, and combined datasets.
#
# Author: Vaughn M. Shirey

# load required libraries
library(red); library(reshape2); library(dplyr)

# load Iberian dataset
idat <- read.csv("iberian_occur.txt", sep = "\t", header = TRUE)

# load global dataset
gdat <- read.csv("srli_occur.txt", sep = "\t", header = TRUE)

# reformat both Iberian and global literature data to match Red-package GBIF output, grab unique taxa
idat <- (data.frame(long = idat[, 17], lat = idat[, 16], V1 = idat[, 19]))
gdat <- (data.frame(long = gdat[, 9], lat = gdat[, 8], V1 = gdat[, 13]))
idat <- idat[!is.na(idat$long),]
gdat <- gdat[!is.na(gdat$long),]

itaxa <- as.vector(unique(idat[, 3]))
gtaxa <- as.vector(unique(gdat[, 3]))

# grab GBIF records for all unique taxa in the Iberian dataset, remove records from contributed literature dataset
n <- length(itaxa)
temp <- data.frame()
idat_gbif <- list()

for(i in 1:n){
  taxon <- itaxa[i]
  
  print(paste("Processing: ", i, " of ", n, "."))
  
  temp <- data.frame()   
  try(temp <- records(taxon))
  try(temp$V1 <- taxon)
  
  idat_gbif [[i]] <- temp
}

idat_gbif = do.call(rbind, idat_gbif) 
idat_gbif <- unique(idat_gbif) 
idat_gbif <- anti_join(idat_gbif, idat, by=c("long", "lat", "V1"))

# grab GBIF records for all unique taxa in the global dataset, remove records from contributed literature dataset
n <- length(gtaxa)
temp <- data.frame()
gdat_gbif <- list()

for(i in 1:n){
  taxon <- gtaxa[i]
  
  print(paste("Processing: ", i, " of ", n, "."))
  
  temp <- data.frame()   
  try(temp <- records(taxon))
  try(temp$V1 <- taxon)
  
  gdat_gbif[[i]] <- temp
}

gdat_gbif = do.call(rbind, gdat_gbif)
gdat_gbif <- unique(gdat_gbif)
gdat_gbif <- anti_join(gdat_gbif, gdat, by=c("long", "lat", "V1"))

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
  idat_gbif_eoo[i, 2] <- eoo(idat_gbif[which(idat_gbif[, 3]==taxon), c(1,2)])
}

n <- length(gtaxa)
gdat_gbif_eoo <- data.frame()

for(i in 1:n){
  taxon <- gtaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  gdat_gbif_eoo[i, 1] <- taxon
  gdat_gbif_eoo[i, 2] <- eoo(gdat_gbif[which(gdat_gbif[, 3]==taxon), c(1,2)])
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
  idat_merge_eoo[i, 2] <- eoo(idat_merge[which(idat_merge[, 3]==taxon), c(1,2)])
}

n <- length(gtaxa)
gdat_merge_eoo <- data.frame()

for(i in 1:n){
  taxon <- gtaxa[i]
  
  print(paste("Calculating: ", i, " of ", n, "."))
  
  gdat_merge_eoo[i, 1] <- taxon
  gdat_merge_eoo[i, 2] <- eoo(gdat_merge[which(gdat_merge[, 3]==taxon), c(1,2)])
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