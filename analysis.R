# Obtains current occurrence data in GBIF for a taxonomic list of spiders and two .csv files
# containing literature based records for spiders of the Iberian Penninsula and elsewhere.
# Calculates extent of occurrence (an IUCN metric for Red Lists) using these records and compares the putative
# IUCN Red List designations of the litereture, GBIF, and combined datasets.
#
# Author: Vaughn M. Shirey

# load required libraries
library(red); library(reshape2)

# load and extract unique taxa from Iberian dataset
idat <- read.csv("iberian_occur.txt", sep = "\t", header = TRUE)
itaxa <- as.vector(unique(idat$scientificName))

# load and extract unique taxa from global dataset
gdat <- read.csv("srli_occur.txt", sep = "\t", header = TRUE)
gtaxa <- as.vector(unique(gdat$scientificName))

# grab GBIF records for all unique taxa in the Iberian dataset, remove records from contributed literature dataset
n <- length(itaxa)
temp <- data.frame()
dat <- list()

for(i in 1:n){
  taxon <- itaxa[i] ## grab names
  
  temp <- data.frame()   
  try(temp <- records(taxon))  ## try getting records from GBIF
  try(temp$V1 <- taxon)
  
  dat[[i]] <- temp
}

# grab GBIF records for all unique taxa in the global dataset, remove records from contributed literature dataset

