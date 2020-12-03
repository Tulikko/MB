# Uine Kailamäki 09/11/2020
# File for normalizing pXRF results
# Passphrase : dominion


###############

# READING RAW DATA (Change working directory to where your Results file is!)

# Access necessary libraries

library(dplyr); library(openxlsx)

# Read data

Results <- read.xlsx("Results.xlsx", sep=";")

# Select which columns to keep

keep_c <- c("Sample", "MgO", "MgO;Err", "Al2O3", "Al2O3;Err", "SiO2",  "SiO2;Err", "P2O5", "P2O5;Err", "S", "S;Err", "Cl", "Cl;Err", "K2O", "K2O;Err", "CaO", "CaO;Err", "Ti", "Ti;Err", "V", "V;Err", "Cr", "Cr;Err", "Mn", "Mn;Err", "Fe", "Fe;Err", "Co", "Co;Err", "Ni", "Ni;Err", "Cu",  "Cu;Err", "Zn", "Zn;Err", "As", "As;Err", "Se", "Se;Err", "Rb", "Rb;Err", "Sr", "Sr;Err", "Y", "Y;Err", "Zr", "Zr;Err", "Nb", "Nb;Err", "Mo", "Mo;Err", "Rh", "Rh;Err", "Pd", "Pd;Err", "Ag", "Ag;Err", "Cd", "Cd;Err", "Sn", "Sn;Err", "Sb", "Sb;Err", "Ba", "Ba;Err", "La", "La;Err", "Ce", "Ce;Err", "Hf", "Hf;Err", "Ta", "Ta;Err", "W", "W;Err", "Pt", "Pt;Err", "Au", "Au;Err", "Hg", "Hg;Err", "Tl", "Tl;Err", "Pb", "Pb;Err", "Bi", "Bi;Err", "Th", "Th;Err", "U", "U;Err")

# Create the analysis dataset with chosen columns

pxrf <- select(Results, one_of(keep_c))

# Change "Sample" to a factor

pxrf$Sample <- factor(pxrf$Sample)

# Remove any rows containing 'ERROR' or 'TEST'

pxrf <- filter(pxrf, !grepl('TEST', Sample))
pxrf <- filter(pxrf, !grepl('ERROR', Sample))


###############

# TURNING LOD:s and missing values to NA:s

values <- c("MgO", "Al2O3", "SiO2", "P2O5", 
            "S", "Cl", "K2O", "CaO", "Ti", "V", "Cr", 
            "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "As", 
            "Se", "Rb", "Sr", "Y", "Zr", "Nb", "Mo", 
            "Rh", "Pd", "Ag", "Cd", "Sn", "Sb", "Ba", 
            "La", "Ce", "Hf", "Ta", "W", "Pt", "Au", 
            "Hg", "Tl", "Pb", "Bi", "Th", "U")

normal <- select(pxrf, one_of(values))

n <- normal %>% mutate_if(is.character,as.numeric)


###############

# NORMALIZING COLUMNS TO VALUES BETWEEN 0-1

# Create function “normalize” (na.rm=TRUE excludes NA values from analysis)
# nn = (x-min)/(max-min)
normalize <- function(x, na.rm=TRUE){(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

# Execute “normalize” to “n”, check min/max values
nn <- as.data.frame(apply(n, 2, normalize))


###############

# STANDARDIZING BY Z-SCORE (CENTRALIZED AROUND 0; PRESERVES DIFFERENCES IN DISTRIBUTION BETTER THAN 0-1)
# zz = (x - mean(x)) / sd(x)

# Standardizing by z-standardization
zz <- as.data.frame(scale(n))


###############

# AVERAGE NORMALIZED VALUES PER SAMPLE

# Add column sample from original file

nn <- nn %>% mutate(sample = pxrf$Sample)
zz <- zz %>% mutate(sample = pxrf$Sample)

# Averages by "sample" (this creates a new column "Group.1")

avrg_n <- aggregate(nn, by = list(nn$sample), FUN = mean)
avrg_z <- aggregate(zz, by = list(zz$sample), FUN = mean)

# Remove original "sample", create new "Sample" from "Group.1", 
avrg_n <- avrg_n %>% select(-sample)
names(avrg_n)[names(avrg_n) == "Group.1"] <- "Sample"

avrg_z <- avrg_z %>% select(-sample)
names(avrg_z)[names(avrg_z) == "Group.1"] <- "Sample"


###############

# REMOVING NA VALUES

# Omitting columns if they have only NA:s / if they have any NA:s
not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

avrg_n1 <- avrg_n %>% select_if(not_all_na)
avrg_n2 <- avrg_n %>% select_if(not_any_na)                 

avrg_z1 <- avrg_z %>% select_if(not_all_na)
avrg_z2 <- avrg_z %>% select_if(not_any_na) 


###############

# SAVING ANALYSIS DATASETS

# Save analysis datasets as .txt and .xlsx
write.xlsx(avrg_n1, file="n1-pXRF.xlsx")
write.xlsx(avrg_n2, file="n2-pXRF.xlsx")
write.xlsx(avrg_z1, file="z1-pXRF.xlsx")
write.xlsx(avrg_z2, file="z2-pXRF.xlsx")

write.table(avrg_n1, file="n1-pXRF.txt")
write.table(avrg_n2, file="n2-pXRF.txt")
write.table(avrg_z1, file="z1-pXRF.xlsx")
write.table(avrg_z2, file="z2-pXRF.xlsx")

###############

# OTHER OPTIONS FOR NORMALIZING

# Normalize functions from Marta
# library(mQTL.NMR)
# normalise(x, method, refID)

# library(metabolomics)
# https://rdrr.io/cran/metabolomics/man/Normalise.html
# Normalise(x, method, refvec)