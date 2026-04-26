##CEC Prioritization ECOTOX Benchmark Derivation## TIER 1 ####

library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(stringr)
library(xlsx)
library(rJava)
library(ggpubr)

##pull together ECOTOX data####
glri.output <- read.csv("aquatic-data-all-fields-glri.csv")

cec.prioritization <- glri.output

names(cec.prioritization)

#select relevant data fields & filter ####

list(unique(cec.prioritization.1$CONC1_TYPE_STANDARDIZED))
list(unique(cec.prioritization.1$CONC1_MEAN_OP_STANDARDIZED))
list(unique(cec.prioritization.1$CHEMICAL_ANALYSIS))
list(unique(cec.prioritization.1$CONC1_UNITS_STANDARDIZED))
list(unique(cec.prioritization.1$OBSERVED_DURATION_MEAN_OP_DAYS))
list(unique(cec.prioritization.1$ENDPOINT))
list(unique(cec.prioritization.1$EFFECT))
list(unique(cec.prioritization.1$MEDIA_TYPE))
list(unique(cec.prioritization.1$EXPOSURE_TYPE))
list(unique(cec.prioritization.1$STATISTICAL_SIGNIFICANCE))
list(unique(cec.prioritization.1$SPECIES_KINGDOM))

##measured concentrations####
cec.prioritization.1 <- cec.prioritization%>% 
  select(CAS_NUMBER, CHEMICAL_NAME, CHEMICAL_ANALYSIS, TEST_METHOD, SPECIES_SCIENTIFIC_NAME, SPECIES_COMMON_NAME, SPECIES_KINGDOM, SPECIES_PHYLUM, SPECIES_SUBPHYLUM,
         SPECIES_GROUP, ORGANISM_LIFESTAGE, EXPOSURE_TYPE, MEDIA_TYPE, TEST_LOCATION, CONC1_MEAN_OP_STANDARDIZED, 
         CONC1_TYPE_STANDARDIZED, CONC1_MEAN_STANDARDIZED, CONC1_MIN_STANDARDIZED, CONC1_MAX_STANDARDIZED, CONC1_UNITS_STANDARDIZED, EFFECT, EFFECT_MEASUREMENT, ENDPOINT, STATISTICAL_SIGNIFICANCE, SIGNIFICANCE_LEVEL_MEAN, 
         OBSERVED_DURATION_MEAN_DAYS, OBSERVED_DURATION_MEAN_OP_DAYS, STUDY_DURATION_MEAN_DAYS,  STUDY_DURATION_MEAN_OP_DAYS, REFERENCE_NUMBER, TITLE, AUTHOR, PUBLICATION_YEAR) %>%
  filter(CONC1_TYPE_STANDARDIZED !=("Formulation"), !CONC1_MEAN_OP_STANDARDIZED %in% c(">", "<", "<=", "~", ">="), 
         CHEMICAL_ANALYSIS %in% c("Measured", "Chemical analysis reported", "Not coded", "Unspecified", "Not reported"),
         CONC1_UNITS_STANDARDIZED %in% c("AI mg/L", "nmol/L", "mg/L", "ug/L", "ai mg/ml"), !OBSERVED_DURATION_MEAN_OP_DAYS %in% c("<","~",">","<=",">="),
         !ENDPOINT %in% c("BCF", "NR", "BCFD", "BCF/", "BAF", "ATCN","NR/", "ET50", "LT50"), EFFECT != "Accumulation", MEDIA_TYPE == "Fresh water", 
         !EXPOSURE_TYPE %in% c("Choice", "Food", "in Vitro", "Direct application", "Multiple routes between application groups", "Spray"), STATISTICAL_SIGNIFICANCE !="Not significant at all concentrations",
         SPECIES_KINGDOM != "Monera")

summary(cec.prioritization.1)
names(cec.prioritization.1)
summary(list(unique(cec.prioritization.1$CAS_NUMBER)))


#collate mean concentration column #####
cec.prioritization.2 <- cec.prioritization.1
cec.prioritization.2$CONC1_MIN_STANDARDIZED <- as.numeric(cec.prioritization.2$CONC1_MIN_STANDARDIZED)
cec.prioritization.2$CONC1_MAX_STANDARDIZED <- as.numeric(cec.prioritization.2$CONC1_MAX_STANDARDIZED)
cec.prioritization.2$CONC_MEAN_MINMAXAVG <- ((cec.prioritization.2$CONC1_MIN_STANDARDIZED + cec.prioritization.2$CONC1_MAX_STANDARDIZED) * 0.5)
cec.prioritization.2$CONC1_MEAN_STANDARDIZED <- as.numeric(cec.prioritization.2$CONC1_MEAN_STANDARDIZED)
cec.prioritization.2$CONC_MEAN <- ifelse(is.na(cec.prioritization.2$CONC1_MEAN_STANDARDIZED), cec.prioritization.2$CONC_MEAN_MINMAXAVG, cec.prioritization.2$CONC1_MEAN_STANDARDIZED)

cec.prioritization.3 <- cec.prioritization.2 %>% select(-c(CONC1_MEAN_STANDARDIZED, CONC1_MEAN_OP_STANDARDIZED, CONC_MEAN_MINMAXAVG)) %>% relocate(CONC_MEAN, .after = TEST_LOCATION)
sum(is.na(cec.prioritization.3$CONC_MEAN))
sum(is.nan(cec.prioritization.3$CONC_MEAN))

#filter out concentration units that can't be converted & convert concentrations to ppb
cec.prioritization.5 <- cec.prioritization.3 %>% filter(CONC1_UNITS_STANDARDIZED != "nmol/L") %>% rename("CONC_UNITS" = "CONC1_UNITS_STANDARDIZED")
cec.prioritization.5$CONC_MEAN <- (cec.prioritization.5$CONC_MEAN * 10^3)
cec.prioritization.5$CONC_UNITS <- "ug/L"
View(cec.prioritization.5)

##collate duration columns ####
cec.prioritization.6 <- cec.prioritization.5
cec.prioritization.6$OBSERVED_DURATION_MEAN_DAYS <- as.numeric(cec.prioritization.6$OBSERVED_DURATION_MEAN_DAYS)
cec.prioritization.6$STUDY_DURATION_MEAN_DAYS <- as.numeric(cec.prioritization.6$STUDY_DURATION_MEAN_DAYS)
cec.prioritization.6$STUDY_DURATION <- ifelse(is.na(cec.prioritization.6$OBSERVED_DURATION_MEAN_DAYS), cec.prioritization.6$STUDY_DURATION_MEAN_DAYS, cec.prioritization.6$OBSERVED_DURATION_MEAN_DAYS)

cec.prioritization.7 <- cec.prioritization.6 %>% filter(!is.na(STUDY_DURATION)) %>% select(-c(OBSERVED_DURATION_MEAN_DAYS, STUDY_DURATION_MEAN_DAYS, STUDY_DURATION_MEAN_OP_DAYS, OBSERVED_DURATION_MEAN_OP_DAYS))
sum(is.nan(cec.prioritization.7$STUDY_DURATION))

#Group based on general taxa####
list(unique(cec.prioritization.7$SPECIES_GROUP))
cec.prioritization.8 <- cec.prioritization.7
cec.prioritization.8$SPECIES <- ifelse(cec.prioritization.8$SPECIES_GROUP %in% c("Fish", "Fish; Standard Test Species; U.S. Threatened and Endangered Species; U.S. Invasive Species",
                                         "Fish; Standard Test Species; U.S. Invasive Species", "Fish; Standard Test Species; U.S. Threatened and Endangered Species",  "Amphibians",
                                         "Amphibians; Standard Test Species", "Amphibians; Standard Test Species; U.S. Invasive Species", "Fish; U.S. Threatened and Endangered Species",
                                   "Amphibians; U.S. Invasive Species","Fish; U.S. Invasive Species"), "Fish",
                                   ifelse(cec.prioritization.8$SPECIES_GROUP %in% c("Insects/Spiders" , "Crustaceans", "Crustaceans; Standard Test Species",
                                                                                    "Invertebrates", "Molluscs; U.S. Invasive Species", "Insects/Spiders; Standard Test Species", "Worms; Standard Test Species",
                                                                                    "Worms", "Molluscs", "Molluscs; Standard Test Species", "Invertebrates; Standard Test Species", "Molluscs; Standard Test Species; U.S. Invasive Species", "Crustaceans; U.S. Invasive Species"), "Invertebrate",
                                          ifelse(cec.prioritization.8$SPECIES_GROUP %in% c("Flowers, Trees, Shrubs, Ferns; Standard Test Species; U.S. Invasive Species" , "Algae", "Algae; Standard Test Species", "Flowers, Trees, Shrubs, Ferns",
                                                                                           "Flowers, Trees, Shrubs, Ferns; Standard Test Species", "Flowers, Trees, Shrubs, Ferns; U.S. Invasive Species"), "Plant",
                                                 ifelse(cec.prioritization.8$SPECIES_SCIENTIFIC_NAME %in% c("Pimephales promelas",  "Lepomis macrochirus", "Danio rerio", "Salvelinus fontinalis", "Ictalurus punctatus",
                                                                                           "Salvelinus namaycush", "Oryzias latipes", "Lepomis cyanellus", "Catostomus commersoni", "Poecilia reticulata",
                                                                                           "Cyprinodon variegatus", "Gasterosteus aculeatus", "Esox lucius", "Gambusia sp.", "Fundulus heteroclitus", "Jordanella floridae"), "Fish",
                                       ifelse(cec.prioritization.8$SPECIES_SCIENTIFIC_NAME %in% c("Aedes albopictus", "Arthropoda"), "Invertebrate", 
                                              ifelse(cec.prioritization.8$SPECIES_SCIENTIFIC_NAME %in% c("Plankton"), "Plant", "Miscellaneous"))))))

misc.spp <- cec.prioritization.8 %>% filter(SPECIES == "Miscellaneous")
list(unique(misc.spp$SPECIES_COMMON_NAME))

cec.prioritization.9 <- cec.prioritization.8 %>% filter(SPECIES != "Miscellaneous")
list(unique(cec.prioritization.9$SPECIES))


#add column defining studies as acute or chronic####
cec.prioritization.9$STUDY_TYPE <- ifelse(cec.prioritization.9$STUDY_DURATION < 7, "acute", "chronic")
list(unique(cec.prioritization.9$STUDY_TYPE))

#group endpoint types ####
list(unique(cec.prioritization.9$ENDPOINT))
cec.prioritization.10 <- cec.prioritization.9
cec.prioritization.10$ENDPOINT_TYPE <- ifelse(cec.prioritization.10$ENDPOINT %in% c("NOEL", "NOEC", "NR-ZERO", "LC0", "NOEC/", "EC01", "LC05", "LC01", "IC05", "EC05", "EC0"), "No Effect",
                                              ifelse(cec.prioritization.10$ENDPOINT %in% c("LOEL", "LOEC", "LC10", "MATC", "EC25", "IC20", "LC20", "ER20", "LOEC/", "BMC20", "IC10", "LC25", "EC20", "LC15",
                                                                                                "IC25", "EC10/", "IC40", "MATC/", "EC16", "ER10", "EC10"), "Low Effect",
                                                     ifelse(cec.prioritization.10$ENDPOINT %in% c("NR-LETH", "LC50", "EC50", "LC90", "LC100", "IC50", "LT50", "EC50*", "EC50/", "ET50", "LC50*", "LC50/", "ER50", "EC90", "EC100", "BMC50", "LC95", "LETC", "IC50/", "LD50", "LC99", "EC80", "NR-LETH/", "LC85"), "Effect", "Miscellaneous")))
list(unique(cec.prioritization.10$ENDPOINT_TYPE))

#subset into groups based on effect type ####
list(unique(cec.prioritization.10$EFFECT))

#group into tier groups
cec.prioritization.11 <- cec.prioritization.10 %>% filter(EFFECT != "No Effect")
list(unique(cec.prioritization.11$EFFECT))

cec.prioritization.11$TIER <- ifelse(cec.prioritization.11$EFFECT %in% c("Mortality", "Reproduction", "Behavior", "Population", "Growth", "Development"), "Tier 1",
                                     ifelse(cec.prioritization.11$EFFECT %in% c("Enzyme(s)", "Genetics", "Histology", "Intoxication", "Biochemistry", "Morphology", "Physiology",
                                                                                "Feeding behavior", "Multiple", "Cell(s)", "Immunological", "Hormone(s)", "Avoidance", "Injury", "Ecosystem process"), "Tier 2", "Miscellaneous"))
list(unique(cec.prioritization.11$TIER))

#generate boxplots to identify outliers in effect data ####
#tier1#
tier1 <- cec.prioritization.11 %>% filter(TIER == "Tier 1")

tier1.box2 <- boxplot(tier1$CONC_MEAN ~ tier1$CAS_NUMBER)
tier1.box2$out

tier1.outliers <- subset(tier1, tier1$CONC_MEAN %in% tier1.box2$out)
View(tier1.outliers)

tier1.box.info <- tier1 %>% group_by(CAS_NUMBER) %>% summarise(Q1 = quantile(CONC_MEAN, 0.25), Q3 = quantile(CONC_MEAN, 0.75))

tier1.outliers1 <- left_join(tier1.outliers, tier1.box.info, by = "CAS_NUMBER")
View(tier1.outliers1)

tier1.outliers1$OUTLIER.CLASS <- ifelse(tier1.outliers1$CONC_MEAN < tier1.outliers1$Q1, "POTENTIAL LOWER OUTLIER", "POTENTIAL UPPER OUTLIER")

tier1.outliers2 <- tier1.outliers1 %>% filter(OUTLIER.CLASS == "POTENTIAL LOWER OUTLIER")

write_xlsx(tier1.outliers2, "CEC.TIER1.POUTLIERS.xlsx")


#read back in outliers and remove from dataset/replace####
#tier 1 #
em.outliers <- read_excel("CEC.TIER1.POUTLIERS - EM.Final.xlsx")
dv.outliers <- read_excel("CEC.TIER1.POUTLIERS - DV.Final.xlsx")
me.outliers <- read_excel("CEC.TIER1.POUTLIERS - ME.EM.Final.xlsx")

tier1.outlier.list <- bind_rows(em.outliers, dv.outliers, me.outliers)
names(tier1.outlier.list)

tier1.outlier.list.exclude <- tier1.outlier.list %>% filter(Action == "Exclude") %>%
  select(-c("Q1":"Action", "CAS"))
tier1.outlier.list.replace <- tier1.outlier.list %>% filter(Action == "Replace") %>%
  select(-c("Q1":"Action", "CAS"))
write_xlsx(tier1.outlier.list.replace, "tier1.replacements.xlsx")

names(tier1.outlier.list.exclude)

tier1.outlier.list.exclude$CAS_NUMBER <- as.numeric(tier1.outlier.list.exclude$CAS_NUMBER)
tier1.outlier.list.replace$CAS_NUMBER <- as.numeric(tier1.outlier.list.replace$CAS_NUMBER)

#Exclude outliers#
tier1$CAS_NUMBER <- as.numeric(tier1$CAS_NUMBER)
View(tier1)

tier1.1 <- anti_join(tier1, tier1.outlier.list.exclude)
tier1.2 <- anti_join(tier1.1, tier1.outlier.list.replace)

#read in replacement values#
tier1.replacements <- read_xlsx("tier1.replacements.1.xlsx")
tier1.replacements$CAS_NUMBER <- as.numeric(tier1.replacements$CAS_NUMBER)
tier1.replacements1 <- tier1.replacements %>% select(-c("Q1":"Action", "CAS"))

tier1.3 <- bind_rows(tier1.2,tier1.replacements1)
tier1.3$Concentration_Type <- "Measured"

#annotate and bind full dataset back together ####

cec.prioritization.12 <- tier1.3

#bind CAS numbers and common names to dataset
CAS.list <- read_excel("CAS List for ECOTOX search.xlsx")

CAS.list$CAS_NUMBER <- gsub("-", "", CAS.list$CAS)

CAS.list1 <- CAS.list %>% select("Preferred Name", "CAS", "CAS_NUMBER") %>% rename("Chemical_Name_Common" = "Preferred Name")
CAS.list1$CAS_NUMBER <- as.numeric(CAS.list1$CAS_NUMBER)

cec.prioritization.13 <- left_join(cec.prioritization.12, CAS.list1, by = "CAS_NUMBER")
View(cec.prioritization.13)

#nominal values ####
cec.prioritization.1.nom <- cec.prioritization%>% 
  select(CAS_NUMBER, CHEMICAL_NAME, CHEMICAL_ANALYSIS, TEST_METHOD, SPECIES_SCIENTIFIC_NAME, SPECIES_COMMON_NAME, SPECIES_KINGDOM, SPECIES_PHYLUM, SPECIES_SUBPHYLUM,
         SPECIES_GROUP, ORGANISM_LIFESTAGE, EXPOSURE_TYPE, MEDIA_TYPE, TEST_LOCATION, CONC1_MEAN_OP_STANDARDIZED, 
         CONC1_TYPE_STANDARDIZED, CONC1_MEAN_STANDARDIZED, CONC1_MIN_STANDARDIZED, CONC1_MAX_STANDARDIZED, CONC1_UNITS_STANDARDIZED, EFFECT, EFFECT_MEASUREMENT, ENDPOINT, STATISTICAL_SIGNIFICANCE, SIGNIFICANCE_LEVEL_MEAN, 
         OBSERVED_DURATION_MEAN_DAYS, OBSERVED_DURATION_MEAN_OP_DAYS, STUDY_DURATION_MEAN_DAYS,  STUDY_DURATION_MEAN_OP_DAYS, REFERENCE_NUMBER, TITLE, AUTHOR, PUBLICATION_YEAR) %>%
  filter(CONC1_TYPE_STANDARDIZED !=("Formulation"), !CONC1_MEAN_OP_STANDARDIZED %in% c(">", "<", "<=", "~", ">="), 
         CONC1_UNITS_STANDARDIZED %in% c("AI mg/L", "nmol/L", "mg/L", "ug/L", "ai mg/ml"), !OBSERVED_DURATION_MEAN_OP_DAYS %in% c("<","~",">","<=",">="),
         !ENDPOINT %in% c("BCF", "NR", "BCFD", "BCF/", "BAF", "ATCN","NR/", "ET50", "LT50"), EFFECT != "Accumulation", MEDIA_TYPE == "Fresh water", 
         !EXPOSURE_TYPE %in% c("Choice", "Food", "in Vitro", "Direct application", "Multiple routes between application groups", "Spray"), STATISTICAL_SIGNIFICANCE !="Not significant at all concentrations",
         SPECIES_KINGDOM != "Monera")

only.nominal <- anti_join(cec.prioritization.1.nom, cec.prioritization.13, by = "CAS_NUMBER")
summary(list(unique(only.nominal$CAS_NUMBER)))

#collate mean concentration column - nominal values #####
only.nominal.2 <- only.nominal
only.nominal.2$CONC1_MIN_STANDARDIZED <- as.numeric(only.nominal.2$CONC1_MIN_STANDARDIZED)
only.nominal.2$CONC1_MAX_STANDARDIZED <- as.numeric(only.nominal.2$CONC1_MAX_STANDARDIZED)
only.nominal.2$CONC_MEAN_MINMAXAVG <- ((only.nominal.2$CONC1_MIN_STANDARDIZED + only.nominal.2$CONC1_MAX_STANDARDIZED) * 0.5)
only.nominal.2$CONC1_MEAN_STANDARDIZED <- as.numeric(only.nominal.2$CONC1_MEAN_STANDARDIZED)
only.nominal.2$CONC_MEAN <- ifelse(is.na(only.nominal.2$CONC1_MEAN_STANDARDIZED), only.nominal.2$CONC_MEAN_MINMAXAVG, only.nominal.2$CONC1_MEAN_STANDARDIZED)

summary(only.nominal.2)

only.nominal.3 <- only.nominal.2 %>% select(-c(CONC1_MEAN_STANDARDIZED, CONC1_MEAN_OP_STANDARDIZED, CONC_MEAN_MINMAXAVG)) %>% relocate(CONC_MEAN, .after = TEST_LOCATION)
sum(is.na(only.nominal.3$CONC_MEAN))
sum(is.nan(only.nominal.3$CONC_MEAN))

#convert other concentrations to ug/L#
list(unique(only.nominal.3$CONC1_UNITS_STANDARDIZED))

only.nominal.4 <- only.nominal.3 %>% rename("CONC_UNITS" = "CONC1_UNITS_STANDARDIZED")
only.nominal.4$CONC_MEAN <- (only.nominal.4$CONC_MEAN * 10^3)
only.nominal.4$CONC_UNITS <- "ug/L"
View(only.nominal.4)

##collate duration columns - nominal ####
only.nominal.5 <- only.nominal.4
only.nominal.5$OBSERVED_DURATION_MEAN_DAYS <- as.numeric(only.nominal.5$OBSERVED_DURATION_MEAN_DAYS)
only.nominal.5$STUDY_DURATION_MEAN_DAYS <- as.numeric(only.nominal.5$STUDY_DURATION_MEAN_DAYS)
only.nominal.5$STUDY_DURATION <- ifelse(is.na(only.nominal.5$OBSERVED_DURATION_MEAN_DAYS), only.nominal.5$STUDY_DURATION_MEAN_DAYS, only.nominal.5$OBSERVED_DURATION_MEAN_DAYS)

only.nominal.6 <- only.nominal.5 %>% filter(!is.na(STUDY_DURATION)) %>% select(-c(OBSERVED_DURATION_MEAN_DAYS, STUDY_DURATION_MEAN_DAYS, STUDY_DURATION_MEAN_OP_DAYS, OBSERVED_DURATION_MEAN_OP_DAYS))
sum(is.nan(only.nominal.6$STUDY_DURATION))

#group species into general taxanomic groups - nominal ####
list(unique(only.nominal.6$SPECIES_GROUP))

only.nominal.7 <- only.nominal.6
only.nominal.7$SPECIES <- ifelse(only.nominal.7$SPECIES_GROUP %in% c("Crustaceans; Standard Test Species", "Crustaceans", "Molluscs", "Insects/Spiders", "Insects/Spiders; Standard Test Species", 
                                                                     "Invertebrates; Standard Test Species", "Invertebrates", "Worms", "Insects/Spiders; U.S. Invasive Species", "Molluscs; U.S. Invasive Species"), "Invertebrate",
                                 ifelse(only.nominal.7$SPECIES_GROUP %in% c("Algae; Standard Test Species", "Flowers, Trees, Shrubs, Ferns; Standard Test Species; U.S. Invasive Species", "Flowers, Trees, Shrubs, Ferns",
                                                                            "Flowers, Trees, Shrubs, Ferns; U.S. Invasive Species", "Algae","Flowers, Trees, Shrubs, Ferns; Standard Test Species"), "Plant",
                                        ifelse(only.nominal.7$SPECIES_GROUP %in% c("Fish", "Fish; Standard Test Species", "Fish; Standard Test Species; U.S. Threatened and Endangered Species; U.S. Invasive Species",
                                                                                   "Fish; Standard Test Species; U.S. Invasive Species", "Amphibians; Standard Test Species", "Amphibians", "Amphibians; Standard Test Species; U.S. Invasive Species"), "Fish",
                                               "Miscellaneous")))
list(unique(only.nominal.7$SPECIES))

#add column defining studies as acute or chronic - nominal####
only.nominal.8 <- only.nominal.7
only.nominal.8$STUDY_TYPE <- ifelse(only.nominal.8$STUDY_DURATION < 7, "acute", "chronic")
list(unique(only.nominal.8$STUDY_TYPE))

#group endpoint types - nominal ####
list(unique(only.nominal.8$ENDPOINT))
only.nominal.9 <- only.nominal.8 %>% filter(ENDPOINT != "LT100")
only.nominal.9$ENDPOINT_TYPE <- ifelse(only.nominal.9$ENDPOINT %in% c("NOEC", "NR-ZERO", "NOEL", "EC0", "EC05"), "No Effect",
                                       ifelse(only.nominal.9$ENDPOINT %in% c("LOEC", "EC10", "LOEL","IC10", "EC20", "LC10", "EC16", "EC20/", "IC25","LOEC/"), "Low Effect",
                                              ifelse(only.nominal.9$ENDPOINT %in% c("LC50", "EC50", "LC95", "NR-LETH", "IC50", "LC90", "EC50/", "EC84", "EC100", "IC100", "EC50*/", "EC80", "LD50"), "Effect",
                                                                                    "Miscellaneous")))
list(unique(only.nominal.9$ENDPOINT_TYPE))


#subset into groups based on effect type ####
list(unique(only.nominal.9$EFFECT))

#group into tier groups
only.nominal.10 <- only.nominal.9

only.nominal.10$TIER <- ifelse(only.nominal.10$EFFECT %in% c("Mortality", "Reproduction", "Behavior", "Population", "Growth", "Development"), "Tier 1",
                                     ifelse(only.nominal.10$EFFECT %in% c("Enzyme(s)", "Genetics", "Histology", "Intoxication", "Biochemistry", "Morphology", "Physiology",
                                                                                "Feeding behavior", "Multiple", "Cell(s)", "Immunological", "Hormone(s)", "Avoidance", "Injury", "Ecosystem process"), "Tier 2", "Miscellaneous"))
list(unique(only.nominal.10$TIER))

#generate boxplots to identify outliers in effect data - nominal ####
#tier1#
tier1.nom <- only.nominal.10 %>% filter(TIER == "Tier 1")

tier1.nom.box2 <- boxplot(tier1.nom$CONC_MEAN ~ tier1.nom$CAS_NUMBER)
tier1.nom.box2$out

tier1.nom.outliers <- subset(tier1.nom, tier1.nom$CONC_MEAN %in% tier1.nom.box2$out)
View(tier1.nom.outliers)

tier1.nom.box.info <- tier1.nom %>% group_by(CAS_NUMBER) %>% summarise(Q1 = quantile(CONC_MEAN, 0.25), Q3 = quantile(CONC_MEAN, 0.75))

tier1.nom.outliers1 <- left_join(tier1.nom.outliers, tier1.nom.box.info, by = "CAS_NUMBER")
View(tier1.nom.outliers1)

tier1.nom.outliers1$OUTLIER.CLASS <- ifelse(tier1.nom.outliers1$CONC_MEAN < tier1.nom.outliers1$Q1, "POTENTIAL LOWER OUTLIER", "POTENTIAL UPPER OUTLIER")

tier1.nom.outliers2 <- tier1.nom.outliers1 %>% filter(OUTLIER.CLASS == "POTENTIAL LOWER OUTLIER")

tier1.nom.outliers3 <- left_join(tier1.nom.outliers2, CAS.list1, by= "CAS_NUMBER")

write_xlsx(tier1.nom.outliers3, "CEC.TIER1.NOM.POUTLIERS.xlsx")

#read back in outliers and remove from dataset/replace - nominal####

tier1.nom.out <- read_excel( "CEC.TIER1.NOM.POUTLIERS - EM.xlsx")
names(tier1.nom.out)

tier1.nom.out1 <- tier1.nom.out %>% filter(Action == "Exclude") %>% select(-c("Full Article Accessible?":"Action"))

tier1.nom.nout <- anti_join(tier1.nom, tier1.nom.out1)
View(tier1.nom.nout)

tier1.nom.1 <- tier1.nom.nout

tier1.nom.1$EC_Type <- "Nominal"

cec.nominal <- tier1.nom.1

#bind CAS numbers and chemical names to cec.nominal

cec.nominal1 <- left_join(cec.nominal, CAS.list1)

##bind full dataset back together####
cec.prioritization.14 <- cec.prioritization.13 %>% rename("EC_Type" = "Concentration_Type")
cec.prioritization.15 <- bind_rows(cec.prioritization.14, cec.nominal1)

names(cec.prioritization.15)
summary(cec.prioritization.15)
View(cec.prioritization.15)

###derive benchmarks for chemicals in this list###

#split chemicals into effect groups#
noeffect <- cec.prioritization.15 %>% filter(ENDPOINT_TYPE == "No Effect") %>%
  group_by(CAS, Chemical_Name_Common, EC_Type, TIER, STUDY_TYPE, SPECIES, ENDPOINT_TYPE) %>%
  summarise(min_EC = min(CONC_MEAN), unique_species = length(unique(SPECIES_SCIENTIFIC_NAME)), totalresults = length(CAS)) %>%
  spread(SPECIES, unique_species) %>% group_by(CAS, Chemical_Name_Common, TIER, STUDY_TYPE, EC_Type, ENDPOINT_TYPE) %>%
  summarise(min_concentration = min(min_EC), uFish = sum(Fish, na.rm = T), uInvert = sum(Invertebrate, na.rm = T), uPlant = sum(Plant, na.rm = T))

View(noeffect)

loweffect <- cec.prioritization.15 %>% filter(ENDPOINT_TYPE == "Low Effect") %>%
  group_by(CAS, Chemical_Name_Common, EC_Type, TIER, STUDY_TYPE, SPECIES, ENDPOINT_TYPE) %>%
  summarise(min_EC = min(CONC_MEAN), unique_species = length(unique(SPECIES_SCIENTIFIC_NAME)), totalresults = length(CAS)) %>%
  spread(SPECIES, unique_species) %>% group_by(CAS, Chemical_Name_Common, TIER, STUDY_TYPE, EC_Type, ENDPOINT_TYPE) %>%
  summarise(min_concentration = min(min_EC), uFish = sum(Fish, na.rm = T), uInvert = sum(Invertebrate, na.rm = T), uPlant = sum(Plant, na.rm = T))

View(loweffect)

effect <- cec.prioritization.15 %>% filter(ENDPOINT_TYPE == "Effect") %>%
  group_by(CAS, Chemical_Name_Common, EC_Type, TIER, STUDY_TYPE, SPECIES, ENDPOINT_TYPE) %>%
  summarise(min_EC = min(CONC_MEAN), unique_species = length(unique(SPECIES_SCIENTIFIC_NAME)), totalresults = length(CAS)) %>%
  spread(SPECIES, unique_species) %>% group_by(CAS, Chemical_Name_Common, TIER, STUDY_TYPE, EC_Type, ENDPOINT_TYPE) %>%
  summarise(min_concentration = min(min_EC), uFish = sum(Fish, na.rm = T), uInvert = sum(Invertebrate, na.rm = T), uPlant = sum(Plant, na.rm = T))

View(effect)

cec.prioritization.16 <- bind_rows(noeffect, loweffect, effect)

#Chemicals with missing ECOTOX data ####
setwd("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data")

#identify missing chemicals
missing.chemicals <- anti_join(CAS.list1, cec.prioritization.16, by = "CAS")
View(missing.chemicals)

write.xlsx(missing.chemicals, "CECs_no_ECOTOX_26_02_2021.xlsx")

#identify missing chemicals from list - 26/02/2021
missing.missing.chemicals <- anti_join(missing.chemicals, missing.chem.physchem1, by = "CAS")
View(missing.missing.chemicals)

write_xlsx(missing.missing.chemicals, "CECS_no_ECOTOX_Extra_25_02_2021.xlsx")

#read in logKow/MW estimates##
missing.chem.physchem <- read.xlsx("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\Physical Properties\\COMPTOX_logKOW_MW_estimates.xlsx", 1)
names(missing.chem.physchem)

missing.chem.physchem1 <- missing.chem.physchem %>% rename("CAS" = "INPUT", "MW" = "AVERAGE_MASS", "logKow" = "OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED") %>% select("CAS", "MW", "logKow")
names(missing.chem.physchem1)

#bind estimates to missing chemicals#
missing.chemicals1 <- left_join(missing.chemicals,missing.chem.physchem1, by="CAS")
View(missing.chemicals1)
missing.chemicals1$MW <- as.numeric(missing.chemicals1$MW)
missing.chemicals1$logKow <- as.numeric(missing.chemicals1$logKow)

missing.chem <- missing.chemicals1

missing.chem$algae.base <- (-1)*missing.chem$logKow - 1.23
missing.chem$algae.base <- 10^(missing.chem$algae.base)
missing.chem$algae.base <- missing.chem$algae.base * missing.chem$MW
missing.chem$algae.base <- missing.chem$algae.base * 10^6
missing.chem$daphnid.base <- (-.95)*missing.chem$logKow - 1.19
missing.chem$daphnid.base <- 10^(missing.chem$daphnid.base)
missing.chem$daphnid.base <- missing.chem$daphnid.base * missing.chem$MW
missing.chem$daphnid.base <- missing.chem$daphnid.base * 10^6
missing.chem$fish.base <- (-0.85)*missing.chem$logKow-1.14
missing.chem$fish.base <- 10^missing.chem$fish.base
missing.chem$fish.base <- missing.chem$fish.base * missing.chem$MW
missing.chem$fish.base <- missing.chem$fish.base * 10^6

#Read in QSAR estimates - collation on separate R script#
file.choose()

qsar.estimates <- read_xlsx("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\QSAR_final_converted.xlsx")
names(qsar.estimates)

qsar.estimates1 <- qsar.estimates %>% select("CAS", "ECOSAR_Fathead", "ECOSAR_Daphnid", "ECOSAR_Plant", 
                                            "Fish_TEST_converted", "Invert_TEST_converted", "Plant_TEST_converted") %>%
  rename("Fish_TEST" = "Fish_TEST_converted", "Invert_TEST" = "Invert_TEST_converted", "Plant_TEST" = "Plant_TEST_converted")

#bind together missing data & generate benchmarks#
names(missing.chem)

missing.chem1 <- left_join(missing.chem, qsar.estimates1)
View(missing.chem1)

View(cec.prioritization.16)

missing.chem2 <- missing.chem1 %>% gather("algae.base":"Plant_TEST", key = "QSAR_Type", value = "EC")
View(missing.chem2)

missing.chem3 <- missing.chem2 %>% group_by(CAS, Chemical_Name_Common) %>% summarise(min_concentration = min(EC, na.rm = TRUE))
View(missing.chem3)

missing.chem3$min_concentration <- gsub("Inf", "No Benchmark", missing.chem3$min_concentration)

missing.chem4 <- missing.chem3
missing.chem4$TIER <- "Tier 1"
missing.chem4$STUDY_TYPE <- "acute"
missing.chem4$EC_Type <- "Estimated"
missing.chem4$ENDPOINT_TYPE <- "Effect"
missing.chem4$uFish <- 0
missing.chem4$uInvert <- 0
missing.chem4$uPlant <- 0

View(missing.chem4)

##bind together missing chem and cec.prioritization dataset to get full dataset!
missing.chem4$min_concentration <- as.numeric(missing.chem4$min_concentration)

cec.prioritization.17 <- bind_rows(cec.prioritization.16, missing.chem4)

##read in PB values - and bind to dataset (collated and manipulated on other R spreadsheet)#
file.choose()
e.fate <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Environmental Fate\\E_FATE_summary.xlsx")

cec.prioritization.18 <- left_join(cec.prioritization.17, e.fate)

View(cec.prioritization.18)


#add application factors#### 

cec.prioritization.19 <- cec.prioritization.18

cec.prioritization.19$AF_Effect <- ifelse(cec.prioritization.19$ENDPOINT_TYPE == "No Effect", 1,
                                          ifelse(cec.prioritization.19$ENDPOINT_TYPE == "Low Effect", 2.5,
                                                 ifelse(cec.prioritization.19$ENDPOINT_TYPE == "Effect", 5, "missing")))
list(unique(cec.prioritization.19$AF_Effect))


cec.prioritization.19$AF_StudyLength <- ifelse(cec.prioritization.19$STUDY_TYPE == "acute", 2,
                                               ifelse(cec.prioritization.19$STUDY_TYPE == "chronic", 1, "missing"))
list(unique(cec.prioritization.19$AF_StudyLength))

cec.prioritization.19$AF_datarichness <- 1.25
cec.prioritization.19$AF_datarichness <- ifelse((cec.prioritization.19$uFish == 0 & cec.prioritization.19$uInvert == 0 & cec.prioritization.19$uPlant == 0), 5, 
                             (ifelse((cec.prioritization.19$uFish > 0 & cec.prioritization.19$uInvert == 0 & cec.prioritization.19$uPlant >= 0), 3.75, 
                                     (ifelse((cec.prioritization.19$uFish == 0 & cec.prioritization.19$uInvert >= 0 & cec.prioritization.19$uPlant > 0), 3.75, 
                                             (ifelse((cec.prioritization.19$uFish >= 0 & cec.prioritization.19$uInvert > 0 & cec.prioritization.19$uPlant == 0), 3.75, 
                                                     (ifelse((cec.prioritization.19$uFish == 1 & cec.prioritization.19$uInvert == 1 & cec.prioritization.19$uPlant == 1), 2.5,
                                                             (ifelse((cec.prioritization.19$uFish >= 3 & cec.prioritization.19$uInvert >= 3 & cec.prioritization.19$uPlant >= 3), 1, cec.prioritization.19$AF_datarichness)))))))))))


list(unique(cec.prioritization.19$AF_datarichness))

cec.prioritization.19$AF_EFate <- ifelse(cec.prioritization.19$Half_life_days < 40 & cec.prioritization.19$BCF_Lkg < 2000, 1,
                                         ifelse(cec.prioritization.19$Half_life_days >= 40 & cec.prioritization.19$Half_life_days < 60 & cec.prioritization.19$BCF_Lkg < 2000, 2,
                                                ifelse(cec.prioritization.19$Half_life_days < 40 & cec.prioritization.19$BCF_Lkg >= 2000 & cec.prioritization.19$BCF_Lkg < 5000, 2,
                                                       ifelse(cec.prioritization.19$Half_life_days >= 60 & cec.prioritization.19$BCF_Lkg < 2000, 3,
                                                              ifelse(cec.prioritization.19$Half_life_days < 40 & cec.prioritization.19$BCF_Lkg >= 5000, 3, 
                                                                     ifelse(cec.prioritization.19$Half_life_days >= 40 & cec.prioritization.19$Half_life_days < 60 & cec.prioritization.19$BCF_Lkg >= 2000 & cec.prioritization.19$BCF_Lkg < 5000, 4,
                                                                            ifelse(cec.prioritization.19$Half_life_days >= 60 & cec.prioritization.19$BCF_Lkg >= 2000 & cec.prioritization.19$BCF_Lkg < 5000, 5,
                                                                                   ifelse(cec.prioritization.19$Half_life_days >= 40 & cec.prioritization.19$Half_life_days < 60 & cec.prioritization.19$BCF_Lkg >= 5000, 5, 
                                                                                          ifelse(cec.prioritization.19$Half_life_days >= 60 & cec.prioritization.19$BCF_Lkg >= 5000, 6, "missing")))))))))
cec.prioritization.19$AF_EFate <- as.numeric(cec.prioritization.19$AF_EFate)
cec.prioritization.19$AF_datarichness <- as.numeric(cec.prioritization.19$AF_datarichness)

View(cec.prioritization.19)

#remove NaNs & QAQC messed up values

cec.20 <- cec.prioritization.19 %>% drop_na(min_concentration)
View(cec.20)

fadrazole <- cec.20 %>% filter(CAS == "102676-47-1")
fadrazole$AF_EFate <- 1
View(fadrazole)

PhenylBDG <- cec.20 %>% filter(CAS == "17685-05-1")
View(PhenylBDG)
PhenylBDG$BCF_Lkg <- 3.16
PhenylBDG$AF_EFate <- 1

cec.21 <- cec.20 %>% filter(!CAS %in% c("102676-47-1", "17685-05-1"))

cec.22 <- bind_rows(cec.21, fadrazole,PhenylBDG)
View(cec.22)

##sum AF#
summary(cec.22)
cec.22$AF_EFate <- as.numeric(cec.22$AF_EFate)
cec.22$AF_Effect <- as.numeric(cec.22$AF_Effect)
cec.22$AF_StudyLength <- as.numeric(cec.22$AF_StudyLength)

cec.22$AF_Total <- cec.22$AF_datarichness * cec.22$AF_EFate * cec.22$AF_Effect * cec.22$AF_StudyLength

write_xlsx(cec.22, "preBenchmark_file.xlsx")

##divide AF_total by min concentration to get benchmark value##
setwd("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Final Benchmarks - Iterations")

cec.23 <- cec.22

cec.23$AF_BM <- cec.23$min_concentration / cec.23$AF_Total
write_xlsx(cec.23, "Benchmark_file_preminimization.xlsx")

names(cec.23)
cec.23.1 <- cec.23 %>% group_by(CAS, Chemical_Name_Common) %>% summarise(minBM_AF_corrected = min(AF_BM))
cec.23.2 <- left_join(cec.23, cec.23.1)
View(cec.23.2)
cec.23.3 <- cec.23.2 %>% unite("Data_Type", c(EC_Type, TIER), sep = " ", remove = FALSE)

cec.23.annotated <- cec.23.3 %>% ungroup() %>% filter(AF_BM == minBM_AF_corrected) %>%
  select(CAS, Chemical_Name_Common, AF_Effect:minBM_AF_corrected, -AF_BM, Data_Type)
View(cec.23.annotated)

cec.24 <- cec.23 %>% group_by(CAS, Chemical_Name_Common) %>% summarise(minBM_AF_corrected = min(AF_BM), minBM_uncorrected = min(min_concentration))
View(cec.24)

cec.25 <- left_join(cec.24, cec.23.annotated)

View(cec.25)

write_xlsx(cec.25, "CEC_benchmark_iteration_25_02_2021.xlsx")

#extract chemicals without a benchmark value #
file.choose()

full.cas.list <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\CEC lists and Concentration Data\\CAS List for ECOTOX search.xlsx")

noBM.chem <- anti_join(full.cas.list, cec.25, by = "CAS")
View(noBM.chem)

noBM.chem.QSAR <- left_join(noBM.chem, qsar.estimates1)
View(noBM.chem.QSAR)

noBM.chem.QSAR.EFATE <- left_join(noBM.chem.QSAR, e.fate)
View(noBM.chem.QSAR.EFATE)

write_xlsx(noBM.chem.QSAR.EFATE, "noBM_chem_forevaluation_25_02_2021.xlsx")
