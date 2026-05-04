##CEC Prioritization ECOTOX Benchmark Derivation## TIER 2 ####

library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(stringr)
library(xlsx)
library(rJava)
library(ggpubr)

setwd("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data")

##pull together ECOTOX data####
glri.output <- read.csv("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\aquatic-data-all-fields-GLRI\\aquatic-data-all-fields-glri.csv") %>% mutate_all(as.character)
glri.output1 <- read.csv("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\aquatic-data-all-fields-GLRI\\additional-aquatic-data-all-fields-glri.csv")%>% mutate_all(as.character)
glri.output2 <- read.csv("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\aquatic-data-all-fields-GLRI\\aquatic-data-all-fields-part3.csv")%>% mutate_all(as.character)

cec.prioritization <- bind_rows(glri.output, glri.output1, glri.output2)

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
         CHEMICAL_ANALYSIS %in% c("Measured", "Chemical analysis reported"),
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
                                                                                           "IC25", "EC10/", "IC40", "MATC/", "EC16", "ER10", "EC10", "LC20/"), "Low Effect",
                                                     ifelse(cec.prioritization.10$ENDPOINT %in% c("NR-LETH", "LC50", "EC50", "LC90", "LC100", "IC50", "LT50", "EC50*", "EC50/", "ET50", "LC50*", "LC50/", "ER50", "EC90", "EC100", "BMC50", "LC95", "LETC", "IC50/", "LD50", "LC99", "EC80", "NR-LETH/", "LC85", "ED90", "LC50*/"), "Effect", "Miscellaneous")))
list(unique((cec.prioritization.10 %>% filter(ENDPOINT_TYPE == "Miscellaneous"))$ENDPOINT))

#subset into groups based on effect type ####
list(unique(cec.prioritization.10$EFFECT))

#group into tier groups
cec.prioritization.11 <- cec.prioritization.10 %>% filter(EFFECT != "No Effect")
list(unique(cec.prioritization.11$EFFECT))

cec.prioritization.11$TIER <- ifelse(cec.prioritization.11$EFFECT %in% c("Mortality", "Reproduction", "Behavior", "Population", "Growth", "Development", "Intoxication", "Mortality (Delayed)", "Growth (Delayed)", "Population (Delayed)", "Feeding behavior","Morphology", "Avoidance"), "Tier 1",
                                      ifelse(cec.prioritization.11$EFFECT %in% c("Enzyme(s)", "Genetics", "Histology", "Biochemistry", "Physiology",
                                                                                 "Multiple", "Cell(s)", "Immunological", "Hormone(s)", "Injury", "Ecosystem process"), "Tier 2", "Miscellaneous"))
list(unique((cec.prioritization.11 %>% filter(TIER == "Miscellaneous"))$EFFECT))

#select tier 2#

tier2 <- cec.prioritization.11 %>% filter(TIER == "Tier 2")
View(tier2)

tier2.box2 <- boxplot(tier2$CONC_MEAN ~ tier2$CAS_NUMBER)
tier2.box2$out

tier2.outliers <- subset(tier2, tier2$CONC_MEAN %in% tier2.box2$out)
View(tier2.outliers)

tier2.box.info <- tier2 %>% group_by(CAS_NUMBER) %>% summarise(Q1 = quantile(CONC_MEAN, 0.25), Q3 = quantile(CONC_MEAN, 0.75))

tier2.outliers1 <- left_join(tier2.outliers, tier2.box.info, by = "CAS_NUMBER")
View(tier2.outliers1)

tier2.outliers1$OUTLIER.CLASS <- ifelse(tier2.outliers1$CONC_MEAN < tier2.outliers1$Q1, "POTENTIAL LOWER OUTLIER", "POTENTIAL UPPER OUTLIER")

tier2.outliers2 <- tier2.outliers1 %>% filter(OUTLIER.CLASS == "POTENTIAL LOWER OUTLIER")

#bind in the CAS numbers#
CAS.list <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\CEC lists and Concentration Data\\CAS lists\\CAS List for ECOTOX search.xlsx")
CAS.list1 <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\CEC lists and Concentration Data\\CAS lists\\additional_CAS_for_ECOTOX_29_07_2021.xlsx")
CAS.list2 <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\CEC lists and Concentration Data\\CAS lists\\CAS_list_part3.xlsx")
CAS.list <- bind_rows(CAS.list, CAS.list1, CAS.list2)
CAS.list$CAS_NUMBER <- gsub("-", "", CAS.list$CAS)
View(CAS.list)

tier2.outliers3 <- left_join(tier2.outliers2, CAS.list)

write_xlsx(tier2.outliers3, "CEC.TIER2.POUTLIERS_18082021.xlsx")

#data limited outliers ####
tier2_summary <- tier2 %>% group_by(CAS_NUMBER) %>% summarize(min2_conc = nth(CONC_MEAN, 2, order_by = CONC_MEAN), min_conc = min(CONC_MEAN), n_EC = length(CONC_MEAN), n_Study = length(unique(REFERENCE_NUMBER)))
View(tier2_summary)

tier2_min_outliers <- left_join(tier2, tier2_summary)
tier2_min_outliers$min2_v_min <- tier2_min_outliers$min2_conc / tier2_min_outliers$min_conc

tier2_min_outliers$n_EC <- as.numeric(tier2_min_outliers$n_EC)
tier2_min_outliers$min2_v_min <- as.numeric(tier2_min_outliers$min2_v_min)
tier2_min_outliers$flag_EC <- ifelse(tier2_min_outliers$n_EC < 5 & tier2_min_outliers$min2_v_min >= 10 | tier2_min_outliers$n_EC < 5 & is.na(tier2_min_outliers$min2_conc), "Potential Lower Outlier", "")
tier2_min_outliers$flag_Study <- ifelse(tier2_min_outliers$n_Study < 5 & tier2_min_outliers$min2_v_min >= 10 | tier2_min_outliers$n_Study < 5 & is.na(tier2_min_outliers$min2_conc), "Potential Lower Outlier", "")

tier2_min_outliers_EC <- tier2_min_outliers %>% filter(flag_EC == "Potential Lower Outlier")
tier2_min_outliers_Study <- tier2_min_outliers %>% filter(flag_Study == "Potential Lower Outlier")

#bind in chemicals
CAS.list_1 <- CAS.list %>% select("CAS", "CAS_NUMBER")

tier2_min_outliers_EC1 <- left_join((tier2_min_outliers_EC %>% mutate_all(as.character)), (CAS.list_1 %>% mutate_all(as.character)))
tier2_min_outliers_Study1 <- left_join((tier2_min_outliers_Study %>% mutate_all(as.character)), (CAS.list_1 %>% mutate_all(as.character)))

#limited number of study outliers#
write_xlsx(tier2_min_outliers_Study1, "tier2_min_outliers_Study_1808_2021.xlsx") #153

#read back in outliers and remove from dataset/replace####
tier2.outliers_em <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\Tier 1 Measured Concentration QAQC 10-02-2021\\CEC.TIER2.POUTLIERS_EM_02_03_2021.xlsx") %>% mutate_all(as.character)
tier2.outliers_me <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\Outlier QAQC 02-04-2021\\Tier2_outliers_annotated\\tier2_min_outliers_Study_ME.Final.xlsx") %>% mutate_all(as.character)
tier2.outliers_nv <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\Outlier QAQC 02-04-2021\\Tier2_outliers_annotated\\tier2_min_outliers_Study_NV.Final.xlsx") %>% mutate_all(as.character)
tier2.outliers_final <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\missing_t2_outliers_EM.xlsx")
tier2_outliers_final <- bind_rows(tier2.outliers_em, tier2.outliers_me, tier2.outliers_nv, tier2.outliers_final)
names(tier2.outliers_em)

#pull out outliers that haven't been QAQC'd####
all_t2_outliers <- bind_rows(tier2_min_outliers_Study1, (tier2.outliers3 %>% mutate_all(as.character))) %>% 
  select(-c(min2_conc:IUPAC_NAME))
names(all_t2_outliers)

missing_t2_outliers <- anti_join(all_t2_outliers, tier2_outliers_final)
View(missing_t2_outliers)
write_xlsx(missing_t2_outliers, "missing_t2_outliers.xlsx")

#normal code after outliers####
tier2.outliers_exclude <- tier2_outliers_final %>% filter(Action %in% c("Exclude", "replace")) %>%
  select(-c("Q1":"Action"))

names(tier2.outliers_exclude)

#Exclude outliers#
tier2.1 <- anti_join((tier2 %>% mutate_all(as.character)), tier2.outliers_exclude)
tier2.1$Concentration_Type <- "Measured"

#put in replacements
names(tier2.replacements)
tier2.replacements <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\missing_t2_outliers_EM.xlsx", 2) %>% mutate_all(as.character) %>%
  select(-c("Full Article Accessible?":"Action"))
tier2.replacements$Concentration_Type <- "Measured"

#annotate and bind full dataset back together ####

cec.prioritization.12.2 <- bind_rows(tier2.1, tier2.replacements)

#bind CAS numbers and common names to dataset

CAS.list1 <- CAS.list %>% select("Preferred Name", "CAS", "CAS_NUMBER") %>% rename("Chemical_Name_Common" = "Preferred Name")
CAS.list1$CAS_NUMBER <- as.numeric(CAS.list1$CAS_NUMBER)

cec.prioritization.13.2 <- left_join(cec.prioritization.12.2, (CAS.list1 %>% mutate_all(as.character)), by = "CAS_NUMBER")

#read in annotated files
tier2_measured_annotated <- read_excel("tier2.2_for_QAQC_EM_annotated_22_02_2022.xlsx")

cec.prioritization.14 <- left_join(cec.prioritization.13.2, tier2_measured_annotated) %>% distinct()

write_xlsx(cec.prioritization.14, "tier2_measured_for_QAQC.xlsx")

#read-in annotated spreadsheet
tier2_measured_qad <- read_excel("tier2_measured_for_QAQC_EM_annotated.xlsx") %>% filter(Action == "Keep")
tier2_measured_qad_replacements <- read_excel("tier2_measured_for_QAQC_EM_annotated.xlsx")

tier2_measured_final <- bind_rows(tier2_measured_qad, tier2_measured_qad_replacements) %>% filter(Action == "Keep") %>% select(-c("Full Article Accessible?":"Action"))
names(tier2_measured_final)
View(tier2_measured_final)

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

only.nominal <- anti_join(cec.prioritization.1.nom, (tier2_measured_final %>% select(CAS_NUMBER)), by = "CAS_NUMBER")
summary(list(unique(only.nominal$CAS_NUMBER)))
View(only.nominal)
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
                                                                     "Invertebrates; Standard Test Species", "Invertebrates", "Worms", "Insects/Spiders; U.S. Invasive Species", "Molluscs; U.S. Invasive Species", "Insects/Spiders", "Crustaceans; Standard Test Species", 
                                                                     "Crustaceans", "Insects/Spiders; Standard Test Species", "Invertebrates; Standard Test Species", "Molluscs", "Worms; Standard Test Species", "Worms", "Invertebrates", "Insects/Spiders; U.S. Invasive Species", "Molluscs; U.S. Invasive Species", "Molluscs; Standard Test Species"), "Invertebrate",
                                 ifelse(only.nominal.7$SPECIES_GROUP %in% c("Algae; Standard Test Species", "Flowers, Trees, Shrubs, Ferns; Standard Test Species; U.S. Invasive Species", "Flowers, Trees, Shrubs, Ferns",
                                                                            "Flowers, Trees, Shrubs, Ferns; U.S. Invasive Species", "Algae","Flowers, Trees, Shrubs, Ferns; Standard Test Species","Algae; Standard Test Species", "Algae", "Flowers, Trees, Shrubs, Ferns", "Flowers, Trees, Shrubs, Ferns; Standard Test Species; U.S. Invasive Species", "Flowers, Trees, Shrubs, Ferns; Standard Test Species",
                                                                            "Flowers, Trees, Shrubs, Ferns; U.S. Invasive Species") | only.nominal.7$SPECIES_COMMON_NAME == "Plankton", "Plant",
                                        ifelse(only.nominal.7$SPECIES_GROUP %in% c("Fish", "Fish; Standard Test Species", "Fish; Standard Test Species; U.S. Threatened and Endangered Species; U.S. Invasive Species",
                                                                                   "Fish; Standard Test Species; U.S. Invasive Species", "Amphibians; Standard Test Species", "Amphibians", "Amphibians; Standard Test Species; U.S. Invasive Species", "Fish; Standard Test Species", "Fish; Standard Test Species; U.S. Threatened and Endangered Species; U.S. Invasive Species", "Fish", 
                                                                                   "Amphibians", "Amphibians; Standard Test Species; U.S. Invasive Species", "Fish; Standard Test Species; U.S. Threatened and Endangered Species", "Fish; Standard Test Species; U.S. Invasive Species", "Fish; U.S. Threatened and Endangered Species", "Amphibians; Standard Test Species", "Fish; U.S. Invasive Species"), "Fish",
                                               "Miscellaneous")))
list(unique(only.nominal.7$SPECIES))

misc <- only.nominal.7 %>% filter(SPECIES == "Miscellaneous")
list(unique(misc$SPECIES))

only.nominal.7.1 <- only.nominal.7 %>% filter(SPECIES != "Miscellaneous")

#add column defining studies as acute or chronic - nominal####
only.nominal.8 <- only.nominal.7.1
only.nominal.8$STUDY_TYPE <- ifelse(only.nominal.8$STUDY_DURATION < 7, "acute", "chronic")
list(unique(only.nominal.8$STUDY_TYPE))

#group endpoint types - nominal ####
list(unique(only.nominal.8$ENDPOINT))
only.nominal.9 <- only.nominal.8 %>% filter(ENDPOINT != "LT100")
only.nominal.9$ENDPOINT_TYPE <- ifelse(only.nominal.9$ENDPOINT %in% c("NOEC", "NR-ZERO", "NOEL", "EC0", "EC05", "NOEC/", "LC05", "LC01", "LC0", "EC01"), "No Effect",
                                       ifelse(only.nominal.9$ENDPOINT %in% c("LOEC", "EC10", "LOEL","IC10", "EC20", "LC10", "EC16", "EC20/", "IC25","LOEC/", "MATC", "LC40", "LC25", "LC15", "EC25", "MATC/", "LC34", "LC20", "LC30"), "Low Effect",
                                              ifelse(only.nominal.9$ENDPOINT %in% c("LC50", "EC50", "LC95", "NR-LETH", "IC50", "LC90", "EC50/", "EC84", "EC100", "IC100", "EC50*/", "EC80", "LD50",
                                                                                    "LC50*", "EC50*", "LC50/", "IC50/", "IC90/", "IC99/", "LC99", "LC75", "LC85", "LC100", "LC80", "LC60", "LD90","EC95/", "LC70", "EC70", "EC60", "EC95","EC90","NR-LETH/", "EC100/",
                                                                                    "LC96"), "Effect",
                                                     "Miscellaneous")))

misc <- only.nominal.9 %>% filter(ENDPOINT_TYPE == "Miscellaneous")
list(unique(misc$ENDPOINT))
list(unique(only.nominal.9$ENDPOINT_TYPE))

only.nominal.9.1 <- only.nominal.9 %>% filter(ENDPOINT_TYPE != "Miscellaneous")

#subset into groups based on effect type ####
list(unique(only.nominal.9.1$EFFECT))

#group into tier groups
only.nominal.10 <- only.nominal.9.1

only.nominal.10$TIER <- ifelse(only.nominal.10$EFFECT %in% c("Mortality", "Reproduction", "Behavior", "Population", "Growth", "Development", "Intoxication", "Mortality (Delayed)", "Growth (Delayed)", "Population (Delayed)", "Feeding behavior","Morphology", "Avoidance"), "Tier 1",
                               ifelse(only.nominal.10$EFFECT %in% c("Enzyme(s)", "Genetics", "Histology", "Biochemistry", "Physiology",
                                                                                                            "Multiple", "Cell(s)", "Immunological", "Hormone(s)", "Injury", "Ecosystem process"), "Tier 2", "Miscellaneous"))
list(unique(only.nominal.10$TIER))
list(unique((only.nominal.10 %>% filter(TIER == "Miscellaneous"))$EFFECT))

only.nominal.10.1 <- only.nominal.10 %>% filter(TIER != "Miscellaneous")

#generate boxplots to identify outliers in effect data - nominal, tier 2 ####

tier2.nom <- only.nominal.10.1 %>% filter(TIER == "Tier 2")

tier2.nom.box2 <- boxplot(tier2.nom$CONC_MEAN ~ tier2.nom$CAS_NUMBER)
tier2.nom.box2$out

tier2.nom.outliers <- subset(tier2.nom, tier2.nom$CONC_MEAN %in% tier2.nom.box2$out)

tier2.nom.box.info <- tier2.nom %>% group_by(CAS_NUMBER) %>% summarise(Q1 = quantile(CONC_MEAN, 0.25), Q3 = quantile(CONC_MEAN, 0.75))

tier2.nom.outliers1 <- left_join(tier2.nom.outliers, tier2.nom.box.info, by = "CAS_NUMBER")

tier2.nom.outliers1$OUTLIER.CLASS <- ifelse(tier2.nom.outliers1$CONC_MEAN < tier2.nom.outliers1$Q1, "POTENTIAL LOWER OUTLIER", "POTENTIAL UPPER OUTLIER")

tier2.nom.outliers2 <- tier2.nom.outliers1 %>% filter(OUTLIER.CLASS == "POTENTIAL LOWER OUTLIER")

tier2.nom.outliers3 <- left_join(tier2.nom.outliers2, (CAS.list1 %>% mutate_all(as.character)), by= "CAS_NUMBER")

write_xlsx(tier2.nom.outliers3, "CEC.TIER2.NOM.POUTLIERS_18_08_2021.xlsx")

#data limited outliers ####
tier2_nom_summary <- tier2.nom %>% group_by(CAS_NUMBER) %>% summarize(min2_conc = nth(CONC_MEAN, 2, order_by = CONC_MEAN), min_conc = min(CONC_MEAN), n_EC = length(CONC_MEAN), n_Study = length(unique(REFERENCE_NUMBER)))

tier2_nom_min_outliers <- left_join(tier2.nom, tier2_nom_summary)
tier2_nom_min_outliers$min2_v_min <- tier2_nom_min_outliers$min2_conc / tier2_nom_min_outliers$min_conc
View(tier2_nom_min_outliers)

tier2_nom_min_outliers$n_EC <- as.numeric(tier2_nom_min_outliers$n_EC)
tier2_nom_min_outliers$min2_v_min <- as.numeric(tier2_nom_min_outliers$min2_v_min)
tier2_nom_min_outliers$flag_EC <- ifelse(tier2_nom_min_outliers$n_EC < 5 & tier2_nom_min_outliers$min2_v_min >= 10 | tier2_nom_min_outliers$n_EC < 5 & is.na(tier2_nom_min_outliers$min2_conc), "Potential Lower Outlier", "")
tier2_nom_min_outliers$flag_Study <- ifelse(tier2_nom_min_outliers$n_Study < 5 & tier2_nom_min_outliers$min2_v_min >= 10 | tier2_nom_min_outliers$n_Study < 5 & is.na(tier2_nom_min_outliers$min2_conc), "Potential Lower Outlier", "")

tier2_nom_min_outliers_EC <- tier2_nom_min_outliers %>% filter(flag_EC == "Potential Lower Outlier")
tier2_min_nom_outliers_Study <- tier2_nom_min_outliers_EC %>% filter(flag_Study == "Potential Lower Outlier")

#bind in chemicals
CAS.list_1 <- CAS.list %>% select("CAS", "CAS_NUMBER")

tier2_nom_min_outliers_EC1 <- left_join(tier2_nom_min_outliers_EC, (CAS.list_1 %>% mutate_all(as.character)))
tier2_min_nom_outliers_Study1 <- left_join(tier2_min_nom_outliers_Study, (CAS.list_1 %>% mutate_all(as.character)))

#limited number of study outliers#
write_xlsx(tier2_min_nom_outliers_Study1, "tier2_min_nom_outliers_Study_18_08_2021.xlsx") #153

#read in annotated outliers#
tier2_nom_outliers_final <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\Outlier QAQC 02-04-2021\\Tier2_outliers_annotated\\tier2_nom_EM.final.xlsx") %>% mutate_all(as.character)
tier2_nom_outliers_extra <- read_excel( "C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\tier2_nom_extra_outliers_EM.xlsx")%>% mutate_all(as.character)

#pull out outliers that haven't been QAQC'd####
tier2_nom_outliers <- bind_rows(tier2_min_nom_outliers_Study1, tier2.nom.outliers3)
tier2_nom_extra_outliers <- anti_join((tier2_nom_outliers %>% mutate_all(as.character)), (tier2_nom_outliers_final %>% mutate_all(as.character)), by = c("CAS_NUMBER", "CHEMICAL_NAME", "CHEMICAL_ANALYSIS", "TEST_METHOD", "SPECIES_SCIENTIFIC_NAME", "SPECIES_COMMON_NAME", "SPECIES_KINGDOM", "SPECIES_PHYLUM", "SPECIES_SUBPHYLUM", "SPECIES_GROUP", "ORGANISM_LIFESTAGE", "EXPOSURE_TYPE", "MEDIA_TYPE", "TEST_LOCATION", "CONC_MEAN", "CONC1_TYPE_STANDARDIZED", "CONC1_MIN_STANDARDIZED", "CONC1_MAX_STANDARDIZED", "CONC_UNITS", "EFFECT", "EFFECT_MEASUREMENT", "ENDPOINT", "STATISTICAL_SIGNIFICANCE", "SIGNIFICANCE_LEVEL_MEAN", "REFERENCE_NUMBER", "TITLE", "AUTHOR", "PUBLICATION_YEAR", "STUDY_DURATION", "SPECIES", "STUDY_TYPE", "ENDPOINT_TYPE", "TIER"))
write_xlsx(tier2_nom_extra_outliers, "tier2_nom_extra_outliers.xlsx")

#continue code####
tier_2_nom_outliers <- bind_rows(tier2_nom_outliers_final, tier2_nom_outliers_extra)
names(tier_2_nom_outliers)

tier2_nom_outliers_exclude <- tier_2_nom_outliers %>% filter(Action == "Exclude") %>%
  select(-c("Q1":"flag_Study"))


#Exclude outliers#
tier2.nom.1 <- anti_join((tier2.nom %>% mutate_all(as.character)), tier2_nom_outliers_exclude)
tier2.nom.1$EC_Type <- "Nominal"

#bind CAS numbers and chemical names to cec.nominal
cec.t2.nominal1 <- left_join(tier2.nom.1, (CAS.list1 %>% mutate_all(as.character)))

#write out nominal tier 2 benchmarks and QA
tier_2_qad <- read_excel("tier2.2_for_QAQC_EM_annotated_22_02_2022.xlsx")
annotated_new_nominals <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\ECOTOX Data\\nominals_new_12_t2_for_QAQC_EM_annotated.xlsx") %>% 
  filter(Action %in% c("Exclude", "Replace")) %>% select(-c("Full Article Accessible?":"Action"))

annotated_new_nominals$CONC_MEAN <- as.character(annotated_new_nominals$CONC_MEAN)
annotated_new_nominals$CONC1_MIN_STANDARDIZED <- as.character(annotated_new_nominals$CONC1_MIN_STANDARDIZED)
annotated_new_nominals$CONC1_MAX_STANDARDIZED <- as.character(annotated_new_nominals$CONC1_MAX_STANDARDIZED)
annotated_new_nominals$STUDY_DURATION <- as.character(annotated_new_nominals$STUDY_DURATION)

cec.t2.nominal2 <- left_join(cec.t2.nominal1, tier_2_qad)
cec.t2.nominal3 <- left_join(cec.t2.nominal2, annotated_new_nominals)

write_xlsx(cec.t2.nominal3, "tier2_nominal_forQAQC.xlsx")

#read in 
tier2_nominal_qad <- read_excel("tier2_nominal_forQAQC_EM_annotated.xlsx") %>% filter(Action == "Keep")
tier2_nominal_replace <- read_excel("tier2_nominal_forQAQC_EM_annotated.xlsx", 2)

final_tier2_nominal <- bind_rows(tier2_nominal_qad, tier2_nominal_replace) %>% select(-c("Full Article Accessible?":"Action")) %>% distinct()
names(final_tier2_nominal)

#pull together nominal and measured###
tier2_collated <- bind_rows(tier2_measured_final, (final_tier2_nominal %>% rename("Concentration_Type" = "EC_Type"))) %>% distinct()
names(tier2_collated)

#benchmark derivation####
#get minimum EC values for each before assigning AF 
tier2.4 <- tier2_collated
tier2.4$CONC_MEAN <- as.double(tier2.4$CONC_MEAN)
tier2.4$CONC1_MIN_STANDARDIZED <- as.double(tier2.4$CONC1_MIN_STANDARDIZED)
tier2.4$CONC1_MAX_STANDARDIZED <- as.double(tier2.4$CONC1_MAX_STANDARDIZED)
tier2.4$STUDY_DURATION <- as.double(tier2.4$STUDY_DURATION)

tier2.5 <- tier2.4
tier2.6 <- tier2.5 %>% group_by(CAS) %>% summarize(min_EC = min(CONC_MEAN))
tier2.7 <- left_join(tier2.5, tier2.6) %>% filter(CONC_MEAN == min_EC)

#summarize number of studies available in ECOTOX - irregardless of whether they have been selected for this screening effort#
tier2data_for_data_availability <- cec.prioritization%>% 
  select(CAS_NUMBER, CHEMICAL_NAME, CHEMICAL_ANALYSIS, TEST_METHOD, SPECIES_SCIENTIFIC_NAME, SPECIES_COMMON_NAME, SPECIES_KINGDOM, SPECIES_PHYLUM, SPECIES_SUBPHYLUM,
         SPECIES_GROUP, ORGANISM_LIFESTAGE, EXPOSURE_TYPE, MEDIA_TYPE, TEST_LOCATION, CONC1_MEAN_OP_STANDARDIZED, 
         CONC1_TYPE_STANDARDIZED, CONC1_MEAN_STANDARDIZED, CONC1_MIN_STANDARDIZED, CONC1_MAX_STANDARDIZED, CONC1_UNITS_STANDARDIZED, EFFECT, EFFECT_MEASUREMENT, ENDPOINT, STATISTICAL_SIGNIFICANCE, SIGNIFICANCE_LEVEL_MEAN, 
         OBSERVED_DURATION_MEAN_DAYS, OBSERVED_DURATION_MEAN_OP_DAYS, STUDY_DURATION_MEAN_DAYS,  STUDY_DURATION_MEAN_OP_DAYS, REFERENCE_NUMBER, TITLE, AUTHOR, PUBLICATION_YEAR) %>%
  filter(CONC1_TYPE_STANDARDIZED !=("Formulation"), !CONC1_MEAN_OP_STANDARDIZED %in% c(">", "<", "<=", "~", ">="), 
         CONC1_UNITS_STANDARDIZED %in% c("AI mg/L", "nmol/L", "mg/L", "ug/L", "ai mg/ml"), !OBSERVED_DURATION_MEAN_OP_DAYS %in% c("<","~",">","<=",">="),
         !ENDPOINT %in% c("BCF", "NR", "BCFD", "BCF/", "BAF", "ATCN","NR/", "ET50", "LT50"), EFFECT != "Accumulation", MEDIA_TYPE == "Fresh water", 
         !EXPOSURE_TYPE %in% c("Choice", "Food", "in Vitro", "Direct application", "Multiple routes between application groups", "Spray"), STATISTICAL_SIGNIFICANCE !="Not significant at all concentrations",
         SPECIES_KINGDOM != "Monera", EFFECT %in% c("Enzyme(s)", "Genetics", "Histology", "Biochemistry", "Physiology",
                                                    "Multiple", "Cell(s)", "Immunological", "Hormone(s)", "Injury", "Ecosystem process"))

View(tier2data_for_data_availability)
tier2data_for_data_availability$SPECIES <- ifelse(tier2data_for_data_availability$SPECIES_GROUP %in% c("Crustaceans; Standard Test Species", "Crustaceans", "Molluscs", "Insects/Spiders", "Insects/Spiders; Standard Test Species", 
                                                                                                       "Invertebrates; Standard Test Species", "Invertebrates", "Worms", "Insects/Spiders; U.S. Invasive Species", "Molluscs; U.S. Invasive Species", "Worms; Standard Test Species", "Molluscs; Standard Test Species",
                                                                                                       "Crustaceans; U.S. Invasive Species", "Molluscs; Standard Test Species; U.S. Invasive Species", "Molluscs; U.S. Threatened and Endangered Species", "Crustaceans; U.S. Threatened and Endangered Species"), "Invertebrate",
                                                  ifelse(tier2data_for_data_availability$SPECIES_GROUP %in% c("Algae; Standard Test Species", "Flowers, Trees, Shrubs, Ferns; Standard Test Species; U.S. Invasive Species", "Flowers, Trees, Shrubs, Ferns",
                                                                                                              "Flowers, Trees, Shrubs, Ferns; U.S. Invasive Species", "Algae","Flowers, Trees, Shrubs, Ferns; Standard Test Species", "Miscellaneous"), "Plant",
                                                         ifelse(tier2data_for_data_availability$SPECIES_GROUP %in% c("Fish", "Fish; Standard Test Species", "Fish; Standard Test Species; U.S. Threatened and Endangered Species; U.S. Invasive Species",
                                                                                                                     "Fish; Standard Test Species; U.S. Invasive Species", "Amphibians; Standard Test Species", "Amphibians", "Amphibians; Standard Test Species; U.S. Invasive Species", "Fish; Standard Test Species; U.S. Threatened and Endangered Species",
                                                                                                                     "Fish; U.S. Invasive Species", "Fish; U.S. Threatened and Endangered Species", "Amphibians; U.S. Threatened and Endangered Species","Amphibians; U.S. Invasive Species"), "Fish",
                                                                "misc")))
list(unique((tier2data_for_data_availability %>% filter(SPECIES == "misc"))$SPECIES_ScIENTIFIC_NAME))

tier2data_for_data_availability_1 <- tier2data_for_data_availability %>% filter(SPECIES != "Miscellaneous")

tier2data_for_data_availability <- tier2data_for_data_availability_1 %>%
  group_by(CAS_NUMBER, SPECIES) %>% summarize(unique_species = length(unique(SPECIES_SCIENTIFIC_NAME)), totalresults = length(CAS_NUMBER))%>%
  spread(SPECIES, unique_species) %>% group_by(CAS_NUMBER) %>% summarize(uFish = sum(Fish, na.rm = T), uInvert = sum(Invertebrate, na.rm = T), uPlant = sum(Plant, na.rm = T))
View(tier2data_for_data_availability)

tier2.8 <- left_join(tier2.7,tier2data_for_data_availability)

#add application factors#### 
tier2.8$AF_Effect <- ifelse(tier2.8$ENDPOINT_TYPE == "No Effect", 1,
                                          ifelse(tier2.8$ENDPOINT_TYPE == "Low Effect", 5,
                                                 ifelse(tier2.8$ENDPOINT_TYPE == "Effect", 10, "missing")))
list(unique(tier2.8$AF_Effect))


tier2.8$AF_StudyLength <- ifelse(tier2.8$STUDY_TYPE == "acute", 5,
                                               ifelse(tier2.8$STUDY_TYPE == "chronic", 1, "missing"))
list(unique(tier2.8$AF_StudyLength))

tier2.8$AF_datarichness <- 2
tier2.8$AF_datarichness <- ifelse((tier2.8$uFish == 0 & tier2.8$uInvert == 0 & tier2.8$uPlant == 0), 5, 
                                                (ifelse((tier2.8$uFish > 0 & tier2.8$uInvert == 0 & tier2.8$uPlant >= 0), 4, 
                                                        (ifelse((tier2.8$uFish == 0 & tier2.8$uInvert >= 0 & tier2.8$uPlant > 0), 4, 
                                                                (ifelse((tier2.8$uFish >= 0 & tier2.8$uInvert > 0 & tier2.8$uPlant == 0), 4, 
                                                                        (ifelse((tier2.8$uFish == 1 & tier2.8$uInvert == 1 & tier2.8$uPlant == 1), 3,
                                                                                (ifelse((tier2.8$uFish >= 3 & tier2.8$uInvert >= 3 & tier2.8$uPlant >= 3), 1, tier2.8$AF_datarichness)))))))))))

list(unique(tier2.8$AF_datarichness))
View((tier2.8 %>% filter(is.na(AF_datarichness))))

tier2.8$AF_datarichness <- as.numeric(tier2.8$AF_datarichness)

tier2.9 <- tier2.8 %>% drop_na(min_EC)
View(tier2.9)

##sum AF#
summary(tier2.9)

tier2.9$AF_Effect <- as.numeric(tier2.9$AF_Effect)
tier2.9$AF_StudyLength <- as.numeric(tier2.9$AF_StudyLength)

tier2.9$AF_Total <- tier2.9$AF_datarichness * tier2.9$AF_Effect * tier2.9$AF_StudyLength

write_xlsx(tier2.9, "preBenchmark_file_tier2.xlsx")

##divide AF_total by min concentration to get benchmark value##
setwd("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Final Benchmarks - Iterations")

tier2.10 <- tier2.9
names(tier2.10)

tier2.10$AF_BM <- tier2.10$min_EC / tier2.10$AF_Total

tier2.11 <- tier2.10 %>% group_by(CAS) %>% summarize(min_AF_BM = min(AF_BM))

tier2.12 <- left_join(tier2.10, tier2.11) %>% filter(AF_BM == min_AF_BM) %>% select(-min_AF_BM) %>%
  select(-c(CAS_NUMBER:TEST_METHOD, SPECIES_COMMON_NAME:SPECIES_GROUP, CONC1_TYPE_STANDARDIZED:CONC_UNITS,
            STATISTICAL_SIGNIFICANCE:SIGNIFICANCE_LEVEL_MEAN))
names(tier2.12)

tier2.13 <- tier2.12 %>% rename("min_concentration" = "min_EC") %>% relocate(CAS, .before = SPECIES_SCIENTIFIC_NAME) %>% 
  relocate(Chemical_Name_Common, .after = CAS) %>% relocate(TIER:Concentration_Type, .after= Chemical_Name_Common) %>%
  relocate(SPECIES:ENDPOINT_TYPE, .after = Concentration_Type) %>%
  relocate(uFish:uPlant, .after = ENDPOINT_TYPE) %>% relocate(AF_Effect:AF_Total, .after = uPlant) %>%
  relocate(min_concentration, .after = AF_Total) %>% relocate(AF_BM, .after = min_concentration) %>%
  relocate(TITLE:PUBLICATION_YEAR, .after = AF_BM) %>% relocate(REFERENCE_NUMBER, .after = PUBLICATION_YEAR) %>%
  relocate(SPECIES_SCIENTIFIC_NAME:ENDPOINT, .after = REFERENCE_NUMBER)

names(tier2.13)
write_xlsx(tier2.13, "Tier2_BM_iteration_05_03_2022.xlsx")

