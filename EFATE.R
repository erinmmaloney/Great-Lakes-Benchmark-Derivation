#Environmental Fate Data Collation - CEC Prioritization#

library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(stringr)
library(xlsx)
library(rJava)
library(ggpubr)
library(viridis)
library(ComplexHeatmap)
library(lattice)

setwd("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Environmental Fate")

PB <- read_xlsx("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Environmental Fate\\CAS List - ENVIRONMENTAL FATE_R_INPUT.xlsx")
names(PB)

PB$OPERA.BCF_unconverted <- ifelse(PB$AD_index_BCF == 0, "NaN", PB$LogBCF_pred)
PB$OPERA_T12_unconverted <- ifelse(PB$AD_index_BioDeg == 0, "NaN", PB$BioDeg_LogHalfLife_pred)

PB$OPERA.BCF <- as.numeric(PB$OPERA.BCF_unconverted)
PB$OPERA.BCF <- 10^(PB$OPERA.BCF)

PB$OPERA.T12 <- as.numeric(PB$OPERA_T12_unconverted)
PB$OPERA.T12 <- 10^(PB$OPERA.T12)

#bind in MW and flag chemicals with a MW > 697.1 and with a MW < 31.06 as outside of AD for BIOWIN_P estimates; 
#flag chemicals with MW > 959.17 and a MW < 68.08 & those with logKow > 11.26 as outside of AD of BCFBAF model
write_xlsx((PB %>% select(CAS) %>% distinct()), "CAS_for_physprop.xlsx")

phys.prop <- read_csv("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Environmental Fate\\phys_prop.csv")
names(phys.prop)
View(phys.prop)
names(PB)

phys.prop1 <- phys.prop %>% rename("MW"  = "AVERAGE_MASS", "LOGKOW"="OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED") %>%
  select("CAS", "MW", "LOGKOW")

PB.1 <- left_join(PB, phys.prop1, by = "CAS")
PB.1$MW <- as.numeric(PB.1$MW)
PB.1$BIOWIN_AD <- 1
PB.1$BIOWIN_AD <- ifelse(PB.1$MW < 31.06, 0, PB.1$BIOWIN_AD)
PB.1$BIOWIN_AD <- ifelse(PB.1$MW > 697.1, 0, PB.1$BIOWIN_AD)
PB.1$BIOWIN_AD <- ifelse(is.na(PB.1$MW), 0, PB.1$BIOWIN_AD)

PB.1$LOGKOW <- as.numeric(PB.1$LOGKOW)
PB.1$BCFBAF_AD <- 1
PB.1$BCFBAF_AD <- ifelse(PB.1$MW > 959.17, 0, PB.1$BCFBAF_AD) 
PB.1$BCFBAF_AD <- ifelse(PB.1$MW < 68.08, 0, PB.1$BCFBAF_AD) 
PB.1$BCFBAF_AD <- ifelse(is.na(PB.1$MW), 0, PB.1$BCFBAF_AD) 
PB.1$BCFBAF_AD <- ifelse(PB.1$LOGKOW < -6.50, 0, PB.1$BCFBAF_AD) 
PB.1$BCFBAF_AD <- ifelse(PB.1$LOGKOW > 11.26, 0, PB.1$BCFBAF_AD) 
PB.1$BCFBAF_AD <- ifelse(is.na(PB.1$LOGKOW), 0, PB.1$BCFBAF_AD) 

View(PB.1)

list(unique(PB.1$P_EPI_output))
PB.1$P_EPI <- ifelse(PB.1$P_EPI_output > 4.75, 0.17,
                   ifelse(PB.1$P_EPI_output > 4.25 & PB.1$P_EPI_output <= 4.75, 1.25,
                          ifelse(PB.1$P_EPI_output > 3.75 & PB.1$P_EPI_output <= 4.25, 2.33,
                                 ifelse(PB.1$P_EPI_output > 3.25 & PB.1$P_EPI_output <= 3.75, 8.67,
                                        ifelse(PB.1$P_EPI_output >  2.75 & PB.1$P_EPI_output <= 3.25, 15, 
                                               ifelse(PB.1$P_EPI_output > 2.25 & PB.1$P_EPI_output <= 2.75, 37.5,
                                                      ifelse(PB.1$P_EPI_output > 1.75 & PB.1$P_EPI_output <= 2.25, 120,
                                                             ifelse(PB.1$P_EPI_output > 1.25 & PB.1$P_EPI_output <= 1.75, 240,
                                                                    ifelse(PB.1$P_EPI_output <= 1.25, 720, "NaN")))))))))

summary(PB.1$P_EPI)
PB.1$P_EPI <- as.numeric(PB.1$P_EPI)
sum(is.na(PB.1$P_EPI))

View(PB.1)

#eliminate factors outside of EPI & BCFBAF AD
names(PB.1)
PB.2 <- PB.1
PB.2$P_EPI <- ifelse(PB.2$BIOWIN_AD == 0, "NaN", PB.2$P_EPI)
PB.2$BCF_EPI <- ifelse(PB.2$BCFBAF_AD == 0, "NaN", PB.2$BCF_EPI)

View(PB.2)

#bind in VEGA estimates#

#P model
VEGA_P <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Environmental Fate\\VEGA Output\\P_IMFRN_Quant.xlsx")

VEGA_P1 <- VEGA_P  %>% select("Id", "Pred_HL_d", "ADI") %>% rename("CAS" = "Id", "VEGA_P" = "Pred_HL_d")
VEGA_P1$ADI <- as.numeric(VEGA_P1$ADI)
VEGA_P1$P_VEGA <- ifelse(VEGA_P1$ADI > 0.75, VEGA_P1$VEGA_P, NaN)

VEGA_P2 <- VEGA_P1 %>% select("CAS", "P_VEGA")

#BCF models
VEGA_B_KNN <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Environmental Fate\\VEGA Output\\report_BCF_KNN.xlsx")

VEGA_B_MEYLAN <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Environmental Fate\\VEGA Output\\report_BCF_MEYLAN.xlsx")

VEGA_B_CAESAR <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Environmental Fate\\VEGA Output\\report_BCF_CAESAR.xlsx")

#KNN
VEGA_B_KNN_1 <- VEGA_B_KNN %>% select("Id", "Predicted_log_BCF", "Exp_BCF", "ADI") %>%
  rename("CAS" = "Id")
  
VEGA_B_KNN_1$Exp_BCF <- as.numeric(VEGA_B_KNN_1$Exp_BCF)
VEGA_B_KNN_1$Predicted_log_BCF <- as.numeric(VEGA_B_KNN_1$Predicted_log_BCF)
VEGA_B_KNN_1$ADI <- as.numeric(VEGA_B_KNN_1$ADI)

VEGA_B_KNN_1$VEGA_B_KNN <- ifelse(is.na(VEGA_B_KNN_1$Exp_BCF), VEGA_B_KNN_1$Predicted_log_BCF, VEGA_B_KNN_1$Exp_BCF)
VEGA_B_KNN_1$VEGA_B_KNN <- 10^(VEGA_B_KNN_1$VEGA_B_KNN)
VEGA_B_KNN_1$B_VEGA_KNN <- ifelse(VEGA_B_KNN_1$ADI > 0.75, VEGA_B_KNN_1$VEGA_B_KNN, NaN)

VEGA_B_KNN_2 <- VEGA_B_KNN_1 %>% select("CAS", "B_VEGA_KNN")
View(VEGA_B_KNN_2)

#MEYLAN
names(VEGA_B_MEYLAN)
VEGA_B_MEYLAN_1 <- VEGA_B_MEYLAN %>% select("Id", "Pred_BCF", "Exp_logBCF", "ADI") %>%
  rename("CAS" = "Id")

VEGA_B_MEYLAN_1$Exp_logBCF <- as.numeric(VEGA_B_MEYLAN_1$Exp_logBCF)
VEGA_B_MEYLAN_1$Pred_BCF <- as.numeric(VEGA_B_MEYLAN_1$Pred_BCF)
VEGA_B_MEYLAN_1$ADI <- as.numeric(VEGA_B_MEYLAN_1$ADI)

VEGA_B_MEYLAN_1$Exp_BCF <- 10^(VEGA_B_MEYLAN_1$Exp_logBCF )
VEGA_B_MEYLAN_1$VEGA_B_MEY <- ifelse(is.na(VEGA_B_MEYLAN_1$Exp_BCF), VEGA_B_MEYLAN_1$Pred_BCF, VEGA_B_MEYLAN_1$Exp_BCF)
VEGA_B_MEYLAN_1$B_VEGA_MEY <- ifelse(VEGA_B_MEYLAN_1$ADI > 0.75, VEGA_B_MEYLAN_1$VEGA_B_MEY, NaN)

VEGA_B_MEYLAN_2 <- VEGA_B_MEYLAN_1 %>% select("CAS", "B_VEGA_MEY")

#CAESAR
names(VEGA_B_CAESAR)
VEGA_B_CAESAR_1 <- VEGA_B_CAESAR %>% select("Id", "Pred_BCF", "Experimental [log(L/kg)]", "ADI") %>%
  rename("CAS" = "Id", "Exp_logBCF" = "Experimental [log(L/kg)]")

VEGA_B_CAESAR_1$Exp_logBCF <- as.numeric(VEGA_B_CAESAR_1$Exp_logBCF)
VEGA_B_CAESAR_1$Pred_BCF <- as.numeric(VEGA_B_CAESAR_1$Pred_BCF)
VEGA_B_CAESAR_1$ADI <- as.numeric(VEGA_B_CAESAR_1$ADI)

VEGA_B_CAESAR_1$Exp_BCF <- 10^(VEGA_B_CAESAR_1$Exp_logBCF )
VEGA_B_CAESAR_1$VEGA_B_CAES <- ifelse(is.na(VEGA_B_CAESAR_1$Exp_BCF), VEGA_B_CAESAR_1$Pred_BCF, VEGA_B_CAESAR_1$Exp_BCF)
VEGA_B_CAESAR_1$B_VEGA_CAES <- ifelse(VEGA_B_CAESAR_1$ADI > 0.75, VEGA_B_CAESAR_1$VEGA_B_CAES, NaN)

VEGA_B_CAESAR_2 <- VEGA_B_CAESAR_1 %>% select("CAS", "B_VEGA_CAES")


##bind in everything
PB.3 <- left_join(PB.2, VEGA_P2)
VEGA_B_KNN_2$CAS <- as.character(VEGA_B_KNN_2$CAS)
PB.4 <- left_join(PB.3, VEGA_B_KNN_2)
VEGA_B_MEYLAN_2$CAS <- as.character(VEGA_B_MEYLAN_2$CAS)
PB.5 <- left_join(PB.4, VEGA_B_MEYLAN_2)
VEGA_B_CAESAR_2$CAS <- as.character(VEGA_B_CAESAR_2$CAS)
PB.6 <- left_join(PB.5, VEGA_B_CAESAR_2)
PB.6 <- PB.6 %>% distinct()
View(PB.6)

#pull everything together
names(PB.semifinal)
PB.semifinal <- PB.6 %>% select("CAS", "Preferred Name", "BCF_EPI", "OPERA.BCF", "B_comptox","B_VEGA_KNN", "B_VEGA_MEY", "B_VEGA_CAES",
                                "P_EPI", "OPERA.T12", "P_comptox", "P_VEGA") %>% gather(BCF_EPI:B_VEGA_CAES, key = "BCF_type", value = "BCF") %>% gather(P_EPI:P_VEGA, key = "P_type", value = "T12") %>% distinct()

PB.semifinal$T12 <- as.numeric(PB.semifinal$T12)
PB.semifinal$BCF <- as.numeric(PB.semifinal$BCF)
View(PB.semifinal)

PB.semifinale <- PB.semifinal %>% group_by(CAS) %>% summarize(geomean_P = exp(mean(log(T12), na.rm = TRUE)), geomean_B = exp(mean(log(BCF), na.rm = TRUE)))

PB.semifinal_1 <- left_join(PB.semifinale, PB.6) %>% select("CAS", "geomean_P", "geomean_B", B_comptox, P_comptox)
names(PB.semifinal_1)

PB.final <- PB.semifinal_1

PB.final$B_comptox <- as.numeric(PB.final$B_comptox)
PB.final$P_comptox <- as.numeric(PB.final$P_comptox)

PB.final$P_estimate <- ifelse(is.na(PB.final$P_comptox), PB.final$geomean_P, PB.final$P_comptox)
PB.final$B_estimate <- ifelse(is.na(PB.final$B_comptox), PB.final$geomean_B, PB.final$B_comptox)
PB.final$P_note <- ifelse(is.na(PB.final$P_comptox) & is.na(PB.final$geomean_P), "Data Limited",
                          ifelse(is.na(PB.final$P_comptox) & !is.na(PB.final$geomean_P), "Estimated", "Measured"))
                          
PB.final$B_note <- ifelse(is.na(PB.final$B_comptox) & is.na(PB.final$geomean_B), "Data Limited",
                          ifelse(is.na(PB.final$B_comptox) & !is.na(PB.final$geomean_B), "Estimated", "Measured"))

PB.final1 <- PB.final %>% rename("Half_life_days" = "P_estimate", "BCF_Lkg" = "B_estimate")

write_xlsx(PB.final1, "E_FATE_summary.xlsx")

#bind to names from CAS list
names(PB)
PB_descriptors <- PB %>% select("CAS", "Preferred Name") %>% rename("Chemical Name" = "Preferred Name")

PB.final2 <- left_join(PB_descriptors, PB.final1)

#annotate compounds due to Environmental Fate characteristics 

PB.final2$EFate <- ifelse(PB.final2$Half_life_days <= 40 & PB.final2$BCF_Lkg <= 2000, "nPnB",
                                         ifelse(PB.final2$Half_life_days > 40 & PB.final2$Half_life_days <= 60 & PB.final2$BCF_Lkg <= 2000, "P",
                                                ifelse(PB.final2$Half_life_days <= 40 & PB.final2$BCF_Lkg > 2000 & PB.final2$BCF_Lkg <= 5000, "B",
                                                       ifelse(PB.final2$Half_life_days > 60 & PB.final2$BCF_Lkg <= 2000, "vP",
                                                              ifelse(PB.final2$Half_life_days <= 40 & PB.final2$BCF_Lkg > 5000, "vB", 
                                                                     ifelse(PB.final2$Half_life_days > 40 & PB.final2$Half_life_days <= 60 & PB.final2$BCF_Lkg > 2000 & PB.final2$BCF_Lkg <= 5000, "PB",
                                                                            ifelse(PB.final2$Half_life_days > 60 & PB.final2$BCF_Lkg > 2000 & PB.final2$BCF_Lkg <= 5000, "vPB",
                                                                                   ifelse(PB.final2$Half_life_days > 40 & PB.final2$Half_life_days <= 60 & PB.final2$BCF_Lkg > 5000, "PvB", 
                                                                                          ifelse(PB.final2$Half_life_days > 60 & PB.final2$BCF_Lkg > 5000, "vPvB", "Data Limited")))))))))

View(PB.final2)

PB.final2$EFate <- ifelse(is.na(PB.final2$EFate) & (PB.final2$Half_life_days <= 40 | PB.final2$BCF_Lkg <= 2000), "nP/nB - Data Limited",
                          ifelse(is.na(PB.final2$EFate) & PB.final2$Half_life_days > 40 & PB.final2$Half_life_days <= 60, "P - Data Limited", 
                                 ifelse(is.na(PB.final2$EFate) & PB.final2$Half_life_days > 60, "vP - Data Limited",
                                              ifelse(is.na(PB.final2$EFate) & PB.final2$BCF_Lkg > 5000, "vB - Data Limited",
                                                     ifelse(is.na(PB.final2$EFate) & PB.final2$BCF_Lkg > 2000 & PB.final2$BCF_Lkg <= 5000, "B - Data Limited",
                                                            PB.final2$EFate)))))

View(PB.final2)


PB.final2$Bin <- ifelse(PB.final2$EFate %in% c("nPnB", "nP/nB - Data Limited"), 1, 
                        ifelse(PB.final2$EFate %in% c("P", "B", "P - Data Limited", "B - Data Limited"), 2, 
                               ifelse(PB.final2$EFate %in% c("PB"), 3,
                               ifelse(PB.final2$EFate %in% c("vP", "vB", "vP - Data Limited", "vB - Data Limited"), 4, 
                                                             ifelse(PB.final2$EFate %in% c("vPB", "PvB"), 5, 
                                                                    ifelse(PB.final2$EFate %in% c("vPvB"), 6, 
                                                                           "DL"))))))

PB.final2$Score <- ifelse(PB.final2$Bin == 1, 1, 
                          ifelse(PB.final2$Bin == 2, 2, 
                                 ifelse(PB.final2$Bin == 3, 3, 
                                        ifelse(PB.final2$Bin == 4, 4, 
                                               ifelse(PB.final2$Bin == 5, 5,
                                                      ifelse(PB.final2$Bin == 6, 6,
                                                      "DL"))))))
View(PB.final2)

PB.final2.noe <- PB.final2 %>% filter(`Chemical Name` %in% c("Omeprazole", "Esomeprazole")) %>% select(-c(CAS, `Chemical Name`))
PB.final2.noe$CAS <- "73590-58-6 + 161796-78-7"
PB.final2.noe$`Chemical Name` <- "Omeprazole + Esomprazole"

PB.final2.npe <- PB.final2 %>% filter(`Chemical Name` %in% c("Pseudoephedrine", "Ephedrine"))
PB.final2.npe$CAS <- "90-82-4 + 299-42-3"
PB.final2.npe$`Chemical Name` <- "Pseudoephedrine + Ephedrine"

PB.final2.no <- PB.final2 %>% filter(!`Chemical Name` %in% c("Omeprazole", "Esomeprazole", "Pseudoephedrine", "Ephedrine"))

PB.final3 <- bind_rows(PB.final2.noe, PB.final2.npe, PB.final2.no) %>% distinct()
summary(PB.final3$Bin)

#Water Eval####

#pull out E-Fate for water only
water_chemicals <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\LoE Evaluation for MS 1\\E_Fate_doublecheck.xlsx") %>%
  select(CAS, "Chemical Name",  `Experimental Value (CompTox)...5`, `Experimental Value (CompTox)...9`) %>% distinct()

#match PB_estimates to chem list

PB.final4 <- PB.final3 %>% select("CAS", "Half_life_days", "BCF_Lkg", "P_note", "B_note", "EFate", "Bin", "Score")

water_chem_pb <- left_join(water_chemicals, PB.final4) %>% filter(CAS != "73590-58-6")
water_chem_pb$Bin <- ifelse(is.na(water_chem_pb$Bin), "DL", water_chem_pb$Bin)
water_chem_pb$Score <- ifelse(is.na(water_chem_pb$Score), "DL", water_chem_pb$Score)
water_chem_pb$Half_life_days <- ifelse(water_chem_pb$`Experimental Value (CompTox)...5` %in% c("-", "No", "no"), water_chem_pb$Half_life_days, water_chem_pb$`Experimental Value (CompTox)...5`)
water_chem_pb$BCF_Lkg <- ifelse(water_chem_pb$`Experimental Value (CompTox)...9` %in% c("-", "No", "no"), water_chem_pb$BCF_Lkg, water_chem_pb$`Experimental Value (CompTox)...9`)

water_chem_pb$EFate <- ifelse(water_chem_pb$Half_life_days <= 40 & water_chem_pb$BCF_Lkg <= 2000, "nPnB",
                          ifelse(water_chem_pb$Half_life_days > 40 & water_chem_pb$Half_life_days <= 60 & water_chem_pb$BCF_Lkg <= 2000, "P",
                                 ifelse(water_chem_pb$Half_life_days <= 40 & water_chem_pb$BCF_Lkg > 2000 & water_chem_pb$BCF_Lkg <= 5000, "B",
                                        ifelse(water_chem_pb$Half_life_days > 60 & water_chem_pb$BCF_Lkg <= 2000, "vP",
                                               ifelse(water_chem_pb$Half_life_days <= 40 & water_chem_pb$BCF_Lkg > 5000, "vB", 
                                                      ifelse(water_chem_pb$Half_life_days > 40 & water_chem_pb$Half_life_days <= 60 & water_chem_pb$BCF_Lkg > 2000 & water_chem_pb$BCF_Lkg <= 5000, "PB",
                                                             ifelse(water_chem_pb$Half_life_days > 60 & water_chem_pb$BCF_Lkg > 2000 & water_chem_pb$BCF_Lkg <= 5000, "vPB",
                                                                    ifelse(water_chem_pb$Half_life_days > 40 & water_chem_pb$Half_life_days <= 60 & water_chem_pb$BCF_Lkg > 5000, "PvB", 
                                                                           ifelse(water_chem_pb$Half_life_days > 60 & water_chem_pb$BCF_Lkg > 5000, "vPvB", "Data Limited")))))))))


water_chem_pb$EFate <- ifelse(is.na(water_chem_pb$EFate) & (water_chem_pb$Half_life_days <= 40 | water_chem_pb$BCF_Lkg <= 2000), "nP/nB - Data Limited",
                          ifelse(is.na(water_chem_pb$EFate) & water_chem_pb$Half_life_days > 40 & water_chem_pb$Half_life_days <= 60, "P - Data Limited", 
                                 ifelse(is.na(water_chem_pb$EFate) & water_chem_pb$Half_life_days > 60, "vP - Data Limited",
                                        ifelse(is.na(water_chem_pb$EFate) & water_chem_pb$BCF_Lkg > 5000, "vB - Data Limited",
                                               ifelse(is.na(water_chem_pb$EFate) & water_chem_pb$BCF_Lkg > 2000 & water_chem_pb$BCF_Lkg <= 5000, "B - Data Limited",
                                                      water_chem_pb$EFate)))))


write_xlsx(water_chem_pb, "E_FATE_LOE_water.xlsx")

file.choose()
efate_update <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\LoE evaluation for MS 1\\EFATE_update.xlsx") %>% select(-"Chemical Name", -"Experimental Value (CompTox)...5" , -"Experimental Value (CompTox)...9")

efate <- left_join(water_chem_pb, efate_update, by =c("CAS")) %>% select(-c("t1/2 (d)":"B Value", "Bin":"Score")) %>% distinct()
write_xlsx(efate, "efate_24_05_2022.xlsx")
names(efate)

#join in the remainder of the dataset 
names(PB.6)

EFATE_1 <- left_join(water_chem_pb, PB.6) %>% select(-c(Bin, Score, DTSXID, "Preferred Name", "MoleculeID", AD_BCF, AD_index_BCF, Conf_index_BCF,
                                                        AD_index_BioDeg, Conf_index_BioDeg,LogBCF_pred, BioDeg_LogHalfLife_pred, "OPERA.BCF_unconverted",
                                                        "OPERA_T12_unconverted", AD_BioDeg, P_EPI_output)) %>% distinct()
View(EFATE_1)
names(EFATE_1)

write_xlsx(EFATE_1, "E_FATE_LOE_water_for_SI.xlsx")

water_chem_pb_summary <- water_chem_pb %>% group_by(Bin) %>% summarize(nchem = n_distinct(CAS))
write_xlsx(water_chem_pb_summary, "E_FATE_LOE_water_summary.xlsx")

#efate for everything (water + passive)####
names(PB.final3)
names(PB.6)

PB_all <- left_join(PB.6, (PB.final3 %>% select(-c("B_comptox", P_comptox)))) %>% select(-c("DTSXID", "Preferred Name", "MoleculeID"))
names(PB_all)

write_xlsx(PB_all, "PB_all_table_for_SI.xlsx")

#plot P & B graphs to demonstrate E-Fate properties of chemicals for MS_1 ####
chemicals <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\WoE Prioritization\\benchmark_comp_file_10_05_2022.xlsx", 2) %>%
  filter(`Detected Water` == "TRUE") %>% select("CAS", "Class")
chemicals_2 <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\WoE Prioritization\\benchmark_comp_file_10_05_2022.xlsx", 2) %>%
  filter(`Detected Passive` == "TRUE") %>% select("CAS", "Class")

chemicals <- bind_rows(chemicals, chemicals_2) %>% distinct()

names(chemicals)
PB.final5 <- left_join(chemicals, PB.final3)

PB.final5$P_class <- ifelse(PB.final5$Half_life_days <= 40, "nP",
                            ifelse(PB.final5$Half_life_days > 60, "vP",
                                   ifelse(PB.final5$Half_life_days <= 60 & PB.final5$Half_life_days > 40, "P",
                                          "DL")))

PB.final5$B_class <- ifelse(PB.final5$BCF_Lkg <= 2000, "nB",
                            ifelse(PB.final5$BCF_Lkg > 5000, "vB",
                                   ifelse(PB.final5$BCF_Lkg > 2000 & PB.final5$BCF_Lkg <= 5000, "B",
                                          "DL")))


PB.final5$E_fate_class <- paste(PB.final5$P_class, PB.final5$B_class, sep = "")
list(unique(PB.final5$E_fate_class))
PB.final5$E_fate_class <- as.factor(PB.final5$E_fate_class)
summary(PB.final5$E_fate_class)
# NANA NAnB  nPB nPNA nPnB nPvB  PnB  PvB  vPB vPnB vPvB 
# 4    1    3    1  317    1   27    1   10   53   24


PB.final5$Bin <- ifelse(PB.final5$E_fate_class == "nPnB", "Bin 1",
                        ifelse(PB.final5$E_fate_class == "vPnB", "Bin 4", 
                               ifelse(PB.final5$E_fate_class == "nPvB", "Bin 4",
                                      ifelse(PB.final5$E_fate_class == "nPB", "Bin 2", 
                                             ifelse(PB.final5$E_fate_class == "NANA", "DL",
                                                    ifelse(PB.final5$E_fate_class == "PnB","Bin 2",
                                                           ifelse(PB.final5$E_fate_class == "NAnB", "Bin 1",
                                                                  ifelse(PB.final5$E_fate_class == "vPB", "Bin 5",
                                                                         ifelse(PB.final5$E_fate_class == "nPNA", "Bin 1",
                                                                                ifelse(PB.final5$E_fate_class == "vPvB", "Bin 6",
                                                                                       ifelse(PB.final5$E_fate_class == "PvB", "Bin 5", 
                                                                                              ifelse(PB.final5$E_fate_class == "PB", "Bin 3",
                                                                                                     "misc"))))))))))))
  
list(unique(PB.final5$Bin))

#plot P vs. B to demonstrate Bins

Efate_plot <- PB.final5 %>% filter(Bin != "DL") %>% ggscatter("Half_life_days", "BCF_Lkg", color = "Bin") + xlab("Aquatic Half Life (t1/2, d)") + ylab("Bioconcentration Factor (BCF, L/kg)") + 
  scale_y_log10() + scale_x_log10() + scale_color_viridis_d(direction = 1) + geom_vline(xintercept = 40, linetype = "dashed") + 
  geom_vline(xintercept = 60, linetype = "dashed") + geom_hline(yintercept = 2000, linetype = "dashed") + geom_hline(yintercept = 5000, linetype = "dashed") + theme(legend.title = element_blank())

ggsave("EFate_plot.jpeg", height = 8, width = 8)

write_xlsx(PB.final6, "efate_eval.xlsx")

#grab + passive - separate analysis

chemical_info <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\WoE Prioritization\\benchmark_comp_file_10_05_2022.xlsx", 2) %>%
  select("CAS", "Chemical Name", "Class", "Detected Water", "Detected Passive")

PB.final6 <- left_join(PB.final5, chemical_info)
grab <- PB.final6 %>% filter(`Detected Water` == "TRUE")
passive <- PB.final6 %>% filter(`Detected Passive` == "TRUE")

summary(grab$E_fate_class)
# NANA NAnB  nPB nPNA nPnB nPvB  PnB  PvB  vPB vPnB vPvB 
# 1    0    1    1  195    0   14    1    1   27    0 

summary(passive$E_fate_class)
# NANA NAnB  nPB nPNA nPnB nPvB  PnB  PvB  vPB vPnB vPvB 
# 1    0    2    0  122    0   13    1    7   21    7 