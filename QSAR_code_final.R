#QSAR benchmarks#

library(tidyr)
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(stringr)
library(xlsx)
library(rJava)
library(ggpubr)

setwd("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data")

#read in CAS, phys prop & names####

QSAR <- read_csv("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\Physical Properties\\Physical_Properties.csv")
names(QSAR)

QSAR_1 <- QSAR %>% rename("CAS" = "INPUT", "Chemical_Name" = "PREFERRED_NAME", "MW" = "AVERAGE_MASS") %>% select("CAS", "Chemical_Name", "MW", "logKOW")
names(QSAR_1)

QSAR_1$MW <- as.numeric(QSAR_1$MW)
QSAR_1$logKOW <- as.numeric(QSAR_1$logKOW)

#bind estimates to missing chemicals#
QSAR_1$algae.base <- (-1)*QSAR_1$logKOW - 1.23
QSAR_1$algae.base <- 10^(QSAR_1$algae.base)
QSAR_1$algae.base <- QSAR_1$algae.base * QSAR_1$MW
QSAR_1$algae.base <- QSAR_1$algae.base * 10^6
QSAR_1$daphnid.base <- (-.95)*QSAR_1$logKOW - 1.19
QSAR_1$daphnid.base <- 10^(QSAR_1$daphnid.base)
QSAR_1$daphnid.base <- QSAR_1$daphnid.base * QSAR_1$MW
QSAR_1$daphnid.base <- QSAR_1$daphnid.base * 10^6
QSAR_1$fish.base <- (-0.85)*QSAR_1$logKOW-1.14
QSAR_1$fish.base <- 10^QSAR_1$fish.base
QSAR_1$fish.base <- QSAR_1$fish.base * QSAR_1$MW
QSAR_1$fish.base <- QSAR_1$fish.base * 10^6

#ECOSAR estimates#
ecosar <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\QSAR_final_compilation\\ECOSAR_data.xlsx")

names(ecosar)
View(ecosar)

ecosar$ECOSAR_Fish <- ifelse(ecosar$ECOSAR_Fish_AD == "Inside", ecosar$ECOSAR_Fish, "NaN")
ecosar$ECOSAR_Fish <- as.numeric(ecosar$ECOSAR_Fish)
ecosar$ECOSAR_Fish <- ecosar$ECOSAR_Fish*10^3

ecosar$ECOSAR_Invert <- ifelse(ecosar$ECOSAR_Invert_AD == "Inside", ecosar$ECOSAR_Invert, "NaN")
ecosar$ECOSAR_Invert <- as.numeric(ecosar$ECOSAR_Invert)
ecosar$ECOSAR_Invert <- ecosar$ECOSAR_Invert * 10^3

ecosar$ECOSAR_Plant <- ifelse(ecosar$ECOSAR_Plant_AD == "Inside", ecosar$ECOSAR_Plant, "NaN")
ecosar$ECOSAR_Plant  <- as.numeric(ecosar$ECOSAR_Plant)
ecosar$ECOSAR_Plant <- ecosar$ECOSAR_Plant * 10^3

ecosar_1 <- ecosar %>% select("CAS", "ECOSAR_Fish", "ECOSAR_Invert", "ECOSAR_Plant")
View(ecosar_1)

QSAR_2 <- left_join(QSAR_1, ecosar_1)

View(QSAR_2)

#TEST####
TEST_Fish <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\TEST_02_04_2021\\Batch_Fathead_minnow_LC50_(96_hr)_Consensus.xlsx", 2) 
TEST_Invert <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\TEST_02_04_2021\\Batch_Daphnia_magna_LC50_(48_hr)_Consensus.xlsx", 2)
TEST_Plant <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\TEST_02_04_2021\\Batch_T._pyriformis_IGC50_(48_hr)_Consensus.xlsx", 2)

TEST_Fish$species <- "FISH"
names(TEST_Fish)
TEST_Fish1 <- TEST_Fish %>% rename("exp" = "Exp\r\n-Log10(mol/L)", "HC" = "Pred_Hierarchical clustering\r\n-Log10(mol/L)", "SM" = "Pred_Single model\r\n-Log10(mol/L)", "GC" = "Pred_Group contribution\r\n-Log10(mol/L)", "NN" = "Pred_Nearest neighbor\r\n-Log10(mol/L)", "CON" = "Pred_Consensus\r\n-Log10(mol/L)", "CAS" = "ID") 
names(TEST_Fish1)
TEST_Fish1$units <- "log10(mol/L)"

TEST_Invert$species <- "INVERT"
names(TEST_Invert)
TEST_Invert1 <- TEST_Invert %>% rename("exp" = "Exp\r\n-Log10(mol/L)", "HC" = "Pred_Hierarchical clustering\r\n-Log10(mol/L)", "SM" = "Pred_Single model\r\n-Log10(mol/L)", "GC" = "Pred_Group contribution\r\n-Log10(mol/L)", "NN" = "Pred_Nearest neighbor\r\n-Log10(mol/L)", "CON" = "Pred_Consensus\r\n-Log10(mol/L)", "CAS" = "ID")
TEST_Invert1$units <- "log10(mol/L)"

TEST_Plant$species <- "PLANT"
names(TEST_Plant)
TEST_Plant1 <- TEST_Plant %>% rename("exp" = "Exp\r\n-Log10(mol/L)", "HC" = "Pred_Hierarchical clustering\r\n-Log10(mol/L)","GC" = "Pred_Group contribution\r\n-Log10(mol/L)", "NN" = "Pred_Nearest neighbor\r\n-Log10(mol/L)", "CON" = "Pred_Consensus\r\n-Log10(mol/L)", "CAS" = "ID")
TEST_Plant1$units <- "log10(mol/L)"

TEST_all <- bind_rows(TEST_Fish1, TEST_Invert1, TEST_Plant1)

names(TEST_all)
TEST_all$CON <- as.numeric(TEST_all$CON)
TEST_all$HC <- as.numeric(TEST_all$HC)
TEST_all$SM <- as.numeric(TEST_all$SM)
TEST_all$NN <- as.numeric(TEST_all$NN)
TEST_all$GC <- as.numeric(TEST_all$GC)
TEST_all$exp <- as.numeric(TEST_all$exp)

TEST_all$Final_Pred <- ifelse(!is.na(TEST_all$exp), TEST_all$exp, 
                              ifelse(!is.na(TEST_all$CON) & is.na(TEST_all$exp),TEST_all$CON, 
                                     ifelse(is.na(TEST_all$CON) & is.na(TEST_all$exp) & is.na(TEST_all$SM) 
                                            & is.na(TEST_all$NN) & is.na(TEST_all$GC), TEST_all$HC,
                                            ifelse(is.na(TEST_all$CON) & is.na(TEST_all$exp) & is.na(TEST_all$HC) 
                                                   & is.na(TEST_all$NN) & is.na(TEST_all$GC), TEST_all$SM,
                                                   ifelse(is.na(TEST_all$CON) & is.na(TEST_all$exp) & is.na(TEST_all$HC) 
                                                          & is.na(TEST_all$SM) & is.na(TEST_all$GC), TEST_all$NN,
                                                          ifelse(is.na(TEST_all$CON) & is.na(TEST_all$exp) & is.na(TEST_all$HC) 
                                                                 & is.na(TEST_all$SM) & is.na(TEST_all$NN), TEST_all$GC, "No Estimate"))))))

View(TEST_all)

#convert units & split TEST pred 

MW <- QSAR_2 %>% select("CAS", "MW")
View(MW)

TEST_all_1 <- left_join(TEST_all, MW)
View(TEST_all_1)

TEST_all_1$Final_Pred <- as.numeric(TEST_all_1$Final_Pred)
TEST_all_1$Final_Pred <- ((10^(-TEST_all_1$Final_Pred))*TEST_all_1$MW)*(10^6)
TEST_all_1$units <- "ug/L"

TEST_fish_final <- TEST_all_1 %>% filter(species == "FISH") %>% select(CAS, Final_Pred) %>% rename("TEST_Fish" = "Final_Pred")
TEST_invert_final <- TEST_all_1 %>% filter(species == "INVERT") %>% select(CAS, Final_Pred) %>% rename("TEST_Invert" = "Final_Pred")
TEST_plant_final <- TEST_all_1 %>% filter(species == "PLANT") %>% select(CAS, Final_Pred) %>% rename("TEST_Plant" = "Final_Pred")

TEST_final <- left_join(TEST_fish_final, TEST_invert_final)
TEST_final_1 <- left_join(TEST_final, TEST_plant_final)
TEST_final_2 <- TEST_final_1 %>% unique()
View(TEST_final_2)

#join to dataset#

QSAR_3 <- left_join(QSAR_2, TEST_final_2)
View(QSAR_3)

#VEGA####

algae_IRFHM <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\VEGA\\ALGAE_EC50_IRFHM.xlsx")

fish_KNN <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\VEGA\\FHM_KNN.xlsx")
fish_KNN_readacross <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\VEGA\\FISH_KNN_READACROSS.xlsx")
fish_LC50 <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\VEGA\\FISH_LC50.xlsx")

invert_EC50 <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\QSAR Data\\VEGA\\INVERT_EC50.xlsx")

#bind in spp name
algae_IRFHM$spp <- "plant"
fish_KNN$spp <- "fish"
fish_KNN_readacross$spp <- "fish"
fish_LC50$spp <- "fish"
invert_EC50$spp <- "invert"

#look at column names & extract important info

#algae_IRFHM
names(algae_IRFHM)
View(algae_IRFHM)
algae_IRFHM_1 <- algae_IRFHM %>% rename("CAS" = "Id", "plant_IRFHM" = "Assessment", "ACF_index" = "ACF index") %>% select("CAS", "plant_IRFHM", "ADI")
algae_IRFHM_1$plant_IRFHM <- ifelse(algae_IRFHM_1$ADI <= 0.75, NaN, algae_IRFHM_1$plant_IRFHM)
algae_IRFHM_1$plant_IRFHM <- as.numeric(algae_IRFHM_1$plant_IRFHM)
algae_IRFHM_1$plant_IRFHM <- algae_IRFHM_1$plant_IRFHM * 10^3
algae_IRFHM_2 <- algae_IRFHM_1 %>% select(-"ADI")

QSAR_4 <- left_join(QSAR_3, algae_IRFHM_2)

#fish_KNN
names(fish_KNN)
View(fish_KNN)
fish_KNN_1 <- fish_KNN %>% rename("CAS" = "Id", "fish_KNN" = "Assessment", "ACF_index" = "ACF index") %>% select("CAS", "fish_KNN", "ADI")
fish_KNN_1$fish_KNN <- ifelse(fish_KNN_1$ADI <= 0.75, NaN, fish_KNN_1$fish_KNN)
fish_KNN_1$fish_KNN <- as.numeric(fish_KNN_1$fish_KNN)
fish_KNN_1$fish_KNN <- fish_KNN_1$fish_KNN * 10^3
fish_KNN_2 <- fish_KNN_1 %>% select(-"ADI")

QSAR_5 <- left_join(QSAR_4, fish_KNN_2)


#fish_KNN_readacross
names(fish_KNN_readacross)
View(fish_KNN_readacross)
fish_KNN_readacross_1 <- fish_KNN_readacross %>% rename("CAS" = "Id", "fish_KNN_readacross" = "Assessment", "ACF_index" = "ACF index") %>% select("CAS", "fish_KNN_readacross", "ADI")
fish_KNN_readacross_1$fish_KNN_readacross <- ifelse(fish_KNN_readacross_1$ADI <= 0.75, NaN, fish_KNN_readacross_1$fish_KNN_readacross)
fish_KNN_readacross_1$fish_KNN_readacross <- as.numeric(fish_KNN_readacross_1$fish_KNN_readacross)
fish_KNN_readacross_1$fish_KNN_readacross <- fish_KNN_readacross_1$fish_KNN_readacross * 10^3
fish_KNN_readacross_2 <- fish_KNN_readacross_1 %>% select(-"ADI")

QSAR_6 <- left_join(QSAR_5, fish_KNN_readacross_2)

#fish_LC50
names(fish_LC50)
fish_LC50_1 <- fish_LC50 %>% rename("CAS" = "Id", "fish_LC50" = "Assessment", "ACF_index" = "ACF index") %>% select("CAS", "fish_LC50", "ADI")
fish_LC50_1$fish_LC50 <- ifelse(fish_LC50_1$ADI <= 0.75, NaN, fish_LC50_1$fish_LC50)
fish_LC50_1$fish_LC50 <- as.numeric(fish_LC50_1$fish_LC50)
fish_LC50_1$fish_LC50 <- fish_LC50_1$fish_LC50 * 10^3
fish_LC50_2 <- fish_LC50_1 %>% select(-"ADI")
View(fish_LC50_2)

QSAR_7 <- left_join(QSAR_6, fish_LC50_2)

#invert_EC50
names(invert_EC50)
View(invert_EC50)
invert_EC50_1 <- invert_EC50 %>% rename("CAS" = "Id", "invert_EC50" = "Assessment", "ACF_index" = "ACF index") %>% select("CAS", "invert_EC50", "ADI")
invert_EC50_1$invert_EC50 <- ifelse(invert_EC50_1$ADI <= 0.75, NaN, invert_EC50_1$invert_EC50)
invert_EC50_1$invert_EC50 <- as.numeric(invert_EC50_1$invert_EC50)
invert_EC50_1$invert_EC50 <- invert_EC50_1$invert_EC50 * 10^3
invert_EC50_2 <- invert_EC50_1 %>% select(-"ADI")

QSAR_8 <- left_join(QSAR_7, invert_EC50_2)

#view final QSAR_version
View(QSAR_8)

QSAR_8$CAS <- gsub("01/01/4640", "4640-01-1", QSAR_8$CAS)
QSAR_8$CAS <- gsub("03/11/2599", "2599-11-3", QSAR_8$CAS)
QSAR_8$CAS <- gsub("05/06/2497", "2497-06-5", QSAR_8$CAS)
QSAR_8$CAS <- gsub("05/07/6804", "6804-07-5", QSAR_8$CAS)
QSAR_8$CAS <- gsub("06/03/2588", "2588-03-6", QSAR_8$CAS)
QSAR_8$CAS <- gsub("07/04/2588", "2588-04-7", QSAR_8$CAS)
QSAR_8$CAS <- gsub("08/05/2588", "2588-05-8", QSAR_8$CAS)
QSAR_8$CAS <- gsub("09/06/2588", "2588-06-9", QSAR_8$CAS)
QSAR_8$CAS <- gsub("06/05/6493", "6493-05-6", QSAR_8$CAS)
QSAR_8$CAS <- gsub("06/07/2497", "2497-07-6", QSAR_8$CAS)
QSAR_8$CAS <- gsub("08/02/2327", "2327-02-8", QSAR_8$CAS)
QSAR_8$CAS <- gsub("09/08/4497", "4497-08-9", QSAR_8$CAS)


#pull together benchmarks
names(QSAR_8)

QSAR_13 <- QSAR_8 %>% gather("algae.base":"invert_EC50", key = "QSAR_Type", value = "EC")
View(QSAR_13)

QSAR_13$QSAR_Type <- as.factor(QSAR_13$QSAR_Type)
summary(QSAR_13$QSAR_Type)

QSAR_13$spp <- ifelse(QSAR_13$QSAR_Type %in% c("algae.base", "ECOSAR_Plant", "TEST_Plant", "plant_IRFHM"), "plant",
                      ifelse(QSAR_13$QSAR_Type %in% c("daphnid.base", "ECOSAR_Invert", "invert_EC50", "TEST_Invert"), "invert",
                             ifelse(QSAR_13$QSAR_Type %in% c("ECOSAR_Fish", "fish.base", "fish_KNN", "fish_KNN_readacross", "fish_LC50", "TEST_Fish"), "fish", 
                                    "misc")))

list(unique(QSAR_13$spp))

#get mean estimates for QSAR# 

QSAR_14 <- QSAR_13 %>% group_by(CAS, Chemical_Name, spp) %>% summarize(mean_EC = exp(mean(log(EC), na.rm = TRUE)))
QSAR_14$CAS <- gsub("01/01/4640", "4640-01-1", QSAR_14$CAS)
QSAR_14$CAS <- gsub("03/11/2599", "2599-11-3", QSAR_14$CAS)
QSAR_14$CAS <- gsub("05/06/2497", "2497-06-5", QSAR_14$CAS)
QSAR_14$CAS <- gsub("05/07/6804", "6804-07-5", QSAR_14$CAS)
QSAR_14$CAS <- gsub("06/03/2588", "2588-03-6", QSAR_14$CAS)
QSAR_14$CAS <- gsub("07/04/2588", "2588-04-7", QSAR_14$CAS)
QSAR_14$CAS <- gsub("08/05/2588", "2588-05-8", QSAR_14$CAS)
QSAR_14$CAS <- gsub("09/06/2588", "2588-06-9", QSAR_14$CAS)
QSAR_14$CAS <- gsub("06/05/6493", "6493-05-6", QSAR_14$CAS)
QSAR_14$CAS <- gsub("06/07/2497", "2497-07-6", QSAR_14$CAS)
QSAR_14$CAS <- gsub("08/02/2327", "2327-02-8", QSAR_14$CAS)
QSAR_14$CAS <- gsub("09/08/4497", "4497-08-9", QSAR_14$CAS)

#find min QSAR value
View(QSAR_14)
QSAR_15 <- QSAR_14 %>% group_by(CAS, Chemical_Name) %>% summarize(min_EC = min(mean_EC, na.rm = TRUE))
View(QSAR_15)

QSAR_15$min_EC <- gsub("Inf", "No Benchmark", QSAR_15$min_EC)

#Annotate
QSAR_16 <- QSAR_15
View(QSAR_16)

QSAR_16$CAS <- gsub("01/01/4640", "4640-01-1", QSAR_16$CAS)
QSAR_16$CAS <- gsub("03/11/2599", "2599-11-3", QSAR_16$CAS)
QSAR_16$CAS <- gsub("05/06/2497", "2497-06-5", QSAR_16$CAS)
QSAR_16$CAS <- gsub("05/07/6804", "6804-07-5", QSAR_16$CAS)
QSAR_16$CAS <- gsub("06/03/2588", "2588-03-6", QSAR_16$CAS)
QSAR_16$CAS <- gsub("07/04/2588", "2588-04-7", QSAR_16$CAS)
QSAR_16$CAS <- gsub("08/05/2588", "2588-05-8", QSAR_16$CAS)
QSAR_16$CAS <- gsub("09/06/2588", "2588-06-9", QSAR_16$CAS)
QSAR_16$CAS <- gsub("06/05/6493", "6493-05-6", QSAR_16$CAS)
QSAR_16$CAS <- gsub("06/07/2497", "2497-07-6", QSAR_16$CAS)
QSAR_16$CAS <- gsub("08/02/2327", "2327-02-8", QSAR_16$CAS)
QSAR_16$CAS <- gsub("09/08/4497", "4497-08-9", QSAR_16$CAS)

QSAR_16_pe <- QSAR_16 %>% filter(Chemical_Name %in% c("Psuedoephedrine", "Ephedrine"))
QSAR_16_pe$CAS <- "90-82-4 + 299-42-3"
QSAR_16_pe$Chemical_Name <- "Pseudoephedrine + Ephedrine"

QSAR_16_oe <- QSAR_16 %>% filter(Chemical_Name %in% c("Omeprazole", "Esomeprazole"))
QSAR_16_oe$CAS <- "73590-58-6 + 161796-78-7"
QSAR_16_oe$Chemical_Name <- "Omeprazole + Esomeprazole"

QSAR_16_noep <- QSAR_16 %>% filter(!Chemical_Name %in% c("Psuedoephedrine", "Ephedrine", "Omeprazole", "Esomeprazole"))

QSAR_17 <- bind_rows(QSAR_16_pe, QSAR_16_oe, QSAR_16_noep) %>% distinct()

#extract chemicals without a benchmark value #
benchmark_list <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\WoE Prioritization\\Benchmark_Comp_File_15_03_2022.xlsx", 2)
CAS_list_h2o <- benchmark_list %>% select(CAS, `Chemical Name`)

noBM.chem <- anti_join(CAS_list_h2o, QSAR_17, by = "CAS") %>% filter(CAS != "73590-58-6")
View(noBM.chem)
names(noBM.chem)
noBM.chem_1 <- noBM.chem %>% select("CAS", "Chemical Name") %>% rename("Chemical_Name" = "Chemical Name")
noBM.chem_1$Data_Type <- "No Benchmark"

#QSAR BM 

QSAR_final <- bind_rows(QSAR_17, noBM.chem_1)
View(QSAR_final_1)

QSAR_final_1 <- QSAR_final %>% rename("Note" = "Data_Type")

write_xlsx(QSAR_final_1, "QSAR_final_BM.xlsx")

#bind in a version with all the estimates & the min
QSAR_consensus <- QSAR_14 %>% spread("mean_EC", key = "spp", value = "mean_EC")
View(QSAR_consensus)

QSAR_for_SI <- left_join((QSAR_8), (QSAR_consensus)) %>% distinct()
QSAR_for_SI_1 <- left_join((QSAR_for_SI), (QSAR_17)) %>% distinct()
QSAR_for_SI_1$Note <- ifelse(QSAR_for_SI_1$min_EC == QSAR_for_SI_1$fish, "Minimum value based on consensus of fish estimates.",
                             ifelse(QSAR_for_SI_1$min_EC == QSAR_for_SI_1$invert, "Minimum value based on consensus of invertebrate estimates.",
                             ifelse(QSAR_for_SI_1$min_EC == QSAR_for_SI_1$plant, "Minimum value based on consensus of plant estimates.", 
                             "misc")))


write_xlsx(QSAR_for_SI_1, "QSAR_for_SI.xlsx")
names(QSAR_for_SI_1)
