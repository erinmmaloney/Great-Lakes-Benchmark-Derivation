#Water Quality Benchmarks - CEC_Group1
library(tidyr)
library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
library(fuzzyjoin)

#usepa_opp 10_01_2022####
CAS_chnm_all <- read_excel("WQG_repository.xlsx", "USEPA_OPP") 
usepa_opp <- read_csv("Aquatic Life Benchmarks and Ecological Risk Assessments for Registered Pesticides  US EPALockPrimary navigation.csv")
names(usepa_opp)
names(CAS_chnm_all)

CAS_chnm <- CAS_chnm_all %>% select(CAS, `Chemical Name`)
OPP_by_CAS <- left_join(CAS_chnm, (usepa_opp %>% select(-Pesticide, -"CAS number", -QAQC_CAS, -QAQC_name))) 
OPP_by_chnm <- left_join(CAS_chnm, (usepa_opp %>% select(-CAS, -"CAS number", -QAQC_CAS, -QAQC_name) %>% rename("Chemical Name" = "Pesticide")))

OPP_for_QAQC <- bind_rows(OPP_by_CAS, OPP_by_chnm) %>% distinct() %>% rename("Fish_acute" = "Acute1", "Fish_Chronic" = "Chronic2",
                                                                             "Inverts_Acute" = "Acute3", "Inverts_Chronic" = "Chronic4",
                                                                             "Nonvascular_Plants_Acute" = "Acute5", "Vascular_Plants_Acute" = "Acute6",
                                                                             "Maximum_Concentration_CMC" = "Maximum \t\t\tConcentration \t\t\t(CMC)", 
                                                                             "Continuous_Concentration_CCC" = "Continuous \t\t\tConcentration \t\t\t(CCC)")
write_xlsx(OPP_for_QAQC, "WQG\\OPP_for_QAQC.xlsx")



#double check again
OPP_for_QAQC_EM_annotated <- read_excel("OPP_for_QAQC_EM.xlsx") %>% filter(!QAQC %in% c("Y", "y", "Yes", "yes")) %>% select(`Chemical Name`, CAS)
replacements <- left_join(OPP_for_QAQC_EM_annotated, (usepa_opp %>% rename("Chemical Name" = "Pesticide") ))
replacements3 <- stringdist_inner_join(OPP_for_QAQC_EM_annotated, (usepa_opp %>% rename("Chemical Name" = "Pesticide")%>% select(-CAS)), max_dist = 6) %>% filter("CAS" == "CAS number")

View(replacements3)
names(OPP_for_QAQC_EM_annotated)

#looks good - write it out#
OPP_final <- write_xlsx(OPP_for_QAQC, "WQG\\OPP_final.xlsx")

#ccme data ####
CAS_chnm_all <- read_excel("WQG_repository.xlsx", "CCME")%>% select(CAS, `Chemical Name`)
ccme <- read_csv("CCME_flatfile.csv") %>% rename("CAS" = "CASRN", "AquaticLife_Freshwater_ShortTerm" = "(Freshwater) Concentration (µg/L) Short Term", "AquaticLife_Freshwater_LongTerm" = "(Freshwater) Concentration (µg/L) Long Term",
                                                      "Date" = "(Freshwater) Date") %>% select("Chemical Name", "CAS", AquaticLife_Freshwater_ShortTerm, AquaticLife_Freshwater_LongTerm, Date)
names(ccme)

ccme_CAS <- left_join(CAS_chnm_all, (ccme %>% select(-"Chemical Name")))
ccme_chnm <- left_join(CAS_chnm_all, (ccme %>% select(-CAS)))

ccme_final <- bind_rows(ccme_CAS, ccme_chnm) %>% distinct()
ccme_final$AquaticLife_Freshwater_ShortTerm <- gsub("No data", "", ccme_final$AquaticLife_Freshwater_ShortTerm)
ccme_final$AquaticLife_Freshwater_ShortTerm <- gsub("NRG", "", ccme_final$AquaticLife_Freshwater_ShortTerm)
list(unique(ccme_final$AquaticLife_Freshwater_LongTerm))
ccme_final$AquaticLife_Freshwater_LongTerm <- gsub("Insufficient data", "", ccme_final$AquaticLife_Freshwater_LongTerm)
ccme_final$AquaticLife_Freshwater_LongTerm <- gsub("Narrative", "", ccme_final$AquaticLife_Freshwater_LongTerm)
ccme_final$AquaticLife_Freshwater_LongTerm <- gsub("Variable", "", ccme_final$AquaticLife_Freshwater_LongTerm)
ccme_final$AquaticLife_Freshwater_LongTerm <- gsub("Table", "", ccme_final$AquaticLife_Freshwater_LongTerm)
ccme_final$AquaticLife_Freshwater_LongTerm <- gsub("Guidance Framework", "", ccme_final$AquaticLife_Freshwater_LongTerm)
ccme_final$AquaticLife_Freshwater_LongTerm <- gsub("No data", "", ccme_final$AquaticLife_Freshwater_LongTerm)
ccme_final$AquaticLife_Freshwater_LongTerm <- gsub("6.5 to 9.0", "6.5", ccme_final$AquaticLife_Freshwater_LongTerm)

View(ccme_final)
write_xlsx(ccme_final, "WQG\\CCME_Final.xlsx")

#collate all WQG####
usepa_opp <- read_excel("WQG_repository.xlsx", "USEPA_OPP")
ccme <- read_excel("WQG_repository.xlsx", "CCME")
cad_data <- read_excel("WQG_repository.xlsx", "CAD_data")
feqgcs <- read_excel("WQG_repository.xlsx", "FEQGCs")
EU_EQS <- read_excel("WQG_repository.xlsx", "EU_EQS")
usepa_reg4 <- read_excel("WQG_repository.xlsx", "USEPA_Reg4")
noaa_sqrt <- read_excel("WQG_repository.xlsx", "NOAA_SQRT")
usepa_ecotox <- read_excel("WQG_repository.xlsx", "USEPA_ECOTOX")
usepa_esb <- read_excel("WQG_repository.xlsx", "USEPA_ESB")
usepa_nrwqc <- read_excel("WQG_repository.xlsx", "USEPA_NRWQC")
cad_prov <- read_excel("WQG_repository.xlsx", "CAD_Prov")
usepa_cwa <- read_excel("WQG_repository.xlsx", "USEPA_CWA2")
norman <- read_excel("WQG_repository.xlsx", "NORMAN")
echa <- read_excel("WQG_repository.xlsx", "ECHA")


#OPP
names(usepa_opp)
usepa_opp_1 <- usepa_opp %>% gather("Fish_acute":"Continuous_Concentration_CCC", key = "Benchmark_description", value = "Benchmark_ugL") %>% select(CAS, `Chemical Name`, Benchmark_description, Benchmark_ugL)
usepa_opp_1$Benchmark_Source <- "US EPA Office of Pesticide Programs (OPP) Aquatic Life Benchmark"
usepa_opp_1$Reference <- "US EPA, 2021a"
usepa_opp_1$Benchmark_ugL <- as.numeric(usepa_opp_1$Benchmark_ugL)
usepa_opp_1$Benchmark_Type <- "Screening_Apical"

#ccme
names(ccme)
ccme_1 <- ccme %>% gather("AquaticLife_Freshwater_ShortTerm":"AquaticLife_Freshwater_LongTerm", key ="Benchmark_description", value = "Benchmark_ugL") %>% select(-Access_Date)
ccme_1$Benchmark_Source <- "Canadian Council of Ministers of the Environment Water Quality Guidelines for the Protection of Aquatic Life"
ccme_1$Reference <- "CCME, 2021"
ccme_1$Benchmark_ugL <- as.numeric(ccme_1$Benchmark_ugL)

#cad_data
names(cad_data)
cad_data_1 <- cad_data %>% rename(Benchmark_ugL = guideline_ppb) %>% unite("Benchmark_description", "Media":"Operational Protection Goal", sep = "; ") %>% select(-c("Main Jurisdiction", "guideline_units"), -`Accessed Date`)
cad_data_1$Benchmark_Source <- "Federal Contaminated Sites Action Plan (FCSAP) Database of Guidelines (2012)"
cad_data_1$Benchmark_ugL <- as.numeric(cad_data_1$Benchmark_ugL)
cad_data_2 <- cad_data_1 %>% separate_rows(Reference, sep = ";", convert = FALSE)
cad_data_2$Reference <- gsub(" EPA, 2005", "EPA, 2005", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" AE, 1999", "AE, 1999", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" BC MOE, 2010", "BC MOE, 2010", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" Saskatchewan MOE, 2006", "Saskatchewan MOE, 2006", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" WAC, n.d.a", "WAC, n.d.a", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" NDDH, 2001", "NDDH, 2001", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" ADEC, 2008", "ADEC, 2008", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" EPA, 2001", "EPA, 2001", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" MDEQ, 2010", "MDEQ, 2010", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" MPCA, 2012", "MPCA, 2012", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" CalEPA, 2011", "CalEPA, 2011", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" NYSDEC, 2000", "NYSDEC, 2000", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" NYSDEC, 1993", "NYSDEC, 1993", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" EPA Region 3", "EPA Region 3", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" MEDEP, 2012", "MEDEP, 2012", cad_data_2$Reference)
cad_data_2$Reference <- gsub("Alberta Environment (AE), 1999", "AE, 1999", cad_data_2$Reference)
cad_data_2$Reference <- gsub("Alberta Environment (AE), 1999", "AE, 1999", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" AE 1999", "AE, 1999", cad_data_2$Reference)
cad_data_2$Reference <- gsub(" IDEQ, n.d.b", "IDEQ, n.d.b", cad_data_2$Reference)

cad_data_2$Benchmark_ugL <- as.numeric(cad_data_2$Benchmark_ugL)

#feqgcs
names(feqgcs)
feqgcs_1 <- feqgcs %>% rename("Benchmark_ugL" = "Federal_EQG_Water") %>% select(-units, -`Evaluation Date`)
feqgcs_1$Benchmark_description <- "Canadian_Federal_EQG_water"
feqgcs_1$Benchmark_Source <- "Health Canada Federal Environmental Quality Guidelines (FEQGs)"
feqgcs_1$Reference <- "Health Canada, 2021"
feqgcs_1$Benchmark_ugL <- as.numeric(feqgcs_1$Benchmark_ugL)

#EU_EQS
names(EU_EQS)
EU_EQS_1 <- EU_EQS %>% gather("AA-EQS_inland":"MAC_EQS_other", key = "Benchmark_description", value = "Benchmark_ugL") %>% select(-Note, -Access_Date)
EU_EQS_1$Reference <- "EU, 2008"
EU_EQS_1$Benchmark_ugL <- as.numeric(EU_EQS_1$Benchmark_ugL)
EU_EQS_1$Benchmark_Source <- "European Union Environmental Quality Standards for Priority Substances and Certain Other Pollutants "

#usepa_reg4
names(usepa_reg4)
usepa_reg4_1 <- usepa_reg4 %>% select(c(CAS, "Chemical Name", "1a Freshwater_Screen_Acute",  "1a Freshwater_Screen_Chronic",
                                        "1d Freshwater_Screen_Acute", "1d Freshwater_Screen_Chronic",
                                        "1e Freshwater_Screen_Acute", "1e Freshwater_Screen_Chronic")) %>% 
  gather("1a Freshwater_Screen_Acute":"1e Freshwater_Screen_Chronic", key = "Benchmark_description", value = "Benchmark_ugL") %>%
  filter(!is.na(Benchmark_ugL)) 

usepa_reg4_2 <- usepa_reg4 %>% select(c(CAS, "Chemical Name", "1a Acute_Benchmark_Type", 
                                        "1d Acute_Benchmark_Type","1aChronic_Benchmark_Type", 
                                        "1d Chronic_Benchmark_Type", "1e Acute_Benchmark_Type",
                                        "1e Chronic_Benchmark_Type")) %>% 
  gather("1a Acute_Benchmark_Type":"1e Chronic_Benchmark_Type", key = "Benchmark_description", value = "Benchmark_Type") %>%
  filter(Benchmark_Type %in% c("Screening_Apical", "Screening_Estimate"))
names(usepa_reg4_2)
list(unique(usepa_reg4_2$Benchmark_description))
list(unique(usepa_reg4_1$Benchmark_description))

usepa_reg4_2$Benchmark_description <- gsub("1a Acute_Benchmark_Type", "1a Freshwater_Screen_Acute", usepa_reg4_2$Benchmark_description)
usepa_reg4_2$Benchmark_description <- gsub("1d Acute_Benchmark_Type", "1d Freshwater_Screen_Acute", usepa_reg4_2$Benchmark_description)
usepa_reg4_2$Benchmark_description <- gsub("1aChronic_Benchmark_Type", "1a Freshwater_Screen_Chronic", usepa_reg4_2$Benchmark_description)
usepa_reg4_2$Benchmark_description <- gsub("1d Chronic_Benchmark_Type", "1d Freshwater_Screen_Chronic", usepa_reg4_2$Benchmark_description)
usepa_reg4_2$Benchmark_description <- gsub("1e Acute_Benchmark_Type", "1e Freshwater_Screen_Acute", usepa_reg4_2$Benchmark_description)
usepa_reg4_2$Benchmark_description <- gsub("1e Chronic_Benchmark_Type", "1e Freshwater_Screen_Chronic", usepa_reg4_2$Benchmark_description)

usepa_reg4_3 <- left_join(usepa_reg4_1, usepa_reg4_2)

usepa_reg4_3$Reference <- "US EPA, 2018"
usepa_reg4_3$Benchmark_ugL <- as.numeric(usepa_reg4_3$Benchmark_ugL)
usepa_reg4_3$Benchmark_Source <- "US EPA Region 4 Ecological Risk Assesssment Supplemental Guidance"

#noaa_sqrt
names(noaa_sqrt)
noaa_sqrt_1 <- noaa_sqrt %>% select(-c(Acute_ref, Chronic_ref, unit)) %>% gather("Surface_H2O_Fresh_acute":"Surface_H2O_Fresh_chronic", key = "Benchmark_description", value = "Benchmark_ugL") %>% select(-"Access Date")
noaa_sqrt_1$Reference <- "NOAA, 2008"
noaa_sqrt_1$Benchmark_ugL <- as.numeric(noaa_sqrt_1$Benchmark_ugL)
noaa_sqrt_1$Benchmark_Source <- "NOAA Screening Quick Reference Tables (SQuiRTs)"

#usepa_ecotox
names(usepa_ecotox)
usepa_ecotox_1 <- usepa_ecotox %>% select(-c(AWQFC_ref, TierII_ref, unit)) %>% gather("Surface_H2O_Freshwater_AWQCFCV":"Surface_H2O_Freshwater_TierII", key = "Benchmark_description", value = "Benchmark_ugL") %>% select(-"Accessed Date")
usepa_ecotox_1$Reference <- "US EPA, 1996"
usepa_ecotox_1$Benchmark_ugL <- as.numeric(usepa_ecotox_1$Benchmark_ugL)
usepa_ecotox_1$Benchmark_Source <- "US EPA Ecotox Thresholds"

#usepa_esb
names(usepa_esb)
usepa_esb_1 <- usepa_esb %>% rename("Benchmark_ugL" = "PAH_FCV") %>% select(-units, -"Accessed Date")
usepa_esb_1$Benchmark_description <- "PAH_FCV"
usepa_esb_1$Reference <- "US EPA, 2003"
usepa_esb_1$Benchmark_ugL <- as.numeric(usepa_esb_1$Benchmark_ugL)
usepa_esb_1$Benchmark_Source <- "US EPA Equilibrium Partitioning Sediment Benchmarks (ESBs) for the Protection of Benthic Organisms: PAH Mixtures"

#usepa_nrwqc
names(usepa_nrwqc)
usepa_nrwqc_1 <- usepa_nrwqc %>% gather("Freshwater_CMC_acute":"Freswater_CCC_chronic", key = "Benchmark_description", value = "Benchmark_ugL") %>% select(-Accessed_Date)
usepa_nrwqc_1$Reference <- "US EPA, 2021b"
usepa_nrwqc_1$Benchmark_ugL <- as.numeric(usepa_nrwqc_1$Benchmark_ugL)
usepa_nrwqc_1$Benchmark_Source <- "US EPA National Recommended Water Quality Criteria - Aquatic Life Criteria"

#cad_prov
names(cad_prov)
cad_prov_1 <- cad_prov %>% separate_rows(Province, sep = ";", convert = FALSE) %>% rename("Benchmark_ugL" = "Criteria Value", "Benchmark_description" = "Length") %>% select("CAS", "Chemical Name", "Benchmark_ugL", "Benchmark_description", "Province")

list(unique(cad_prov_1$Province))

cad_prov_1$Province <- gsub(" Ontario", "Ontario", cad_prov_1$Province)
cad_prov_1$Province <- gsub(" Manitoba", "Manitoba", cad_prov_1$Province)
cad_prov_1$Province <- gsub(" Saskatchewan", "Saskatchewan", cad_prov_1$Province)
cad_prov_1$Province <- gsub(" Quebec", "Quebec", cad_prov_1$Province)
cad_prov_1$Province <- gsub(" Alberta", "Alberta", cad_prov_1$Province)

cad_prov_1$Benchmark_description <- gsub("Acute", "Acute Benchmark", cad_prov_1$Benchmark_description)
cad_prov_1$Benchmark_description <- gsub("Chronic", "Chronic Benchmark", cad_prov_1$Benchmark_description)

cad_prov_1$Reference <- ifelse(cad_prov_1$Province == "Quebec", "MDEQ, 2021",
                               ifelse(cad_prov_1$Province == "Ontario", "Ontario Ministry of the Environment, 2019",
                                      ifelse(cad_prov_1$Province == "British Columbia", "B.C. Ministry of Environment and Climate Change, 2021",
                                             ifelse(cad_prov_1$Province == "Alberta", "Government of Alberta, 2018",
                                                    ifelse(cad_prov_1$Province == "Manitoba", "Manitoba Water Stewardship, 2011",
                                                           ifelse(cad_prov_1$Province == "Saskatchewan", "Water Security Agency, 2015", NA))))))

cad_prov_1$Benchmark_Source <- "Canadian Provincial Water Quality Guidelines"
cad_prov_1$Benchmark_ugL <- as.numeric(cad_prov_1$Benchmark_ugL)

cad_prov_2 <- cad_prov_1 %>% select(-Province)

#usepa_cwa
names(usepa_cwa)
usepa_cwa_1 <- usepa_cwa %>% rename("Benchmark_ugL" = "Criteria Value") %>% unite(Benchmark_description, "Length":"Criteria_Descriptor", sep = "; ") %>% select(-Units, -Ref) %>% separate_rows("State", sep = "; ", convert = FALSE)

usepa_cwa_1$Benchmark_ugL <- as.numeric(usepa_cwa_1$Benchmark_ugL)
usepa_cwa_1$Benchmark_Source <- "State-Specific Water Quality Standards Effective under the Clean Water Act (CWA)"
list(unique(usepa_cwa_1$State))

usepa_cwa_2 <- usepa_cwa_1
usepa_cwa_2$Reference <- usepa_cwa_2$State
usepa_cwa_2$Reference <- gsub("RI", "Rhode Island Department of Environmental Management, 2021", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("CT", "Conneticut Department of Energy and Environmental Protection, 2013", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("ME", "Maine Department of Environmental Protection, 2012", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("MA", "Massachusetts Division of Water Pollution Control, 2010", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("NH", "State of New Hampshire, 2015", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("VT", "State of Vermont Agency of Natural Resources, 2017", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("VA", "State of Virginia, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("MD", "Maryland Department of the Environment, 2018", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("DC", "District of Columbia Department of Energy and Environment, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("PA", "Commonwealth of Pennsylvania, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("TN", "Tennessee Department of Environment and Conservation, 2019", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("MS", "Mississippi Commission on Environmental Quality, 2016", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("AL", "Alabama Department of Environmental Management, 2017", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("KY", "State of Kentucky, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("SC", "South Carolina Department of Health and Environmental Control, 2014", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("NM", "State of New Mexico Water Quality Control Commission, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("ND", "State of North Dakota, 2019", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("NE", "Nebraska Department of Water Quality, 2019", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("UT", "State of Utah, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("TX", "Texas Commission on Environmental Quality, 2016", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("CO", "Colorado Department of Public Health and Environment, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("WY", "State of Wyoming, 2018", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("AK", "Alaska Department of Environmental Conservation, 2008", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("ID","Idaho Department of Environmental Quality, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("304A","US EPA National Recommended Water Quality Criteria (304a), 2021", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("KS", "Kansas Department of Health and Environment, 2017", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("MO", "Missouri Department of Natural Resources, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("MT", "Montana Department of Environmental Quality, 2018", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("NV", "State of Nevada, 2017", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("NTR", "National Toxics Rule, 2016", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("CTR", "California Toxics Rule, 2016", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("SKT", "Confederated Salish and Kootenai Tribes of the Flathead Reservation, 2019", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("NAM", "Pueblo of Nambe, 2018", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("STCLA", "Pueblo of Santa Clara, 2021", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("CA6", "State of California Region 6, 2016", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("AZ", "Arizona Department of Environmental Quality, 2016", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("HI", "Hawaii Department of Health, 2014",usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("NY", "New York State Division of Water, 2004", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("NJ", "State of New Jersey, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("DE", "Delaware River Basin Commission, 2013", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("NC", "North Carolina Department of Environmental Quality, 2019", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("OR", "Oregon Department of Environmental Quality, 2021", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("OK", "Oklaholma Water Resources Board, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("OHKA", "Ohkay Owingeh Tribal Council, 2017", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("IN", "State of Indiana, 2021", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("IA", "State of Iowa, 2017", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("SD", "State of South Dakota, 2018", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("AR", "State of Arkansas, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("LA", "State of Louisiana, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("WA", "State of Washington, 2019", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("CHE", "Confederated Tribes of the Chehalis Reservation, 2021", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("WV", "State of West Virginia, 2018", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("MN", "Minnesota Pollution Control Agency, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("OH",  "State of Ohio, 2018", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("WI", "Wisconsin Department of Natural Resources, 2010", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("IL", "State of Illinois, 2020", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("MI", "Michigan Department of Environmental Quality, 2006", usepa_cwa_2$Reference)
usepa_cwa_2$Reference <- gsub("ISL", "Pueblo of Isleta Tribal Council, 2002", usepa_cwa_2$Reference)
                                                                                                                                                                                                                                                                                                                                                                                      
usepa_cwa_3 <- usepa_cwa_2 %>% select(-State)
usepa_cwa_3$Benchmark_ugL <- as.numeric(usepa_cwa_3$Benchmark_ugL)

#norman
names(norman)
norman_1 <- norman %>% select(CAS, `Chemical Name`, `Lowest_PNEC (ug/L)`, Benchmark_Type) %>% rename("Benchmark_ugL" = "Lowest_PNEC (ug/L)")
norman_1$Benchmark_description <- "Lowest_PNEC (ug/L)"
norman_1$Reference <- "NORMAN, 2021"
norman_1$Benchmark_Source <- "NORMAN Substance Database (NORMAN SusDat)"
norman_1$Benchmark_ugL <- as.numeric(norman_1$Benchmark_ugL)

#echa
names(echa)
echa_1 <- echa %>% select(CAS, `Chemical Name`, Std_value, Threshold_Type, Benchmark_Type) %>% rename("Benchmark_ugL" = "Std_value", "Benchmark_description" = "Threshold_Type")
echa_1$Reference <- "ECHA, 2021"
echa_1$Benchmark_Source <- "ECHA Registration Dossier"
echa_1$Benchmark_ugL <- as.numeric(echa_1$Benchmark_ugL)


#join all & remove blanks

wqb_collated <- bind_rows(usepa_opp_1, ccme_1, cad_data_2, feqgcs_1, EU_EQS_1, usepa_reg4_3, noaa_sqrt_1, usepa_ecotox_1, usepa_esb_1, usepa_nrwqc_1, cad_prov_2, usepa_cwa_3,
                          norman_1, echa_1) %>% filter(!is.na(Benchmark_ugL)) %>% select(-"units")


View(wqb_collated)

list(unique(wqb_collated$Benchmark_Source))

wqb_collated$Reference <- gsub("NYSDEC, 1993", "New York State Department of Environmental Conservation, 1993", wqb_collated$Reference)
wqb_collated$Reference <- gsub("EPA, 2005", "US EPA, 2005", wqb_collated$Reference)
wqb_collated$Reference <- gsub("ANZECC, 2000", "Australia & New Zealand Environment Conservation Council, 2000", wqb_collated$Reference)
wqb_collated$Reference <- gsub("BC Regs 375/96, 1996", "British Columbia Contaminated Sites Regulation, 2010", wqb_collated$Reference)
wqb_collated$Reference <- gsub("MDEQ, 2008", "Michigan Department of Environmental Quality, 2008", wqb_collated$Reference)
wqb_collated$Reference <- gsub("MOE, 1994", "Ontario Ministry of the Environment, 1994", wqb_collated$Reference)
wqb_collated$Reference <- gsub("SAVEX, 2000", "Quebec MENV- Service des avis et des expertises, 2000", wqb_collated$Reference)
wqb_collated$Reference <- gsub("EPA, 1995", "US EPA, 1995", wqb_collated$Reference)
wqb_collated$Reference <- gsub("EPA, 1999", "US EPA, 1999", wqb_collated$Reference)
wqb_collated$Reference <- gsub("BC MOE, 2010", "British Columbia Ministry of the Environment, 2010", wqb_collated$Reference)
wqb_collated$Reference <- gsub("MPCA, 2012", "Minnesota Pollution Control Agency, 2012", wqb_collated$Reference)
wqb_collated$Reference <- gsub("AE, 1999", "Alberta Environment (AE), 1999", wqb_collated$Reference)
wqb_collated$Reference <- gsub("ADEC, 2008", "Alaska Department of Environmental Conservation, 2008", wqb_collated$Reference)
wqb_collated$Reference <- gsub("CalEPA, 2011", "California Environmental Protection Agency, 2011", wqb_collated$Reference)
wqb_collated$Reference <- gsub("Saskatchewan MOE, 2006", "Saskatchewan Ministry of the Environment, 2006", wqb_collated$Reference)
wqb_collated$Reference <- gsub("IDEQ, n.d.b", "Idaho Department of Environmental Quality, n.d.b", wqb_collated$Reference)
wqb_collated$Reference <- gsub("MEDEP, 2012", "Maine Department of Environmental Protection, 2012", wqb_collated$Reference)
wqb_collated$Reference <- gsub("WAC, n.d.a", "State of Washington, n.d.a", wqb_collated$Reference)
wqb_collated$Reference <- gsub("EPA, 2009", "US EPA, 2009", wqb_collated$Reference)
wqb_collated$Reference <- gsub("NYSDEC, 2000", "New York State Department of Environmental Conservation, 2000", wqb_collated$Reference)
wqb_collated$Reference <- gsub("TRNCC, 1997", "Texas Commission on Environmental Quality, 1997", wqb_collated$Reference)
wqb_collated$Reference <- gsub("EPA Region 3", "US EPA, 2004", wqb_collated$Reference)
wqb_collated$Reference <- gsub("MPCA, 2007", "Minnesota Pollution Control Agency, 2012", wqb_collated$Reference)
wqb_collated$Reference <- gsub("NDDH, 2001", "North Dakota Department of Health, 2001", wqb_collated$Reference)
wqb_collated$Reference <- gsub("(US EPA, 2016)", "US EPA, 2016", wqb_collated$Reference)
wqb_collated$Reference <- gsub("MDEQ, 2010", "Montana Department of Environment Quality, 2010", wqb_collated$Reference)
wqb_collated$Reference <- gsub("EPA, 2001", "US EPA, 2001", wqb_collated$Reference)
names(wqb_collated)

wqb_ref_collated <- wqb_collated %>% filter(!Reference %in% c("WSDE, 2011b", "SERT, 1998", "SERT, 1995"), !is.na(Reference)) %>% distinct() %>% drop_na(Benchmark_ugL) %>% group_by(CAS, `Chemical Name`, Benchmark_ugL, Benchmark_description) %>% summarize(ref_list = paste0(Reference, collapse = "; ")) %>% ungroup()
View(wqb_ref_collated)

write_xlsx(wqb_ref_collated, "Collated_WQB_26_01_2022.xlsx")

#eliminate benchmarks that couldn't be evaluated for screening vs. wq

wqb_collated_1 <- wqb_collated %>% filter(!Reference %in% c("WSDE, 2011b", "SERT, 1998", "SERT, 1995", "US EPA, 1995",  "US EPA, 2005", "EC, 2010", "US EPA, 2004", "Australia & New Zealand Environment Conservation Council, 2000", "US EPA, 2009"), Benchmark_ugL != 0, !is.na(Reference)) %>% distinct() %>% drop_na(Benchmark_ugL)
list(unique(wqb_collated_1$Benchmark_Source))

#look at NA's 
na_benchmark_Source <- wqb_collated_1 %>% filter(is.na(Benchmark_Source))
View(na_benchmark_Source)

screening_wqb <- wqb_collated_1 %>% filter(Benchmark_Source %in% c("US EPA Office of Pesticide Programs (OPP) Aquatic Life Benchmark", "Federal Contaminated Sites Action Plan (FCSAP) Database of Guidelines (2012)", 
                                                                   "US EPA Region 4 Ecological Risk Assesssment Supplemental Guidance", "NOAA Screening Quick Reference Tables (SQuiRTs)", "US EPA Ecotox Thresholds",
                                                                   "US EPA Equilibrium Partitioning Sediment Benchmarks (ESBs) for the Protection of Benthic Organisms: PAH Mixtures", "NORMAN Substance Database (NORMAN SusDat)",
                                                                   "ECHA Registration Dossier"))

View(screening_wqb)
list(unique(screening_wqb$Reference))
list(unique(screening_wqb$Benchmark_description))
list(unique(screening_wqb$Benchmark_Type))

#annotate benchmarks
screening_wqb$Benchmark_description <- gsub("acute", "Acute", screening_wqb$Benchmark_description)
screening_wqb$Benchmark_description <- gsub("chronic", "Chronic", screening_wqb$Benchmark_description)
screening_wqb$Benchmark_description <- gsub("PNEC (freshwater)", "PNEC", screening_wqb$Benchmark_description)

screening_wqb$annotated_desc <-ifelse(grepl("Chronic", screening_wqb$Benchmark_description), "Aquatic_Life_Chronic",
                                      ifelse(grepl("Acute", screening_wqb$Benchmark_description), "Aquatic_Life_Acute",
                                             ifelse(grepl("PNEC freshwater", screening_wqb$Benchmark_description), "Aquatic_Life_Lowest_PNEC_intermittent_release",
                                                    ifelse(grepl("PNEC", screening_wqb$Benchmark_description), "Aquatic_Life_Lowest_PNEC",
                                                           ifelse(screening_wqb$Benchmark_description %in% c("PAH_FCV", "Continuous_Concentration_CCC", "Surface_H2O_Freshwater_AWQCFCV"), "Aquatic_Life_Chronic", 
                                                                  ifelse(screening_wqb$Benchmark_description %in% c("Maximum_Concentration_CMC"), "Aquatic_Life_Acute",
                                                                         ifelse(screening_wqb$Benchmark_description %in% c("Surface_H2O_Freshwater_TierII", "EQS-proposal", "aquatic life water quality criteria", "EQS water (=AA-EQS)"), "Aquatic_Life_Unspecified_Length",
                                                          screening_wqb$Benchmark_description)))))))
list(unique(screening_wqb$annotated_desc))
View(screening_wqb)

screening_apical <- screening_wqb %>% filter(Benchmark_Type == "Screening_Apical")
screening_estimate <- screening_wqb %>% filter(Benchmark_Type == "Screening_Estimate")

write_xlsx(screening_apical, "screening_apical_long.xlsx")
write_xlsx(screening_estimate, "screening_estimated_long.xlsx")

#assess official WQB for outliers 

official_wqb <- wqb_collated_1 %>% filter(Benchmark_Source %in% c("Canadian Council of Ministers of the Environment Water Quality Guidelines for the Protection of Aquatic Life",
                                                                  "European Union Environmental Quality Standards for Priority Substances and Certain Other Pollutants ",
                                                                  "US EPA National Recommended Water Quality Criteria - Aquatic Life Criteria", "Canadian Provincial Water Quality Guidelines",
                                                                  "State-Specific Water Quality Standards Effective under the Clean Water Act (CWA)", "Health Canada Federal Environmental Quality Guidelines (FEQGs)"))

names(official_wqb)
official_wqb$Reference <- gsub("WI", "Wisconsin Department of Natural Resources, 2010",official_wqb$Reference)


official_wqb$annotated_desc <- ifelse(grepl("Chronic", official_wqb$Benchmark_description), "Aquatic_Life_Chronic",
                                      ifelse(grepl("Acute", official_wqb$Benchmark_description), "Aquatic_Life_Acute",
                                             ifelse(official_wqb$Benchmark_description %in% c("AquaticLife_Freshwater_ShortTerm", "Freshwater_CMC_acute", "MAC_EQS_other", "MAC_EQS_inland"), "Aquatic_Life_Acute",
                                                    ifelse(official_wqb$Benchmark_description %in% c("AquaticLife_Freshwater_LongTerm", "Freswater_CCC_chronic", "AA-EQS_inland", "AA_EQS_other"), "Aquatic_Life_Chronic",
                                             "Aquatic_Life_Unspecified_Length"))))
View(official_wqb)

#look at distributions & identify potential lower outliers####

#boxplot outliers + < lower IQR

official_wqb_box <- boxplot(official_wqb$Benchmark_ugL ~ official_wqb$CAS)
official_wqb_box$out

official_wqb_potential_outliers <- subset(official_wqb, official_wqb$Benchmark_ugL %in% official_wqb_box$out)
View(official_wqb_potential_outliers)

official_wqb_box_info <- official_wqb %>% group_by(CAS) %>% summarise(Q1 = quantile(Benchmark_ugL, 0.25), Q3 = quantile(Benchmark_ugL, 0.75))

official_wqb_potential_outliers_1 <- left_join(official_wqb_potential_outliers, official_wqb_box_info, by = "CAS")
View(official_wqb_potential_outliers_1)

official_wqb_potential_outliers_1$outlier_type <- ifelse(official_wqb_potential_outliers_1$Benchmark_ugL < official_wqb_potential_outliers_1$Q1, "POTENTIAL LOWER OUTLIER", "POTENTIAL UPPER OUTLIER")

official_wqb_potential_outliers_2 <- official_wqb_potential_outliers_1 %>% filter(outlier_type == "POTENTIAL LOWER OUTLIER")

#data_limited outliers
official_wqb_mins <- official_wqb %>% group_by(CAS) %>% summarize(min2_conc = nth(Benchmark_ugL, 2, order_by = Benchmark_ugL), min_conc = min(Benchmark_ugL), n_EC = length(Benchmark_ugL), n_ref = length(unique(Reference)))
View(official_wqb_mins)

official_wqb_min_potential_outliers <- left_join(official_wqb, official_wqb_mins)
official_wqb_min_potential_outliers$min2_v_min <- official_wqb_min_potential_outliers$min2_conc / official_wqb_min_potential_outliers$min_conc

official_wqb_min_potential_outliers$n_EC <- as.numeric(official_wqb_min_potential_outliers$n_EC)
official_wqb_min_potential_outliers$min2_v_min <- as.numeric(official_wqb_min_potential_outliers$min2_v_min)
official_wqb_min_potential_outliers$min_Flag <- ifelse(official_wqb_min_potential_outliers$n_ref < 5 & official_wqb_min_potential_outliers$min2_v_min >= 10 | official_wqb_min_potential_outliers$n_ref < 5 & is.na(official_wqb_min_potential_outliers$min2_conc), "Potential Lower Outlier", "")

official_wqb_min_potential_outliers_1 <- official_wqb_min_potential_outliers %>% filter(min_Flag == "Potential Lower Outlier")


#generate final 'potential outlier' list
official_wqb_potential_outliers_final <- bind_rows(official_wqb_potential_outliers_2, official_wqb_min_potential_outliers_1) %>% distinct()
write_xlsx(official_wqb_potential_outliers_final, "official_wqb_potential_outliers.xlsx")


#generate final file for BM analysis####
write_xlsx(official_wqb, "official_wqb_whole.xlsx")

