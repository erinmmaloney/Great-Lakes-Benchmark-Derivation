#pharmaceutical potential#
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

file.choose()
setwd("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Pharmaceutical Potential")

#bind together all cmax files
cmax <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Pharmaceutical Potential\\Synth_1_water_toxcast_ecotox_summary_wCMax.xlsx", "CMax from MaPPFAST")
cmax_extra <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Pharmaceutical Potential\\additional_CAS_for_ECOTOX_29_07_2021_wCmax.xlsx")
cmax_extra_extra <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Pharmaceutical Potential\\missing_cmax_CMS.xlsx") 

names(cmax)
names(cmax_extra)
names(cmax_extra_extra)

cmax_1 <- cmax %>% select("Active Ingredient", "Cmax (ug/ml)", "Primary CAS#") %>% rename("Chemical Name" = "Active Ingredient", "CAS" = "Primary CAS#", "Cmax (ug/mL)" = "Cmax (ug/ml)")

cmax_compiled <- bind_rows(cmax_1, cmax_extra, cmax_extra_extra)
names(cmax_compiled)

#convert to ppb
cmax_compiled$Cmax_ppm <- cmax_compiled$`Cmax (ug/mL)`  * 1000

cmax_2 <- cmax_compiled %>% select("Chemical Name", "CAS", "Cmax_ppm") %>% rename("Cmax" = "Cmax_ppm") %>% filter(!is.na(Cmax))
cmax_2$units <- "ug/L"

write_xlsx(cmax_2, "CMAX.xlsx")
