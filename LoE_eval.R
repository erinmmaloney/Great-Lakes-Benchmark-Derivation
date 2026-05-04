#Manuscript #1 Data Crunching#

library(tidyverse)
library(ggpubr)
library(readxl)
library(writexl)
library(gridExtra)
library(viridis)
library(ComplexHeatmap)
library(dplyr)
library(VennDiagram)
library(UpSetR)
library(VennDiagram)
library(ComplexHeatmap)

setwd("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\LoE evaluation for MS 1")
#pull the number of chemicals detected in water and passive 
chem_list <- read_excel("benchmark_comp_file_05_08_2024_for_MS1_analysis.xlsx", "Included Benchmarks")%>% filter(`Detected Water` == "TRUE")
names(chem_list)
benchmark_comparison <- read_excel("benchmark_comp_file_05_08_2024_for_MS1_analysis.xlsx", "Chemical Summary") %>% filter(type == "Water", det_freq != 0)
names(benchmark_comparison)
detection_summary <- read_excel("detection_frequency_overview_10_05_2022_for_MS1_analysis .xlsx") %>% filter(type == "Water")
cas_class <- chem_list %>% select(CAS, "General Class")
names(detection_summary)

#detection characteristics####
names(detection_summary)
names(chem_list)
detect_water <- detection_summary %>% filter(type == "Water") %>% select(CAS, "Chemical Name", det_freq, n_samples) %>% distinct()
detect_water1 <- left_join(detect_water, cas_class) %>% select(CAS, det_freq, "General Class", n_samples) %>% distinct()

detect_water1$detection_classification <- ifelse(detect_water1$det_freq == 0, "non_detect", "detect")
detect_water1$detection_classification <- as.factor(detect_water1$detection_classification)
summary(detect_water1$detection_classification)

# 334+245 = 579
# 334/579 58% of monitored chemicals were detected 
  
detect_water1$`General Class` <- ifelse(detect_water1$CAS %in% c("2327-08-02", "2599-11-03"), "Pesticide TPs", detect_water1$`General Class`)
detect_water1$`General Class` <- ifelse(detect_water1$CAS %in% c("6493-05-06"), "PPCPs", detect_water1$`General Class`)

water_df_class <- detect_water1 %>% filter(detection_classification != "non_detect") %>% distinct() %>%
  group_by(`General Class`) %>% summarize(n_class = n_distinct(CAS), freq_class = (n_class/334)*100) 

View(detect_water1)
(water_df_class)

# `General Class`      n_class freq_class
# <chr>                  <int>      <dbl>
#   1 Fire retardants            4       1.20
# 2 Fuels/PAHs                10       2.99
# 3 Industrial/Mixed-Use      36      10.8 
# 4 PPCPs                    173      51.8 
# 5 Pesticide TP              42      12.6 
# 6 Pesticides                65      19.5 
# 7 Sterols                    4       1.20

pie(water_df_class$n_class, labels = rep("",7), col = c("#440154", "#3b528b", "#21918c", "#35b779","#c8e020", "#90d743", "#fde725"))

#divide based on detection frequency####
detect_water2 <- detect_water1 %>% filter(detection_classification != "non_detect")
detect_water2$detection_flag <- ifelse(detect_water2$det_freq < 0.25, "low frequency",
                                       ifelse(detect_water2$det_freq >= 0.25 & detect_water2$det_freq < 0.75, "medium frequency", 
                                              ifelse(detect_water2$det_freq >= 0.75, "high frequency", 
                                                     "misc")))
names(detect_water2)
quantile(detect_water2$n_samples, 0.25) #214
mean(detect_water2$n_samples) #616.5 ~ 617
2357/4

detect_water2$sample_flag <- ifelse(detect_water2$n_samples < 214, "data_limited", "not_data_limited") 

detect_water2$flags <- paste(detect_water2$detection_flag, detect_water2$sample_flag, sep = "_")

detect_water2$detection_flag <- as.factor(detect_water2$detection_flag)
detect_water2$flags <- as.factor(detect_water2$flags)

summary(detect_water2$detection_flag)
# high frequency    low frequency medium frequency 
# 5              245               84 
summary(detect_water2$flags)
# high frequency_not_data_limited        low frequency_data_limited 
# 5                                48 
# low frequency_not_data_limited     medium frequency_data_limited 
# 197                                19 
# medium frequency_not_data_limited 
# 65 

#Chemical Coverage Comparison####
names(chem_list)
water_detect <- chem_list %>% filter(`Detected Water` == "TRUE")
list(unique(water_detect$CAS)) #334
names(water_detect)
water_detect_long <-water_detect %>% select(-c("Detected Water":"Detected Passive")) %>% gather(c("Tier_2_Unadjusted_BM":"QSAR"),
                                                                                                key = "benchmark_type", value = "availability")
water_detect_long$availability_score <- ifelse(water_detect_long$availability == "TRUE", 1, 0)

water_detect_long_summary <- water_detect_long %>% group_by(benchmark_type) %>% summarize(n_avail_chem = sum(availability_score)) %>%
  arrange(n_avail_chem)

water_detect_long_summary <- water_detect_long %>% group_by(benchmark_type) %>% summarize(n_avail_chem = sum(availability_score)) %>%
  arrange(n_avail_chem)
  
water_detect_long_summary$percent <- (water_detect_long_summary$n_avail_chem / 334) * 100

water_detect_long_class_summary <- water_detect_long %>% group_by(benchmark_type, `General Class`) %>% summarize(n_avail_class = sum(availability_score))

water_detect_long_summary_final <- left_join(water_detect_long_summary,water_detect_long_class_summary)
water_detect_long_summary_final$percent_class <- (water_detect_long_summary_final$n_avail_class / water_detect_long_summary_final$n_avail_chem)*100

water_detect_long_1 <- left_join(water_detect_long_summary, water_detect_long_class_summary) %>% distinct() %>% spread(key = "General Class", value = "n_avail_class") %>%
  filter(!benchmark_type %in% c("Tier_2_Unadjusted_BM", "Tier_1_Unadjusted_BM" , "Cytotox - AF-adjusted BM", "ToxCast - AF-adjusted BM"))

water_detect_long_1$benchmark_type <- gsub("Cytotox - Unadjusted BM", "Cytotoxic_Burst", water_detect_long_1$benchmark_type)
water_detect_long_1$benchmark_type <- gsub("ToxCast - Unadjusted BM", "ToxCast", water_detect_long_1$benchmark_type)
water_detect_long_1$benchmark_type <- gsub("Tier_2_AF_adjusted_BM", "Tier2_ECOTOX", water_detect_long_1$benchmark_type)
water_detect_long_1$benchmark_type <- gsub("Tier_1_AF_adjusted_BM", "Tier1_ECOTOX", water_detect_long_1$benchmark_type)
water_detect_long_1$benchmark_type <- gsub("Water_Quality_Benchmark", "WQB", water_detect_long_1$benchmark_type)

head(water_detect_long_1)
list(unique(water_detect_long_1$benchmark_type))

#graph
names(water_detect_long_1)
water_detect_long_2 <- water_detect_long_1 %>% gather(c("Fire retardants":"Sterols"), key = "General Class", value= "n_Class") %>% arrange(desc(n_avail_chem))
water_detect_plot <- water_detect_long_2 %>% ggbarplot("benchmark_type", "n_Class", fill = "General Class", title = "A", palette = c("#440154", "#3b528b", "#21918c", "#35b779","#c8e020", "#90d743", "#fde725")) +
  xlab("Ecotoxicological Hazard Concentration Type") +   scale_y_continuous(
    
    # Features of the first axis
    name = "Number of Chemicals with Available Hazard Concentrations",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~./3.33, name="Detected Chemicals (%)")
  ) + geom_hline(yintercept = 333, linetype = "dashed") + geom_hline(yintercept = (333/2), linetype = "dashed") + rotate_x_text(angle = 90)
water_detect_plot


#UpSet Plot
head(water_detect_long)

water_detect_long$benchmark_type <- gsub("Cytotox - Unadjusted BM", "Cytotoxic_Burst", water_detect_long$benchmark_type)
water_detect_long$benchmark_type <- gsub("ToxCast - Unadjusted BM", "ToxCast", water_detect_long$benchmark_type)
water_detect_long$benchmark_type <- gsub("Tier_2_AF_adjusted_BM", "Tier2_ECOTOX", water_detect_long$benchmark_type)
water_detect_long$benchmark_type <- gsub("Tier_1_AF_adjusted_BM", "Tier1_ECOTOX", water_detect_long$benchmark_type)

water_detect_wide_1 <- water_detect_long %>% select(-availability, -"Chemical Name") %>% filter(!benchmark_type %in% c("Tier_2_Unadjusted_BM", "Tier_1_Unadjusted_BM" , "Cytotox - AF-adjusted BM", "ToxCast - AF-adjusted BM")) %>%
  spread(key = "benchmark_type", value = "availability_score") %>% column_to_rownames("CAS") %>% select(-`General Class`, -Class)

names(water_detect_wide_1)
summary(water_detect_wide_1)
View(water_detect_wide_1)
write_xlsx(water_detect_wide_1, "water_detect_wide_1.xlsx")

water_detect_upset <- upset(water_detect_wide_1, nset = 9, order.by = "freq",
                            mainbar.y.label = "Chemical Coverage of Ecotoxicological Benchmarks",
                            queries = list(
                              list(query = intersects,
                                   params = list("QSAR", "Screening_Estimate", "Pharm_Potential"), 
                                   color = "#000004", 
                                   active = T, 
                                   query.name = "Estimated"),
                              list(query = intersects,
                                   params = list("QSAR", "Screening_Estimate"), 
                                   color = "#000004", 
                                   active = T, 
                                   query.name = "Estimated"),
                              list(query = intersects,
                                   params = list("QSAR"), 
                                   color = "#000004", 
                                   active = T, 
                                   query.name = "Estimated"),
                              list(query = intersects,
                                   params = list("QSAR", "Screening_Estimate", "Pharm_Potential", "ToxCast"), 
                                   color = "#8c2981", 
                                   active = T, 
                                   query.name = "Non-Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR", "Screening_Estimate", "Pharm_Potential", "ToxCast", "Cytotoxic_Burst"), 
                                   color = "#8c2981", 
                                   active = T, 
                                   query.name = "Non-Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR", "Screening_Estimate", "ToxCast"), 
                                   color = "#8c2981", 
                                   active = T, 
                                   query.name = "Non-Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR", "ToxCast", "Screening_Estimate", "Cytotoxic_Burst"), 
                                   color = "#8c2981", 
                                   active = T, 
                                   query.name = "Non-Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR", "ToxCast", "Screening_Estimate"), 
                                   color = "#8c2981", 
                                   active = T, 
                                   query.name = "Non-Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX", "Screening_Apical", "Cytotoxic_Burst", "Tier2_ECOTOX"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX", "Screening_Apical", "Pharm_Potential", "Cytotoxic_Burst", "Tier2_ECOTOX"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX", "Screening_Apical"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX", "Screening_Apical", "Pharm_Potential"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX","Screening_Estimate", "Screening_Apical"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX","Screening_Apical", "Cytotoxic_Burst"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "ToxCast", "Tier1_ECOTOX", "Tier2_ECOTOX"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR", "ToxCast", "Tier1_ECOTOX", "Screening_Apical", "Pharm_Potential", "Cytotoxic_Burst"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX","Screening_Apical", "Tier2_ECOTOX"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX","Screening_Apical", "Cytotoxic_Burst", "Tier2_ECOTOX"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX","Screening_Apical", "Pharm_Potential", "Tier2_ECOTOX"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "ToxCast", "Tier1_ECOTOX","Cytotoxic_Burst", "Tier2_ECOTOX"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "ToxCast", "Tier1_ECOTOX","Cytotoxic_Burst", "Pharm_Potential"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "ToxCast", "Tier1_ECOTOX","Screening_Apical", "Tier2_ECOTOX"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "ToxCast", "Tier1_ECOTOX","Screening_Apical","Cytotoxic_Burst", "Tier2_ECOTOX"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Screening_Apical","Pharm_Potential", "Cytotoxic_Burst"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "ToxCast", "Tier1_ECOTOX","Pharm_Potential","Tier2_ECOTOX", "Cytotoxic_Burst"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Tier1_ECOTOX","Screening_Apical","Tier2_ECOTOX"), 
                                   color = "#de4968", 
                                   active = T, 
                                   query.name = "Apical, Non-Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX","Screening_Estimate", "Screening_Apical", "Water_Quality_Benchmark"), 
                                   color = "#fe9f6d", 
                                   active = T, 
                                   query.name = "All Benchmark Categories"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX","Screening_Apical", "Tier2_ECOTOX", "Cytotoxic_Burst", "Water_Quality_Benchmark"), 
                                   color = "#fe9f6d", 
                                   active = T, 
                                   query.name = "All Benchmark Categories"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "ToxCast", "Tier1_ECOTOX","Screening_Apical", "Tier2_ECOTOX", "Cytotoxic_Burst", "Water_Quality_Benchmark"), 
                                   color = "#fe9f6d", 
                                   active = T, 
                                   query.name = "All Benchmark Categories"),
                              list(query = intersects,
                                   params = list("QSAR", "ToxCast", "Tier1_ECOTOX","Screening_Apical", "Tier2_ECOTOX","Water_Quality_Benchmark"), 
                                   color = "#fe9f6d", 
                                   active = T, 
                                   query.name = "All Benchmark Categories"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "ToxCast", "Tier1_ECOTOX","Screening_Apical", "Tier2_ECOTOX","Water_Quality_Benchmark"), 
                                   color = "#fe9f6d", 
                                   active = T, 
                                   query.name = "All Benchmark Categories"),
                              list(query = intersects,
                                   params = list("QSAR", "ToxCast", "Tier1_ECOTOX","Screening_Apical", "Water_Quality_Benchmark"), 
                                   color = "#fe9f6d", 
                                   active = T, 
                                   query.name = "All Benchmark Categories"),
                              list(query = intersects,
                                   params = list("QSAR","Tier1_ECOTOX","Screening_Apical","Tier2_ECOTOX", "Water_Quality_Benchmark"), 
                                   color = "#fe9f6d", 
                                   active = T, 
                                   query.name = "All Benchmark Categories"),
                              list(query = intersects,
                                   params = list("QSAR","ToxCast", "Tier1_ECOTOX","Screening_Apical","Pharm_Potential", "Tier2_ECOTOX", "Water_Quality_Benchmark"), 
                                   color = "#fe9f6d", 
                                   active = T, 
                                   query.name = "All Benchmark Categories"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate","ToxCast", "Tier1_ECOTOX","Screening_Apical", "Cytotoxic_Burst", "Water_Quality_Benchmark"), 
                                   color = "#fe9f6d", 
                                   active = T, 
                                   query.name = "All Benchmark Categories"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "Tier1_ECOTOX","Screening_Apical","Water_Quality_Benchmark"), 
                                   color = "#fcfdbf", 
                                   active = T, 
                                   query.name = "Water Quality Benchmarks, Apical, and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Apical", "Pharm_Potential"), 
                                   color = "#3b0f70", 
                                   active = T, 
                                   query.name = "Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "Tier1_ECOTOX"), 
                                   color = "#3b0f70", 
                                   active = T, 
                                   query.name = "Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "Tier1_ECOTOX", "Pharm_Potential"), 
                                   color = "#3b0f70", 
                                   active = T, 
                                   query.name = "Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR", "Tier1_ECOTOX", "Screening_Apical"), 
                                   color = "#3b0f70", 
                                   active = T, 
                                   query.name = "Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "Tier1_ECOTOX"), 
                                   color = "#3b0f70", 
                                   active = T, 
                                   query.name = "Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Screening_Estimate", "Screening_Apical"), 
                                   color = "#3b0f70", 
                                   active = T, 
                                   query.name = "Apical and Estimated"),
                              list(query = intersects,
                                   params = list("QSAR","Tier1_ECOTOX", "Screening_Apical", "Pharm_Potential"), 
                                   color = "#3b0f70", 
                                   active = T, 
                                   query.name = "Apical and Estimated")
                              
                              ))
water_detect_upset


#venn inset 
list(unique(water_detect_long_2$benchmark_type))

water_detect_long_2 <- water_detect_long %>% select(-availability, -"Chemical Name") %>% filter(!benchmark_type %in% c("Tier_2_Unadjusted_BM", "Tier_1_Unadjusted_BM" , "Cytotox - AF-adjusted BM", "ToxCast - AF-adjusted BM"))
water_detect_long_2$benchmark_group <- if_else(water_detect_long_2$benchmark_type %in% c("Tier2_ECOTOX", "ToxCast", "Cytotoxic_Burst"), "Non-Apical",
                                             ifelse(water_detect_long_2$benchmark_type %in% c("Tier1_ECOTOX", "Screening_Apical"), "Apical",
                                                                                            ifelse(water_detect_long_2$benchmark_type %in% c("QSAR", "Pharm_Potential", "Screening_Estimate"), "Estimated",
                                                                                                   water_detect_long_2$benchmark_type)))

water_detect_long_3 <- water_detect_long_2 %>% group_by(benchmark_group) %>% summarize(n_avail = sum(availability_score))
water_detect_long_4 <- left_join(water_detect_long_2, water_detect_long_3) %>% filter(availability_score != 0)
View(water_detect_long_4)
list(unique(water_detect_long_4$benchmark_group))

wqb <- water_detect_long_4 %>% filter(benchmark_group == "Water_Quality_Benchmark") %>% select(CAS) %>% unlist()
apical <- water_detect_long_4 %>% filter(benchmark_group == "Apical") %>% select(CAS)%>% unlist()
non_apical <- water_detect_long_4 %>% filter(benchmark_group == "Non-Apical") %>% select(CAS)%>% unlist()
estimated <- water_detect_long_4 %>% filter(benchmark_group == "Estimated") %>% select(CAS)%>% unlist()

venn <- venn.diagram(x = list(wqb, apical, non_apical, estimated),
             category.names = c("WQB", "Apical", "Non-Apical", "Estimated"),
             filename = "coverage.png", output = "TRUE",
             imagetype="png" ,
             height = 600 , 
             width = 600 , 
             resolution = 300,
             compression = "lzw",
             lwd = 1,
             col=c("#f0f921", '#ed7953', '#9c179e',"#0d0887"),
             fill = c(alpha("#f0f921",0.3), alpha('#ed7953',0.3), alpha('#9c179e',0.3), alpha("#0d0887", 0.3)),
             cex = 0.5,
             fontfamily = "sans",
             cat.cex = 0.3,
             cat.default.pos = "outer",
             cat.fontfamily = "sans",
             cat.col = c("#fe9f6d", '#de4968', '#8c2981',"#000004")
)

#get summary for coverage for different benchmark types 
water_detect_summary <- water_detect_long_2 %>% filter(availability_score == 1) %>% group_by(benchmark_type) %>% summarise(total_chem = n_distinct(CAS))
water_detect_summary

class_detect_summary <- water_detect_long_2 %>% group_by(`General Class`) %>% summarize(n_chem = n_distinct(CAS))
class_detect_summary

class_water_detect_summary <- water_detect_long_2 %>% filter(availability_score == 1) %>% group_by(benchmark_type, `General Class`) %>% summarize(n_chem = n_distinct(CAS))
class_water_detect_summary1 <- left_join(class_water_detect_summary, water_detect_summary)
class_water_detect_summary1$percent <- (class_water_detect_summary1$n_chem / class_water_detect_summary1$total_chem) *100
View(class_water_detect_summary1)

####Exceedence#####
#WQB####
cas_class <- chem_list %>%
  filter(`Detected Water` == "TRUE") %>% select(CAS, "General Class")
names(cas_class)

names(benchmark_comparison)
wqb <- benchmark_comparison %>% filter(benchmarks == "Water_Quality_Benchmark")
wqb_1 <- left_join(cas_class, wqb) %>% filter(!is.na(TQ_max)) %>% distinct()
wqb_1$TQ_annotation <- ifelse(wqb_1$TQ_max >= 1, "High Priority",
                              ifelse(wqb_1$TQ_max < 0.001, "Low Priority",
                                     "Medium Priority"))
wqb_1$TQ_annotation <- as.factor(wqb_1$TQ_annotation)
summary(wqb_1$TQ_annotation)

# High Priority    Low Priority Medium Priority 
# 32               2              27 

# 32+2+27
# 32/61 - 54 %

hi_priority_breakdown <- wqb_1 %>% filter(TQ_annotation == "High Priority")
hi_priority_breakdown$`General Class` <- as.factor(hi_priority_breakdown$`General Class`)
summary(hi_priority_breakdown$`General Class`)
View(hi_priority_breakdown)

med_priority_breakdown <- wqb_1 %>% filter(TQ_annotation == "Medium Priority")
med_priority_breakdown$`General Class` <- as.factor(med_priority_breakdown$`General Class`)
summary(med_priority_breakdown$`General Class`)

wqb_1 <- wqb_1 %>% arrange(TQ_max)

wqb_plot <- wqb_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", palette = viridis(n = 6, option = "D", direction = -1), orientation = "horiz") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            
                                                                 ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
wqb_plot
ggsave("wqb_plot.jpeg", wqb_plot, width = 10)

#screening apical####
screening_apical <- benchmark_comparison %>% filter(benchmarks == "Screening_Apical")
screening_apical_1 <- left_join(cas_class, screening_apical) %>% filter(!is.na(TQ_max)) %>% distinct()

View(screening_apical_1) #149 chemicals
screening_apical_1$TQ_annotation <- ifelse(screening_apical_1$TQ_max >= 1, "High Priority",
                              ifelse(screening_apical_1$TQ_max < 0.001, "Low Priority",
                                     "Medium Priority"))
screening_apical_1$TQ_annotation <- as.factor(screening_apical_1$TQ_annotation)
summary(screening_apical_1$TQ_annotation)

# High Priority    Low Priority Medium Priority 
# 59              13              79

hi_priority_breakdown <- screening_apical_1 %>% filter(TQ_annotation == "High Priority")
hi_priority_breakdown$`General Class` <- as.factor(hi_priority_breakdown$`General Class`)
summary(hi_priority_breakdown$`General Class`)
View(hi_priority_breakdown)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 4                    8                   11                    1                   26                    9 

med_priority_breakdown <- screening_apical_1 %>% filter(TQ_annotation == "Medium Priority")
med_priority_breakdown$`General Class` <- as.factor(med_priority_breakdown$`General Class`)
summary(med_priority_breakdown$`General Class`)
# Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 2                    9                    3                   33                   29 

lo_priority_breakdown <- screening_apical_1 %>% filter(TQ_annotation == "Low Priority")
lo_priority_breakdown$`General Class` <- as.factor(lo_priority_breakdown$`General Class`)
summary(lo_priority_breakdown$`General Class`)

# Pesticide TP   Pesticides        PPCPs 
# 3            3            7 

screening_apical_1 <- screening_apical_1 %>% arrange(TQ_max)

screening_apical_plot <- screening_apical_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", palette = viridis(n = 6, option = "D", direction = -1), orientation = "horiz") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
screening_apical_plot

ggsave("screening_apical_plot.jpeg", screening_apical_plot)

#tier 1 ECOTOX####
list(unique(benchmark_comparison$benchmarks))
t1_unadjusted <- benchmark_comparison %>% filter(benchmarks == "Tier_1_Unadjusted_BM")
t1_unadjusted_1 <- left_join(cas_class, t1_unadjusted) %>% filter(!is.na(TQ_max)) %>% distinct()

View(t1_unadjusted_1) #166 tier 1 eco available
t1_unadjusted_1$TQ_annotation <- ifelse(t1_unadjusted_1$TQ_max >= 1, "High Priority",
                                           ifelse(t1_unadjusted_1$TQ_max < 0.001, "Low Priority",
                                                  "Medium Priority"))
t1_unadjusted_1$TQ_annotation <- as.factor(t1_unadjusted_1$TQ_annotation)
t1_unadjusted_1$`General Class` <- as.factor(t1_unadjusted_1$`General Class`)
summary(t1_unadjusted_1$TQ_annotation)

# High Priority    Low Priority Medium Priority 
# 35              57              78 

hi_priority_breakdown <- t1_unadjusted_1 %>% filter(TQ_annotation == "High Priority")
summary(hi_priority_breakdown$`General Class`)
View(hi_priority_breakdown)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use         Pesticide TP           Pesticides                PPCPs 
# 1                    2                    6                    2                   19                    5 
# Sterols 
# 0 

med_priority_breakdown <- t1_unadjusted_1 %>% filter(TQ_annotation == "Medium Priority")
summary(med_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs              Sterols 
# 2                   10                   11                    4                   32                   17                    1 

  
low_priority_breakdown <- t1_unadjusted_1 %>% filter(TQ_annotation == "Low Priority")
summary(low_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs              Sterols 
# 1                    0                    6                    4                   16                   32                    0 

t1_unadjusted_1 <- t1_unadjusted_1 %>% arrange(TQ_max)

t1_unadjusted_1_plot <- t1_unadjusted_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", 
                                                       palette = viridis(n = 7, option = "D", direction = -1), orientation = "horiz", title = "A") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            
                                                                 ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
t1_unadjusted_1_plot

#tier 1 AF-adjusted
list(unique(benchmark_comparison$benchmarks))
t1_AFadjusted <- benchmark_comparison %>% filter(benchmarks == "Tier_1_AF_adjusted_BM")
t1_AFadjusted_1 <- left_join(cas_class, t1_AFadjusted) %>% filter(!is.na(TQ_max)) %>% distinct()

View(t1_AFadjusted_1) #166 t1 eco available 
t1_AFadjusted_1$TQ_annotation <- ifelse(t1_AFadjusted_1$TQ_max >= 1, "High Priority",
                                        ifelse(t1_AFadjusted_1$TQ_max < 0.001, "Low Priority",
                                               "Medium Priority"))
t1_AFadjusted_1$TQ_annotation <- as.factor(t1_AFadjusted_1$TQ_annotation)
t1_AFadjusted_1$`General Class` <- as.factor(t1_AFadjusted_1$`General Class`)
summary(t1_AFadjusted_1$TQ_annotation)
# 
# High Priority    Low Priority Medium Priority 
# 79              12              79 

hi_priority_breakdown <- t1_AFadjusted_1 %>% filter(TQ_annotation == "High Priority")
summary(hi_priority_breakdown$`General Class`)
View(hi_priority_breakdown)

med_priority_breakdown <- t1_AFadjusted_1 %>% filter(TQ_annotation == "Medium Priority")
summary(med_priority_breakdown$`General Class`)

# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs              Sterols 
# 3                    7                   11                    4                   34                   30                    0 

low_priority_breakdown <- t1_AFadjusted_1 %>% filter(TQ_annotation == "Low Priority")
summary(low_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs              Sterols 
# 0                    0                    1                    1                    8                   14                    0 

t1_AFadjusted_1 <- t1_AFadjusted_1 %>% arrange(TQ_max)

t1_AFadjusted_1_plot <- t1_AFadjusted_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", 
                                                       palette = viridis(n = 7, option = "D", direction = -1), orientation = "horiz", title = "B") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            
                                                                 ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
t1_AFadjusted_1_plot

tier1_ecotox_plot <- grid.arrange(t1_unadjusted_1_plot, t1_AFadjusted_1_plot)
ggsave("tier1_ecotox_plot.jpeg", tier1_ecotox_plot, height = 10, width = 7.5)

#tier 2 ECOTOX####
list(unique(benchmark_comparison$benchmarks))
t2_unadjusted <- benchmark_comparison %>% filter(benchmarks == "Tier_2_Unadjusted_BM")
t2_unadjusted_1 <- left_join(cas_class, t2_unadjusted) %>% filter(!is.na(TQ_max)) %>% distinct()

View(t2_unadjusted_1) #96 screening available
t2_unadjusted_1$TQ_annotation <- ifelse(t2_unadjusted_1$TQ_max >= 1, "High Priority",
                              ifelse(t2_unadjusted_1$TQ_max < 0.001, "Low Priority",
                                     "Medium Priority"))
t2_unadjusted_1$TQ_annotation <- as.factor(t2_unadjusted_1$TQ_annotation)
t2_unadjusted_1$`General Class` <- as.factor(t2_unadjusted_1$`General Class`)
summary(t2_unadjusted_1$TQ_annotation)
# High Priority    Low Priority Medium Priority 
# 37              26              56

hi_priority_breakdown <- t2_unadjusted_1 %>% filter(TQ_annotation == "High Priority")
summary(hi_priority_breakdown$`General Class`)
View(hi_priority_breakdown)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use 
# 1                    0                    9 
# Pesticide TPs           Pesticides                PPCPs 
# 1                   11                   12 

low_priority_breakdown <- t2_unadjusted_1 %>% filter(TQ_annotation == "Low Priority")
summary(low_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 0                    2                    2                    0                    6                    7 

medium_priority_breakdown <- t2_unadjusted_1 %>% filter(TQ_annotation == "Medium Priority")
summary(medium_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 3                    2                    6                    3                   17                   14 

t2_unadjusted_1 <- t2_unadjusted_1 %>% arrange(TQ_max)

t2_unadjusted_1_plot <- t2_unadjusted_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", 
                                                       palette = viridis(n = 6, option = "D", direction = -1), orientation = "horiz", title = "A") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
t2_unadjusted_1_plot

#tier 2 AF-adjusted
list(unique(benchmark_comparison$benchmarks))
t2_AFadjusted <- benchmark_comparison %>% filter(benchmarks == "Tier_2_AF_adjusted_BM")
t2_AFadjusted_1 <- left_join(cas_class, t2_AFadjusted) %>% filter(!is.na(TQ_max)) %>% distinct()

View(t2_AFadjusted_1) #96 screening available
t2_AFadjusted_1$TQ_annotation <- ifelse(t2_AFadjusted_1$TQ_max >= 1, "High Priority",
                                        ifelse(t2_AFadjusted_1$TQ_max < 0.001, "Low Priority",
                                               "Medium Priority"))
t2_AFadjusted_1$TQ_annotation <- as.factor(t2_AFadjusted_1$TQ_annotation)
t2_AFadjusted_1$`General Class` <- as.factor(t2_AFadjusted_1$`General Class`)
summary(t2_AFadjusted_1$TQ_annotation)
# High Priority    Low Priority Medium Priority 
# 56               5              35 

hi_priority_breakdown <- t2_AFadjusted_1 %>% filter(TQ_annotation == "High Priority")
summary(hi_priority_breakdown$`General Class`)
View(hi_priority_breakdown)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use 
# 3                    0                   13 
# Pesticide TPs           Pesticides                PPCPs 
# 4                   16                   20 

low_priority_breakdown <- t2_AFadjusted_1 %>% filter(TQ_annotation == "Low Priority")
summary(low_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 0                    0                    0                    0                    3                    2 

medium_priority_breakdown <- t2_AFadjusted_1 %>% filter(TQ_annotation == "Medium Priority")
summary(medium_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 1                    4                    4                    0                   15                   11 

t2_AFadjusted_1 <- t2_AFadjusted_1 %>% arrange(TQ_max)

t2_AFadjusted_1_plot <- t2_AFadjusted_1 %>% distinct() %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", 
                                                       palette = viridis(n = 6, option = "D", direction = -1), orientation = "horiz", title = "B") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
t2_AFadjusted_1_plot

tier2_ecotox_plots <- grid.arrange(t2_unadjusted_1_plot, t2_AFadjusted_1_plot)
ggsave("tier2_ecotox_plots.jpeg", tier2_ecotox_plots, height = 10, width = 7.5)

#ToxCast####
#unadjusted#
list(unique(benchmark_comparison$endPoint))
toxcast_unadjusted <- benchmark_comparison %>% filter(endPoint == "ToxCast - Unadjusted BM")
toxcast_unadjusted_1 <- left_join(cas_class, toxcast_unadjusted) %>% filter(!is.na(TQ_max)) %>% distinct()

View(toxcast_unadjusted_1) #204 screening available
toxcast_unadjusted_1$TQ_annotation <- ifelse(toxcast_unadjusted_1$TQ_max >= 1, "High Priority",
                                        ifelse(toxcast_unadjusted_1$TQ_max < 0.001, "Low Priority",
                                               "Medium Priority"))
toxcast_unadjusted_1$TQ_annotation <- as.factor(toxcast_unadjusted_1$TQ_annotation)
toxcast_unadjusted_1$`General Class` <- as.factor(toxcast_unadjusted_1$`General Class`)
summary(toxcast_unadjusted_1$TQ_annotation)
# High Priority    Low Priority Medium Priority 
# 9              99              96 

hi_priority_breakdown <- toxcast_unadjusted_1 %>% filter(TQ_annotation == "High Priority")
summary(hi_priority_breakdown$`General Class`)
View(hi_priority_breakdown)

medium_priority_breakdown <- toxcast_unadjusted_1 %>% filter(TQ_annotation == "Medium Priority")
summary(medium_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 3                    3                   22                    3                   25                   40

low_priority_breakdown <- toxcast_unadjusted_1 %>% filter(TQ_annotation == "Low Priority")
summary(low_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 1                    4                    4                    6                   31                   53 

toxcast_unadjusted_1 <- toxcast_unadjusted_1 %>% arrange(TQ_max)

toxcast_unadjusted_1_plot <- toxcast_unadjusted_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", 
                                                                 palette = viridis(n = 6, option = "D", direction = -1), orientation = "horiz", title = "A") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
toxcast_unadjusted_1_plot

#AF-adjusted
list(unique(benchmark_comparison$endPoint))
toxcast_AFadjusted <- benchmark_comparison %>% filter(endPoint == "ToxCast - AF-adjusted BM")
toxcast_AFadjusted_1 <- left_join(cas_class, toxcast_AFadjusted) %>% filter(!is.na(TQ_max)) %>% distinct()

View(toxcast_AFadjusted_1) #204 screening available
toxcast_AFadjusted_1$TQ_annotation <- ifelse(toxcast_AFadjusted_1$TQ_max >= 1, "High Priority",
                                             ifelse(toxcast_AFadjusted_1$TQ_max < 0.001, "Low Priority",
                                                    "Medium Priority"))
toxcast_AFadjusted_1$TQ_annotation <- as.factor(toxcast_AFadjusted_1$TQ_annotation)
toxcast_AFadjusted_1$`General Class` <- as.factor(toxcast_AFadjusted_1$`General Class`)
summary(toxcast_AFadjusted_1$TQ_annotation)
# High Priority    Low Priority Medium Priority 
# 20              70             114 

high_priority_breakdown <- toxcast_AFadjusted_1 %>% filter(TQ_annotation == "High Priority")
summary(high_priority_breakdown$`General Class`)
View(high_priority_breakdown)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 0                    1                    2                    0                    3                   14 

medium_priority_breakdown <- toxcast_AFadjusted_1 %>% filter(TQ_annotation == "Medium Priority")
summary(medium_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 3                    7                   25                    7                   31                   41 

low_priority_breakdown <- toxcast_AFadjusted_1 %>% filter(TQ_annotation == "Low Priority")
summary(low_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs 
# 1                    0                    1                    2                   24                   42 

toxcast_AFadjusted_1 <- toxcast_AFadjusted_1 %>% arrange(TQ_max)

toxcast_AFadjusted_1_plot <- toxcast_AFadjusted_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", 
                                                                 palette = viridis(n = 6, option = "D", direction = -1), orientation = "horiz", title = "B") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
toxcast_AFadjusted_1_plot

toxcast_plots <- grid.arrange(toxcast_unadjusted_1_plot, toxcast_AFadjusted_1_plot)
ggsave("toxcast_plots.jpeg", toxcast_plots, height = 10, width = 7.5)

#Cytotox#####
#unadjusted 
list(unique(benchmark_comparison$endPoint))
cytotox_unadjusted <- benchmark_comparison %>% filter(endPoint == "Cytotox - Unadjusted BM")
cytotox_unadjusted_1 <- left_join(cas_class, cytotox_unadjusted) %>% filter(!is.na(TQ_max)) %>% distinct()

View(cytotox_unadjusted_1) #101 screening available

cytotox_unadjusted_1$TQ_annotation <- ifelse(cytotox_unadjusted_1$TQ_max >= 1, "High Priority",
                                             ifelse(cytotox_unadjusted_1$TQ_max < 0.001, "Low Priority",
                                                    "Medium Priority"))
cytotox_unadjusted_1$TQ_annotation <- as.factor(cytotox_unadjusted_1$TQ_annotation)
cytotox_unadjusted_1$`General Class` <- as.factor(cytotox_unadjusted_1$`General Class`)
summary(cytotox_unadjusted_1$TQ_annotation)
# High Priority    Low Priority Medium Priority 
# 18              22              61 

high_priority_breakdown <- cytotox_unadjusted_1 %>% filter(TQ_annotation == "High Priority")
summary(high_priority_breakdown$`General Class`)
View(high_priority_breakdown)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use           Pesticides                PPCPs 
# 2                    0                    5                    4                    7 

medium_priority_breakdown <- cytotox_unadjusted_1 %>% filter(TQ_annotation == "Medium Priority")
summary(medium_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use           Pesticides                PPCPs 
# 2                    5                   10                   16                   28 

low_priority_breakdown <- cytotox_unadjusted_1 %>% filter(TQ_annotation == "Low Priority")
summary(low_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use           Pesticides                PPCPs 
# 0                    0                    0                    5                   17 

cytotox_unadjusted_1 <- cytotox_unadjusted_1 %>% arrange(TQ_max)

cytotox_unadjusted_1_plot <- cytotox_unadjusted_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", 
                                                                 palette = viridis(n = 6, option = "D", direction = -1), orientation = "horiz", title = "A") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
cytotox_unadjusted_1_plot

#AF-adjusted
list(unique(benchmark_comparison$endPoint))
cytotox_AFadjusted <- benchmark_comparison %>% filter(endPoint == "Cytotox - AF-adjusted BM")
cytotox_AFadjusted_1 <- left_join(cas_class, cytotox_AFadjusted) %>% filter(!is.na(TQ_max)) %>% distinct()

View(cytotox_AFadjusted_1) #100 screening available
cytotox_AFadjusted_1$TQ_annotation <- ifelse(cytotox_AFadjusted_1$TQ_max >= 1, "High Priority",
                                             ifelse(cytotox_AFadjusted_1$TQ_max < 0.001, "Low Priority",
                                                    "Medium Priority"))
cytotox_AFadjusted_1$TQ_annotation <- as.factor(cytotox_AFadjusted_1$TQ_annotation)
cytotox_AFadjusted_1$`General Class` <- as.factor(cytotox_AFadjusted_1$`General Class`)
summary(cytotox_AFadjusted_1$TQ_annotation)

# High Priority    Low Priority Medium Priority 
# 20              12              69 

high_priority_breakdown <- cytotox_AFadjusted_1 %>% filter(TQ_annotation == "High Priority")
summary(high_priority_breakdown$`General Class`)
View(high_priority_breakdown)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use           Pesticides                PPCPs 
# 2                    1                    6                    4                    7 

medium_priority_breakdown <- cytotox_AFadjusted_1 %>% filter(TQ_annotation == "Medium Priority")
summary(medium_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use           Pesticides                PPCPs 
# 2                    4                    9                   16                   38 

low_priority_breakdown <- cytotox_AFadjusted_1 %>% filter(TQ_annotation == "Low Priority")
summary(low_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use           Pesticides                PPCPs 
# 0                    0                    0                    5                    7 

cytotox_AFadjusted_1 <- cytotox_AFadjusted_1 %>% arrange(TQ_max)

cytotox_AFadjusted_1_plot <- cytotox_AFadjusted_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", 
                                                                 palette = viridis(n = 6, option = "D", direction = -1), orientation = "horiz", title = "B") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
cytotox_AFadjusted_1_plot

cytotx_plot <- grid.arrange(cytotox_unadjusted_1_plot, cytotox_AFadjusted_1_plot)
ggsave("cytotx_plot.jpeg", cytotx_plot, height = 10, width = 7.5)

#estimated screening values####
list(unique(benchmark_comparison$benchmarks))
screening_estimate <- benchmark_comparison %>% filter(benchmarks == "Screening_Estimate")
screening_estimate_1 <- left_join(cas_class, screening_estimate) %>% filter(!is.na(TQ_max)) %>% distinct()

View(screening_estimate_1) #220 screening available
screening_estimate_1$TQ_annotation <- ifelse(screening_estimate_1$TQ_max >= 1, "High Priority",
                                             ifelse(screening_estimate_1$TQ_max < 0.001, "Low Priority",
                                                    "Medium Priority"))
screening_estimate_1$TQ_annotation <- as.factor(screening_estimate_1$TQ_annotation)
screening_estimate_1$`General Class` <- as.factor(screening_estimate_1$`General Class`)
summary(screening_estimate_1$TQ_annotation)
# High Priority    Low Priority Medium Priority 
# 43              10             167 

hi_priority_breakdown <- screening_estimate_1 %>% filter(TQ_annotation == "High Priority")
summary(hi_priority_breakdown$`General Class`)
View(hi_priority_breakdown)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use 
# 1                    4                   14 
# Pesticide TPs           Pesticides                PPCPs 
# 3                    4                   14 
# Sterols 
# 3 

medium_priority_breakdown <- screening_estimate_1 %>% filter(TQ_annotation == "Medium Priority")
summary(medium_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs              Sterols 
# 0                    6                   17                   23                   15                  106                    0 

low_priority_breakdown <- screening_estimate_1 %>% filter(TQ_annotation == "Low Priority")
summary(low_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs              Sterols 
# 0                    0                    0                    1                    0                    9                    0 

screening_estimate_1 <- screening_estimate_1 %>% arrange(TQ_max)
screening_estimate_1_plot <- screening_estimate_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", 
                                                                 palette = viridis(n = 7, option = "D", direction = -1), orientation = "horiz") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
screening_estimate_1_plot

ggsave("screening_estimate_1_plot.jpeg", screening_estimate_1_plot, width = 7.5)

#pharms#####
list(unique(benchmark_comparison$benchmarks))
pharms <- benchmark_comparison %>% filter(benchmarks == "Pharm_Potential")
pharms_1 <- left_join(cas_class, pharms) %>% filter(!is.na(TQ_max)) %>% distinct()
View(pharms_1) #128 pharm available

pharms_1$TQ_annotation <- ifelse(pharms_1$TQ_max >= 1, "High Priority",
                                             ifelse(pharms_1$TQ_max < 0.001, "Low Priority",
                                                    "Medium Priority"))
pharms_1$TQ_annotation <- as.factor(pharms_1$TQ_annotation)
pharms_1$`General Class` <- as.factor(pharms_1$`General Class`)
summary(pharms_1$TQ_annotation)

# High Priority    Low Priority Medium Priority 
# 1              77              50 

pharms_1 <- pharms_1 %>% arrange(TQ_max)

pharms_1_plot <- pharms_1 %>% ggbarplot( "CAS", "TQ_max",color = "#3b528b", fill = "#3b528b", 
                                         orientation = "horiz") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
pharms_1_plot

ggsave("pharms_1_plot.jpeg", pharms_1_plot)

#QSARs####
list(unique(benchmark_comparison$benchmarks))
QSAR <- benchmark_comparison %>% filter(benchmarks == "QSAR")
QSAR_1 <- left_join(cas_class, QSAR) %>% filter(!is.na(TQ_max)) %>% distinct()
View(QSAR_1) #334 chem available

QSAR_1$TQ_annotation <- ifelse(QSAR_1$TQ_max >= 1, "High Priority",
                                 ifelse(QSAR_1$TQ_max < 0.001, "Low Priority",
                                        "Medium Priority"))
QSAR_1$TQ_annotation <- as.factor(QSAR_1$TQ_annotation)
QSAR_1$`General Class` <- as.factor(QSAR_1$`General Class`)
summary(QSAR_1$TQ_annotation)
# High Priority    Low Priority Medium Priority 
# 5             292              37 

medium_priority_breakdown <- QSAR_1 %>% filter(TQ_annotation == "Medium Priority")
summary(medium_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs              Sterols 
# 3                    8                   17                    0                    5                    4                    0 

low_priority_breakdown <- QSAR_1 %>% filter(TQ_annotation == "Low Priority")
summary(low_priority_breakdown$`General Class`)
# Fire retardants           Fuels/PAHs Industrial/Mixed-Use        Pesticide TPs           Pesticides                PPCPs              Sterols 
# 1                    2                   20                   42                   60                  167                    0 

QSAR_1 <- QSAR_1 %>% arrange(TQ_max)

QSAR_1_plot <- QSAR_1 %>% ggbarplot( "CAS", "TQ_max",color = "General Class", fill = "General Class", 
                                     palette = viridis(n = 7, option = "D", direction = -1), 
                                     orientation = "horiz") +
  rremove("y.text") + rremove("y.ticks") + scale_y_log10()+ xlab("Detected Chemicals (%)
                                            ") + 
  ylab("Maximum Toxicity Quotient (TQ_max)") + geom_hline(yintercept = 1, linetype = "dashed") + geom_hline(yintercept = 0.001, linetype = "dashed")
QSAR_1_plot

ggsave("QSAR_1_plot.jpeg", QSAR_1_plot, width = 7.5)

#exceedence comparisons - venn diagrams and Upset plots####
names(benchmark_comparison)
list(unique(benchmark_comparison$endPoint))
exceedence_comparisons <- benchmark_comparison %>% select(CAS, endPoint, TQ_max, endPoint)
list(unique(exceedence_comparisons$endPoint))

exceedence_comparisons$TQ_annotation <- "Medium Priority"
exceedence_comparisons$TQ_annotation <- ifelse(exceedence_comparisons$TQ_max >= 1,
                                               "High Priority",
                                               ifelse(exceedence_comparisons$TQ_max < 0.001,
                                                      "Low Priority", exceedence_comparisons$TQ_annotation))

exceedence_comparisons$TQ_annotation <- as.factor(exceedence_comparisons$TQ_annotation)
summary(exceedence_comparisons$TQ_annotation)

#make into binary chart####
#high priority same benchmark####
exceedence_comparisons$high_priority_annotation <- ifelse(exceedence_comparisons$TQ_mean >= 1, 1, 0)
exceedence_comparisons$medium_priority_annotation <- ifelse(exceedence_comparisons$TQ_mean >= 0.001 & exceedence_comparisons$TQ_mean < 1, 1, 0)
exceedence_comparisons$low_priority_annotation <- ifelse(exceedence_comparisons$TQ_mean < 0.001, 1, 0)

exceedence_comparisons_high_wide <- exceedence_comparisons %>% select(CAS, endPoint, high_priority_annotation)%>% 
  distinct() %>% spread(key = "endPoint", value = "high_priority_annotation") %>% column_to_rownames("CAS")
exceedence_comparisons_high_wide[is.na(exceedence_comparisons_high_wide)] <- 0 
exceedence_upset_high <- upset(exceedence_comparisons_high_wide, nset = 14, order.by = "freq",
                           mainbar.y.label = "High Priority Compounds")
exceedence_upset_high

exceedence_comparisons_medium_wide <- exceedence_comparisons %>% select(CAS, endPoint, medium_priority_annotation)%>% 
  distinct() %>% spread(key = "endPoint", value = "medium_priority_annotation") %>% column_to_rownames("CAS")
exceedence_comparisons_medium_wide[is.na(exceedence_comparisons_medium_wide)] <- 0 
exceedence_upset_medium <- upset(exceedence_comparisons_medium_wide, nset = 14, order.by = "freq",
                               mainbar.y.label = "Medium Priority Compounds")
exceedence_upset_medium

exceedence_comparisons_low_wide <- exceedence_comparisons %>% select(CAS, endPoint, low_priority_annotation)%>% 
  distinct() %>% spread(key = "endPoint", value = "low_priority_annotation") %>% column_to_rownames("CAS")
exceedence_comparisons_low_wide[is.na(exceedence_comparisons_low_wide)] <- 0 
exceedence_upset_low <- upset(exceedence_comparisons_low_wide, nset = 14, order.by = "freq",
                               mainbar.y.label = "Low Priority Compounds")
exceedence_upset_low

#high priority with different benchmarks####
exceedence_comparisons_priority_by_benchmark <- exceedence_comparisons %>% group_by(endPoint, TQ_annotation) %>%
  summarize(n_CAS = n_distinct(CAS))
names(exceedence_comparisons_priority_by_benchmark)

exceedence_comparisons_priority_by_benchmark_total <- exceedence_comparisons %>% group_by(endPoint) %>%
  summarize(ntotal_CAS = n_distinct(CAS))

exceedence_comparisons_priority_by_benchmark1 <- left_join(exceedence_comparisons_priority_by_benchmark, exceedence_comparisons_priority_by_benchmark_total)
exceedence_comparisons_priority_by_benchmark1$TQ_annotation <- factor(exceedence_comparisons_priority_by_benchmark1$TQ_annotation, levels = c("High Priority", "Medium Priority", "Low Priority"))

write_xlsx(exceedence_comparisons_priority_by_benchmark1, "exceedence_comparisons_priority_by_benchmark.xlsx")

exceedence_comparisons_priority_by_benchmark1 <- exceedence_comparisons_priority_by_benchmark1 %>%
  rename("Priority Group" = "TQ_annotation")
exceedence_comparisons_priority_by_benchmark1$`Priority Group` <- factor(exceedence_comparisons_priority_by_benchmark1$`Priority Group`, levels = c("High Priority", "Medium Priority", "Low Priority"))

exceedence_comparisons_only_high_barplot <- exceedence_comparisons_priority_by_benchmark1 %>% arrange(desc(ntotal_CAS))%>%
  ggbarplot("endPoint", "n_CAS", color = NA, fill = "Priority Group", palette = viridis(n = 3, option = "C"), title = "A") +
  xlab("Ecotoxicological Hazard Concentration") + ylab("Number of Detected Chemicals") + rotate_x_text(angle = 90)
exceedence_comparisons_only_high_barplot
ggsave("exceedence_comparisons_barplot.jpeg", exceedence_comparisons_only_high_barplot, width = 14, height = 10)

list(unique(exceedence_comparisons$endPoint))
names(exceedence_comparisons)
exceedence_comparisons_only_high <- exceedence_comparisons %>% filter(!endPoint %in% c("Tier_2_Unadjusted_BM", "Tier_1_Unadjusted_BM",
                                                                                       "ToxCast - Unadjusted BM", "Cytotox - Unadjusted BM")) %>% distinct()
names(exceedence_comparisons_only_high)

exceedence_comparisons_only_high$priority_group <- ifelse(exceedence_comparisons_only_high$TQ_annotation == "High Priority", 1, 0)
exceedence_comparisons_only_high_wide <- exceedence_comparisons_only_high %>% select(CAS, endPoint, priority_group)%>% 
  distinct() %>% spread(key = "endPoint", value = "priority_group") %>% column_to_rownames("CAS")
exceedence_comparisons_only_high_wide[is.na(exceedence_comparisons_only_high_wide)] <- 0 
names(exceedence_comparisons_only_high_wide)

exceedence_comparisons_only_high_wide <- exceedence_comparisons_only_high_wide %>% 
  rename("AF-adjusted Cytotox" = "Cytotox - AF-adjusted BM", "Pharm_Potential" = "Pharms", 
         "Tier1_AF" = "Tier_1_AF_adjusted_BM", "Tier2_AF" = "Tier_2_AF_adjusted_BM", 
         "AF-adjusted ToxCast" = "ToxCast - AF-adjusted BM")

write_xlsx(exceedence_comparisons_only_high_wide, "exceedence_comparisons_only_high_wide.xlsx")

#upset plot
names(exceedence_comparisons_only_high_wide)
exceedence_upset2 <- upset(exceedence_comparisons_only_high_wide, nset = 9, order.by = "freq",
                            mainbar.y.label = "High Priority Compounds",
                           queries = list(
                             list(query = intersects,
                                  params = list("Screening_Estimate"), 
                                  color = "#440154", 
                                  active = T, 
                                  query.name = "Estimated"),
                             list(query = intersects,
                                  params = list("Tier1_AF"), 
                                  color = "#35b779", 
                                  active = T, 
                                  query.name = "Apical"),
                             list(query = intersects,
                                  params = list("Tier2_AF"), 
                                  color = "#31688e", 
                                  active = T, 
                                  query.name = "Non-Apical"),
                             list(query = intersects,
                                  params = list("Tier1_AF", "Tier2_AF"), 
                                  color = "#75BBB8", 
                                  active = T, 
                                  query.name = "Apical + Non-Apical"),
list(query = intersects,
     params = list("Tier1_AF","Water_Quality_Benchmark", "Screening_Apical", "Tier2_AF"), 
     color = "#5ec962", 
     active = T, 
     query.name = "WQB + Apical + Non-Apical"),
list(query = intersects,
     params = list("Screening_Apical", "Tier2_AF", "Tier1_AF"), 
     color = "#75BBB8", 
     active = T, 
     query.name = "Apical + Non-Apical"),
list(query = intersects,
     params = list("AF-adjusted Cytotox"), 
     color = "#31688e", 
     active = T, 
     query.name = "Non-Apical"),
list(query = intersects,
     params = list("Screening_Apical", "Tier1_AF"), 
     color = "#35b779", 
     active = T, 
     query.name = "Apical"),
list(query = intersects,
     params = list("AF-adjusted ToxCast"), 
     color = "#31688e", 
     active = T, 
     query.name = "Non-Apical"),
list(query = intersects,
     params = list("Tier1_AF", "Tier2_AF", "Screening_Apical", "Screening_Estimate", "Water_Quality_Benchmark"), 
     color = "black", 
     active = T, 
     query.name = "All"),
list(query = intersects,
     params = list("Screening_Apical"), 
     color = "#35b779", 
     active = T, 
     query.name = "Apical"),
list(query = intersects,
     params = list("Screening_Estimate","Tier2_AF"), 
     color = "#3b528b", 
     active = T, 
     query.name = "Non-Apical + Estimated"),
list(query = intersects,
     params = list("Screening_Apical", "Tier2_AF"), 
     color = "#75BBB8", 
     active = T, 
     query.name = "Apical + Non-Apical"),
list(query = intersects,
     params = list("Screening_Estimate","Tier1_AF"), 
     color = "#287c8e", 
     active = T, 
     query.name = "Apical + Estimated"),
list(query = intersects,
     params = list("Water_Quality_Benchmark", "Screening_Apical", "Tier2_AF", "Tier1_AF", "AF-adjusted Cytotox"), 
     color = "#5ec962", 
     active = T, 
     query.name = "WQB + Apical + Non-Apical"),
list(query = intersects,
     params = list("Water_Quality_Benchmark", "Screening_Apical"), 
     color = "#90d743", 
     active = T, 
     query.name = "WQB + Apical"),
list(query = intersects,
     params = list("Screening_Estimate","AF-adjusted ToxCast"), 
     color = "#3b528b", 
     active = T, 
     query.name = "Non-Apical + Estimated"),
list(query = intersects,
     params = list("Screening_Estimate","AF-adjusted Cytotox"), 
     color = "#3b528b", 
     active = T, 
     query.name = "Non-Apical + Estimated"),
list(query = intersects,
     params = list("Water_Quality_Benchmark", "Screening_Apical", "Tier1_AF"), 
     color = "#90d743", 
     active = T, 
     query.name = "WQB + Apical"),
list(query = intersects,
params = list("AF-adjusted ToxCast", "Tier2_AF", "Screening_Apical", "Tier1_AF"), 
color = "#75BBB8", 
active = T, 
query.name = "Apical + Non-Apical"),
list(query = intersects,
                                  params = list("Pharm_Potential"), 
                                  color = "#440154", 
                                  active = T, 
                                  query.name = "Estimated"),
list(query = intersects,
     params = list("AF-adjusted ToxCast", "Screening_Apical"), 
     color = "#75BBB8", 
     active = T, 
     query.name = "Apical + Non-Apical"),
list(query = intersects,
     params = list("AF-adjusted Cytotox", "Screening_Apical"), 
     color = "#75BBB8", 
     active = T, 
     query.name = "Apical + Non-Apical"),
list(query = intersects,
     params = list("Tier2_AF", "AF-adjusted Cytotox"), 
     color = "#31688e", 
     active = T, 
     query.name = "Non-Apical"),
list(query = intersects,
     params = list("Tier2_AF", "Water_Quality_Benchmark"), 
     color = "#443983", 
     active = T, 
     query.name = "Non-Apical + WQB"),
list(query = intersects,
     params = list("AF-adjusted ToxCast","AF-adjusted Cytotox", "Screening_Apical", "Tier2_AF"), 
     color = "#75BBB8", 
     active = T, 
     query.name = "Apical + Non-Apical"),
list(query = intersects,
     params = list("Tier2_AF", "Screening_Apical", "Screening_Estimate", "AF-adjusted Cytotox"), 
     color = "#481f70", 
     active = T, 
     query.name = "Non-Apical + Apical + Estimated"),
list(query = intersects,
     params = list("AF-adjusted ToxCast", "Tier1_AF"), 
     color = "#75BBB8", 
     active = T, 
     query.name = "Apical + Non-Apical"),
list(query = intersects,
     params = list("Water_Quality_Benchmark", "Tier1_AF"), 
     color = "#90d743", 
     active = T, 
     query.name = "WQB + Apical"),
list(query = intersects,
     params = list("AF-adjusted ToxCast", "Screening_Apical", "Tier1_AF"), 
     color = "#21918c", 
     active = T, 
     query.name = "Apical + Non-Apical"),
list(query = intersects,
     params = list("Water_Quality_Benchmark", "Screening_Apical", "Tier1_AF", "AF-adjusted ToxCast"), 
     color = "#5ec962", 
     active = T, 
     query.name = "WQB + Apical + Non-Apical"),
list(query = intersects,
     params = list("Screening_Estimate","Tier1_AF", "Water_Quality_Benchmark", "Screening_Apical"), 
     color = "#20a486", 
     active = T, 
     query.name = "Apical + Estimated + WQB"),
list(query = intersects,
     params = list("Screening_Estimate","Tier1_AF", "Water_Quality_Benchmark", "Screening_Apical", "AF-adjusted ToxCast"), 
     color = "black", 
     active = T, 
     query.name = "all"),
list(query = intersects,
     params = list("Water_Quality_Benchmark", "Tier2_AF", "Tier1_AF"), 
     color = "#5ec962", 
     active = T, 
     query.name = "WQB + Apical + Non-Apical"),
list(query = intersects,
     params = list("Tier2_AF", "Tier1_AF", "Screening_Estimate"), 
     color = "#481f70", 
     active = T, 
     query.name = "Non-Apical + Apical + Estimated"),
list(query = intersects,
     params = list("AF-adjusted Cytotox", "Tier2_AF", "Tier1_AF", "Screening_Apical"), 
     color = "#75BBB8", 
     active = T, 
     query.name = "Apical + Non-Apical"),
list(query = intersects,
     params = list("Tier2_AF","Tier1_AF", "Screening_Apical","Screening_Estimate", "Water_Quality_Benchmark","AF-adjusted Cytotox", "AF-adjusted ToxCast"), 
     color = "black", 
     active = T, 
     query.name = "all"),
))

exceedence_upset2

binary_table1 <- exceedence_upset2$New_data
binary_table1$Non_apical <- ifelse((binary_table1$`Cytotox - AF-adjusted BM` == 1 | binary_table1$Tier2_AF_adjusted== 1| binary_table1$`ToxCast - AF-adjusted BM` == 1) & 
                                     binary_table1$Pharmacological == 0 & binary_table1$QSAR == 0 & binary_table1$Screening_Apical == 0 & binary_table1$Screening_Estimate == 0 &
                                     binary_table1$Tier1_AF_adjusted == 0 & binary_table1$Water_Quality_Benchmarks == 0, "Yes", "No")

list(unique(binary_table1$Non_apical))

non_apical_only <- binary_table1 %>% filter(Non_apical == "Yes") %>% rownames_to_column() %>% rename(CAS = rowname)
non_apical_only1<- left_join(non_apical_only, cas_class)
non_apical_only1$`General Class` <- as.factor(non_apical_only1$`General Class`)
summary(non_apical_only1$`General Class`)


binary_table1$Apical <- ifelse((binary_table1$Tier1_AF_adjusted == 1 | binary_table1$Screening_Apical== 1) & 
                                     binary_table1$Pharmacological == 0 & binary_table1$QSAR == 0 & binary_table1$`Cytotox - AF-adjusted BM` == 0 & binary_table1$Screening_Estimate == 0 &
                                    binary_table1$Water_Quality_Benchmarks == 0 &  binary_table1$`ToxCast - AF-adjusted BM`== 0 & binary_table1$Tier2_AF_adjusted== 0, "Yes", "No")
list(unique(binary_table1$Apical))
apical_only <- binary_table1 %>% filter(Apical == "Yes") %>% rownames_to_column() %>% rename(CAS = rowname)
apical_only1<- left_join(apical_only, cas_class)
apical_only1$`General Class` <- as.factor(apical_only1$`General Class`)
summary(apical_only1$`General Class`)


binary_table1$Estimated <- ifelse((binary_table1$Pharmacological == 1 | binary_table1$QSAR== 1 | binary_table1$Screening_Estimate == 1) & 
                                 binary_table1$Tier1_AF_adjusted == 0 & binary_table1$Screening_Apical == 0 & binary_table1$`Cytotox - AF-adjusted BM` == 0 &
                                 binary_table1$Water_Quality_Benchmarks == 0 &  binary_table1$`ToxCast - AF-adjusted BM`== 0 & binary_table1$Tier2_AF_adjusted== 0, "Yes", "No")
list(unique(binary_table1$Estimated))
estimated_only <- binary_table1 %>% filter(Estimated == "Yes") %>% rownames_to_column() %>% rename(CAS = rowname)
estimated_only1<- left_join(estimated_only, cas_class)
estimated_only1$`General Class` <- as.factor(estimated_only1$`General Class`)
summary(estimated_only1$`General Class`)


binary_table1$WQB <- ifelse((binary_table1$Water_Quality_Benchmarks == 1) & 
                                    binary_table1$Tier1_AF_adjusted == 0 & binary_table1$Screening_Apical == 0 & binary_table1$`Cytotox - AF-adjusted BM` == 0 &
                                    binary_table1$Pharmacological == 0 &  binary_table1$`ToxCast - AF-adjusted BM`== 0 & binary_table1$Tier2_AF_adjusted== 0 &
                              binary_table1$QSAR== 0 & binary_table1$Screening_Estimate == 0, "Yes", "No")
list(unique(binary_table1$WQB))

summary(binary_table1)
View(exceedence_upset2$New_data)

ggsave("high_priority_upset.jpeg", exceedence_upset2, height = 10, width = 12.5)

#pull together a high priority Venn####
names(exceedence_comparisons_only_high)
list(unique(exceedence_comparisons_only_high$endPoint))

exceedence_comparisons_only_high$endPoint <- if_else(exceedence_comparisons_only_high$endPoint %in% c("Tier_2_AF_adjusted_BM", "ToxCast - AF-adjusted BM", "Cytotox - AF-adjusted BM"), "Non-Apical",
                                               ifelse(exceedence_comparisons_only_high$endPoint %in% c("Tier_1_AF_adjusted_BM", "Screening_Apical"), "Apical",
                                                      ifelse(exceedence_comparisons_only_high$endPoint %in% c("QSAR", "Pharms", "Screening_Estimate"), "Estimated",
                                                             exceedence_comparisons_only_high$endPoint)))

list(unique(exceedence_comparisons_only_high$endPoint))
exceedence_comparisons_only_high1 <- exceedence_comparisons_only_high %>% filter(TQ_annotation == "High Priority") %>% group_by(endPoint) %>% summarize(n_avail = sum(priority_group))
exceedence_comparisons_only_high2 <- left_join(exceedence_comparisons_only_high, exceedence_comparisons_only_high1) %>% filter(priority_group != 0)
View(exceedence_comparisons_only_high2)
list(unique(exceedence_comparisons_only_high2$endPoint))

wqb <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Water_Quality_Benchmarks") %>% distinct() %>% select(CAS) %>% unlist()
apical <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Apical") %>% distinct() %>% select(CAS)%>% unlist()
non_apical <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Non-Apical") %>% distinct() %>% select(CAS)%>% unlist()
estimated <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Estimated") %>% distinct() %>% select(CAS)%>% unlist()

venn <- venn.diagram(x = list(wqb, apical, non_apical, estimated),
                     category.names = c("WQB", "Apical", "Non-Apical", "Estimated"),
                     filename = "exceedence.png", output = "TRUE",
                     imagetype="png" ,
                     height = 600 , 
                     width = 600 , 
                     resolution = 300,
                     compression = "lzw",
                     lwd = 1,
                     col=c("#fde725", "#35b779", "#31688e", "#440154"),
                     fill = c(alpha("#fde725",0.3),alpha("#35b779",0.3), alpha("#31688e",0.3), alpha("#440154", 0.3)),
                     cex = 0.5,
                     fontfamily = "sans",
                     cat.cex = 0.3,
                     cat.default.pos = "outer",
                     cat.fontfamily = "sans"
)

#high priority venn with ToxCast exceedence criteria = 10-3# 
names(exceedence_comparisons_only_high)
exceedence_comparisons_only_high <- exceedence_comparisons %>% filter(!endPoint %in% c("Tier_2_Unadjusted_BM", "Tier_1_Unadjusted_BM",
                                                                                       "ToxCast - Unadjusted BM", "Cytotox - Unadjusted BM")) %>% distinct()

exceedence_comparisons_ToxCast_103 <- exceedence_comparisons_only_high
list(unique(exceedence_comparisons_only_high103$endPoint))

exceedence_comparisons_ToxCast_103$TQ_annotation <- ifelse(exceedence_comparisons_ToxCast_103$endPoint == "ToxCast - AF-adjusted BM" & exceedence_comparisons_ToxCast_103$TQ_max >= 0.001, 
                                                         "High Priority",
                                                         ifelse(exceedence_comparisons_ToxCast_103$endPoint != "ToxCast - AF-adjusted BM" & exceedence_comparisons_ToxCast_103$TQ_max >= 1,
                                                                "High Priority", "Medium/Low Priority"))

View(exceedence_comparisons_ToxCast_103)
exceedence_comparisons_ToxCast_103$priority_group <- ifelse(exceedence_comparisons_ToxCast_103$TQ_annotation == "High Priority", 
                                                           1, 0)
exceedence_comparisons_ToxCast_103$endPoint <- if_else(exceedence_comparisons_ToxCast_103$endPoint %in% c("Tier_2_AF_adjusted_BM", "ToxCast - AF-adjusted BM", "Cytotox - AF-adjusted BM"), "Non-Apical",
                                                     ifelse(exceedence_comparisons_ToxCast_103$endPoint %in% c("Tier_1_AF_adjusted_BM", "Screening_Apical"), "Apical",
                                                            ifelse(exceedence_comparisons_ToxCast_103$endPoint %in% c("QSAR", "Pharms", "Screening_Estimate"), "Estimated",
                                                                   exceedence_comparisons_ToxCast_103$endPoint)))


exceedence_comparisons_only_high1 <- exceedence_comparisons_ToxCast_103 %>% filter(TQ_annotation == "High Priority") %>% group_by(endPoint) %>% summarize(n_avail = sum(priority_group))
exceedence_comparisons_only_high2 <- left_join(exceedence_comparisons_ToxCast_103, exceedence_comparisons_only_high1) %>% filter(priority_group != 0)
View(exceedence_comparisons_only_high2)

wqb <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Water_Quality_Benchmarks") %>% distinct() %>% select(CAS) %>% unlist()
apical <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Apical") %>% distinct() %>% select(CAS)%>% unlist()
non_apical <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Non-Apical") %>% distinct() %>% select(CAS)%>% unlist()
estimated <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Estimated") %>% distinct() %>% select(CAS)%>% unlist()

venn <- venn.diagram(x = list(wqb, apical, non_apical, estimated),
                     category.names = c("WQB", "Apical", "Non-Apical", "Estimated"),
                     filename = "exceedence_ToxCast_adjusted.png", output = "TRUE",
                     imagetype="png" ,
                     height = 600 , 
                     width = 600 , 
                     resolution = 300,
                     compression = "lzw",
                     lwd = 1,
                     col=c("#fde725", "#35b779", "#31688e", "#440154"),
                     fill = c(alpha("#fde725",0.3),alpha("#35b779",0.3), alpha("#31688e",0.3), alpha("#440154", 0.3)),
                     cex = 0.5,
                     fontfamily = "sans",
                     cat.cex = 0.3,
                     cat.default.pos = "outer",
                     cat.fontfamily = "sans"
)

#add in QSAR = 10-3 and Pharm = 10-3 
list(unique(exceedence_comparisons$endPoint))
exceedence_comparisons_only_high <- exceedence_comparisons %>% filter(!endPoint %in% c("Tier_2_Unadjusted_BM", "Tier_1_Unadjusted_BM",
                                                                                       "ToxCast - Unadjusted BM", "Cytotox - Unadjusted BM")) %>% distinct()

exceedence_comparisons_ToxCast_103 <- exceedence_comparisons_only_high

exceedence_comparisons_ToxCast_103$TQ_annotation <- ifelse(exceedence_comparisons_ToxCast_103$endPoint %in% c("AF-adjusted ToxCast", "QSAR", "Pharms") & exceedence_comparisons_ToxCast_103$TQ_max >= 0.001, 
                                                           "High Priority",
                                                           ifelse(!exceedence_comparisons_ToxCast_103$endPoint %in% c("AF-adjusted ToxCast", "QSAR", "Pharm_Potential") & exceedence_comparisons_ToxCast_103$TQ_max >= 1,
                                                                  "High Priority", "Medium/Low Priority"))

exceedence_comparisons_ToxCast_103$priority_group <- ifelse(exceedence_comparisons_ToxCast_103$TQ_annotation == "High Priority", 
                                                            1, 0)
exceedence_comparisons_ToxCast_103$endPoint <- if_else(exceedence_comparisons_ToxCast_103$endPoint %in% c("Tier_2_AF_adjusted_BM", "ToxCast - AF-adjusted BM", "Cytotox - AF-adjusted BM"), "Non-Apical",
                                                       ifelse(exceedence_comparisons_ToxCast_103$endPoint %in% c("Tier_1_AF_adjusted_BM", "Screening_Apical"), "Apical",
                                                              ifelse(exceedence_comparisons_ToxCast_103$endPoint %in% c("QSAR", "Pharms", "Screening_Estimate"), "Estimated",
                                                                     exceedence_comparisons_ToxCast_103$endPoint)))

exceedence_comparisons_only_high1 <- exceedence_comparisons_ToxCast_103 %>% filter(TQ_annotation == "High Priority") %>% group_by(endPoint) %>% summarize(n_avail = sum(priority_group))
exceedence_comparisons_only_high2 <- left_join(exceedence_comparisons_ToxCast_103, exceedence_comparisons_only_high1) %>% filter(priority_group != 0)

wqb <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Water_Quality_Benchmarks") %>% distinct() %>% select(CAS) %>% unlist()
apical <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Apical") %>% distinct() %>% select(CAS)%>% unlist()
non_apical <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Non-Apical") %>% distinct() %>% select(CAS)%>% unlist()
estimated <- exceedence_comparisons_only_high2 %>% filter(endPoint == "Estimated") %>% distinct() %>% select(CAS)%>% unlist()

venn <- venn.diagram(x = list(wqb, apical, non_apical, estimated),
                     category.names = c("WQB", "Apical", "Non-Apical", "Estimated"),
                     filename = "exceedence_multiple_adjusted.png", output = "TRUE",
                     imagetype="png" ,
                     height = 600 , 
                     width = 600 , 
                     resolution = 300,
                     compression = "lzw",
                     lwd = 1,
                     col=c("#fde725", "#35b779", "#31688e", "#440154"),
                     fill = c(alpha("#fde725",0.3),alpha("#35b779",0.3), alpha("#31688e",0.3), alpha("#440154", 0.3)),
                     cex = 0.5,
                     fontfamily = "sans",
                     cat.cex = 0.3,
                     cat.default.pos = "outer",
                     cat.fontfamily = "sans"
)
#Environmental Fate####
e_fate <- read_excel("C:\\Users\\erinm\\OneDrive\\Desktop\\GLRI Project\\GLRI Chemical Prioritization\\Environmental Fate\\efate_24_05_2022_EM.xlsx")

efate_class <- left_join(cas_class, e_fate)
efate_class$data_availability <- paste(efate_class$P_note, efate_class$B_note, sep = "_")

#plots
names(efate_class)
efate_class$`t1/2 (d)` <- as.numeric(efate_class$`t1/2 (d)`)
efate_class$`BCF (L/kg)` <- as.numeric(efate_class$`BCF (L/kg)`)

efate_plot <- efate_class %>% ggscatter("t1/2 (d)", "BCF (L/kg)", color ="General Class", palette = viridis(n = 7, direction = -1), size = 4) + geom_hline(yintercept = 2000, linetype = "dashed") +
  geom_hline(yintercept = 5000, linetype = "dashed") + geom_vline(xintercept = 40, linetype = "dashed") + geom_vline(xintercept = 60, linetype = "dashed") + scale_y_log10() +  
  xlab("
  Aquatic Half-Life (t1/2, d)") + ylab("Bioconcentration Factor (BCF, L/kg)
                                             ") + scale_x_log10()
efate_plot

ggsave("efate_plot.jpeg", efate_plot, width = 8, height = 8)

efate_class$`Final Classification` <- as.factor(efate_class$`Final Classification`)

efate_class_n_class <- efate_class %>% group_by(`General Class`) %>% summarize(n_class = n_distinct(CAS))

efate_class <- left_join(efate_class, efate_class_n_class)%>% distinct()
efate_class$n_class1 <- 1

B_chemical_coverage <- efate_class %>% select(-data_availability) %>% rename("Data Availability" = "B_note") %>% distinct() %>% arrange(desc(n_class))%>% 
  ggbarplot("General Class","n_class1", fill = "Data Availability", color = "Data Availability",palette = viridis(n = 3, option = "D", direction = 1), title = "A")+
  ylab("Number of Detected Chemicals") + ylim(0, 200) + xlab("Chemical Class
                                                             ")
B_chemical_coverage

P_chemical_coverage <- efate_class %>% select(-data_availability) %>% rename("Data Availability" = "P_note")  %>% arrange(desc(n_class))%>% 
  ggbarplot("General Class", "n_class1", fill = "Data Availability",color = "Data Availability", palette = viridis(n = 3, option = "D", direction = 1), title = "B")+
  ylab("Number of Detected Chemicals") + ylim(0, 200) + xlab("Chemical Class
                                                             ")
P_chemical_coverage

efate_plot2 <- grid.arrange(B_chemical_coverage, P_chemical_coverage, nrow = 2)
ggsave("efate_chemical_coverage.jpeg", efate_plot2, height = 10, width = 12)

#summaries for text
#B
efate_class$B_note <- as.factor(efate_class$B_note)
summary(efate_class$B_note)
# Data Limited    Estimated     Measured 
# 2          261           71 
# 2+261+71 = 334
# 
# 261/334 = 78% estimated
# 71/334 = 21 % measured
# 2/334 = 0.6% data limited

#class summaries
efate_class$`General Class` <- as.factor(efate_class$`General Class`)
summary(efate_class$`General Class`)

n_class <- efate_class %>% group_by(`General Class`) %>% summarize(n_distinct(CAS))

n_dl <- efate_class %>% group_by(`General Class`, B_note) %>% summarize(class_DL = n_distinct(CAS))

class_dl_summary <- left_join(n_dl, n_class)
class_dl_summary$per_class <- class_dl_summary$class_DL/class_dl_summary$`n_distinct(CAS)` * 100

# `General Class`      B_note       class_DL `n_distinct(CAS)` per_class
# <fct>                <fct>           <int>             <int>     <dbl>
#   1 Fire retardants      Measured            4                 4    100   
# 2 Fuels/PAHs           Estimated           1                10     10   
# 3 Fuels/PAHs           Measured            9                10     90   
# 4 Industrial/Mixed-Use Estimated          23                38     60.5 
# 5 Industrial/Mixed-Use Measured           15                38     39.5 
# 6 Pesticide TPs        Estimated          41                42     97.6 
# 7 Pesticide TPs        Measured            1                42      2.38
# 8 Pesticides           Estimated          37                65     56.9 
# 9 Pesticides           Measured           28                65     43.1 
# 10 PPCPs                Data Limited        2               171      1.17
# 11 PPCPs                Estimated         155               171     90.6 
# 12 PPCPs                Measured           14               171      8.19
# 13 Sterols              Estimated           4                 4    100  

#P
efate_class$P_note <- as.factor(efate_class$P_note)
summary(efate_class$P_note)
# Data Limited    Estimated     Measured 
# 2          323            9 
# 2+323+9

n_dl <- efate_class %>% group_by(`General Class`, P_note) %>% summarize(class_DL = n_distinct(CAS))

class_dl_summary <- left_join(n_dl, n_class)
class_dl_summary$per_class <- class_dl_summary$class_DL/class_dl_summary$`n_distinct(CAS)` * 100

# `General Class`      P_note       class_DL `n_distinct(CAS)` per_class
# <fct>                <fct>           <int>             <int>     <dbl>
#   1 Fire retardants      Estimated           4                 4   100    
# 2 Fuels/PAHs           Estimated           1                10    10    
# 3 Fuels/PAHs           Measured            9                10    90    
# 4 Industrial/Mixed-Use Data Limited        1                38     2.63 
# 5 Industrial/Mixed-Use Estimated          37                38    97.4  
# 6 Pesticide TPs        Estimated          42                42   100    
# 7 Pesticides           Estimated          65                65   100    
# 8 PPCPs                Data Limited        1               171     0.585
# 9 PPCPs                Estimated         170               171    99.4  
# 10 Sterols              Estimated           4                 4   100 

#exceedences
efate_class$`Final Classification` <- as.factor(efate_class$`Final Classification`)
summary(efate_class$`Final Classification`)
med_priority <- efate_class %>% filter(`Final Classification` %in% c("PnB", "nPB"))
med_priority$`General Class` <- as.factor(med_priority$`General Class`)
summary(med_priority$`General Class`)

hi_priority <- efate_class %>% filter(`Final Classification` %in% c("vPB", "vPvB", "vPnB"))
hi_priority$`General Class` <- as.factor(hi_priority$`General Class`)
summary(hi_priority$`General Class`)


#Overall HeatMap - Qualitative Comparison of Priority Categorizations #####
ecotox <- exceedence_comparisons %>% select(-benchmarks) %>% rename("benchmarks"= "endPoint") %>%
  select(-TQ_max)
names(ecotox)

names(e_fate)
list(unique(e_fate$EFate))

P <- efate_class %>% select(CAS, "t1/2 (d)") %>% rename("Half_life_days" = "t1/2 (d)")
P$benchmarks <- "Persistence"
P$TQ_annotation <- ifelse(P$Half_life_days < 40, "Low Priority",
                          ifelse(P$Half_life_days >= 60, "High Priority",
                                 ifelse(P$Half_life_days >= 40 & P$Half_life_days < 60, "Medium Priority",
                                        "Data Limited")))

P$TQ_annotation <- ifelse(is.na(P$TQ_annotation), "Data Limited", P$TQ_annotation)
list(unique(P$TQ_annotation))
P_1 <- P %>% select(-Half_life_days)

B <- efate_class %>% select(CAS, "BCF (L/kg)") %>% rename("BCF_Lkg" = "BCF (L/kg)")
B$benchmarks <- "Bioaccumulation"
B$TQ_annotation <- ifelse(B$BCF_Lkg < 2000, "Low Priority",
                          ifelse(B$BCF_Lkg >= 5000, "High Priority", 
                                 ifelse(B$BCF_Lkg >= 2000 & B$BCF_Lkg < 5000, "Medium Priority",
                                        ifelse(is.na(B$BCF_Lkg), "Data Limited", 
                                               "misc"))))
B$TQ_annotation <- ifelse(is.na(B$TQ_annotation), "Data Limited", B$TQ_annotation)
B_1 <- B %>% select(-BCF_Lkg)

comparison_data <- bind_rows(ecotox, P_1, B_1)
comparison_data$benchmarks <- gsub("Pharms", "Pharmacological", comparison_data$benchmarks)
list(unique(comparison_data$benchmarks))

comparison_data_1 <- left_join(comparison_data, cas_class)
comparison_data_1$TQ_annotation <- as.factor(comparison_data_1$TQ_annotation)
comparison_data_1


comparison_data_1$priority_score <- ifelse(comparison_data_1$TQ_annotation == "High Priority", 10,
                                           ifelse(comparison_data_1$TQ_annotation == "Medium Priority", 5, 
                                                  ifelse(comparison_data_1$TQ_annotation == "Low Priority", 0,
                                                         ifelse(comparison_data_1$TQ_annotation == "Data Limited", -5,
                                                                "misc"))))
list(unique(comparison_data_1$priority_score))
comparison_data_1$priority_score <- as.numeric(comparison_data_1$priority_score)

comparison_data_sum <- comparison_data_1 %>% group_by(CAS) %>% summarise(TQ_total = sum(priority_score)) %>% filter(!is.na(CAS))

comparison_data_2 <- left_join(comparison_data_1, comparison_data_sum)%>% filter(!is.na(CAS))

comparison_data_heatmap_predata <- comparison_data_2 %>% arrange(desc(TQ_total)) %>% filter(!is.na(CAS))%>% 
  select(CAS, priority_score, benchmarks) %>% distinct() %>% spread(key = benchmarks, value = priority_score) %>% 
  column_to_rownames("CAS") %>% relocate("Water_Quality_Benchmarks", .before = "Bioaccumulation") %>%
  relocate("Screening_Apical", .after = "Water_Quality_Benchmarks") %>%
  relocate("Tier1_unadjusted", .after = "Screening_Apical") %>% 
  relocate("Tier1_AF_adjusted", .after = "Tier1_unadjusted") %>% 
  relocate("Tier2_unadjusted", .after = "Tier1_AF_adjusted") %>% 
  relocate("Tier2_AF_adjusted", .after = "Tier2_unadjusted") %>% 
  relocate("ToxCast - Unadjusted BM", .after = "Tier2_AF_adjusted") %>% 
  relocate("ToxCast - AF-adjusted BM", .after = "ToxCast - Unadjusted BM") %>%
  relocate("Cytotox - Unadjusted BM", .after = "ToxCast - AF-adjusted BM") %>% 
  relocate("Cytotox - AF-adjusted BM", .after = "Cytotox - Unadjusted BM") %>% 
  relocate("Screening_Estimate", .after = "Cytotox - AF-adjusted BM") %>% 
  relocate("Pharmacological", .after = "Screening_Estimate") %>% 
  relocate("QSAR", .after = "Pharmacological") %>%
  relocate("Persistence", .after = "QSAR") %>%
  relocate("Bioaccumulation", .after="Persistence") %>% 
  rename("Cytotox_AF_adjusted" = "Cytotox - AF-adjusted BM", "Cytotox_unadjusted" = "Cytotox - Unadjusted BM",
         "ToxCast_AF_adjusted" = "ToxCast - AF-adjusted BM", "ToxCast_unadjusted" =  "ToxCast - Unadjusted BM")

comparison_data_heatmap_predata[is.na(comparison_data_heatmap_predata)] <- -5

comparison_data_heatmap_predata1 <- as.matrix(comparison_data_heatmap_predata)
names(comparison_data_heatmap_predata)

comparison_data_heatmap_predata_class <- comparison_data_2 %>% arrange(desc(TQ_total)) %>% filter(!is.na(CAS))%>% 
  select(CAS, "General Class", TQ_total, priority_score, benchmarks) %>% distinct() %>% spread(key = benchmarks, value = priority_score) %>% arrange(desc(TQ_total)) %>% filter(!is.na(CAS)) %>% select(CAS, "General Class") %>% distinct() %>% ungroup() %>%
  select("General Class")%>% rename("Chemical Classification" = "General Class")
list(unique(comparison_data_heatmap_predata_class$`Chemical Classification`))
comparison_data_heatmap_predata_class <- as.matrix(comparison_data_heatmap_predata_class)

row_ha = rowAnnotation(Classification = comparison_data_heatmap_predata_class, col = list(Classification = c("Pesticides" = "darkred",
                                                                                                             "Pesticide TPs" = "red",
                                                                                       "PPCPs" = "darkblue",
                                                                                       "Sterols" = "purple",
                                                                                       "Fuels/PAHs" = "gold",
                                                                                       "Fire retardants" = "orange",
                                                                                       "Industrial/Mixed-Use" = "gold2")))

benchmark_heatmap <- Heatmap(comparison_data_heatmap_predata1, col = c("black", "#440154", "#21918c", "#fde725"), column_title_side = "bottom", 
                         row_title_side = "left", left_annotation = row_ha, show_heatmap_legend = TRUE,
                         row_title = "Detected Chemical", column_title = "Benchmark", show_row_names = FALSE,
                         show_column_dend = FALSE, show_row_dend = FALSE, 
                         column_order = c("Water_Quality_Benchmarks", "Screening_Apical", "Tier1_unadjusted",
                                          "Tier1_AF_adjusted", "Tier2_unadjusted", "Tier2_AF_adjusted", "ToxCast_unadjusted",
                                          "ToxCast_AF_adjusted", "Cytotox_unadjusted", "Cytotox_AF_adjusted",
                                          "Screening_Estimate", "Pharmacological", "QSAR", "Persistence", "Bioaccumulation"))
benchmark_heatmap
draw(water_heatmap)

cas_chnm <- chem_list %>% select(CAS, `Chemical Name`)

qualitative_comparison <- left_join(comparison_data_2, cas_chnm) %>% select(-priority_score)
qualitative_comparison_wide <- qualitative_comparison %>% distinct() %>% spread(key = benchmarks, value = TQ_annotation)
qualitative_comparison_wide[is.na(qualitative_comparison_wide)] <- "Data Limited"

write_xlsx(qualitative_comparison_wide, "qualitative_comparison_wide.xlsx")

#qualitative benchmark exceedence comparison# 
comparison_data_extra <- comparison_data_1 %>% distinct()
names(comparison_data_extra)

comparison_data_extra$priority_score <- ifelse(comparison_data_extra$TQ_annotation == "High Priority", 10,
                                           ifelse(comparison_data_extra$TQ_annotation == "Medium Priority", 5, 
                                                  ifelse(comparison_data_extra$TQ_annotation == "Low Priority", 0,
                                                         ifelse(comparison_data_extra$TQ_annotation == "Data Limited",0,
                                                                "misc"))))
list(unique(comparison_data_extra$priority_score))
comparison_data_extra$priority_score <- as.numeric(comparison_data_extra$priority_score)

comparison_data_sum_xtra <- comparison_data_extra %>% group_by(CAS) %>% distinct() %>% summarise(TQ_total = sum(priority_score)) %>% filter(!is.na(CAS))


comparison_data_2 <- left_join(comparison_data_extra, comparison_data_sum_xtra)%>% filter(!is.na(CAS))
cas_chnm <- chem_list %>% select(CAS, `Chemical Name`)

qualitative_comparison1 <- left_join(comparison_data_2, cas_chnm) %>% select(-priority_score)
qualitative_comparison_wide <- qualitative_comparison %>% distinct() %>% spread(key = benchmarks, value = TQ_annotation)
qualitative_comparison_wide[is.na(qualitative_comparison_wide)] <- "Data Limited"

write_xlsx(qualitative_comparison_wide, "qualitative_comparison_wide_pt2.xlsx")
names(qualitative_comparison_wide)

qualitative_comparison_wide_long1 <- qualitative_comparison_wide %>% gather(Bioaccumulation:Water_Quality_Benchmarks, key = "benchmark", value = "priority_category") %>% filter(priority_category == "High Priority") %>%
  group_by(CAS) %>% summarize(n_hp = n_distinct(benchmark))

qualitative_comparison_wide_long <- qualitative_comparison_wide %>% gather(Bioaccumulation:Water_Quality_Benchmarks, key = "benchmark", value = "priority_category") %>% filter(priority_category != "Data Limited") %>%
  group_by(CAS) %>% summarize(n_bench = n_distinct(benchmark))

qualitative_comparison_wide_long2 <- left_join(qualitative_comparison_wide_long, qualitative_comparison_wide_long1)

qualitative_comparison_3 <- left_join((qualitative_comparison_wide %>% select(CAS, `General Class`, TQ_total, `Chemical Name`)), qualitative_comparison_wide_long2)
qualitative_comparison_3$n_hp <- ifelse(is.na(qualitative_comparison_3$n_hp), 0, qualitative_comparison_3$n_hp)

write_xlsx(qualitative_comparison_3, "qualitative_comparison_wide_pt3.xlsx")


#exceedence comparisons
exceedence_comparisons_only_high1 <- exceedence_comparisons_only_high %>% filter(TQ_annotation == "High Priority") %>% select(CAS, endPoint)
exceedence_comparisons_only_high2 <- left_join(exceedence_comparisons_only_high1, cas_class)

exceedence_comparisons_only_high2$BM_group <- ifelse(exceedence_comparisons_only_high2$endPoint %in% c("Cytotox - AF-adjusted BM", "ToxCast - AF-adjusted BM", "Tier2_AF_adjusted"), "Non-apical", "Misc")
exceedence_comparisons_only_high2$BM_group <- ifelse(exceedence_comparisons_only_high2$endPoint %in% c("Screening_Estimate", "QSAR", "Pharmacological"), "Estimated", exceedence_comparisons_only_high2$BM_group)
exceedence_comparisons_only_high2$BM_group <- ifelse(exceedence_comparisons_only_high2$endPoint %in% c("Screening_Apical", "Tier1_AF_adjusted"), "Apical", exceedence_comparisons_only_high2$BM_group)
exceedence_comparisons_only_high2$BM_group <- ifelse(exceedence_comparisons_only_high2$endPoint %in% c("Water_Quality_Benchmarks"), "WQB", exceedence_comparisons_only_high2$BM_group)
list(unique(exceedence_comparisons_only_high2$BM_group))
exceedence_comparisons_only_high2$BM_group_yes <- exceedence_comparisons_only_high2$BM_group

names(exceedence_comparisons_only_high2)
qualitative_comparison_group_wide <- exceedence_comparisons_only_high2 %>% select(-c(endPoint)) %>% distinct() %>% spread(key = BM_group, value = BM_group_yes)
qualitative_comparison_group_wide$comp_bench <- paste(qualitative_comparison_group_wide$Apical, qualitative_comparison_group_wide$Estimated,qualitative_comparison_group_wide$`Non-apical`, qualitative_comparison_group_wide$WQB)
list(unique(qualitative_comparison_group_wide$comp_bench))

qualitative_comparison_group_wide$comp_bench <- gsub("NA NA Non-apical NA", "Non-apical", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("NA Estimated Non-apical NA", "Estimated_Non-apical", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("Apical NA Non-apical NA", "Apical_Non-apical", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("Apical NA NA NA", "Apical", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("NA Estimated NA NA", "Estimated", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("Apical Estimated Non-apical WQB", "Apical_Estimated_Non-apical_WQB", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("Apical NA NA WQB", "Apical_WQB", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("NA NA Non-apical WQB", "Non-apical_WQB", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("NA Estimated Non-apical WQB", "Estimated_Non-apical_WQB", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("NA NA NA WQB", "WQB", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("Apical NA Non-apical WQB", "Apical_NonApical_WQB", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("Apical Estimated Non-apical WQB", "Apical_Estimated_NonApical", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("Apical Estimated Non-apical NA", "Apical_Estimated_NonApical", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("Apical Estimated NA NA", "Apical_Estimated", qualitative_comparison_group_wide$comp_bench)
qualitative_comparison_group_wide$comp_bench <- gsub("Apical Estimated NA WQB", "Apical_Estimated_WQB", qualitative_comparison_group_wide$comp_bench)

list(unique(qualitative_comparison_group_wide$comp_bench))

View(qualitative_comparison_group_wide)

#qual comparison plot
qual_comparison <- read_excel("qualitative_comparison_wide_pt2_EM.xlsx", "for_graph")
names(qual_comparison)

qual_comparison_plot <- qual_comparison %>% ggscatter("DL_score", "PS") + geom_hline(yintercept = 25, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") + geom_vline(xintercept = 25, linetype = "dashed") + geom_vline(xintercept = 50, linetype = "dashed") + xlab("Percent of Benchmarks Available (%)") +
  ylab("Prioritization Score (%)")
qual_comparison_plot

