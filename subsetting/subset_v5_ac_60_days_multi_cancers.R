#######################################################################################
####                                                                               ####
#### 1. EXCLUDE PATIENTS WHO DO NOT GET AN OUTPATIENT AC WITHIN 60 DAYS AFTER      ####
####    INDEX VTE                                                                  ####
#### 2. CONVERT MULIPLE CANCERS INTO SINGLE CATEGORIES:                            ####
#### stomach + any other cancer --> stomach,                                       ####
#### pancreas + any other cancer --> pancreas,                                     ####
#### stomach + pancreas --> pancreas,                                              ####
#### lung/lymphoma/gynecologic/bladder/testicular cancer + any other non-stomach   ####
####   or pancreas cancer --> the named cancer (either lung, lymphoma, gynecologic,####
####   bladder, testicular (essentially ignore the other non stomach/pancreas      ####
####   cancer),                                                                    ####
#### any combination not listed above --> multiple cancers  .                      ####
####                                                                               ####
#######################################################################################

library(sas7bdat)
library(dplyr)
library(tidyr)
library(xlsx)


setwd("C:/Users/Mengbing Li/Box Sync/Optum Insight - Data Management/subsetting")

### READ IN COMPLETED SUBSET DATA SET
patinfo <- read.sas7bdat("../data/subset_v5_ac_active_nodup.sas7bdat")


### 1. EXCLUDE PATIENTS WHO DO NOT GET AN OUTPATIENT AC WITHIN 60 DAYS --------

# KEEP ROWS WITH AC WITHIN 60 DAYS AFTER INDEX VTE
patid_60 <- patinfo %>%
  filter(fst_dt <= index_dt+60) %>%
  select(patid) %>%
  distinct(patid)

# KEEP ONLY PATIENTS WHO GOT AC WITHIN 60 DAYS AFTER INDEX VTE
patinfo2 <- patinfo %>%
  filter(patid %in% patid_60$patid)







### 2. CONVERT MULIPLE CANCERS INTO SINGLE CATEGORIES -----------------------

# CONVERT WIDE FORMAT WITH MULTILE CANCERS INTO LONG FORMAT WITH ONE CANCER PER ROW
patinfo_long <- patinfo2 %>%
  mutate(cancer_type = strsplit(as.character(cancer_type), ", ")) %>% 
  unnest(cancer_type) %>%
  select(patid, cancer_type) %>% 
  distinct() %>%
  arrange(patid)

# EXTRACT SINGLE CANCERS
patinfo_singlecancer <- patinfo_long %>%
  group_by(patid) %>%
  filter(n()==1) %>%
  mutate(cancer_type_combined = cancer_type)%>%
  select(-cancer_type)

# ONLY CONSIDER MULTIPLE CANCERS
patinfo_multicancer <- patinfo_long %>%
  group_by(patid) %>%
  filter(n()>1)

# SCORE EACH CANCER TYPE
patinfo_multicancer <- patinfo_multicancer %>%
  mutate(score = case_when(
    cancer_type == "Pancreas" ~ 20,
    cancer_type == "Stomach" ~ 10,
    cancer_type %in% c("Lung", "Lymphoma", "Gynecologic","Bladder", "Testicular") ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(patid) %>%
  mutate(score_sum = sum(score))

# IMPLEMENT THE MULTIPLE CANCER ALGORITHM
patinfo_cancers <- patinfo_multicancer %>%
  mutate(cancer_type_combined = case_when(
    score_sum >= 20 ~ "Pancreas",
    (score_sum >=10) & (score_sum <20) ~ "Stomach",
    (score_sum == 1) & (score == 0) ~ "",
    (score_sum == 1) & (score == 1) ~ cancer_type,
    TRUE ~ "Multiple Cancers"
  )) 

# KEEP GOOD ROWS
patinfo_multicancer2 <- patinfo_cancers %>%
  filter(cancer_type_combined != "") %>% 
  distinct(patid, cancer_type_combined) 
cancer_info <- rbind(patinfo_singlecancer,patinfo_multicancer2)
patinfo3 <- merge(x=patinfo2, y=cancer_info, by="patid", all.x = TRUE)
patinfo3 <- patinfo3[,c(1:8,12,9:11)]


# WRITE TABLE OUTPUT
write.csv(patinfo3, file = "../data/subset_v5_ac_60_days_multi_cancers.csv",
           row.names=FALSE)
saveRDS(patinfo3, "../data/subset_v5_ac_60_days_multi_cancers.rds")




