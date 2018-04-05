library(sas7bdat)
library(dplyr)
library(tidyr)
library(data.table)
library(xlsx)
library(lubridate)
setwd("C:\\Users\\mengbing\\Box Sync\\Optum Insight - Data Management")

##### READ IN SAS DATASET
patinfo <- read.csv("subset_v4_nodup.csv")
patinfo <- readRDS("subset_v4_nodup.rds")
# patinfo <- patinfo[1:1000,]
# CONVERT SAS DATE INTO YYYY-MM-DD
patinfo$patid <- as.character(patinfo$patid)
patinfo$fst_dt <- as.Date(patinfo$fst_dt, origin="1960-01-01")
patinfo$index_dt <- as.Date(patinfo$index_dt, origin="1960-01-01")
patinfo$index_cancer_dt <- as.Date(patinfo$index_cancer_dt, origin="1960-01-01")
# SAVE THE CSV FILE AS RDS
saveRDS(patinfo,"subset_v4_nodup.rds")

# EXPORT SAS DATASET TO CSV
write.csv(patinfo, file = "subset_v4.csv")



#####################################################################################
####                                DATA SUMMARY                                 ####
#####################################################################################

##### CALCULATE THE NUMBER OF PATIENTS BY INDEX CANCER TYPE
# CONSIDER MULTIPLE CANCER AS A VECTOR
n_cancertype <- as.data.frame(patinfo %>%
      group_by(cancer_type) %>%
      summarise(Count = n_distinct(patid)) %>%
      arrange(desc(Count)) %>% 
      mutate(freq = paste0(round(100 * Count/sum(Count), 3), "%")))

# CONSIDER MULTIPLE CANCER AS SINGLE CANCERS
patinfo_long <- patinfo %>%
      mutate(cancer_type = strsplit(as.character(cancer_type), ", ")) %>% 
      unnest(cancer_type)
n_cancertype_single <- as.data.frame(patinfo_long %>%
                           group_by(cancer_type) %>%
                           summarise(Count = n_distinct(patid)) %>%
                           arrange(desc(Count)) %>% 
                           mutate(freq = paste0(round(100 * Count/sum(Count), 3), "%")))



##### COUNT THE NUMBER OF PATIENTS BY ANTICOAGULANT PATTERNS (FIRST 3)
# SELECT ROWS WITH AC AFTER INDEX VTE
patinfo_aftervte <- data.table(patinfo %>% filter(fst_dt >= index_dt) %>%
                                 select(patid, fst_dt, category))
dup <- duplicated(patinfo_aftervte) | duplicated(patinfo_aftervte, fromLast = TRUE)
patinfo_aftervte_dup <- patinfo_aftervte[dup]
n_dup_ac <- as.data.frame(patinfo_aftervte_dup %>%
                            group_by(patid, fst_dt, category) %>%
                            summarise(Count = n()) %>%
                            arrange(patid))

# EXCLUDE ROWS OF A PATIENT GETTING MULTIPLE DOSES OF THE SAME AC
patinfo_aftervte_uniq <- data.table(patinfo_aftervte %>% distinct)
uniq_fst3 <- patinfo_aftervte_uniq[, .SD[1:3], by=patid]
uniq_acpattern <- aggregate(category~patid, data=uniq_fst3, paste, collapse = ", ")
n_uniq_acpattern <- as.data.frame(uniq_acpattern %>%
                           group_by(category) %>%
                           summarise(Count = n_distinct(patid)) %>%
                           arrange(desc(Count)) %>% 
                           mutate(freq = paste0(round(100 * Count/sum(Count), 3), "%")))


# OUTPUT THE TABLES 
write.xlsx(n_cancertype, file = "summary_stats.xlsx", sheetName="n_by_cancer_vector",
           row.names=FALSE)
write.xlsx(n_cancertype_single, file = "summary_stats.xlsx", append=TRUE,
           sheetName="n_by_cancer_single", row.names=FALSE)
write.xlsx(n_dup_ac, file = "summary_stats.xlsx", append=TRUE,
           sheetName="n_dup_ac", row.names=FALSE)
write.xlsx(n_uniq_acpattern, file = "summary_stats.xlsx", append=TRUE,
           sheetName="n_by_acpattern", row.names=FALSE)



#####################################################################################
####                             IDENTIFY AC PATTERN                             ####
#####################################################################################

### SUM UP COPAYS FOR THE SAME AC CATEGORY ON THE SAME DATE
# CALCULATE THE SUM OF COPAYS BY PATID AND FST_DT, AND REMOVE DUPLICATE ROWS
patinfo2 <- patinfo %>%
  group_by(patid, fst_dt, category) %>%
  mutate(Copay_sum = sum(Copay)) %>%
  ungroup(fst_dt, category) %>%
  distinct(patid, fst_dt, category, .keep_all=TRUE)


### CHECK IF THERE ARE WARFARIN AND LMWH ON THE SAME DATE. IF YES, REMOVE THE ROW
### WITH LMWH
# IDENTIFY LMWH THAT APPEARS ON THE SAME DATE AS WARFARIN
patinfo2 <- patinfo2 %>%
  dplyr::select(patid, fst_dt, category, gen_name, Copay_sum, index_dt, index_cancer_dt, Days_Sup) %>%
  group_by(patid,fst_dt) %>%
  mutate('warf_lmwh'=(('Warfarin' %in% category) & (category=='LMWH'))) %>%
  ungroup(fst_dt)
# REMOVE ROWS WITH SUCH LMWH
patinfo2 <- patinfo2[!patinfo2$warf_lmwh, !names(patinfo2) %in% c('warf_lmwh')]


### CHECK FOR GAPS GREATER THAN DAYS_SUP + 28 DAYS BETWEEN TWO ADJACENT FST_DT
# CALCULATE THE DATE OF DAYS_SUP+28 DAYS PAST THE CORRESPONDING FILL DT
patinfo3 <- patinfo2 %>% mutate(fst_sup = fst_dt+Days_Sup, up_dt = fst_dt+Days_Sup+28) 
# SAVE A COPY OF THIS DATA SET
write.csv(patinfo3, "patinfo_rm_lmwh.csv")
saveRDS(patinfo3, "patinfo_rm_lmwh.rds")
rm(patinfo2)
# IDENTIFY FILL DATE PAST THE FLAG DATE
patinfo3 <- patinfo3 %>% group_by(patid) %>%
  mutate(no_fill=(fst_dt>lag(up_dt) & !is.na(fst_dt >= lag(up_dt))) )
# OUTPUT THE TWO TABLES 
write.csv(patinfo3, file = "check_gaps.csv")
# REMOVE ALL ROWS OF THE PATIENT WITH SUCH A FILL DATE
patinfo3 <- patinfo3 %>% group_by(patid) %>%
  filter(all(no_fill==FALSE)) %>%
  dplyr::select(-no_fill)


### GET THE MOST RECENT AC
# CALCULATE THE NUMBER OF MONTHS OF FILL DATE AFTER INDEX VTE DATE
patinfo4 <- patinfo3 %>% mutate(mon_afterVTE = as.numeric(fst_dt-index_dt)%/%30)
rm(patinfo3)
# FIND THE MOST RECENT AC FOR EACH PATIENT IN EACH MONTH
patinfo4 <- patinfo4 %>%
  group_by(patid, mon_afterVTE) %>%
  filter(fst_dt==max(fst_dt))

### OUTPUT THE DATA SET TO CSV FILE
write.csv(patinfo4, file = "ac_pattern.csv")



##########             AS OF 3/22/2018            ##########
### INCLUDE MORE INR CODES INTO THE SHINY PLOT

### SUM UP COPAYS FOR THE SAME AC CATEGORY ON THE SAME DATE
# CALCULATE THE SUM OF COPAYS BY PATID AND FST_DT, AND REMOVE DUPLICATE ROWS
patinfo <- readRDS("subset_v4_nodup.rds")

patinfo2 <- patinfo %>%
  group_by(patid, fst_dt, category) %>%
  mutate(Copay_sum = sum(Copay)) %>%
  ungroup(fst_dt, category) %>%
  distinct(patid, fst_dt, category, .keep_all=TRUE)

### CHECK FOR GAPS GREATER THAN DAYS_SUP + 28 DAYS BETWEEN TWO ADJACENT FST_DT
# CALCULATE THE DATE OF DAYS_SUP+28 DAYS PAST THE CORRESPONDING FILL DT
patinfo2 <- patinfo2 %>% mutate(fst_sup = fst_dt+Days_Sup, up_dt = index_dt+90) 



#####################################################################################
####                  CREATE WORKING DATA SET FOR THE SHINY APP                  ####
#####################################################################################

### CREATE NUMERIC SCALE ON THE Y AXIS FOR AC CATEGORIES
patinfo <- patinfo2 %>%
  filter(fst_dt >= index_dt) %>%
  group_by(patid, category) %>%
  mutate(id = (category=="DOACS")*seq(from=45, by=-0.25, length.out=n()) +
           (category=="LMWH")*seq(from=30, by=-0.25, length.out=n()) +
           (category=="Warfarin")*seq(from=15, by=-0.25, length.out=n()),
         category_num = 36*(category=="DOACS") + 24*(category=="LMWH") + 8*(category=="Warfarin"),
         fst_sup = fst_dt+Days_Sup, up_dt = fst_dt+Days_Sup+28) %>%
  ungroup(category)
# SAVE THE DATA SET
saveRDS(patinfo, "working_data_added.rds")

### CREATE RDS FILE FOR INR DATA
inr_info2 <- read.csv("lab_inr_added.csv")
inr_info <- inr_info2 %>% mutate(patid = as.character(patid), 
                                index_dt = as.Date(index_dt),
                                inr_dt = as.Date(inr_dt)) %>%
  filter(toupper(Tst_Desc) != "PROTHROMBIN TIME",
         index_dt <= inr_dt,
         patid %in% unique(patinfo$patid))
# SAVE THE DATA SET
saveRDS(inr_info, "inr_info_added.rds")


### MANIPULATE LAB_INR TO ADD ARROWS ON INR DATES IN THE SHINY PLOT
inr_info <- readRDS("inr_info.rds")

inr_info$patid <- as.factor(inr_info$patid)
patid_levels <- levels(inr_info$patid)     
inr_info$inr_num <- rep(0,nrow(inr_info))
inr_info$category <- rep("0",nrow(inr_info))

# INITIALIZE A VECTOR TO STORE THE VALUE FOR INR ON THE Y AIS
for( patid in patid_levels ){
  sub_inr <- inr_info[inr_info$patid==patid,]
  sub_patinfo <- patinfo[patinfo$patid==patid,]
  fst_dt_c <- sub_patinfo$fst_dt
  fstsup_dt_c <- sub_patinfo$fst_sup
  id_c <- sub_patinfo$id
  category_c <- as.character(sub_patinfo$category)
  inr_dt_c <- sub_inr$inr_dt
  n_fst <- length(fst_dt_c)
  n_inr <- length(inr_dt_c)
  sub_inr_num <- rep(0,n_inr)
  sub_inr_category <- rep("0", n_inr)
  
  for(i in 1:n_inr){
    inr_dt_i <- inr_dt_c[i]
    max_dt <- max(fstsup_dt_c)
    
    if (inr_dt_i <= max_dt) {
      tmp <- (inr_dt_i >= fst_dt_c & inr_dt_i <= fstsup_dt_c)
      
      # INR HAPPENS BEFORE THE LAST END OF SUPPLY DATE
      # CASE 1: INR HAPPENS BETWEEN FILL DATE AND END OF SUPPLY DATE
      if (any(tmp)) {
        # FIND THE INDEX OF THE FIRST OVERLAPPING OCCURRENCE
        min_tmp <- which.max(tmp)
        sub_inr_num[i] <- id_c[min_tmp]
        sub_inr_category[i] <- category_c[min_tmp]
      }
      
      # CASE 2: INR DOES NOT HAPPEN BETWEEN THE OVERLAPPING PERIOD OF 
      # ANY FILL DATE AND END OF SUPPLY DATE
      else if (!any(tmp)) {
        tmp2 <- (inr_dt_i > fstsup_dt_c & inr_dt_i > fst_dt_c)
        min_tmp <- which.min(tmp2)
        sub_inr_num[i] <- id_c[min_tmp]
        sub_inr_category[i] <- category_c[min_tmp]
      }
    }
    
    # CASE 3: INR HAPPENS AFTER THE LAST END OF SUPPLY DATE
    else {
      min_tmp <- which.max(fstsup_dt_c)
      sub_inr_num[i] <- id_c[min_tmp]
      sub_inr_category[i] <- category_c[min_tmp]
    }
    
  }
  inr_info$inr_num[inr_info$patid==patid] <- sub_inr_num
  inr_info$category[inr_info$patid==patid] <- sub_inr_category
}

inr_info$category <- as.factor(inr_info$category)

# SAVE THE DATA SET
saveRDS(inr_info, "working_data_inr_added.rds")











#######################################################################################
####                                                                               ####
#### PREPARE A WORKING DATA SET FOR FURTHER ANALYSIS. THIS DATA SET SHOULD CONTAIN ####
#### ALL VARIABLES FROM THE SUBSET DATA AND VARIABLES NEEDED FOR TABLE 1. THESE    ####
#### INCLUDE PATID, FST_DT, CATEGORY, BRAND_NAME, COPAY, INDEX_DT,                 ####
#### INDEX_CANCER_DT, CANCER_TYPE, DAYS_SUP, YRDOB, GDR_CD, RACE, ADMIT_DATE,      ####
#### DIAG1-DIAG5 (FROM CONF), DISCH_DT, SMOKE (INDICATOR, NA), TST_DESC            ####
#### (HEMOGLOBIN, PLATELETS, GFR), VTE_HISTORY (INDICATOR), ANTIPLATELETS (NA),    ####
#### VTE_CATEGORY, COMORBIDITIES (?)                                               ####
####                                                                               ####
#######################################################################################

########### ADD GENDER, YRDOB, RACE, DIVISION FROM MEMBER_DETAILS
######### FOR PATIENTS WITH INCONSISTENT INFORMATION ON THESE VARIABLES, USE THE 
######### ONE CLOSEST IN TIME TO INDEX VTE DATE

########### ADD ADMIT_DATE, DIAG1-DIAG5, DISCH_DATE FROM CONF

########## ADD TST_DESC FROM LAB

########## WHERE DO AMOKE, VTE HISTORY, VTE_CATEGORY, AND COMORBIDITIES COME FROM???




### 1. ADD GENDER, YRDOB, RACE, DIVISION FROM MEMBER_DETAILS ------------------------------------------------
member <- fread("member.txt", select=c("Patid","index_dt","Eligeff", "Eligend", "Gdr_Cd","Yrdob","RACE_CD","Division"))
### FOR PATIENTS WITH INCONSISTENT INFORMATION ON THESE VARIABLES, USE THE ONE WITH ELIGEFF <= INDEX_DT <= ELIGEND
member1 <- member %>%
  distinct(Patid, index_dt, Eligeff, Eligend, Gdr_Cd, Yrdob, RACE_CD, Division, keep_all=TRUE) %>%
  mutate(Patid = as.character(Patid),
         index_dt = as.Date(strptime(index_dt, "%m/%d/%Y")),
         Eligeff = as.Date(Eligeff),
         Eligend = as.Date(Eligend)) %>%
  filter(Eligeff <= index_dt & Eligend >= index_dt) %>%
  select(-c(Eligeff, Eligend))

### CHECK FOR INCONCSISTENT INFORMATION FOR EACH PATIENT
member_dup <- member1 %>% group_by(Patid) %>% filter(n() > 1) # 0 duplicated observation

names(member1) <- c("patid", "index_dt", "gender", "yrdob", "race", "division")
# SAVE FOR FUTURE USE
saveRDS(member1, "member_demographics.rds")

### ADD DEMOGRAPHICS TO FINAL DATA SET
patinfo <- readRDS("subset_v4_nodup.rds")
patinfo2 <- patinfo %>% select(-c(Quantity, Strength))
names(patinfo2)[c(4,5,9)] <- c("brand_name", "copay", "days_sup")
patinfo3 <- merge(x = patinfo2, y = member1, by = "patid", all.x = TRUE) %>%
  mutate(index_dt = index_dt.x) %>%
  select(-c(index_dt.y, index_dt.x))
saveRDS(patinfo3, "full_data_1.rds")





### THE FOLLOWING CODES WERE USED TO CREATE A CSV FILE FOR CHECKING INCONSISTENCIES ----------------------------------------
### CHECK FOR INCONSISTENCY IN GENDER
member_check <- function(x){
  member1 %>% select(Patid, x) %>% distinct() %>% group_by(Patid) %>% filter(n() > 1) 
}
# CHECK FOR INCONSISTENCY IN INDEX_DT
member_index_dt <- member_check("index_dt")
# CHECK FOR INCONSISTENCY IN GENDER
member_gdr <- member_check("Gdr_Cd")
# CHECK FOR INCONSISTENCY IN YRDOB
member_Yrdob <- member_check("Yrdob")
# CHECK FOR INCONSISTENCY IN RACE_CD
member_race <- member_check("RACE_CD")
# CHECK FOR INCONSISTENCY IN DIVISION
member_division <- member_check("Division")

# member1 <- member1 %>% mutate(index_dt_check = (Patid %in% member_index_dt$Patid),
#                               gdr_check = (Patid %in% member_gdr$Patid),
#                               Yrdob_check = (Patid %in% member_Yrdob$Patid),
#                               race_check = (Patid %in% member_race$Patid),
#                               division_check = (Patid %in% member_division$Patid))
# member1[,7:11] <- sapply(member1[,7:11], as.numeric)

member_details <- fread("member_details.txt")
# GET PATID FOR THOSE WITH INCONSISTENT INFORMATION
dup_patid <- unique(c(member_index_dt$Patid,
               member_gdr$Patid,
               member_Yrdob$Patid,
               member_race$Patid,
               member_division$Patid))
member2 <- member_details %>% mutate(Patid = as.character(Patid),
                             Pat_PlanId = as.character(Pat_PlanId)) %>%
  # KEEP ONLY THOSE WITH INCONSISTENCY
  filter(Patid %in% unique(member_dup$Patid)) %>%
  # CREATE INDICATOR FOR INCONSISTENT VALUES IN EACH VARIABLE
  mutate(index_dt_check = (Patid %in% member_index_dt$Patid),
         gdr_check = (Patid %in% member_gdr$Patid),
         Yrdob_check = (Patid %in% member_Yrdob$Patid),
         race_check = (Patid %in% member_race$Patid),
         division_check = (Patid %in% member_division$Patid))
member2[,17:21] <- sapply(member2[,17:21], as.numeric)
member3 <- merge(x=member2, y=unique(member1[,c("Patid","index_dt")]), by.x = "Patid",
                 all.x = TRUE)


# OUTPUT THE MEMBER_DUP TABLE
write.csv(member3, file = "table1_stats.csv", row.names=FALSE)
rm(member,member_division, member_dup, member_gdr, member_index_dt, member_race, member_Yrdob)









### CALCULATION OF SUMMARY STATISTICS FOR TABLE 1 -----------------------------------------------------------
 
### READ IN PATINFO
patinfo_full <- readRDS("full_data_1.rds")
patinfo_full2 <- patinfo_full %>%
  mutate(brand_name = as.character(brand_name), category = as.character(category)) %>%
  mutate(male = as.numeric(gender=="M"),
         gen_name = case_when(
           brand_name %in% c("Fragmin", "Lovenox") ~ "LMWH",
           brand_name == "Warfarin" ~ "Warfarin",
           brand_name == "Eliquis" ~ "Apixaban",
           brand_name == "Pradaxa" ~ "Dabigatran",
           brand_name == "Savaysa" ~ "Edoxaban",
           brand_name == "Xarelto" ~ "Rivaroxaban",
           ((category == "DOACS") && (!brand_name %in% c("Eliquis", "Pradaxa", "Savaysa", "Xarelto"))) ~ "Other DOACS",
           TRUE ~ "Other"
         ),
         age = year(index_dt) - yrdob) 

# SUMMARIZE % SEX AND AVERAGE AGE BY AC CATEGORY
table1_sex_age <- patinfo_full2 %>%
  group_by(gen_name) %>% 
  summarise(ac_count = n(), freq = round(n()/nrow(patinfo_full2),5),
            male_count = sum(male), male_perc = sum(male) / n(),
            age_mean = mean(age), age_sd = sd(age))

# SUMMARIZE % INDEX CANCER TYPE BY AC CATEGORY
# CONVERT MULTIPLE CANCER
table2_mag <- patinfo_full2 %>%
  group_by(cancer_type, gen_name) %>%
  summarise(count_by_cancer = n(), freq = round(n()/nrow(patinfo_full2),5))


  
  
  
  

#########################################################################################
####                               FIND CENSORING DATE                               ####
#### DEFINITION:                                                                     ####
#### 1). Find INR test dates for an individual with warfarin prescription;           ####
#### 2). Identify the date of INR test, after which the next INR test is >30 days    ####
####   later (count as recurrence), AND no warfarin (based on the date of the        ####
####   previous warfarin refill and days of supply); Call it t_INR                   ####
#### 3). Compute t_INR+30; define this as the censoring date. Discard information    ####
####   after t_INR+30 from our analyses.                                             ####
#########################################################################################
patinfo <- readRDS("subset_v4_nodup.rds")
inr_info <- readRDS("inr_info.rds")

### COMPUTE THE DIFFERENCE BETWEEN THE CURRENT INR DATE AND THE NEXT INR DATE
inr_info <- inr_info %>% group_by(patid) %>% mutate(d_inr = inr_dt - lag(inr_dt))
inr_info$d_inr[is.na(inr_info$d_inr)] <- 0

patid_all <- unique(patinfo$patid)
patid_inr <- unique(patinfo$patid)
sum(! patid_all %in% patid_inr)











