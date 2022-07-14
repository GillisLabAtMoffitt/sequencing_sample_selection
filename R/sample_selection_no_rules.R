# Impor library
library(tidyverse)
library(data.table)
library(lubridate)


#######################################################################################  I  ### version 4.5----


### Load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar", "MM avatar")

# 1.1.Load Germline with disease status 
Germline <- readxl::read_xlsx(paste0(path, 
                                     "/Raghu MM/Germline data/Avatar_MM_03162021_OUT.xlsx")) %>% 
  filter(DiseaseType == "Not applicable (germline)") %>% 
  select(c("avatar_id", SLID_germline = "DNASequencingLibraryID", collectiondt_germline = "collectiondt", 
           moffittSampleId_germline = "ORIENSpecimenID", Disease_Status_germline = "disease_status"))

# 1.2.Load Sequencing data
WES_tumor <-
  readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/Moffitt_WES_v0.4.3_Disease_Classification_OUT01312020.xlsx")) %>% 
  select(c("avatar_id", "moffitt_sample_id", "collectiondt")) %>% 
  `colnames<-`(c("avatar_id", "moffitt_sample_id_tumor", "collectiondt_tumor"))

#---
Sequencing <-
  read.delim(paste0(path, "/Jamie/v0.4.3.MM.samples.WESdata01.31.20.txt")) %>% 
  select(c(
    "SLID_germline", 
    "SLID_tumor" , "moffitt_sample_id_tumor", 
    "moffitt_sample_id_germline",
    "BaitSet"))
# Sequencing$moffitt_sample_id_tumor == Sequencing$moffitt_sample_id # yes so remove one var
# Sequencing$subject == Sequencing$avatar_id # yes so remove one var
#---
Seq_WES_Raghu <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/MM_Metadata_WES_V044.xlsx")) %>% 
  select(c(avatar_id = "subject", 
           "SLID_germline", "moffitt_sample_id_germline", "collectiondt_germline", 
           "SLID_tumor" , "moffitt_sample_id_tumor", "collectiondt_tumor", 
           "BaitSet"))
# Keep
# Seq_WES_Raghu$moffitt_sample_id_tumor == Seq_WES_Raghu$moffitt_sample_id # yes so rename and remove one var
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$ClinicalSpecimenLinkage_WES # yes
# Seq_WES_Raghu$SLID_tumor == Seq_WES_Raghu$SLID # yes
# Seq_WES_Raghu$subject == Seq_WES_Raghu$ClinicalSpecimenLinkage_subject # yes
# Seq_WES_R$collectiondt_germline == Seq_WES_R$collectiondt_tumor # No -> That's good
#---
Sequencing2 <- # warning message due to a TRUE added in a num var by Raghu (he copy paste an extra patient)
  readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/MM_Metadata_WES_V0441.xlsx")) %>% 
  select(c(avatar_id = "subject",
           "SLID_germline", "moffittSampleId_germline",
           "collectiondt_germline",
           "SLID_tumor" , moffitt_sample_id_tumor = "moffittSampleId_tumor", "collectiondt_tumor",
           BaitSet = "baitSet"))
#---
Seq_WES_Raghu2 <- 
  readxl::read_xlsx(paste0(path, "/Raghu MM/Germline data/MM_Metadata_WES_V045.xlsx")) %>% 
  select(c(avatar_id = "subject", 
           "SLID_germline", "moffittSampleId_germline", "collectiondt_germline", 
           "SLID_tumor" , moffitt_sample_id_tumor = "moffittSampleId_tumor", "collectiondt_tumor", 
           "BaitSet"))

# 1.2.Load Clinical data
# V1 and V2 in V4 format---
Clinical_V12_legacy <-
  fs::path(
    "",
    "Volumes",
    "Gillis_Research",
    "Christelle Colin-Leitzinger",
    "CHIP in Avatar", "MM avatar",
    "Raghu MM",
    "extracted Avatar V124 data and dict",
    "V1V2 legacy V4"
  )

#---
MM_history_Vlegacy <-
  readxl::read_xlsx((paste0(Clinical_V12_legacy, "/Avatar_MM_Clinical_Data_Legacy_V4_10192021.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "mrn", "date_of_diagnosis", "histology", "hematological_malignancy_phase"))
#---


# V1 and V2 in V4 format---
ClinicalCap_V12 <-
  fs::path(
    "",
    "Volumes",
    "Gillis_Research",
    "Christelle Colin-Leitzinger",
    "CHIP in Avatar", "MM avatar",
    "Raghu MM",
    "extracted Avatar V124 data and dict",
    "V1V2 verified by CIOX in V4 format"
  )
#---
MM_history_V12 <-
  readxl::read_xlsx((paste0(ClinicalCap_V12, "/Avatar_Legacy_V4_modif_09282020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "mrn", "date_of_diagnosis", "histology", "hematological_malignancy_phase"))
#---
uid_MM <- paste(unique(MM_history_V12$avatar_id), collapse = '|')

MM_history_Vlegacy <- MM_history_Vlegacy[(!grepl(uid_MM, MM_history_Vlegacy$avatar_id)),]

MM_history_V12 <-
  bind_rows(MM_history_Vlegacy, MM_history_V12)

uid_MM <- paste(unique(MM_history_V12$avatar_id), collapse = '|')

# V4 ---
ClinicalCap_V4 <-
  fs::path(
    "",
    "Volumes",
    "Gillis_Research",
    "Christelle Colin-Leitzinger",
    "CHIP in Avatar", "MM avatar",
    "Raghu MM",
    "extracted Avatar V124 data and dict",
    "V4"
  )
#---
MM_historyV4 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_modif_04272020.xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis", "histology", "hematological_malignancy_phase"))
#---
MM_historyV4.1 <-
  readxl::read_xlsx((paste0(ClinicalCap_V4, "/Avatar_MM_Clinical_Data_V4_OUT_08032020 .xlsx")),
                    sheet = "Myeloma_Disease_History") %>%
  select(c("avatar_id", "date_of_diagnosis", "histology", "hematological_malignancy_phase"))

Diagnosis_ISS <- readxl::read_xlsx(paste0(path, "/Raghu MM/Other raw data/Staging_MM_Diagnosis_07262021_OUT.xlsx")) %>%
  mutate(iss = str_replace(iss, pattern = "Unknown/Not Reported", replacement = NA_character_)) %>%
  select(avatar_id, last_mrn = mrn, MM_date_dx = date_of_diagnosis, ISS_at_MMdx = iss)


### Merge WES and Sequencing----
## For 1st sequencing file

### Bind Germline
Germline <- Germline %>% 
  distinct()

# One of the sequencing data is in 2 part so merge that first
# Are moffitt_sample_id are equal in WES and Sequencing ?
# Sequencing <- Sequencing[order(Sequencing$moffitt_sample_id),]
# WES <- WES[order(WES$moffitt_sample_id),]
# Sequencing$moffitt_sample_id == WES$moffitt_sample_id # =>>>>>>> YES
Sequencing <-
  full_join(
    Sequencing,
    WES_tumor,
    by = "moffitt_sample_id_tumor")
# Bind Sequencing
Seq_WES <- bind_rows(Seq_WES_Raghu, Sequencing2, Seq_WES_Raghu2, Sequencing, .id = "vers") %>% 
  arrange(collectiondt_germline) %>% 
  distinct(SLID_tumor, moffitt_sample_id_tumor, SLID_germline, .keep_all = TRUE)

# duplicated(WES_seq$moffitt_sample_id_tumor) # No duplicate
# duplicated(WES_seq$avatar_id) # has duplicate so
# Reshape to have duplicate ID on same row (per date) but
# Really important to order by dates otherwise cannot find the duplicated lines
Seq_WES <- Seq_WES[order(Seq_WES$collectiondt_tumor), ]
# pivot wider
WES_seq <-
  dcast(setDT(Seq_WES), avatar_id+SLID_germline+moffitt_sample_id_germline+collectiondt_germline ~ rowid(avatar_id),
        value.var = c(
          "SLID_tumor",
          "moffitt_sample_id_tumor",
          "collectiondt_tumor",
          "BaitSet"
        )
  )

# Merge all
Germline <- left_join(WES_seq, Germline, by = "avatar_id") %>% 
  # To eliminate the duplicate with 2 slid and date for A108 patient
  filter(SLID_germline.x == SLID_germline.y | is.na(SLID_germline.x == SLID_germline.y)) %>%
  rename(SLID_germline = "SLID_germline.x", collectiondt_germline = "collectiondt_germline.x") %>% 
  # distinct(avatar_id, SLID_germline, .keep_all = TRUE) %>% 
  mutate(collectiondt_germline = coalesce(collectiondt_germline, collectiondt_germline.y)) %>% 
  select(-SLID_germline.y, -collectiondt_germline.y)

# Cleaning
rm(Sequencing, Sequencing2, WES_tumor, WES_seq, Seq_WES_Raghu, Seq_WES, Seq_WES_Raghu2)


### Clean patient history----
### Create a wider format for each to facilitate date comparison

# Create function to code disease stage for latest version and fit to older version
history_disease <- function(data){
  data <- data %>% 
    filter(histology == 97651 | histology == 97323) %>% 
    mutate(disease_stage = case_when(
      hematological_malignancy_phase == 1         ~ "active",
      hematological_malignancy_phase == 2         ~ "smoldering",
      histology == 97651                          ~ "mgus"
    )) %>% 
    select(-c(hematological_malignancy_phase, histology))
}
MM_history_V12 <- history_disease(MM_history_V12)
MM_historyV4 <- history_disease(MM_historyV4)
MM_historyV4.1 <- history_disease(MM_historyV4.1)

# Change status for patients that Nancy checked
id <- paste("A022604","A027407","A029244","A007364","A000238",
            "A000530","A007146","A010533","A016764", sep = "|")
# Add last Raghu date of MM diagnosis
Dx_date <- Diagnosis_ISS %>% select("avatar_id", "last_mrn", date_of_diagnosis = "MM_date_dx") %>% 
  mutate(disease_stage = "active")

mm_history <- bind_rows(MM_history_V12, #MM_history, MM_historyV2,
                        MM_historyV4, MM_historyV4.1,
                        Dx_date) %>%
  drop_na("date_of_diagnosis") %>%
  mutate(mrn = coalesce(mrn, last_mrn)) %>% 
  group_by(avatar_id) %>% 
  fill(mrn, .direction = "downup") %>% 
  ungroup() %>% 
  distinct(avatar_id, date_of_diagnosis, disease_stage, .keep_all = TRUE) %>% 
  arrange(avatar_id, date_of_diagnosis) %>% 
  # Change status for patients that Nancy checked
  mutate(disease_stage = case_when(
    str_detect(avatar_id, id) &
      disease_stage == "active"   ~ "wrongly classified", 
    TRUE                          ~ disease_stage
  )) %>% 
  # mutate(date_of_diagnosis = case_when(
  #   str_detect(avatar_id, id) &
  #     disease_stage == "wrongly classified"   ~ NA_POSIXct_, 
  #   TRUE                                      ~ date_of_diagnosis)) %>% 
  
  # code smoldering diagnosis date
  group_by(avatar_id) %>% 
  mutate(sm_date_diagnosis = case_when(
    disease_stage == "smoldering"           ~ date_of_diagnosis,
    TRUE                                    ~ NA_POSIXct_
  )) %>% 
  arrange(avatar_id, sm_date_diagnosis) %>% 
  mutate(sm_date_diagnosis = first(sm_date_diagnosis)) %>% 
  ungroup() %>% 
  
  # Code the first active/relapse date of diagnosis as MM diagnosis
  arrange(avatar_id, date_of_diagnosis) %>% 
  group_by(avatar_id) %>% 
  mutate(date_of_MM_diagnosis = case_when(
    str_detect(disease_stage, "active|relapse")        ~ date_of_diagnosis,
    TRUE                                               ~ NA_POSIXct_
  )) %>% 
  arrange(avatar_id, date_of_MM_diagnosis) %>% 
  mutate(date_of_MM_diagnosis = first(date_of_MM_diagnosis)) %>% 
  ungroup() %>% 
  arrange(avatar_id, date_of_diagnosis) %>% 
  
  mutate(is_patient_MM = case_when(
    !is.na(date_of_MM_diagnosis)           ~ "Yes",
    is.na(date_of_MM_diagnosis)            ~ "No"
  )) %>% 
  
  # Create Dx_date_closest_blood date (general diagnosis, in not specific MM)
  left_join(., Germline %>% select("avatar_id", "collectiondt_germline"), # For only 1 date of Dx when multiple germline collection
            by = "avatar_id") %>% 
  # For 180
  mutate(closest_date_180_cleaning = date_of_diagnosis-collectiondt_germline) %>% 
  group_by(avatar_id, date_of_diagnosis, disease_stage) %>% 
  arrange(closest_date_180_cleaning) %>% 
  distinct(avatar_id, date_of_diagnosis, .keep_all = TRUE) %>% 
  ungroup() %>% 
  
  mutate(interval = (interval(start= .$collectiondt_germline, end= .$date_of_diagnosis)/duration(n=1, unit="days"))) %>% 
  mutate(interval = if_else(interval>100, NA_real_, interval)) %>% 
  mutate(interval1 = abs(interval)) %>% 
  arrange(interval1) %>% 
  group_by(avatar_id) %>% mutate(id = 1:n()) %>% ungroup() %>%
  mutate(Dx_date_closest_germline = if_else(id == 1, date_of_diagnosis, NA_POSIXct_)) %>% 
  distinct(avatar_id, date_of_diagnosis, .keep_all = TRUE) %>% 
  group_by(avatar_id) %>% fill(Dx_date_closest_germline, .direction = "updown") %>% 
  arrange(avatar_id, date_of_diagnosis) %>% 
  ungroup() %>% 
  
  # code actual status
  mutate(smoldering_status = case_when(
    !is.na(date_of_MM_diagnosis) &
      !is.na(sm_date_diagnosis)        ~ "Progressed from Smoldering",
    !is.na(date_of_MM_diagnosis) &
      is.na(sm_date_diagnosis)         ~ "Never Smoldering",
    is.na(date_of_MM_diagnosis) &
      !is.na(sm_date_diagnosis)        ~ "Is Smoldering", 
    TRUE                               ~ NA_character_
  ))


MM_history <- dcast(setDT(mm_history), 
                    avatar_id+collectiondt_germline+date_of_MM_diagnosis+is_patient_MM+
                      Dx_date_closest_germline+
                      sm_date_diagnosis+smoldering_status ~ 
                      rowid(avatar_id), 
                    value.var = c("date_of_diagnosis", "disease_stage")) %>% 

  mutate(date_of_MMSMMGUSdiagnosis = case_when(
    disease_stage_1 == "smoldering"      ~ date_of_diagnosis_1,
    disease_stage_2 == "smoldering"      ~ date_of_diagnosis_2,
    disease_stage_3 == "smoldering"      ~ date_of_diagnosis_3,
    disease_stage_4 == "smoldering"      ~ date_of_diagnosis_4,
    disease_stage_1 == "mgus"            ~ date_of_diagnosis_1,
    disease_stage_2 == "mgus"            ~ date_of_diagnosis_2,
    disease_stage_3 == "mgus"            ~ date_of_diagnosis_3,
    disease_stage_4 == "mgus"            ~ date_of_diagnosis_4
  )) %>% 
  mutate(date_of_MMSMMGUSdiagnosis = coalesce(date_of_MM_diagnosis, date_of_MMSMMGUSdiagnosis)) %>% 
  mutate(interval_MM = interval(start= date_of_MM_diagnosis, end= collectiondt_germline)/duration(n=1, unit="days")) %>% 
  mutate(is_MMDx_close_to_blood = case_when(
    is_patient_MM == "Yes" &
      interval_MM < -60                  ~ "No",
    is_patient_MM == "Yes"               ~ "Yes",
    TRUE                                 ~ NA_character_
  )) %>% 
  select(c("avatar_id", "Dx_date_closest_germline", "date_of_MM_diagnosis", "is_patient_MM", 
           "sm_date_diagnosis", "smoldering_status", "date_of_MMSMMGUSdiagnosis", 
           interval_MM, is_MMDx_close_to_blood, everything(), -collectiondt_germline))

# write.csv(MM_history,paste0(path, "/simplified files/MM_history simplify.csv"))


# Cleaning
rm(ClinicalCap_V12, ClinicalCap_V4, 
   uid_MM,
   Dx_date
)

### Merge germline / patient history----
patients_removed_nonMM <- c("A000428", "A000456")
germline_v4_5 <- full_join(Germline %>%  select(c("avatar_id", "moffittSampleId_germline", "SLID_germline",
                                                "collectiondt_germline", "Disease_Status_germline", 
                                                starts_with("SLID_tumor"), starts_with("collectiondt_tumor_"), 
                                                                                       starts_with("moffitt_sample_id_tumor")
                                                )),
                         MM_history, by = "avatar_id") %>% 
  filter(!str_detect(avatar_id, paste0(patients_removed_nonMM, collapse = "|")))
# write.csv(germline_v4.5, paste0(path, "/germline_v4_5.csv"))
write_rds(germline_v4_5, file = "germline_v4_5.rds")


#######################################################################################  II  # version 4.6----

### Load data----
WES_jan2022 <- readxl::read_xlsx(paste0(path, 
                                        "/data/WES/Moffitt_WES_V046_All.xlsx")) %>% 
  select(c(avatar_id, SLID_germline, collectiondt_germline, SLID_tumor, collectiondt_tumor,
           moffittSampleId_germline, moffittSampleId_tumor, moffittSampleId,
           DNASequencingLibraryID,
           mrn, Disease_Status))


### Merge WES and Sequencing----
WES_jan2022 <- WES_jan2022 %>% 
  arrange(avatar_id, collectiondt_germline, collectiondt_tumor)

germline_v4.6 <- dcast(setDT(mm_history), 
                    avatar_id+mrn+date_of_MM_diagnosis+is_patient_MM+
                      sm_date_diagnosis+smoldering_status ~ 
                      rowid(avatar_id), 
                    value.var = c("date_of_diagnosis", "disease_stage")) %>% 
  
  mutate(date_of_MMSMMGUSdiagnosis = case_when(
    disease_stage_1 == "mgus"            ~ date_of_diagnosis_1,
    disease_stage_2 == "mgus"            ~ date_of_diagnosis_2,
    disease_stage_3 == "mgus"            ~ date_of_diagnosis_3,
    disease_stage_4 == "mgus"            ~ date_of_diagnosis_4
  )) %>% 
  mutate(date_of_MMSMMGUSdiagnosis = 
           coalesce(date_of_MM_diagnosis, 
                    sm_date_diagnosis, 
                    date_of_MMSMMGUSdiagnosis)) %>% 
  
  # Select germline closest to Dx of interest (MM, then Sm, then Mgus)
  left_join(., WES_jan2022 %>% 
              select("avatar_id", "collectiondt_germline", 
                     "SLID_germline", "moffittSampleId_germline", 
                     Disease_Status_germline = "Disease_Status") %>% 
              distinct(), # For only 1 date of Dx when multiple germline collection
            by = "avatar_id") %>% 
  
  
  # mutate(remove_germline = case_when(
  #   collectiondt_germline > date_of_MMSMMGUSdiagnosis     ~ "remove"
  # )) %>% 
  # filter(is.na(remove_germline)) %>% 
  # select(-remove_germline) %>% 
  
  
  mutate(interval_germline_dx =(interval(start= collectiondt_germline, 
                                             end= date_of_MMSMMGUSdiagnosis)/
                                      duration(n=1, unit="days"))) %>% 
  # arrange(interval_germline_dx) %>% 
  # distinct(avatar_id, .keep_all = TRUE) %>% 
  mutate(is_MMDx_close_to_blood = case_when(
    is_patient_MM == "Yes" &
      interval_germline_dx < 60          ~ "No",
    is_patient_MM == "Yes"               ~ "Yes",
    TRUE                                 ~ NA_character_
  )) %>% 
  # Select tumor closest to germline
  left_join(., WES_jan2022 %>% 
              select("avatar_id", "collectiondt_tumor",
                     "SLID_tumor", "moffittSampleId_tumor", 
                     Disease_Status_tumor = "Disease_Status") %>% 
              distinct(),
            by = "avatar_id") %>% 
  
  # mutate(remove_tumor = case_when(
  #   collectiondt_tumor > date_of_MMSMMGUSdiagnosis     ~ "remove"
  # )) %>% 
  # filter(is.na(remove_tumor)) %>% 
  # select(-remove_tumor) %>% 
  
  
  mutate(interval_germline_tummor = abs(interval(start = collectiondt_germline, 
                                                 end = collectiondt_tumor) /
                                          duration(n=1, units = "days"))) %>% 
  # arrange(interval_germline_tummor) %>% 
  # distinct(avatar_id, .keep_all = TRUE)
  filter(!is.na(collectiondt_germline))

### Merge germline / patient history----

write_rds(germline_v4.6, file = "germline_v4.6.rds")


#######################################################################################  III  # version 4.7----

### Load data----
path2 <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                  "CH all tumor types")
sample_data_v4_7 <- 
  readxl::read_xlsx(paste0(path2, "/raw data/CDSC/v4.6and4.7/10R22000169_20220624_outfile.xlsx"),
                    sheet = "CDSC-AvatarMasterList_SDR-2 ",
                    na = "NULL") %>% 
  janitor::clean_names()

### Merge WES and Sequencing----
germline_v4_7 <- sample_data_v4_7 %>%
  mutate(disease_type = case_when(
    str_detect(disease_type_conformed, "germline")      ~ NA_character_,
    TRUE                                                ~ disease_type_conformed
  )) %>% 
  group_by(orien_avatar_patient_id) %>% 
  fill(disease_type, .direction = "updown") %>% 
  ungroup() %>% 
  filter(str_detect(disease_type, "Myeloma"))
  
duplicated(germline_v4_7$dna_sequencing_library_id)
  
  
  filter(str_detect(disease_type_conformed, "germline") &
           specimen_site_of_collection == "Blood") %>%
  select(avatar_key = "orien_avatar_patient_id", orien_specimen_id, dna_sequencing_library_id,
         mrn, sample_family_id, tumor_germline_heme_project_id,
         specimen_site_of_collection, date_of_specimen_collection,
         dob, date_of_diagnosis, age_at_diagnosis)

### Merge germline / patient history----











