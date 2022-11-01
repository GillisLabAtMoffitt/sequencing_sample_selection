# v04.6/v04.7

# Import packages
library(tidyverse)
library(lubridate)

#######################################################################################  I  ### Load data----
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                  "CH all tumor types")


sample_data_v4_7_dates <- 
  readxl::read_xlsx(paste0(path, "/raw data/CDSC/v4.6and4.7/10R22000169_20220624_outfile.xlsx"),
                    sheet = "CDSC-AvatarMasterList_SDR-2 ",
                    na = "NULL") %>% 
  janitor::clean_names() %>% 
  mutate(mrn = as.character(mrn))


#######################################################################################  II  ### Data cleaning----
Germline_v4.7 <- sample_data_v4_7_dates %>% 
  filter(str_detect(disease_type_conformed, "germline") &
           specimen_site_of_collection == "Blood") %>%
  select(avatar_key = "orien_avatar_patient_id", mrn, 
         germline_orien_id = orien_specimen_id, 
         SLID_germline = dna_sequencing_library_id,
         germline_sample_family_id = sample_family_id,
         germline_collection_date = date_of_specimen_collection,
         germline_site_of_collection = specimen_site_of_collection, 
         date_of_specimen_collection,
         dob)

Tumor_v4.7 <- sample_data_v4_7_dates %>% 
  filter(!str_detect(disease_type_conformed, "germline")) %>%
  mutate() %>% 
  select(avatar_key = "orien_avatar_patient_id", mrn, 
         tumor_orien_id = orien_specimen_id, 
         dna_sequencing_library_id, rna_sequencing_library_id,
         tumor_sample_family_id = sample_family_id,
         tumor_site_of_collection = specimen_site_of_collection, 
         tumor_collection_date = date_of_specimen_collection,
         date_of_specimen_collection,
         disease_type_conformed, histology, release,
         dob, date_of_diagnosis, age_at_diagnosis)

# Merge blood and tumor samples
WES_v4.7 <- left_join(Germline_v4.7, Tumor_v4.7, 
                      by = c("avatar_key", "mrn", "dob")) %>% 
  mutate(interval_germ_tumor = abs(interval(start= germline_collection_date, end= tumor_collection_date) /
                                     duration(n=1, unit="days"))) %>% 
  arrange(avatar_key, SLID_germline, interval_germ_tumor)
write_csv(WES_v4.7, paste0(path, "/processed WES ids list/matched germline tumor samples ids all tumor type v04.7.csv"))

MM_WES_v4.7 <- WES_v4.7 %>% 
  filter(disease_type_conformed == "HEM - Myeloma Spectrum" &
           histology == "97323 Multiple myeloma")
write_csv(MM_WES_v4.7, paste0(path, "/processed WES ids list/matched germline tumor samples ids Multiple Myeloma v04.7.csv"))

Mgus_WES_v4.7 <- WES_v4.7 %>% 
  filter(disease_type_conformed == "HEM - Myeloma Spectrum" &
           histology == "97651 Monoclonal gammopathy")
write_csv(Mgus_WES_v4.7, paste0(path, "/processed WES ids list/matched germline tumor samples ids MGUS v04.7.csv"))

HEM_WES <- WES_v4.7 %>% 
  filter(disease_type_conformed == "HEM - Myeloma Spectrum")
write_csv(HEM_WES, paste0(path, "/processed WES ids list/matched germline tumor samples ids HEM disease v04.7.csv"))




#### NOT RUN, This is to compare samples/clinical data lists available ####
# Garrick
path <- fs::path("","Volumes","Gillis_Research","Christelle Colin-Leitzinger", "CHIP in Avatar",
                 "CH all tumor types", "raw data", "M2GEN")

mrn1 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-17_avatar_v2_clinical-with-events/MRN.csv"), na = c("PRBB-DO NOT USE")) %>% 
  filter(!is.na(MRN))
mrn2 <-
  read_csv(paste0(path, "/Data/10R20000463_2021-05-10_avatar_v4_clinical-with-events/MRN.csv"), na = c("PRBB-DO NOT USE")) %>% 
  filter(!is.na(MRN))
mrn <- bind_rows(mrn1, mrn2) %>% 
  distinct()

data <- full_join(germline_v4_5 %>% 
                    select(avatar_id, Raghu_moffittSampleId_germline = moffittSampleId_germline, RaghuSLID_germline = SLID_germline, Raghu_Disease_Status_germline = Disease_Status_germline) %>% 
                    filter(!is.na(Raghu_moffittSampleId_germline)) %>% 
                    distinct(avatar_id, .keep_all = TRUE) %>% 
                    mutate(Raghu="MM v4.5"), 
                  MM_WES_v4.7 %>%
                    distinct(avatar_key)%>% 
                    mutate(version="MM Yifen v4.7"), 
                  by= c("avatar_id" = "avatar_key")) %>% 
  arrange(version) %>% 
  full_join(., Jamie %>% select(subject) %>% 
              mutate(version="Jamie v4.7"), 
            by= c("avatar_id" = "subject")) %>% 
  full_join(., Germline_v4.7 %>% 
              select(avatar_key) %>%
              mutate(version="Yifen v4.7"), 
            by= c("avatar_id" = "avatar_key")) %>% 
  distinct(avatar_id, .keep_all = TRUE) %>% 
  full_join(., mrn %>% 
              select(AvatarKey) %>%
              mutate(Garrick="Clinical v4.5"), 
            by= c("avatar_id" = "AvatarKey")
              )

#####################################################################

