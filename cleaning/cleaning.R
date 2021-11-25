
# Setting-up --------------------------------------------------------------

packages = c("devtools",
             "usethis",
             "here",
             "readr",
             "data.table",
             "readxl",
             "tidyverse",
             "tidylog",
             "lubridate",
             "psych",
             "ggplot2",
             "tidylog",
             "ggplotgui",
             "ggthemes",
             "arsenal")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

getwd()
rm(list=ls())

# Import data -------------------------------------------------------------

setwd("input")
patient_data = fread("2021102512_1_Patient_data_2021_002_SCE.csv.gz")
emr_disease_data = fread("2021102512_2_EMR_Disease_data_2021_002_SCE.csv.gz")
emr_drug_data = fread("2021102512_3_EMR_Drug_data_2021_002_SCE.csv.gz")
emr_admission_data = fread("2021102512_4_EMR_Admission_data_2021_002_SCE.csv.gz")
emr_lab_data = fread("2021102512_5_EMR_Laboratory_data_2021_002_SCE.csv.gz")
claim_disease_data = fread("2021102512_51_Claim_Disease_data_2021_002_SCE.csv.gz")
claim_procedure_data = fread("2021102512_52_Claim_Procedure_data_2021_002_SCE.csv.gz")
dpc_ef1_data = fread("2021102512_71_DPC_FF1_data_2021_002_SCE.csv.gz")
drug_codelist = fread("2021102512_101_Drug_codelist_2021_002_SCE.csv.gz")
disease_codelist = fread("2021102512_103_Disease_codelist_2021_002_SCE.csv.gz")
procedure_codelist = fread("2021102512_105_Procedure_codelist_2021_002_SCE.csv.gz")

setwd("C:/Users/akihi/Downloads/RWD-recurrent-COPD-exacerbation")

# DPC EF1 Data ------------------------------------------------------------

# This dataset is used for the patient selection.

dpc_ef1_data %>% glimpse()
dpc_ef1_data %>% colnames()

# check the number of the included patients 

dpc_ef1_data_duplicate <- dpc_ef1_data %>% 
  filter(項目名 == "入院の契機となった傷病名に対するICD10コード" & データ == "J441") %>% 
  group_by(患者ID) %>% #2169
  filter(n() >= 2) %>% #1033
  select(1,2) %>% 
  arrange(患者ID, 入院日) %>% 
  rename(id = "患者ID",
         adm = "入院日") %>% 
  distinct(id,adm) %>% 
  mutate(adm = ymd(adm)) %>% 
  mutate(lag_adm = lag(adm),
         diff_time = adm - lag_adm + 1) %>% 
  ungroup()
dpc_ef1_data_duplicate %>% glimpse()
length(unique(dpc_ef1_data_duplicate$id)) #333

key <- dpc_ef1_data_duplicate %>% 
  distinct(id,adm)

# extract the information of "様式1" among the included patients

dpc_ef1_data <- dpc_ef1_data %>% 
  arrange(患者ID, 入院日) %>% 
  rename(id = "患者ID",
         adm = "入院日") %>%
  mutate(adm = ymd(adm))

dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_with_c <- dpc_ef1_data_selected <- inner_join(dpc_ef1_data, key, by = c("id", "adm"))
dpc_ef1_data_selected_without_c %>% colnames()
dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_without_c %>% 
  select(1,2,9,10) %>% 
  distinct(id, adm, 項目名, .keep_all=TRUE) %>% 
  pivot_wider(names_from = 項目名,
              values_from = データ)
dpc_ef1_data_selected_without_c %>% colnames()
dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_without_c %>% 
  select(1,2,3,4,8,9,10,11,12,13,14,16,18,20,21,31,32,34)
dpc_ef1_data_selected_without_c$入院時のADLスコア <- sapply(strsplit(dpc_ef1_data_selected_without_c$入院時のADLスコア,""), function(x) sum(as.numeric(x))) 
dpc_ef1_data_selected_without_c$退院時のADLスコア <- sapply(strsplit(dpc_ef1_data_selected_without_c$退院時のADLスコア,""), function(x) sum(as.numeric(x))) 

# add information of comorbidity

dpc_ef1_data_selected_with_c %>% colnames()
dpc_ef1_data_selected_with_c1 <- dpc_ef1_data_selected_with_c %>% 
  filter(項目名 == "入院時併存症名") %>% 
  pivot_wider(
    names_from = c(項目名, 連番), 
    values_from = データ
  ) %>% 
  distinct(id, adm, .keep_all=TRUE) %>% 
  select(1,2,9,10,11,12,13,14,15,16,17,18)
dpc_ef1_data_selected_with_c2 <- dpc_ef1_data_selected_with_c %>% 
  filter(項目名 == "入院時併存症名に対するICD10コード") %>% 
  pivot_wider(
    names_from = c(項目名, 連番), 
    values_from = データ
  ) %>% 
  distinct(id, adm, .keep_all=TRUE) %>% 
  select(1,2,9,10,11,12,13,14,15,16,17,18)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected_without_c, dpc_ef1_data_selected_with_c1, by = c("id", "adm"))
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected_without_c, dpc_ef1_data_selected_with_c2, by = c("id", "adm"))


# EMR Admission Data ------------------------------------------------------

# just for justification
emr_admission_data %>% glimpse()
emr_admission_data <- emr_admission_data %>% 
  rename(id = "患者ID",
         adm = "入院日",
         ent = "退院日") %>% 
  mutate(adm = ymd(adm),
         ent = ymd(ent))
inner_join(emr_admission_data, key, by = c("id","adm")) # some conflicts

# I guess this is caused by the data storage problems in EMR admission Data


# EMR Drug Data -----------------------------------------------------------

emr_drug_data %>% glimpse()
emr_drug_data %>% colnames()
list <- read_csv("memo/drug_code_list.csv")

# list: https://www.mhlw.go.jp/topics/2021/04/tp20210401-01.html

anti_pseudo <- read_excel("memo/anti_pseudo.xlsx")
anti_pseudo <- anti_pseudo %>% 
  pull(drug)
filter_anti_pseudo <- str_c(anti_pseudo, collapse = "|")

# oral abx
oral <- read_excel("memo/oral.xlsx")
oral %>% colnames()
anti_pseudo_oral <- oral %>% 
  filter(str_detect(成分名, filter_anti_pseudo)) %>% 
  select(2) %>% 
  pull()
filter_anti_pseudo_oral <- str_c(anti_pseudo_oral, collapse = "|")
anti_pseudo_oral_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_anti_pseudo_oral))
anti_pseudo_oral_use %>% glimpse()
anti_pseudo_oral_use %>% colnames()
anti_pseudo_oral_use1 <- anti_pseudo_oral_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         end1 = "終了日",
         code1 = "薬価コード",
         name1 = "薬剤名",
         dose1 = "用量",
         department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         end1 = ymd(end1))
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_oral_use1, by = c("id","adm")) 

anti_pseudo_oral_use2 <- anti_pseudo_oral_use1 %>% 
  mutate(adm = adm - 1) %>% 
  rename(end2 = "end1",
         code2 = "code1",
         name2 = "name1",
         dose2 = "dose1",
         department2 = "department1")
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_oral_use2, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(end, starts_with("end"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(code, starts_with("code"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(name, starts_with("name"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(dose, starts_with("dose"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  unite(department, starts_with("department"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE)

# iv abx
iv <- read_excel("memo/iv.xlsx")
iv %>% colnames()
anti_pseudo_iv <- iv %>% 
  filter(str_detect(成分名, filter_anti_pseudo)) %>% 
  select(2) %>% 
  pull()
filter_anti_pseudo_iv <- str_c(anti_pseudo_iv, collapse = "|")
anti_pseudo_iv_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_anti_pseudo_iv))
anti_pseudo_iv_use %>% glimpse()
anti_pseudo_iv_use %>% colnames()
anti_pseudo_iv_use1 <- anti_pseudo_iv_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         end1 = "終了日",
         code1 = "薬価コード",
         name1 = "薬剤名",
         dose1 = "用量",
         department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         end1 = ymd(end1)) 
anti_pseudo_iv_use1[duplicated(anti_pseudo_iv_use1[,2:3])] # find duplicated cells
anti_pseudo_iv_use1 <- anti_pseudo_iv_use1 %>% 
  distinct(id, adm, .keep_all=TRUE) # later check the duplicates
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_iv_use1, by = c("id","adm")) 

anti_pseudo_oral_use2 <- anti_pseudo_oral_use1 %>% 
  mutate(adm = adm - 1) %>% 
  rename(end2 = "end1",
         code2 = "code1",
         name2 = "name1",
         dose2 = "dose1",
         department2 = "department1")
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_oral_use2, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(end, starts_with("end"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(code, starts_with("code"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(name, starts_with("name"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(dose, starts_with("dose"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  unite(department, starts_with("department"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE)



abx_list1 <- list %>% 
  slice(7:346) %>% 
  drop_na(YJコード) %>% 
  pull(YJコード)
filter_abx <- str_c(abx_list1, collapse = "|")
abx_oral <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_abx))
unique(abx_oral$薬剤名)
oral_anti_pseudo <- abx_oral %>% 
  distinct(薬剤名) %>% 
  as_tibble()
oral_anti_pseudo <- oral_anti_pseudo[c(3,10,11,15,16,26,41,42,69,75,82,83,84,100,101,
                                       103,106,116,117,119,120,121,123,124,125,127,129,
                                       132,133,143,144,148,149150,151,152,156,157,158,
                                       159,160,163,166,167,168,175,177,178,179,182,183),] %>% pull(薬剤名)
filter_oral_anti_pseudo <- str_c(oral_anti_pseudo, collapse = "|")

# iv abx

abx_list1 <- list %>% 
  slice(423:816) %>% 
  drop_na(YJコード) %>% 
  pull(YJコード)
filter_abx <- str_c(abx_list1, collapse = "|")
abx <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_abx))
anti_pseudo <- unique(abx$薬剤名) %>% 
  select(3,10,11,15,16,)

# oral steroid

abx_list1 <- list %>% 
  slice(423:816) %>% 
  drop_na(YJコード) %>% 
  pull(YJコード)
filter_abx <- str_c(abx_list1, collapse = "|")
abx <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_abx))
anti_pseudo <- unique(abx$薬剤名) %>% 
  select(3,10,11,15,16,)

# iv steroid

abx_list1 <- list %>% 
  slice(352:423) %>% 
  drop_na(YJコード) %>% 
  pull(YJコード)
filter_abx <- str_c(abx_list1, collapse = "|")
abx <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_abx))
anti_pseudo <- unique(abx$薬剤名) %>% 
  select(3,10,11,15,16,)

# EMR Disease Data --------------------------------------------------------

# This dataset is used for the patient selection.

emr_disease_data %>% glimpse()
emr_disease_data %>% colnames()
emr_disease_data_main <- emr_disease_data %>% 
  filter(主傷病 == 1 & ICD10 == "J441")
emr_disease_data_main %>% glimpse()
length(unique(emr_disease_data_main$患者ID))
emr_disease_data_main_duplicate <- emr_disease_data_main %>% 
  group_by(患者ID) %>% 
  filter(n() >= 2) %>% 
  arrange(患者ID, 開始日)
emr_disease_data_main_duplicate %>% glimpse()
length(unique(emr_disease_data_main_duplicate$患者ID))

# Patient Data ------------------------------------------------------------

# This is all the patient data regardless of the disease name!

patient_data %>% glimpse()
length(unique(patient_data$患者ID))
patient_data %>% colnames()
# check the patient enrollment period
describe(patient_data$`観察期間開始日(EMR)`)
describe(patient_data$`観察期間開始日(レセプト)`)
describe(patient_data$`観察期間開始日(DPC)`)

# EMR Admission Data ---------------------------------------------------------------

emr_admission_data

# Claim Disease Code ------------------------------------------------------

claim_disease_data %>% glimpse()



