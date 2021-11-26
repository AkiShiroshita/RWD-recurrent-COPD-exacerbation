
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
unique(dpc_ef1_data$項目名)

# check the number of the included patients 

dpc_ef1_data_duplicate <- dpc_ef1_data %>% 
  filter((str_detect(dpc_ef1_data$項目名,"主傷病に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J440")) |
           (str_detect(dpc_ef1_data$項目名,"主傷病に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J441")) |
           (str_detect(dpc_ef1_data$項目名,"主傷病に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J09")) |
           (str_detect(dpc_ef1_data$項目名,"主傷病に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J1")) |
           (str_detect(dpc_ef1_data$項目名,"主傷病に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J2")) |
           
           (str_detect(dpc_ef1_data$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J440")) |
           (str_detect(dpc_ef1_data$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J441")) |
           (str_detect(dpc_ef1_data$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J09")) |
           (str_detect(dpc_ef1_data$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J1")) |
           (str_detect(dpc_ef1_data$項目名,"入院の契機となった傷病名に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J2")) |
           
           (str_detect(dpc_ef1_data$項目名,"医療資源を最も投入した傷病名に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J440")) |
           (str_detect(dpc_ef1_data$項目名,"医療資源を最も投入した傷病名に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J441")) |
           (str_detect(dpc_ef1_data$項目名,"医療資源を最も投入した傷病名に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J09")) |
           (str_detect(dpc_ef1_data$項目名,"医療資源を最も投入した傷病名に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J1")) |
           (str_detect(dpc_ef1_data$項目名,"医療資源を最も投入した傷病名に対するICD10コード") & str_detect(dpc_ef1_data$データ,"J2"))) %>% 
  select(1,2) %>%  
  rename(id = "患者ID",
         adm = "入院日") %>% 
  arrange(id, adm) %>% 
  distinct(id, adm, .keep_all=TRUE) %>% 
  group_by(id) %>% 
  filter(n() >= 2) %>% 
  mutate(adm = ymd(adm)) %>% 
  mutate(lag_adm = lag(adm),
         diff_time = adm - lag_adm + 1) %>% 
  ungroup()
dpc_ef1_data_duplicate %>% glimpse()
length(unique(dpc_ef1_data_duplicate$id)) 

# extract the information of "様式1" among the included patients

dpc_ef1_data <- dpc_ef1_data %>% 
  arrange(患者ID, 入院日) %>% 
  rename(id = "患者ID",
         adm = "入院日") %>%
  mutate(adm = ymd(adm))

dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_with_c <- dpc_ef1_data_selected <- inner_join(dpc_ef1_data, dpc_ef1_data_duplicate, by = c("id", "adm"))
dpc_ef1_data_selected_without_c %>% colnames()
dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_without_c %>% 
  select(1,2,9,10) %>% 
  distinct(id, adm, 項目名, .keep_all=TRUE) %>% 
  pivot_wider(names_from = 項目名,
              values_from = データ)
dpc_ef1_data_selected_without_c %>% colnames()
dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_without_c %>% 
  select(id,adm,性別,主傷病に対するICD10コード,主傷病名,入院の契機となった傷病名に対するICD10コード,
         入院の契機となった傷病名,医療資源を最も投入した傷病名に対するICD10コード,医療資源を最も投入した傷病名,
         退院時のADLスコア,入院時意識障害がある場合のJCS,退院時意識障害がある場合のJCS,`Hugh-Jones分類`,
         肺炎の重症度分類,入院時のADLスコア,喫煙指数,BMI,入院年月日,救急車による搬送の有無,退院年月日,
         退院先,退院時転帰,`24時間以内の死亡の有無`,入院経路,医療介護関連肺炎に該当の有無)
dpc_ef1_data_selected_without_c$入院時のADLスコア <- sapply(strsplit(dpc_ef1_data_selected_without_c$入院時のADLスコア,""), function(x) sum(as.numeric(x))) 
dpc_ef1_data_selected_without_c$退院時のADLスコア <- sapply(strsplit(dpc_ef1_data_selected_without_c$退院時のADLスコア,""), function(x) sum(as.numeric(x))) 

# add information of the comorbidity and subsequent

dpc_ef1_data_selected_with_c %>% colnames()

dpc_ef1_data_selected_with_c1 <- dpc_ef1_data_selected_with_c %>% 
  filter(項目名 == "入院時併存症名") %>% 
  pivot_wider(
    names_from = c(項目名, 連番), 
    values_from = データ
  ) %>% 
  distinct(id, adm, .keep_all=TRUE) %>% 
  select(id,adm,入院時併存症名_1,入院時併存症名_2,入院時併存症名_3,入院時併存症名_4,入院時併存症名_5,入院時併存症名_6,
         入院時併存症名_7,入院時併存症名_8,入院時併存症名_9,入院時併存症名_10)

dpc_ef1_data_selected_with_c2 <- dpc_ef1_data_selected_with_c %>% 
  filter(項目名 == "入院時併存症名に対するICD10コード") %>% 
  pivot_wider(
    names_from = c(項目名, 連番), 
    values_from = データ
  ) %>% 
  distinct(id, adm, .keep_all=TRUE) %>% 
  select(id,adm,入院時併存症名に対するICD10コード_1,入院時併存症名に対するICD10コード_2,入院時併存症名に対するICD10コード_3,
         入院時併存症名に対するICD10コード_4,入院時併存症名に対するICD10コード_5,入院時併存症名に対するICD10コード_6,
         入院時併存症名に対するICD10コード_7,入院時併存症名に対するICD10コード_8,入院時併存症名に対するICD10コード_9,
         入院時併存症名に対するICD10コード_10)

dpc_ef1_data_selected_with_c3 <- dpc_ef1_data_selected_with_c %>% 
  filter(項目名 == "入院後発症疾患名") %>% 
  pivot_wider(
    names_from = c(項目名, 連番), 
    values_from = データ
  ) %>% 
  distinct(id, adm, .keep_all=TRUE) %>% 
  select(id,adm,入院後発症疾患名_1,入院後発症疾患名_2,入院後発症疾患名_3,入院後発症疾患名_4,入院後発症疾患名_5,
         入院後発症疾患名_6,入院後発症疾患名_7,入院後発症疾患名_8,入院後発症疾患名_9,入院後発症疾患名_10)

dpc_ef1_data_selected_with_c4 <- dpc_ef1_data_selected_with_c %>% 
  filter(項目名 == "入院後発症疾患名に対するICD10コード") %>% 
  pivot_wider(
    names_from = c(項目名, 連番), 
    values_from = データ
  ) %>% 
  distinct(id, adm, .keep_all=TRUE) %>% 
  select(id,adm,入院後発症疾患名に対するICD10コード_1,入院後発症疾患名に対するICD10コード_2,入院後発症疾患名に対するICD10コード_3,
         入院後発症疾患名に対するICD10コード_4,入院後発症疾患名に対するICD10コード_5,入院後発症疾患名に対するICD10コード_6,
         入院後発症疾患名に対するICD10コード_7,入院後発症疾患名に対するICD10コード_8,入院後発症疾患名に対するICD10コード_9,
         入院後発症疾患名に対するICD10コード_10)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected_without_c, dpc_ef1_data_selected_with_c1, by = c("id", "adm"))
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, dpc_ef1_data_selected_with_c2, by = c("id", "adm"))
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, dpc_ef1_data_selected_with_c3, by = c("id", "adm"))
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, dpc_ef1_data_selected_with_c4, by = c("id", "adm"))

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  filter((str_detect(dpc_ef1_data_selected$主傷病に対するICD10コード,"J440")) |
            (str_detect(dpc_ef1_data_selected$主傷病に対するICD10コード,"J441")) |
            
            (str_detect(dpc_ef1_data_selected$入院の契機となった傷病名に対するICD10コード,"J440")) |
            (str_detect(dpc_ef1_data_selected$入院の契機となった傷病名に対するICD10コード,"J441")) |
            
            (str_detect(dpc_ef1_data_selected$医療資源を最も投入した傷病名に対するICD10コード,"J440")) |
            (str_detect(dpc_ef1_data_selected$医療資源を最も投入した傷病名に対するICD10コード,"J441")) |
            
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_1,"J43")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_2,"J43")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_3,"J43")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_4,"J43")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_5,"J43")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_6,"J43")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_7,"J43")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_8,"J43")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_9,"J43")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_10,"J43")) |
            
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_1,"J44")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_2,"J44")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_3,"J44")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_4,"J44")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_5,"J44")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_6,"J44")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_7,"J44")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_8,"J44")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_9,"J44")) |
            (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_10,"J44")))

key <- dpc_ef1_data_selected %>% 
  distinct(id,adm)

length(unique(key$id)) 

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

# list: https://www.mhlw.go.jp/topics/2021/04/tp20210401-01.html
# abx list: https://amrcrc.ncgm.go.jp/surveillance/030/20181128172757.html

# oral abx

oral <- read_excel("memo/oral.xlsx")
oral_abx <- read_excel("memo/abx.xlsx")
oral_abx %>% colnames()
oral_abx <- oral_abx %>% 
  rename(name = "一般名(日本語)")
oral_abx <- oral_abx %>% 
  drop_na(name) %>% 
  pull(name)
filter_oral_abx_ing <- str_c(oral_abx, collapse = "|")
oral_abx_code <- oral %>% 
  filter(str_detect(成分名, filter_oral_abx_ing)) %>% 
  select(薬価基準収載医薬品コード)
filter_oral_abx_code <- oral_abx_code %>% 
  drop_na(薬価基準収載医薬品コード) %>% 
  pull(薬価基準収載医薬品コード)
filter_oral_abx_code <- str_c(filter_oral_abx_code, collapse = "|")
oral_abx_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_oral_abx_code))
oral_abx_use %>% glimpse()
oral_abx_use %>% colnames()
oral_abx_use1 <- oral_abx_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         oral_end1 = "終了日",
         oral_code1 = "薬価コード",
         oral_name1 = "薬剤名",
         oral_dose1 = "用量",
         oral_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         oral_end1 = ymd(oral_end1)) %>% 
  distinct(id, adm, oral_code1, .keep_all=TRUE)
oral_abx_use1 <- inner_join(key, oral_abx_use1, by = c("id","adm")) 
oral_abx_use1_wide <- oral_abx_use1 %>% 
  select(id,adm,oral_code1) %>% 
  pivot_wider(names_from = oral_code1,
              values_from = oral_code1,
              names_prefix = "drug") %>% 
  unite(oral_abx1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

oral_abx_use2 <- oral_abx_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         oral_end2 = "終了日",
         oral_code2 = "薬価コード",
         oral_name2 = "薬剤名",
         oral_dose2 = "用量",
         oral_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         oral_end2 = ymd(oral_end2)) %>% 
  distinct(id, adm, oral_code2, .keep_all=TRUE)
oral_abx_use2 <- inner_join(key, oral_abx_use2, by = c("id","adm")) 
oral_abx_use2_wide <- oral_abx_use2 %>% 
  select(id,adm,oral_code2) %>% 
  pivot_wider(names_from = oral_code2,
              values_from = oral_code2,
              names_prefix = "drug") %>% 
  unite(oral_abx2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, oral_abx_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, oral_abx_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(oral_abx, starts_with("oral_abx"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# iv abx

iv <- read_excel("memo/iv.xlsx")
iv_abx <- read_excel("memo/abx.xlsx")
iv_abx %>% colnames()
iv_abx <- iv_abx %>% 
  rename(name = "一般名(日本語)")
iv_abx <- iv_abx %>% 
  drop_na(name) %>% 
  pull(name)
filter_iv_abx_ing <- str_c(iv_abx, collapse = "|")
iv_abx_code <- iv %>% 
  filter(str_detect(成分名, filter_iv_abx_ing)) %>% 
  select(薬価基準収載医薬品コード)
filter_iv_abx_code <- iv_abx_code %>% 
  drop_na(薬価基準収載医薬品コード) %>% 
  pull(薬価基準収載医薬品コード)
filter_iv_abx_code <- str_c(filter_iv_abx_code, collapse = "|")
iv_abx_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_iv_abx_code))
iv_abx_use %>% glimpse()
iv_abx_use %>% colnames()
iv_abx_use1 <- iv_abx_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         iv_end1 = "終了日",
         iv_code1 = "薬価コード",
         iv_name1 = "薬剤名",
         iv_dose1 = "用量",
         iv_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         iv_end1 = ymd(iv_end1)) %>% 
  distinct(id, adm, iv_code1, .keep_all=TRUE)
iv_abx_use1 <- inner_join(key, iv_abx_use1, by = c("id","adm")) 
iv_abx_use1_wide <- iv_abx_use1 %>% 
  select(id,adm,iv_code1) %>% 
  pivot_wider(names_from = iv_code1,
              values_from = iv_code1,
              names_prefix = "drug") %>% 
  unite(iv_abx1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

iv_abx_use2 <- iv_abx_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         iv_end2 = "終了日",
         iv_code2 = "薬価コード",
         iv_name2 = "薬剤名",
         iv_dose2 = "用量",
         iv_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         iv_end2 = ymd(iv_end2)) %>% 
  distinct(id, adm, iv_code2, .keep_all=TRUE)
iv_abx_use2 <- inner_join(key, iv_abx_use2, by = c("id","adm")) 
iv_abx_use2_wide <- iv_abx_use2 %>% 
  select(id,adm,iv_code2) %>% 
  pivot_wider(names_from = iv_code2,
              values_from = iv_code2,
              names_prefix = "drug") %>% 
  unite(iv_abx2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 
iv_abx_use2 <- inner_join(key, iv_abx_use2, by = c("id","adm")) 
iv_abx_use2_wide <- iv_abx_use2 %>% 
  select(id,adm,iv_code2) %>% 
  pivot_wider(names_from = iv_code2,
              values_from = iv_code2,
              names_prefix = "drug") %>% 
  unite(iv_abx2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, iv_abx_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, iv_abx_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(iv_abx, starts_with("iv_abx"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# oral anti-pseudomonal drug

anti_pseudo <- read_excel("memo/anti_pseudo.xlsx")
anti_pseudo <- anti_pseudo %>% 
  pull(drug)
filter_anti_pseudo <- str_c(anti_pseudo, collapse = "|")
anti_pseudo_oral <- oral %>% 
  filter(str_detect(成分名, filter_anti_pseudo)) %>% 
  select(2) %>% 
  pull()
filter_anti_pseudo_oral_code <- str_c(anti_pseudo_oral, collapse = "|")
anti_pseudo_oral_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_anti_pseudo_oral_code))
anti_pseudo_oral_use %>% glimpse()
anti_pseudo_oral_use %>% colnames()
anti_pseudo_oral_use1 <- anti_pseudo_oral_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         oral_end1 = "終了日",
         oral_code1 = "薬価コード",
         oral_name1 = "薬剤名",
         oral_dose1 = "用量",
         oral_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         oral_end1 = ymd(oral_end1)) %>% 
  distinct(id, adm, oral_code1, .keep_all=TRUE)
anti_pseudo_oral_use1 <- inner_join(key, anti_pseudo_oral_use1, by = c("id","adm")) 
anti_pseudo_oral_use1_wide <- anti_pseudo_oral_use1 %>% 
  select(id,adm,oral_code1) %>% 
  pivot_wider(names_from = oral_code1,
              values_from = oral_code1,
              names_prefix = "drug") %>% 
  unite(anti_pseudo_oral1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

anti_pseudo_oral_use2 <- anti_pseudo_oral_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         iv_end2 = "終了日",
         iv_code2 = "薬価コード",
         iv_name2 = "薬剤名",
         iv_dose2 = "用量",
         iv_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         iv_end2 = ymd(iv_end2)) %>% 
  distinct(id, adm, iv_code2, .keep_all=TRUE)
anti_pseudo_oral_use2 <- inner_join(key, anti_pseudo_oral_use2, by = c("id","adm")) 
anti_pseudo_oral_use2_wide <- anti_pseudo_oral_use2 %>% 
  select(id,adm,oral_code2) %>% 
  pivot_wider(names_from = oral_code2,
              values_from = oral_code2,
              names_prefix = "drug") %>% 
  unite(anti_pseudo_oral2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_oral_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_oral_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(anti_pseudo_oral, starts_with("anti_pseudo_oral"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# iv anti-pseudomonal drug

anti_pseudo <- read_excel("memo/anti_pseudo.xlsx")
anti_pseudo <- anti_pseudo %>% 
  pull(drug)
filter_anti_pseudo <- str_c(anti_pseudo, collapse = "|")
anti_pseudo_iv <- iv %>% 
  filter(str_detect(成分名, filter_anti_pseudo)) %>% 
  select(2) %>% 
  pull()
filter_anti_pseudo_iv_code <- str_c(anti_pseudo_iv, collapse = "|")
anti_pseudo_iv_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_anti_pseudo_iv_code))
anti_pseudo_iv_use %>% glimpse()
anti_pseudo_iv_use %>% colnames()
anti_pseudo_iv_use1 <- anti_pseudo_iv_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         iv_end1 = "終了日",
         iv_code1 = "薬価コード",
         iv_name1 = "薬剤名",
         iv_dose1 = "用量",
         iv_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         iv_end1 = ymd(iv_end1)) %>% 
  distinct(id, adm, iv_code1, .keep_all=TRUE)
anti_pseudo_iv_use1 <- inner_join(key, anti_pseudo_iv_use1, by = c("id","adm")) 
anti_pseudo_iv_use1_wide <- anti_pseudo_iv_use1 %>% 
  select(id,adm,iv_code1) %>% 
  pivot_wider(names_from = iv_code1,
              values_from = iv_code1,
              names_prefix = "drug") %>% 
  unite(anti_pseudo_iv1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

anti_pseudo_iv_use2 <- anti_pseudo_iv_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         iv_end2 = "終了日",
         iv_code2 = "薬価コード",
         iv_name2 = "薬剤名",
         iv_dose2 = "用量",
         iv_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         iv_end2 = ymd(iv_end2)) %>% 
  distinct(id, adm, iv_code2, .keep_all=TRUE)
anti_pseudo_iv_use2 <- inner_join(key, anti_pseudo_iv_use2, by = c("id","adm")) 
anti_pseudo_iv_use2_wide <- anti_pseudo_iv_use2 %>% 
  select(id,adm,iv_code2) %>% 
  pivot_wider(names_from = iv_code2,
              values_from = iv_code2,
              names_prefix = "drug") %>% 
  unite(anti_pseudo_iv2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_iv_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_iv_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(anti_pseudo_iv, starts_with("anti_pseudo_iv"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# oral steroid

steroid <- read_excel("memo/steroid.xlsx")
steroid <- steroid %>% 
  pull(drug)
filter_steroid <- str_c(steroid, collapse = "|")

oral <- read_excel("memo/oral.xlsx")
oral %>% colnames()
steroid_oral <- oral %>% 
  filter(str_detect(成分名, filter_steroid)) %>% 
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
         oral_end1 = "終了日",
         oral_code1 = "薬価コード",
         oral_name1 = "薬剤名",
         oral_dose1 = "用量",
         oral_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         oral_end1 = ymd(oral_end1))
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_oral_use1, by = c("id","adm")) 

anti_pseudo_oral_use2 <- anti_pseudo_oral_use1 %>% 
  mutate(adm = adm - 1) %>% 
  rename(oral_end2 = "oral_end1",
         oral_code2 = "oral_code1",
         oral_name2 = "oral_name1",
         oral_dose2 = "oral_dose1",
         oral_department2 = "oral_department1")
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_oral_use2, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(oral_end, starts_with("oral_end"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(oral_code, starts_with("oral_code"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(oral_name, starts_with("oral_name"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(oral_dose, starts_with("oral_dose"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  unite(oral_department, starts_with("oral_department"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE)

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
         oral_end1 = "終了日",
         oral_code1 = "薬価コード",
         oral_name1 = "薬剤名",
         oral_dose1 = "用量",
         oral_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         oral_end1 = ymd(oral_end1))
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_oral_use1, by = c("id","adm")) 

anti_pseudo_oral_use2 <- anti_pseudo_oral_use1 %>% 
  mutate(adm = adm - 1) %>% 
  rename(oral_end2 = "oral_end1",
         oral_code2 = "oral_code1",
         oral_name2 = "oral_name1",
         oral_dose2 = "oral_dose1",
         oral_department2 = "oral_department1")
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, anti_pseudo_oral_use2, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(oral_end, starts_with("oral_end"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(oral_code, starts_with("oral_code"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(oral_name, starts_with("oral_name"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>%
  unite(oral_dose, starts_with("oral_dose"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  unite(oral_department, starts_with("oral_department"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE)


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



