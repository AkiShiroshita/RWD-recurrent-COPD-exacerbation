
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
#emr_disease_data = fread("2021102512_2_EMR_Disease_data_2021_002_SCE.csv.gz")
emr_drug_data = fread("2021102512_3_EMR_Drug_data_2021_002_SCE.csv.gz")
#emr_admission_data = fread("2021102512_4_EMR_Admission_data_2021_002_SCE.csv.gz")
emr_lab_data = fread("2021102512_5_EMR_Laboratory_data_2021_002_SCE.csv.gz")
#claim_disease_data = fread("2021102512_51_Claim_Disease_data_2021_002_SCE.csv.gz")
claim_procedure_data = fread("2021102512_52_Claim_Procedure_data_2021_002_SCE.csv.gz")
dpc_ef1_data = fread("2021102512_71_DPC_FF1_data_2021_002_SCE.csv.gz")
#drug_codelist = fread("2021102512_101_Drug_codelist_2021_002_SCE.csv.gz")
#disease_codelist = fread("2021102512_103_Disease_codelist_2021_002_SCE.csv.gz")
#procedure_codelist = fread("2021102512_105_Procedure_codelist_2021_002_SCE.csv.gz")

setwd("C:/Users/akihi/Downloads/RWD-recurrent-COPD-exacerbation")

# DPC EF1 Data ------------------------------------------------------------

# This dataset is used for the patient selection.

dpc_ef1_data %>% glimpse()
dpc_ef1_data %>% colnames()
unique(dpc_ef1_data$項目名)

length(unique(dpc_ef1_data$患者ID)) #38767

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
  mutate(adm = ymd(adm)) %>% 
  mutate(lag_adm = lag(adm),
         diff_time = adm - lag_adm + 1) %>% 
  ungroup()

dpc_ef1_data_duplicate %>% glimpse()
length(unique(dpc_ef1_data_duplicate$id)) 

# extract the information of "様式1" among the included patients

dpc_ef1_data_all <- dpc_ef1_data %>% 
  arrange(患者ID, 入院日) %>% 
  rename(id = "患者ID",
         adm = "入院日") %>%
  mutate(adm = ymd(adm))

dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_with_c <- dpc_ef1_data_selected <- inner_join(dpc_ef1_data_all, dpc_ef1_data_duplicate, by = c("id", "adm"))
dpc_ef1_data_selected_without_c %>% colnames()
dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_without_c %>% 
  select(1,2,9,10,12,13) %>% 
  distinct(id, adm, 項目名, .keep_all=TRUE) %>% 
  pivot_wider(names_from = 項目名,
              values_from = データ)
dpc_ef1_data_selected_without_c %>% colnames()
dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_without_c %>% 
  select(id,adm,性別,主傷病に対するICD10コード,主傷病名,入院の契機となった傷病名に対するICD10コード,
         入院の契機となった傷病名,医療資源を最も投入した傷病名に対するICD10コード,医療資源を最も投入した傷病名,
         退院時のADLスコア,入院時意識障害がある場合のJCS,退院時意識障害がある場合のJCS,`Hugh-Jones分類`,
         肺炎の重症度分類,入院時のADLスコア,喫煙指数,BMI,救急車による搬送の有無,退院年月日,
         退院先,退院時転帰,`24時間以内の死亡の有無`,入院経路,医療介護関連肺炎に該当の有無, diff_time)
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
  filter(
           ((str_detect(dpc_ef1_data_selected$主傷病に対するICD10コード,"J440")) |
           (str_detect(dpc_ef1_data_selected$主傷病に対するICD10コード,"J441")) |
          
           (str_detect(dpc_ef1_data_selected$入院の契機となった傷病名に対するICD10コード,"J440")) |
           (str_detect(dpc_ef1_data_selected$入院の契機となった傷病名に対するICD10コード,"J441")) |
          
           (str_detect(dpc_ef1_data_selected$医療資源を最も投入した傷病名に対するICD10コード,"J440")) |
           (str_detect(dpc_ef1_data_selected$医療資源を最も投入した傷病名に対するICD10コード,"J441"))) |
           
        
           (
           ((str_detect(dpc_ef1_data_selected$主傷病に対するICD10コード,"J09")) |
           (str_detect(dpc_ef1_data_selected$入院の契機となった傷病名に対するICD10コード,"J09")) |
           (str_detect(dpc_ef1_data_selected$医療資源を最も投入した傷病名に対するICD10コード,"J09"))) &
           
           ((str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_1,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_2,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_3,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_4,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_5,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_6,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_7,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_8,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_9,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_10,"J43")))) |

           (
           ((str_detect(dpc_ef1_data_selected$主傷病に対するICD10コード,"J09")) |
           (str_detect(dpc_ef1_data_selected$入院の契機となった傷病名に対するICD10コード,"J09")) |
           (str_detect(dpc_ef1_data_selected$医療資源を最も投入した傷病名に対するICD10コード,"J09"))) &
                 
           ((str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_1,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_2,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_3,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_4,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_5,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_6,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_7,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_8,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_9,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_10,"J44")))) |
             
           (
           ((str_detect(dpc_ef1_data_selected$主傷病に対するICD10コード,"J1")) |
           (str_detect(dpc_ef1_data_selected$入院の契機となった傷病名に対するICD10コード,"J1")) |
           (str_detect(dpc_ef1_data_selected$医療資源を最も投入した傷病名に対するICD10コード,"J1"))) &
                 
           ((str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_1,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_2,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_3,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_4,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_5,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_6,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_7,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_8,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_9,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_10,"J43")))) |
             
           (
           ((str_detect(dpc_ef1_data_selected$主傷病に対するICD10コード,"J1")) |
           (str_detect(dpc_ef1_data_selected$入院の契機となった傷病名に対するICD10コード,"J1")) |
           (str_detect(dpc_ef1_data_selected$医療資源を最も投入した傷病名に対するICD10コード,"J09"))) &
                 
           ((str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_1,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_2,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_3,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_4,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_5,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_6,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_7,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_8,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_9,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_10,"J44")))) |
                          
           (
           ((str_detect(dpc_ef1_data_selected$主傷病に対するICD10コード,"J2")) |
           (str_detect(dpc_ef1_data_selected$入院の契機となった傷病名に対するICD10コード,"J2")) |
           (str_detect(dpc_ef1_data_selected$医療資源を最も投入した傷病名に対するICD10コード,"J2"))) &
                 
           ((str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_1,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_2,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_3,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_4,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_5,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_6,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_7,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_8,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_9,"J43")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_10,"J43")))) |           
            
           (
           ((str_detect(dpc_ef1_data_selected$主傷病に対するICD10コード,"J2")) |
           (str_detect(dpc_ef1_data_selected$入院の契機となった傷病名に対するICD10コード,"J2") |
           (str_detect(dpc_ef1_data_selected$医療資源を最も投入した傷病名に対するICD10コード,"J2"))) &
             
           ((str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_1,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_2,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_3,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_4,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_5,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_6,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_7,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_8,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_9,"J44")) |
           (str_detect(dpc_ef1_data_selected$入院時併存症名に対するICD10コード_10,"J44")))))) # 8273
length(unique(dpc_ef1_data_selected$id)) # 5927

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  group_by(id) %>% 
  filter(n() >= 2) %>% 
  ungroup() #3615
length(unique(dpc_ef1_data_selected$id)) # 1314

key <- dpc_ef1_data_selected %>% 
  distinct(id,adm)

length(unique(key$id)) 

# EMR Admission Data ------------------------------------------------------

# just for justification
#emr_admission_data %>% glimpse()
#emr_admission_data <- emr_admission_data %>% 
#  rename(id = "患者ID",
#         adm = "入院日",
#         ent = "退院日") %>% 
#  mutate(adm = ymd(adm),
#         ent = ymd(ent))
#inner_join(emr_admission_data, key, by = c("id","adm")) # some conflicts

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
         oral_end2 = "終了日",
         oral_code2 = "薬価コード",
         oral_name2 = "薬剤名",
         oral_dose2 = "用量",
         oral_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         oral_end2 = ymd(oral_end2)) %>% 
  distinct(id, adm, oral_code2, .keep_all=TRUE)
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
steroid_oral <- oral %>% 
  filter(str_detect(成分名, filter_steroid)) %>% 
  select(2) %>% 
  pull()
filter_steroid_oral_code <- str_c(steroid_oral, collapse = "|")
steroid_oral_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_steroid_oral_code))
steroid_oral_use %>% glimpse()
steroid_oral_use %>% colnames()
steroid_oral_use1 <- steroid_oral_use %>% 
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
steroid_oral_use1 <- inner_join(key, steroid_oral_use1, by = c("id","adm")) 
steroid_oral_use1_wide <- steroid_oral_use1 %>% 
  select(id,adm,oral_code1) %>% 
  pivot_wider(names_from = oral_code1,
              values_from = oral_code1,
              names_prefix = "drug") %>% 
  unite(steroid_oral1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

steroid_oral_use2 <- steroid_oral_use %>% 
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
steroid_oral_use2 <- inner_join(key, steroid_oral_use2, by = c("id","adm")) 
steroid_oral_use2_wide <- steroid_oral_use2 %>% 
  select(id,adm,oral_code2) %>% 
  pivot_wider(names_from = oral_code2,
              values_from = oral_code2,
              names_prefix = "drug") %>% 
  unite(steroid_oral2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, steroid_oral_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, steroid_oral_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(steroid_oral, starts_with("steroid_oral"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# iv steroid 

steroid <- read_excel("memo/steroid.xlsx")
steroid <- steroid %>% 
  pull(drug)

filter_steroid <- str_c(steroid, collapse = "|")
steroid_iv <- iv %>% 
  filter(str_detect(成分名, filter_steroid)) %>% 
  select(2) %>% 
  pull()
filter_steroid_iv_code <- str_c(steroid_iv, collapse = "|")
steroid_iv_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_steroid_iv_code))
steroid_iv_use %>% glimpse()
steroid_iv_use %>% colnames()
steroid_iv_use1 <- steroid_iv_use %>% 
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
steroid_iv_use1 <- inner_join(key, steroid_iv_use1, by = c("id","adm")) 
steroid_iv_use1_wide <- steroid_iv_use1 %>% 
  select(id,adm,iv_code1) %>% 
  pivot_wider(names_from = iv_code1,
              values_from = iv_code1,
              names_prefix = "drug") %>% 
  unite(steroid_iv1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

steroid_iv_use2 <- steroid_iv_use %>% 
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
steroid_iv_use2 <- inner_join(key, steroid_iv_use2, by = c("id","adm")) 
steroid_iv_use2_wide <- steroid_iv_use2 %>% 
  select(id,adm,iv_code2) %>% 
  pivot_wider(names_from = iv_code2,
              values_from = iv_code2,
              names_prefix = "drug") %>% 
  unite(steroid_iv2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, steroid_iv_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, steroid_iv_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(steroid_iv, starts_with("steroid_iv"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# Claim Procedure Data ------------------------------------------------------

claim_procedure_data %>% glimpse()
claim_procedure_data %>% colnames()
unique(claim_procedure_data$診療行為)

claim_procedure_data1 <- claim_procedure_data %>% 
  rename(id = "患者ID",
         adm = "対象日", # for joining
         code = "診療行為コード",
         name = "診療行為") %>% 
  mutate(adm = ymd(adm))
claim_procedure_data_selected1 <- inner_join(claim_procedure_data1, key, by = c("id","adm"))
claim_procedure_data_selected1 <- claim_procedure_data_selected1 %>% 
  arrange(id, adm) %>% 
  distinct(id, adm, .keep_all=TRUE) %>% 
  pivot_wider(names_from = name,
              values_from = name,
              names_prefix = "procedure") %>% 
  unite(procedure1, starts_with("procedure"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 
unique(claim_procedure_data_selected1$procedure1)

claim_procedure_data2 <- claim_procedure_data %>% 
  rename(id = "患者ID",
         adm = "対象日", # for joining
         code = "診療行為コード",
         name = "診療行為") %>% 
  mutate(adm = ymd(adm))
claim_procedure_data_selected2 <- inner_join(claim_procedure_data2, key, by = c("id","adm"))
claim_procedure_data_selected2 <- claim_procedure_data_selected2 %>% 
  arrange(id, adm) %>% 
  distinct(id, adm, .keep_all=TRUE) %>% 
  pivot_wider(names_from = name,
              values_from = name,
              names_prefix = "procedure") %>% 
  unite(procedure2, starts_with("procedure"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 
unique(claim_procedure_data_selected2$procedure2)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, claim_procedure_data_selected1, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, claim_procedure_data_selected2, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(procedure, starts_with("procedure"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) %>% 
  unite(procedure_code, starts_with("code"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE)

# EMR Laboratory Data -----------------------------------------------------

emr_lab_data %>% glimpse()
emr_lab_data %>% colnames()
unique(emr_lab_data$検査名_英語)

emr_lab_data1 <- emr_lab_data %>% 
  rename(id = "患者ID",
         adm = "検査日", # for joining
         name1 = "検査名_英語",
         data1 = "結果") %>% 
  select(id, adm, name1, data1) %>% 
  mutate(adm = ymd(adm))
emr_lab_data1_selected1 <- inner_join(emr_lab_data1, key, by = c("id","adm"))
emr_lab_data1_selected1 <- emr_lab_data1_selected1 %>% 
  arrange(id, adm) %>% 
  distinct(id, adm, name1, .keep_all=TRUE) %>% 
  pivot_wider(names_from = name1,
              values_from = data1,
              names_prefix = "first")

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, emr_lab_data1_selected1, by = c("id","adm")) 

emr_lab_data2 <- emr_lab_data %>% 
  rename(id = "患者ID",
         adm = "検査日", # for joining
         name2 = "検査名_英語",
         data2 = "結果") %>% 
  select(id, adm, name2, data2) %>% 
  mutate(adm = ymd(adm))
emr_lab_data2_selected2 <- inner_join(emr_lab_data2, key, by = c("id","adm"))
emr_lab_data2_selected2 <- emr_lab_data2_selected2 %>% 
  arrange(id, adm) %>% 
  distinct(id, adm, name2, .keep_all=TRUE) %>% 
  pivot_wider(names_from = name2,
              values_from = data2,
              names_prefix = "second")
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, emr_lab_data2_selected2, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(wbc = ifelse(is.na(firstWBC), firstWBC, secondWBC),
         alb = ifelse(is.na(firstALB), firstALB, secondALB),
         bun = ifelse(is.na(firstBUN), firstBUN, secondBUN),
         crp = ifelse(is.na(firstCRP), firstCRP, secondCRP),
         difwbc = ifelse(is.na(firstDIFWBC), firstDIFWBC, secondDIFWBC)) %>% 
  select(-firstDIFWBC, -firstALB, -firstBUN, -firstCRP , -firstWBC, 
         -secondDIFWBC, -secondWBC, -secondALB, -secondCRP, -secondBUN)

# Overall cleaning --------------------------------------------------------

dpc_ef1_data_selected %>% colnames()
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  rename(sex = "性別",
         main_code = "主傷病に対するICD10コード",
         main = "主傷病名",
         prep_code = "入院の契機となった傷病名に対するICD10コード",
         prep = "入院の契機となった傷病名",
         reso_code = "医療資源を最も投入した傷病名に対するICD10コード",
         reso = "医療資源を最も投入した傷病名",
         disc_adl = "退院時のADLスコア",
         adm_jcs = "入院時意識障害がある場合のJCS",
         disc_jcs = "退院時意識障害がある場合のJCS",
         hugh_johns = "Hugh-Jones分類",
         severity = "肺炎の重症度分類",
         adm_adl = "入院時のADLスコア",
         smk_index = "喫煙指数",
         bmi = "BMI",
         amb = "救急車による搬送の有無",
         disc = "退院年月日",
         disc_to = "退院先",
         prognosis = "退院時転帰",
         death24 = "24時間以内の死亡の有無",
         route = "入院経路",
         nhcap = "医療介護関連肺炎に該当の有無",
         
         com1 = "入院時併存症名_1",
         com2 = "入院時併存症名_2",
         com3 = "入院時併存症名_3",
         com4 = "入院時併存症名_4",
         com5 = "入院時併存症名_5",
         com6 = "入院時併存症名_6",
         com7 = "入院時併存症名_7",
         com8 = "入院時併存症名_8",
         com9 = "入院時併存症名_9",
         com10 = "入院時併存症名_10",
         subs1 = "入院後発症疾患名_1",
         subs2 = "入院後発症疾患名_2",
         subs3 = "入院後発症疾患名_3",
         subs4 = "入院後発症疾患名_4",
         subs5 = "入院後発症疾患名_5",
         subs6 = "入院後発症疾患名_6",
         subs7 = "入院後発症疾患名_7",
         subs8 = "入院後発症疾患名_8",
         subs9 = "入院後発症疾患名_9",
         subs10 = "入院後発症疾患名_10",
         
         com_code1 = "入院時併存症名に対するICD10コード_1",
         com_code2 = "入院時併存症名に対するICD10コード_2",
         com_code3 = "入院時併存症名に対するICD10コード_3",
         com_code4 = "入院時併存症名に対するICD10コード_4",
         com_code5 = "入院時併存症名に対するICD10コード_5",
         com_code6 = "入院時併存症名に対するICD10コード_6",
         com_code7 = "入院時併存症名に対するICD10コード_7",
         com_code8 = "入院時併存症名に対するICD10コード_8",
         com_code9 = "入院時併存症名に対するICD10コード_9",
         com_code10 = "入院時併存症名に対するICD10コード_10",
         subs_code1 = "入院後発症疾患名に対するICD10コード_1",
         subs_code2 = "入院後発症疾患名に対するICD10コード_2",
         subs_code3 = "入院後発症疾患名に対するICD10コード_3",
         subs_code4 = "入院後発症疾患名に対するICD10コード_4",
         subs_code5 = "入院後発症疾患名に対するICD10コード_5",
         subs_code6 = "入院後発症疾患名に対するICD10コード_6",
         subs_code7 = "入院後発症疾患名に対するICD10コード_7",
         subs_code8 = "入院後発症疾患名に対するICD10コード_8",
         subs_code9 = "入院後発症疾患名に対するICD10コード_9",
         subs_code10 = "入院後発症疾患名に対するICD10コード_10",
         
         )

dpc_ef1_data_selected %>% write_rds("output/cleaned_data.rds", compress = "gz")
