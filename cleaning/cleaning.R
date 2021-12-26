
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
             "comorbidity",
             "psych",
             "ggplot2",
             "ggplotgui",
             "ggthemes",
             "arsenal",
             "survival")
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
#patient_data = fread("2021102512_1_Patient_data_2021_002_SCE.csv.gz")
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
dpc_ef1_data %>% colnames
unique(dpc_ef1_data$項目名)

length(unique(dpc_ef1_data$患者ID)) #38767

dpc_ef1_data %>% distinct(患者ID)
dpc_ef1_data %>% distinct(患者ID, 入院日)

# main diagnosis, admission-precipitating diagnosis, or most resource-consuming diagnosis of lower tract infections 

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

# extract the information of "様式1" among the potentially included patients

dpc_ef1_data_all <- dpc_ef1_data %>% 
  arrange(患者ID, 入院日) %>% 
  rename(id = "患者ID",
         adm = "入院日") %>%
  mutate(adm = ymd(adm))

# add information except for comobidities and subsequent diseases

dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_with_c <- dpc_ef1_data_selected <- inner_join(dpc_ef1_data_all, dpc_ef1_data_duplicate, by = c("id", "adm"))
dpc_ef1_data_selected_without_c %>% colnames()
dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_without_c %>% 
  select(1,2,9,10,12,13) %>% 
  distinct(id, adm, 項目名, .keep_all=TRUE) %>% 
  pivot_wider(names_from = 項目名,
              values_from = データ)
dpc_ef1_data_selected_without_c %>% colnames()
dpc_ef1_data_selected_without_c <- dpc_ef1_data_selected_without_c %>% 
  select(id,adm,性別,生年月日,主傷病に対するICD10コード,主傷病名,入院の契機となった傷病名に対するICD10コード,
         入院の契機となった傷病名,医療資源を最も投入した傷病名に対するICD10コード,医療資源を最も投入した傷病名,
         退院時のADLスコア,入院時意識障害がある場合のJCS,退院時意識障害がある場合のJCS,`Hugh-Jones分類`,
         肺炎の重症度分類,入院時のADLスコア,喫煙指数,BMI,救急車による搬送の有無,退院年月日,
         退院先,退院時転帰,`24時間以内の死亡の有無`,入院経路,医療介護関連肺炎に該当の有無, diff_time) %>% 
  filter(!退院年月日 == "0") 
dpc_ef1_data_selected_without_c$入院時のADLスコア <- sapply(strsplit(dpc_ef1_data_selected_without_c$入院時のADLスコア,""), function(x) sum(as.numeric(x))) 
dpc_ef1_data_selected_without_c$退院時のADLスコア <- sapply(strsplit(dpc_ef1_data_selected_without_c$退院時のADLスコア,""), function(x) sum(as.numeric(x))) 

# add information of the comorbidities and subsequent diseases

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

length(unique(dpc_ef1_data_selected$id)) 

# detect recurrent lower tract infections

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  group_by(id) %>% 
  filter(n() >= 2) %>% 
  ungroup() #3615
length(unique(dpc_ef1_data_selected$id)) 

# exclude patients having events within 24 hours

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  filter(`24時間以内の死亡の有無` == "0") %>% 
  filter(!adm == ymd(退院年月日))

key <- dpc_ef1_data_selected %>% 
  distinct(id,adm)

id_key <- key %>% 
  distinct(id, .keep_all=TRUE)

length(unique(key$id)) 

# add CCI

cci <- dpc_ef1_data %>%
  filter(項目名 == "入院時併存症名に対するICD10コード") %>% 
  mutate(id = str_c(患者ID, 入院日, sep = "-")) %>% 
  rename(name = "項目名",
         code = "データ") %>% 
  select(id, code) %>% 
  arrange(id)
charlson <- comorbidity::comorbidity(x = cci, id = "id", code = "code", map = "charlson_icd10_quan", assign0 = FALSE)
charlson
cci_score <- score(charlson, weights = "quan", assign0 = FALSE)
cci_id <- cci %>% 
  distinct(id, .keep_all=TRUE) %>% 
  arrange(id)
cci_df <- cbind(cci_id, cci_score) %>% 
  select(-code) %>% 
  separate(col = id,
           into = c("id", "adm"),
           sep = "-") %>% 
  mutate(id = as.character(id),
         adm = ymd(adm),
         id = as.integer(id))

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, cci_df, by = c("id", "adm"))

# previous hospitalization within 12 months

count_key <- key %>% 
  select(id, adm)

previous <- dpc_ef1_data_all %>% 
  distinct(id, adm) %>% 
  select(id, adm)
previous <- setdiff(previous, count_key, by = c("id","adm"))
count_key <- count_key %>% 
  group_by(id) %>% 
  mutate(count = row_number()) %>% 
  ungroup()

pep_hos_df <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- previous$id
  y1 <- filter_key$adm
  y2 <- previous$adm
  pep_hos_filter <- neardate(id1, id2, y1, y2, best = "prior") # closest one before admission
  #pep_use_after <- neardate(id1, id2, y1, y2) # closest one after first admission
  pep_hos_filter <- ifelse((filter_key$adm - previous$adm[pep_hos_filter]) > 365, NA, pep_hos_filter)
  pep_hos <- previous[pep_hos_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, adm, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  pep_hos_append <- left_join(count, pep_hos, by = "id")
  pep_hos_df <- bind_rows(pep_hos_df, pep_hos_append)
}

pep_hos_df <- pep_hos_df %>% 
  arrange(id, adm.x) %>% 
  drop_na(adm.y) %>% 
  rename(pre_hos = "adm.y",
         adm = adm.x) %>% 
  select(id, adm, pre_hos)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, pep_hos_df, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(pre_hos = if_else(is.na(pre_hos), 0, 1))

# Patient data ------------------------------------------------------------

#patient_data %>% glimpse()
#patient_data %>% colnames()

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

# oral anti-peptide

oral <- read_excel("memo/oral.xlsx")
oral_pep <- read_excel("memo/pep.xlsx")
oral_pep <- oral_pep %>% 
  pull(drug)
filter_pep <- str_c(oral_pep, collapse = "|")
pep <- oral %>% 
  filter(str_detect(成分名, filter_pep)) %>% 
  select(2) %>% 
  pull()
filter_pep_code <- str_c(pep, collapse = "|")
pep_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_pep_code))
pep_use %>% glimpse()
pep_use %>% colnames()
pep_use <- pep_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         day = "開始日", # for joining
         oral_end1 = "終了日",
         oral_code1 = "薬価コード",
         oral_name1 = "薬剤名",
         oral_dose1 = "用量",
         oral_department1 = "診療科") %>% 
  mutate(day = ymd(day))

count_key <- key %>% 
  group_by(id) %>% 
  mutate(count = row_number())
max(count_key$count)

pep_use_before_df <- c()

for(i in 1:17) {
filter_key <- count_key %>% 
  filter(count == i)
id1 <- filter_key$id
id2 <- pep_use$id
y1 <- filter_key$adm
y2 <- pep_use$day
pep_use_before_filter <- neardate(id1, id2, y1, y2, best = "prior") # closest one before admission
#pep_use_after <- neardate(id1, id2, y1, y2) # closest one after first admission
pep_use_before_filter <- ifelse((filter_key$adm - pep_use$day[pep_use_before_filter]) > 30, NA, pep_use_before_filter)
pep_use_before <- pep_use[pep_use_before_filter, ] %>% 
  drop_na(id) %>% 
  distinct(id, .keep_all=TRUE)
count <- count_key %>% 
  filter(count == i)  
pep_use_before_append <- left_join(count, pep_use_before, by = "id")
pep_use_before_df <- bind_rows(pep_use_before_df, pep_use_before_append)
}

pep_use_before_df <- pep_use_before_df %>% 
  arrange(id, adm) %>% 
  drop_na(oral_code1) %>% 
  rename(pep = "oral_code1") %>% 
  select(id, adm, pep)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, pep_use_before_df, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(pep = if_else(is.na(pep), 0, 1))

# oral abx use within 90 days

oral_abx_use %>% glimpse()
oral_abx_use <- oral_abx_use %>% 
  rename(id = "患者ID",
         day = "開始日", 
         end = "終了日",
         code = "薬価コード",
         name = "薬剤名",
         dose = "用量",
         department1 = "診療科") %>% 
  mutate(day = ymd(day))

oral_abx_use_before_df <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- oral_abx_use$id
  y1 <- filter_key$adm
  y2 <- oral_abx_use$day
  oral_abx_use_before_filter <- neardate(id1, id2, y1, y2, best = "prior") # closest one before admission
  #pep_use_after <- neardate(id1, id2, y1, y2) # closest one after first admission
  oral_abx_use_before_filter <- ifelse((filter_key$adm - oral_abx_use$day[oral_abx_use_before_filter]) > 90, NA, oral_abx_use_before_filter)
  oral_abx_use_before <- oral_abx_use[oral_abx_use_before_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  oral_abx_use_before_append <- left_join(count, oral_abx_use_before, by = "id")
  oral_abx_use_before_df <- bind_rows(oral_abx_use_before_df, oral_abx_use_before_append)
}

oral_abx_use_before_df <- oral_abx_use_before_df %>% 
  arrange(id, adm) %>% 
  drop_na(code) %>% 
  rename(oral_abx90 = "code") %>% 
  select(id, adm, oral_abx90)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, oral_abx_use_before_df, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(oral_abx90 = if_else(is.na(oral_abx90), 0, 1))

# iv abx use within 90 days

iv_abx_use %>% glimpse()
iv_abx_use <- iv_abx_use %>% 
  rename(id = "患者ID",
         day = "開始日", 
         end = "終了日",
         code = "薬価コード",
         name = "薬剤名",
         dose = "用量",
         department1 = "診療科") %>% 
  mutate(day = ymd(day))

iv_abx_use_before_df <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- iv_abx_use$id
  y1 <- filter_key$adm
  y2 <- iv_abx_use$day
  iv_abx_use_before_filter <- neardate(id1, id2, y1, y2, best = "prior") # closest one before admission
  #pep_use_after <- neardate(id1, id2, y1, y2) # closest one after first admission
  iv_abx_use_before_filter <- ifelse((filter_key$adm - iv_abx_use$day[iv_abx_use_before_filter]) > 90, NA, iv_abx_use_before_filter)
  iv_abx_use_before <- iv_abx_use[iv_abx_use_before_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  iv_abx_use_before_append <- left_join(count, iv_abx_use_before, by = "id")
  iv_abx_use_before_df <- bind_rows(iv_abx_use_before_df, iv_abx_use_before_append)
}

iv_abx_use_before_df <- iv_abx_use_before_df %>% 
  arrange(id, adm) %>% 
  drop_na(code) %>% 
  rename(iv_abx90 = "code") %>% 
  select(id, adm, iv_abx90)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, iv_abx_use_before_df, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(iv_abx90 = if_else(is.na(iv_abx90), 0, 1))

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

# oral steroid use before 90 days

oral_steroid_use <- steroid_oral_use
oral_steroid_use %>% glimpse()
oral_steroid_use <- oral_steroid_use %>% 
  rename(id = "患者ID",
         day = "開始日", 
         end = "終了日",
         code = "薬価コード",
         name = "薬剤名",
         dose = "用量",
         department1 = "診療科") %>% 
  mutate(day = ymd(day))

oral_steroid_use_before_df <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- oral_steroid_use$id
  y1 <- filter_key$adm
  y2 <- oral_steroid_use$day
  oral_steroid_use_before_filter <- neardate(id1, id2, y1, y2, best = "prior") # closest one before admission
  #pep_use_after <- neardate(id1, id2, y1, y2) # closest one after first admission
  oral_steroid_use_before_filter <- ifelse((filter_key$adm - oral_steroid_use$day[oral_steroid_use_before_filter]) > 90, NA, oral_steroid_use_before_filter)
  oral_steroid_use_before <- oral_steroid_use[oral_steroid_use_before_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  oral_steroid_use_before_append <- left_join(count, oral_steroid_use_before, by = "id")
  oral_steroid_use_before_df <- bind_rows(oral_steroid_use_before_df, oral_steroid_use_before_append)
}

oral_steroid_use_before_df <- oral_steroid_use_before_df %>% 
  arrange(id, adm) %>% 
  drop_na(code) %>% 
  rename(oral_steroid90 = "code") %>% 
  select(id, adm, oral_steroid90)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, oral_steroid_use_before_df, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(oral_steroid90 = if_else(is.na(oral_steroid90), 0, 1))

# iv steroid before 90 days

iv_steroid_use <- steroid_iv_use
iv_steroid_use %>% glimpse()
iv_steroid_use <- iv_steroid_use %>% 
  rename(id = "患者ID",
         day = "開始日", 
         end = "終了日",
         code = "薬価コード",
         name = "薬剤名",
         dose = "用量",
         department1 = "診療科") %>% 
  mutate(day = ymd(day))

iv_steroid_use_before_df <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- iv_steroid_use$id
  y1 <- filter_key$adm
  y2 <- iv_steroid_use$day
  iv_steroid_use_before_filter <- neardate(id1, id2, y1, y2, best = "prior") # closest one before admission
  #pep_use_after <- neardate(id1, id2, y1, y2) # closest one after first admission
  iv_steroid_use_before_filter <- ifelse((filter_key$adm - iv_steroid_use$day[iv_steroid_use_before_filter]) > 90, NA, iv_steroid_use_before_filter)
  iv_steroid_use_before <- iv_steroid_use[iv_steroid_use_before_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  iv_steroid_use_before_append <- left_join(count, iv_steroid_use_before, by = "id")
  iv_steroid_use_before_df <- bind_rows(iv_steroid_use_before_df, iv_steroid_use_before_append)
}

iv_steroid_use_before_df <- iv_steroid_use_before_df %>% 
  arrange(id, adm) %>% 
  drop_na(code) %>% 
  rename(iv_steroid90 = "code") %>% 
  select(id, adm, iv_steroid90)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, iv_steroid_use_before_df, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(iv_steroid90 = if_else(is.na(iv_steroid90), 0, 1))


# oral immunosuppression within 90 days

oral <- read_excel("memo/oral.xlsx")
oral_immuno <- read_excel("memo/immuno.xlsx")
oral_immuno <- oral_immuno %>% 
  pull(drug)
filter_immuno <- str_c(oral_immuno, collapse = "|")
immuno <- oral %>% 
  filter(str_detect(成分名, filter_immuno)) %>% 
  select(2) %>% 
  pull()
filter_immuno_code <- str_c(immuno, collapse = "|")
immuno_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_immuno_code))
immuno_use %>% glimpse()
immuno_use %>% colnames()
immuno_use <- immuno_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         day = "開始日", # for joining
         oral_end1 = "終了日",
         oral_code1 = "薬価コード",
         oral_name1 = "薬剤名",
         oral_dose1 = "用量",
         oral_department1 = "診療科") %>% 
  mutate(day = ymd(day))

count_key <- key %>% 
  group_by(id) %>% 
  mutate(count = row_number())
max(count_key$count)

immuno_use_before_df <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- immuno_use$id
  y1 <- filter_key$adm
  y2 <- immuno_use$day
  immuno_use_before_filter <- neardate(id1, id2, y1, y2, best = "prior") # closest one before admission
  #immuno_use_after <- neardate(id1, id2, y1, y2) # closest one after first admission
  immuno_use_before_filter <- ifelse((filter_key$adm - immuno_use$day[immuno_use_before_filter]) > 30, NA, immuno_use_before_filter)
  immuno_use_before <- immuno_use[immuno_use_before_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  immuno_use_before_append <- left_join(count, immuno_use_before, by = "id")
  immuno_use_before_df <- bind_rows(immuno_use_before_df, immuno_use_before_append)
}

immuno_use_before_df <- immuno_use_before_df %>% 
  arrange(id, adm) %>% 
  drop_na(oral_code1) %>% 
  rename(oral_immuno = "oral_code1") %>% 
  select(id, adm, oral_immuno)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, immuno_use_before_df, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(oral_immuno = if_else(is.na(oral_immuno), 0, 1))

# iv immunosuppression within 90 days

iv <- read_excel("memo/iv.xlsx")
iv_immuno <- read_excel("memo/immuno.xlsx")
iv_immuno <- iv_immuno %>% 
  pull(drug)
filter_immuno <- str_c(iv_immuno, collapse = "|")
immuno <- iv %>% 
  filter(str_detect(成分名, filter_immuno)) %>% 
  select(2) %>% 
  pull()
filter_immuno_code <- str_c(immuno, collapse = "|")
immuno_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_immuno_code))
immuno_use %>% glimpse()
immuno_use %>% colnames()
immuno_use <- immuno_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         day = "開始日", # for joining
         iv_end1 = "終了日",
         iv_code1 = "薬価コード",
         iv_name1 = "薬剤名",
         iv_dose1 = "用量",
         iv_department1 = "診療科") %>% 
  mutate(day = ymd(day))

count_key <- key %>% 
  group_by(id) %>% 
  mutate(count = row_number())
max(count_key$count)

immuno_use_before_df <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- immuno_use$id
  y1 <- filter_key$adm
  y2 <- immuno_use$day
  immuno_use_before_filter <- neardate(id1, id2, y1, y2, best = "prior") # closest one before admission
  #immuno_use_after <- neardate(id1, id2, y1, y2) # closest one after first admission
  immuno_use_before_filter <- ifelse((filter_key$adm - immuno_use$day[immuno_use_before_filter]) > 30, NA, immuno_use_before_filter)
  immuno_use_before <- immuno_use[immuno_use_before_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  immuno_use_before_append <- left_join(count, immuno_use_before, by = "id")
  immuno_use_before_df <- bind_rows(immuno_use_before_df, immuno_use_before_append)
}

immuno_use_before_df <- immuno_use_before_df %>% 
  arrange(id, adm) %>% 
  drop_na(iv_code1) %>% 
  rename(iv_immuno = "iv_code1") %>% 
  select(id, adm, iv_immuno)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, immuno_use_before_df, by = c("id","adm")) 
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(iv_immuno = if_else(is.na(iv_immuno), 0, 1))

## Vasopressor
#
#iv <- read_excel("memo/iv.xlsx")
#vaso <- read_excel("memo/vaso.xlsx")
#vaso %>% colnames()
#vaso <- vaso %>% 
#  pull(drug)
#filter_vaso <- str_c(vaso, collapse = "|")
#vaso <- iv %>% 
#  filter(str_detect(成分名, filter_vaso)) %>% 
#  select(2) %>% 
#  pull()
#filter_vaso_code <- str_c(vaso, collapse = "|")
#vaso_use <- emr_drug_data %>% 
#  filter(str_detect(薬価コード, filter_vaso_code))
#vaso_use %>% glimpse()
#vaso_use %>% colnames()
#vaso_use1 <- vaso_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         vaso_end1 = "終了日",
#         vaso_code1 = "薬価コード",
#         vaso_name1 = "薬剤名",
#         vaso_dose1 = "用量",
#         vaso_department1 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         vaso_end1 = ymd(vaso_end1)) %>% 
#  distinct(id, adm, vaso_code1, .keep_all=TRUE)
#vaso_use1 <- inner_join(key, vaso_use1, by = c("id","adm")) 
#vaso_use1_wide <- vaso_use1 %>% 
#  select(id,adm,vaso_code1) %>% 
#  pivot_wider(names_from = vaso_code1,
#              values_from = vaso_code1,
#              names_prefix = "drug") %>% 
#  unite(vaso1, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#vaso_use2 <- vaso_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         vaso_end2 = "終了日",
#         vaso_code2 = "薬価コード",
#         vaso_name2 = "薬剤名",
#         vaso_dose2 = "用量",
#         vaso_department2 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         adm = adm - 1,
#         vaso_end2 = ymd(vaso_end2)) %>% 
#  distinct(id, adm, vaso_code2, .keep_all=TRUE)
#vaso_use2 <- inner_join(key, vaso_use2, by = c("id","adm")) 
#vaso_use2_wide <- vaso_use2 %>% 
#  select(id,adm,vaso_code2) %>% 
#  pivot_wider(names_from = vaso_code2,
#              values_from = vaso_code2,
#              names_prefix = "drug") %>% 
#  unite(vaso2, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, vaso_use1_wide, by = c("id","adm")) 
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, vaso_use2_wide, by = c("id","adm")) 
#
#dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
#  unite(vaso, starts_with("vaso"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
## Inhaler 
## ics 
#
#app <- read_excel("memo/applicant.xls")
#ics <- read_excel("memo/ics.xlsx")
#
#app %>% glimpse()
#app %>% colnames()
#ics %>% colnames()
#ics <- ics %>% 
#  pull(drug)
#filter_ics <- str_c(ics, collapse = "|")
#ics <- app %>% 
#  filter(str_detect(`...8`, filter_ics)) %>% 
#  select(2) %>% 
#  pull()
#filter_ics_code <- str_c(ics, collapse = "|")
#ics_use <- emr_drug_data %>% 
#  filter(str_detect(薬価コード, filter_ics_code))
#ics_use %>% glimpse()
#ics_use %>% colnames()
#ics_use1 <- ics_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         ics_end1 = "終了日",
#         ics_code1 = "薬価コード",
#         ics_name1 = "薬剤名",
#         ics_dose1 = "用量",
#         ics_department1 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         ics_end1 = ymd(ics_end1)) %>% 
#  distinct(id, adm, ics_code1, .keep_all=TRUE)
#ics_use1 <- inner_join(key, ics_use1, by = c("id","adm")) 
#ics_use1_wide <- ics_use1 %>% 
#  select(id,adm,ics_code1) %>% 
#  pivot_wider(names_from = ics_code1,
#              values_from = ics_code1,
#              names_prefix = "drug") %>% 
#  unite(ics1, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#ics_use2 <- ics_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         ics_end2 = "終了日",
#         ics_code2 = "薬価コード",
#         ics_name2 = "薬剤名",
#         ics_dose2 = "用量",
#         ics_department2 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         adm = adm - 1,
#         ics_end2 = ymd(ics_end2)) %>% 
#  distinct(id, adm, ics_code2, .keep_all=TRUE)
#ics_use2 <- inner_join(key, ics_use2, by = c("id","adm")) 
#ics_use2_wide <- ics_use2 %>% 
#  select(id,adm,ics_code2) %>% 
#  pivot_wider(names_from = ics_code2,
#              values_from = ics_code2,
#              names_prefix = "drug") %>% 
#  unite(ics2, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_use1_wide, by = c("id","adm")) 
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_use2_wide, by = c("id","adm")) 
#
#dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
#  unite(ics, starts_with("ics"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
## LABA 
#
#app <- read_excel("memo/applicant.xls")
#laba <- read_excel("memo/laba.xlsx")
#
#app %>% glimpse()
#app %>% colnames()
#laba %>% colnames()
#laba <- laba %>% 
#  pull(drug)
#filter_laba <- str_c(laba, collapse = "|")
#laba <- app %>% 
#  filter(str_detect(`...8`, filter_laba)) %>% 
#  select(2) %>% 
#  pull()
#filter_laba_code <- str_c(laba, collapse = "|")
#laba_use <- emr_drug_data %>% 
#  filter(str_detect(薬価コード, filter_laba_code))
#laba_use %>% glimpse()
#laba_use %>% colnames()
#laba_use1 <- laba_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         laba_end1 = "終了日",
#         laba_code1 = "薬価コード",
#         laba_name1 = "薬剤名",
#         laba_dose1 = "用量",
#         laba_department1 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         laba_end1 = ymd(laba_end1)) %>% 
#  distinct(id, adm, laba_code1, .keep_all=TRUE)
#laba_use1 <- inner_join(key, laba_use1, by = c("id","adm")) 
#laba_use1_wide <- laba_use1 %>% 
#  select(id,adm,laba_code1) %>% 
#  pivot_wider(names_from = laba_code1,
#              values_from = laba_code1,
#              names_prefix = "drug") %>% 
#  unite(laba1, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#laba_use2 <- laba_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         laba_end2 = "終了日",
#         laba_code2 = "薬価コード",
#         laba_name2 = "薬剤名",
#         laba_dose2 = "用量",
#         laba_department2 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         adm = adm - 1,
#         laba_end2 = ymd(laba_end2)) %>% 
#  distinct(id, adm, laba_code2, .keep_all=TRUE)
#laba_use2 <- inner_join(key, laba_use2, by = c("id","adm")) 
#laba_use2_wide <- laba_use2 %>% 
#  select(id,adm,laba_code2) %>% 
#  pivot_wider(names_from = laba_code2,
#              values_from = laba_code2,
#              names_prefix = "drug") %>% 
#  unite(laba2, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, laba_use1_wide, by = c("id","adm")) 
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, laba_use2_wide, by = c("id","adm")) 
#
#dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
#  unite(laba, starts_with("laba"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
## LAMA 
#
#app <- read_excel("memo/applicant.xls")
#lama <- read_excel("memo/lama.xlsx")
#
#app %>% glimpse()
#app %>% colnames()
#lama %>% colnames()
#lama <- lama %>% 
#  pull(drug)
#filter_lama <- str_c(lama, collapse = "|")
#lama <- app %>% 
#  filter(str_detect(`...8`, filter_lama)) %>% 
#  select(2) %>% 
#  pull()
#filter_lama_code <- str_c(lama, collapse = "|")
#lama_use <- emr_drug_data %>% 
#  filter(str_detect(薬価コード, filter_lama_code))
#lama_use %>% glimpse()
#lama_use %>% colnames()
#lama_use1 <- lama_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         lama_end1 = "終了日",
#         lama_code1 = "薬価コード",
#         lama_name1 = "薬剤名",
#         lama_dose1 = "用量",
#         lama_department1 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         lama_end1 = ymd(lama_end1)) %>% 
#  distinct(id, adm, lama_code1, .keep_all=TRUE)
#lama_use1 <- inner_join(key, lama_use1, by = c("id","adm")) 
#lama_use1_wide <- lama_use1 %>% 
#  select(id,adm,lama_code1) %>% 
#  pivot_wider(names_from = lama_code1,
#              values_from = lama_code1,
#              names_prefix = "drug") %>% 
#  unite(lama1, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#lama_use2 <- lama_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         lama_end2 = "終了日",
#         lama_code2 = "薬価コード",
#         lama_name2 = "薬剤名",
#         lama_dose2 = "用量",
#         lama_department2 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         adm = adm - 1,
#         lama_end2 = ymd(lama_end2)) %>% 
#  distinct(id, adm, lama_code2, .keep_all=TRUE)
#lama_use2 <- inner_join(key, lama_use2, by = c("id","adm")) 
#lama_use2_wide <- lama_use2 %>% 
#  select(id,adm,lama_code2) %>% 
#  pivot_wider(names_from = lama_code2,
#              values_from = lama_code2,
#              names_prefix = "drug") %>% 
#  unite(lama2, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, lama_use1_wide, by = c("id","adm")) 
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, lama_use2_wide, by = c("id","adm")) 
#
#dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
#  unite(lama, starts_with("lama"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
## ICS/LABA 
#
#app <- read_excel("memo/applicant.xls")
#ics_laba <- read_excel("memo/ics_laba.xlsx")
#
#app %>% glimpse()
#app %>% colnames()
#ics_laba %>% colnames()
#ics_laba <- ics_laba %>% 
#  pull(drug)
#filter_ics_laba <- str_c(ics_laba, collapse = "|")
#ics_laba <- app %>% 
#  filter(str_detect(`...8`, filter_ics_laba)) %>% 
#  select(2) %>% 
#  pull()
#filter_ics_laba_code <- str_c(ics_laba, collapse = "|")
#ics_laba_use <- emr_drug_data %>% 
#  filter(str_detect(薬価コード, filter_ics_laba_code))
#ics_laba_use %>% glimpse()
#ics_laba_use %>% colnames()
#ics_laba_use1 <- ics_laba_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         ics_laba_end1 = "終了日",
#         ics_laba_code1 = "薬価コード",
#         ics_laba_name1 = "薬剤名",
#         ics_laba_dose1 = "用量",
#         ics_laba_department1 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         ics_laba_end1 = ymd(ics_laba_end1)) %>% 
#  distinct(id, adm, ics_laba_code1, .keep_all=TRUE)
#ics_laba_use1 <- inner_join(key, ics_laba_use1, by = c("id","adm")) 
#ics_laba_use1_wide <- ics_laba_use1 %>% 
#  select(id,adm,ics_laba_code1) %>% 
#  pivot_wider(names_from = ics_laba_code1,
#              values_from = ics_laba_code1,
#              names_prefix = "drug") %>% 
#  unite(ics_laba1, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#ics_laba_use2 <- ics_laba_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         ics_laba_end2 = "終了日",
#         ics_laba_code2 = "薬価コード",
#         ics_laba_name2 = "薬剤名",
#         ics_laba_dose2 = "用量",
#         ics_laba_department2 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         adm = adm - 1,
#         ics_laba_end2 = ymd(ics_laba_end2)) %>% 
#  distinct(id, adm, ics_laba_code2, .keep_all=TRUE)
#ics_laba_use2 <- inner_join(key, ics_laba_use2, by = c("id","adm")) 
#ics_laba_use2_wide <- ics_laba_use2 %>% 
#  select(id,adm,ics_laba_code2) %>% 
#  pivot_wider(names_from = ics_laba_code2,
#              values_from = ics_laba_code2,
#              names_prefix = "drug") %>% 
#  unite(ics_laba2, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_laba_use1_wide, by = c("id","adm")) 
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_laba_use2_wide, by = c("id","adm")) 
#
#dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
#  unite(ics_laba, starts_with("ics_laba"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
## LABA/LAMA 
#
#app <- read_excel("memo/applicant.xls")
#laba_lama <- read_excel("memo/laba_lama.xlsx")
#
#app %>% glimpse()
#app %>% colnames()
#laba_lama %>% colnames()
#laba_lama <- laba_lama %>% 
#  pull(drug)
#filter_laba_lama <- str_c(laba_lama, collapse = "|")
#laba_lama <- app %>% 
#  filter(str_detect(`...8`, filter_laba_lama)) %>% 
#  select(2) %>% 
#  pull()
#filter_laba_lama_code <- str_c(laba_lama, collapse = "|")
#laba_lama_use <- emr_drug_data %>% 
#  filter(str_detect(薬価コード, filter_laba_lama_code))
#laba_lama_use %>% glimpse()
#laba_lama_use %>% colnames()
#laba_lama_use1 <- laba_lama_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         laba_lama_end1 = "終了日",
#         laba_lama_code1 = "薬価コード",
#         laba_lama_name1 = "薬剤名",
#         laba_lama_dose1 = "用量",
#         laba_lama_department1 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         laba_lama_end1 = ymd(laba_lama_end1)) %>% 
#  distinct(id, adm, laba_lama_code1, .keep_all=TRUE)
#laba_lama_use1 <- inner_join(key, laba_lama_use1, by = c("id","adm")) 
#laba_lama_use1_wide <- laba_lama_use1 %>% 
#  select(id,adm,laba_lama_code1) %>% 
#  pivot_wider(names_from = laba_lama_code1,
#              values_from = laba_lama_code1,
#              names_prefix = "drug") %>% 
#  unite(laba_lama1, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#laba_lama_use2 <- laba_lama_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         laba_lama_end2 = "終了日",
#         laba_lama_code2 = "薬価コード",
#         laba_lama_name2 = "薬剤名",
#         laba_lama_dose2 = "用量",
#         laba_lama_department2 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         adm = adm - 1,
#         laba_lama_end2 = ymd(laba_lama_end2)) %>% 
#  distinct(id, adm, laba_lama_code2, .keep_all=TRUE)
#laba_lama_use2 <- inner_join(key, laba_lama_use2, by = c("id","adm")) 
#laba_lama_use2_wide <- laba_lama_use2 %>% 
#  select(id,adm,laba_lama_code2) %>% 
#  pivot_wider(names_from = laba_lama_code2,
#              values_from = laba_lama_code2,
#              names_prefix = "drug") %>% 
#  unite(laba_lama2, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, laba_lama_use1_wide, by = c("id","adm")) 
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, laba_lama_use2_wide, by = c("id","adm")) 
#
#dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
#  unite(laba_lama, starts_with("laba_lama"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
## ICS/LABA/LAMA 
#
#app <- read_excel("memo/applicant.xls")
#ics_laba_lama <- read_excel("memo/ics_laba_lama.xlsx")
#
#app %>% glimpse()
#app %>% colnames()
#ics_laba_lama %>% colnames()
#ics_laba_lama <- ics_laba_lama %>% 
#  pull(drug)
#filter_ics_laba_lama <- str_c(ics_laba_lama, collapse = "|")
#ics_laba_lama <- app %>% 
#  filter(str_detect(`...8`, filter_ics_laba_lama)) %>% 
#  select(2) %>% 
#  pull()
#filter_ics_laba_lama_code <- str_c(ics_laba_lama, collapse = "|")
#ics_laba_lama_use <- emr_drug_data %>% 
#  filter(str_detect(薬価コード, filter_ics_laba_lama_code))
#ics_laba_lama_use %>% glimpse()
#ics_laba_lama_use %>% colnames()
#ics_laba_lama_use1 <- ics_laba_lama_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         ics_laba_lama_end1 = "終了日",
#         ics_laba_lama_code1 = "薬価コード",
#         ics_laba_lama_name1 = "薬剤名",
#         ics_laba_lama_dose1 = "用量",
#         ics_laba_lama_department1 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         ics_laba_lama_end1 = ymd(ics_laba_lama_end1)) %>% 
#  distinct(id, adm, ics_laba_lama_code1, .keep_all=TRUE)
#ics_laba_lama_use1 <- inner_join(key, ics_laba_lama_use1, by = c("id","adm")) 
#ics_laba_lama_use1_wide <- ics_laba_lama_use1 %>% 
#  select(id,adm,ics_laba_lama_code1) %>% 
#  pivot_wider(names_from = ics_laba_lama_code1,
#              values_from = ics_laba_lama_code1,
#              names_prefix = "drug") %>% 
#  unite(ics_laba_lama1, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#ics_laba_lama_use2 <- ics_laba_lama_use %>% 
#  select(1,3,4,5,7,8,9) %>% 
#  rename(id = "患者ID",
#         adm = "開始日", # for joining
#         ics_laba_lama_end2 = "終了日",
#         ics_laba_lama_code2 = "薬価コード",
#         ics_laba_lama_name2 = "薬剤名",
#         ics_laba_lama_dose2 = "用量",
#         ics_laba_lama_department2 = "診療科") %>% 
#  mutate(adm = ymd(adm),
#         adm = adm - 1,
#         ics_laba_lama_end2 = ymd(ics_laba_lama_end2)) %>% 
#  distinct(id, adm, ics_laba_lama_code2, .keep_all=TRUE)
#ics_laba_lama_use2 <- inner_join(key, ics_laba_lama_use2, by = c("id","adm")) 
#ics_laba_lama_use2_wide <- ics_laba_lama_use2 %>% 
#  select(id,adm,ics_laba_lama_code2) %>% 
#  pivot_wider(names_from = ics_laba_lama_code2,
#              values_from = ics_laba_lama_code2,
#              names_prefix = "drug") %>% 
#  unite(ics_laba_lama2, starts_with("drug"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_laba_lama_use1_wide, by = c("id","adm")) 
#dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_laba_lama_use2_wide, by = c("id","adm")) 
#
#dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
#  unite(ics_laba_lama, starts_with("ics_laba_lama"),
#        sep = "_",
#        remove = TRUE,
#        na.rm = TRUE) 
#
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

claim_procedure_data <- claim_procedure_data %>% 
  rename(id = "患者ID",
         day = "対象日",
         code = "診療行為コード",
         name = "診療行為") %>% 
  mutate(id = as.character(id),
         day = ymd(day)) 

key <- dpc_ef1_data_selected %>% 
  distinct(id, adm, 退院年月日) %>% 
  rename(disc = "退院年月日")

id_key <- key %>% 
  distinct(id, .keep_all=TRUE)

count_key <- key %>% 
  group_by(id) %>% 
  mutate(count = row_number(),
         disc = ymd(disc))
max(count_key$count)

## mechanical ventilation

ventilation <- claim_procedure_data %>% 
  filter(name == "人工呼吸" | name == "人工呼吸（５時間超）" | name == "救命のための気管内挿管" | name == "人工呼吸（鼻マスク式人工呼吸器）" | 
           name == "人工呼吸（鼻マスク式人工呼吸器）（５時間超）" | name == "人工呼吸（閉鎖循環式麻酔装置）（５時間超）" | name == "人工呼吸（閉鎖循環式麻酔装置）" | 
           name == "ＣＰＡＰ" | name == "ＣＰＡＰ（５時間超）" | name == "ＩＭＶ（５時間超）") %>% 
  mutate(id = as.integer(id)) %>% 
  arrange(id, day) %>% 
  distinct(id, day, .keep_all=TRUE)

venti_use <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- ventilation$id
  y1 <- filter_key$adm
  y2 <- ventilation$day
  venti_filter <- neardate(id1, id2, y1, y2)
  ventilation_select <- ventilation[venti_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  venti_use_append <- left_join(count, ventilation_select, by = "id")
  venti_use <- bind_rows(venti_use, venti_use_append)
}

# don't care about the error
venti_use <- venti_use %>% 
  filter(day < disc) %>% 
  arrange(id, adm) %>% 
  drop_na(code) %>% 
  rename(ventilation = "code") %>% 
  select(id, adm, ventilation)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, venti_use, by = c("id", "adm"))
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(ventilation = if_else(is.na(ventilation), 0, 1))

## dialysis

dialysis <- claim_procedure_data %>% 
  filter(name == "持続緩徐式血液濾過" | name == "障害者等加算（持続緩徐式血液濾過）" | name == "人工腎臓（その他）" | name == "透析液水質確保加算（人工腎臓）" | 
           name == "人工腎臓（導入期）加算"  | name == "人工腎臓（慢性維持透析）（４時間未満）" | name == "透析液水質確保加算２" | 
           name == "障害者等加算（人工腎臓）" | name == "人工腎臓（慢性維持透析１）（４時間未満）"  | name == "慢性維持透析濾過加算（人工腎臓）" |
           name == "人工腎臓（慢性維持透析１）（４時間以上５時間未満）" | name == "時間外・休日加算（人工腎臓）" | name ==  "導入期加算２（人工腎臓）" |
           name == "人工腎臓（慢性維持透析濾過）（複雑）" | name == "人工腎臓（慢性維持透析）（４時間以上５時間未満）" | name == "透析液水質確保加算１" |
           name == "人工腎臓（慢性維持透析）（５時間以上）" | name == "人工腎臓（慢性維持透析１）（４時間未満）（イを除く）" | name == "人工腎臓（慢性維持透析１）（５時間以上）" | name == "長時間加算（人工腎臓）") %>%  
  mutate(id = as.integer(id)) %>%
  arrange(id, day) %>% 
  distinct(id, day, .keep_all=TRUE)

dialysis_use <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- dialysis$id
  y1 <- filter_key$adm
  y2 <- dialysis$day
  dialysis_filter <- neardate(id1, id2, y1, y2, best = "prior")
  dialysis_select <- dialysis[dialysis_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  dialysis_use_append <- left_join(count, dialysis_select, by = "id")
  dialysis_use <- bind_rows(dialysis_use, dialysis_use_append)
}

# don't care about the error
dialysis_use <- dialysis_use %>% 
  arrange(id, adm) %>% 
  drop_na(code) %>% 
  rename(dialysis = "code") %>% 
  select(id, adm, dialysis)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, dialysis_use, by = c("id", "adm"))
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(dialysis = if_else(is.na(dialysis), 0, 1))

# intubation

intubation <- claim_procedure_data %>% 
  filter(name == "救命のための気管内挿管") %>% 
  mutate(id = as.integer(id)) %>%
  arrange(id, day) %>% 
  distinct(id, day, .keep_all=TRUE)

intubation_use <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- intubation$id
  y1 <- filter_key$adm
  y2 <- intubation$day
  intubation_filter <- neardate(id1, id2, y1, y2)
  intubation_select <- intubation[intubation_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  intubation_use_append <- left_join(count, intubation_select, by = "id")
  intubation_use <- bind_rows(intubation_use, intubation_use_append)
}

# don't care about the error
intubation_use <- intubation_use %>% 
  filter(day < disc) %>% 
  arrange(id, adm) %>% 
  drop_na(code) %>% 
  rename(intubation = "code") %>% 
  select(id, adm, intubation)

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, intubation_use, by = c("id", "adm"))
dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  mutate(intubation = if_else(is.na(intubation), 0, 1))

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
  select(-disc) %>% 
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
  select(-disc) %>% 
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
         birthday = "生年月日",
         main_code = "主傷病に対するICD10コード",
         main = "主傷病名",
         prep_code = "入院の契機となった傷病名に対するICD10コード",
         prep = "入院の契機となった傷病名",
         reso_code = "医療資源を最も投入した傷病名に対するICD10コード",
         reso = "医療資源を最も投入した傷病名",
         disc_adl = "退院時のADLスコア",
         adm_jcs = "入院時意識障害がある場合のJCS",
         disc = "退院年月日",
         disc_jcs = "退院時意識障害がある場合のJCS",
         hugh_johns = "Hugh-Jones分類",
         severity = "肺炎の重症度分類",
         adm_adl = "入院時のADLスコア",
         smk_index = "喫煙指数",
         bmi = "BMI",
         amb = "救急車による搬送の有無",
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
