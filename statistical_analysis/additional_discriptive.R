
# Hospital beds -----------------------------------------------------------

patient_data = fread("input/2021102512_1_Patient_data_2021_002_SCE.csv.gz")
patient_data <- patient_data %>% 
  rename(id = "患者ID")

df <- read_rds("output/cleaned_data.rds")
key <- df %>% 
  distinct(id,adm)

id_key <- key %>% 
  distinct(id, .keep_all=TRUE)

beds <- left_join(id_key, patient_data, by = "id")  
beds_id <- beds %>% 
  filter(str_detect(beds$病床数, "500床以上") | str_detect(beds$病床数, "300床以上500床未満"))
inner_join(df, beds_id, by = "id")

2623/3455


# Vasopressor -------------------------------------------------------------------

iv <- read_excel("memo/iv.xlsx")
vaso <- read_excel("memo/vaso.xlsx")
vaso %>% colnames()
vaso <- vaso %>% 
  pull(drug)
filter_vaso <- str_c(vaso, collapse = "|")
vaso <- iv %>% 
  filter(str_detect(成分名, filter_vaso)) %>% 
  select(2) %>% 
  pull()
filter_vaso_code <- str_c(vaso, collapse = "|")
vaso_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_vaso_code))
vaso_use %>% glimpse()
vaso_use %>% colnames()
vaso_use1 <- vaso_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         vaso_end1 = "終了日",
         vaso_code1 = "薬価コード",
         vaso_name1 = "薬剤名",
         vaso_dose1 = "用量",
         vaso_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         vaso_end1 = ymd(vaso_end1)) %>% 
  distinct(id, adm, vaso_code1, .keep_all=TRUE)
vaso_use1 <- inner_join(key, vaso_use1, by = c("id","adm")) 
vaso_use1_wide <- vaso_use1 %>% 
  select(id,adm,vaso_code1) %>% 
  pivot_wider(names_from = vaso_code1,
              values_from = vaso_code1,
              names_prefix = "drug") %>% 
  unite(vaso1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

vaso_use2 <- vaso_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         vaso_end2 = "終了日",
         vaso_code2 = "薬価コード",
         vaso_name2 = "薬剤名",
         vaso_dose2 = "用量",
         vaso_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         vaso_end2 = ymd(vaso_end2)) %>% 
  distinct(id, adm, vaso_code2, .keep_all=TRUE)
vaso_use2 <- inner_join(key, vaso_use2, by = c("id","adm")) 
vaso_use2_wide <- vaso_use2 %>% 
  select(id,adm,vaso_code2) %>% 
  pivot_wider(names_from = vaso_code2,
              values_from = vaso_code2,
              names_prefix = "drug") %>% 
  unite(vaso2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, vaso_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, vaso_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(vaso, starts_with("vaso"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# ics -----------------------------------------------------------------

app <- read_excel("memo/applicant.xls")
ics <- read_excel("memo/ics.xlsx")

app %>% glimpse()
app %>% colnames()
ics %>% colnames()
ics <- ics %>% 
  pull(drug)
filter_ics <- str_c(ics, collapse = "|")
ics <- app %>% 
  filter(str_detect(`...8`, filter_ics)) %>% 
  select(2) %>% 
  pull()
filter_ics_code <- str_c(ics, collapse = "|")
ics_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_ics_code))
ics_use %>% glimpse()
ics_use %>% colnames()
ics_use1 <- ics_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         ics_end1 = "終了日",
         ics_code1 = "薬価コード",
         ics_name1 = "薬剤名",
         ics_dose1 = "用量",
         ics_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         ics_end1 = ymd(ics_end1)) %>% 
  distinct(id, adm, ics_code1, .keep_all=TRUE)
ics_use1 <- inner_join(key, ics_use1, by = c("id","adm")) 
ics_use1_wide <- ics_use1 %>% 
  select(id,adm,ics_code1) %>% 
  pivot_wider(names_from = ics_code1,
              values_from = ics_code1,
              names_prefix = "drug") %>% 
  unite(ics1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

ics_use2 <- ics_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         ics_end2 = "終了日",
         ics_code2 = "薬価コード",
         ics_name2 = "薬剤名",
         ics_dose2 = "用量",
         ics_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         ics_end2 = ymd(ics_end2)) %>% 
  distinct(id, adm, ics_code2, .keep_all=TRUE)
ics_use2 <- inner_join(key, ics_use2, by = c("id","adm")) 
ics_use2_wide <- ics_use2 %>% 
  select(id,adm,ics_code2) %>% 
  pivot_wider(names_from = ics_code2,
              values_from = ics_code2,
              names_prefix = "drug") %>% 
  unite(ics2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(ics, starts_with("ics"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# LABA --------------------------------------------------------------------


# LAMA --------------------------------------------------------------------



# ICS/LABA ----------------------------------------------------------------


# LABA/LAMA ---------------------------------------------------------------


# ICS/LABA/LAMA -----------------------------------------------------------



# Atypical change ---------------------------------------------------------

emr_drug_data = fread("input/2021102512_3_EMR_Drug_data_2021_002_SCE.csv.gz")

id_key <- df %>% 
  distinct(id)

emr_drug_data %>% colnames()

oral <- read_excel("memo/oral.xlsx")
iv <- read_excel("memo/iv.xlsx")
anti_atypical <- read_excel("memo/anti_atypical.xlsx")
anti_atypical <- anti_atypical %>% 
  pull(drug)
filter_anti_atypical <- str_c(anti_atypical, collapse = "|")
anti_atypical_oral <- oral %>% 
  filter(str_detect(成分名, filter_anti_atypical)) %>% 
  select(2) %>% 
  pull()
filter_anti_atypical_oral_code <- str_c(anti_atypical_oral, collapse = "|")

anti_atypical <- read_excel("memo/anti_atypical.xlsx")
anti_atypical <- anti_atypical %>% 
  pull(drug)
filter_anti_atypical <- str_c(anti_atypical, collapse = "|")
anti_atypical_iv <- iv %>% 
  filter(str_detect(成分名, filter_anti_atypical)) %>% 
  select(2) %>% 
  pull()
filter_anti_atypical_iv_code <- str_c(anti_atypical_iv, collapse = "|")

abx_use_change <- emr_drug_data %>% 
  select(1,3,4,8) %>% 
  rename(id = "患者ID",
         start = "開始日",
         code = "薬価コード",
         name = "薬剤名") %>% 
  distinct(id, name, .keep_all=TRUE) %>% 
  mutate(atypical_tag = ifelse(str_detect(code, c(filter_anti_atypical_oral_code, filter_anti_atypical_iv_code)), 1, 0)) %>% 
  distinct(id, atypical_tag, .keep_all=TRUE) %>% 
  group_by(id) %>% 
  filter(n() >= 2) %>% 
  mutate(lag_atypical_tag = lag(atypical_tag),
         lag_start = lag(start),
         diff = atypical_tag - lag_atypical_tag,
         diff_time = start - lag_start) %>% 
  #filter(0 < diff_time & diff_time <= 7) %>% 
  ungroup()

selected_abx_use_change <- abx_use_change %>%
  filter(diff == 1) %>% 
  distinct(id, .keep_all=TRUE) %>% # confirm no more than 2 change
  filter(0 < diff_time & diff_time <= 7) 

selected_abx_use_change_final <- inner_join(id_key, selected_abx_use_change, by = "id")

# MRSA --------------------------------------------------------------------

emr_drug_data = fread("input/2021102512_3_EMR_Drug_data_2021_002_SCE.csv.gz")

id_key <- df %>% 
  distinct(id)

emr_drug_data %>% colnames()

oral <- read_excel("memo/oral.xlsx")
iv <- read_excel("memo/iv.xlsx")
anti_mrsa <- read_excel("memo/anti_mrsa.xlsx")
anti_mrsa <- anti_mrsa %>% 
  pull(drug)
filter_anti_mrsa <- str_c(anti_mrsa, collapse = "|")
anti_mrsa_oral <- oral %>% 
  filter(str_detect(成分名, filter_anti_mrsa)) %>% 
  select(2) %>% 
  pull()
filter_anti_mrsa_oral_code <- str_c(anti_mrsa_oral, collapse = "|")

anti_mrsa <- read_excel("memo/anti_mrsa.xlsx")
anti_mrsa <- anti_mrsa %>% 
  pull(drug)
filter_anti_mrsa <- str_c(anti_mrsa, collapse = "|")
anti_mrsa_iv <- iv %>% 
  filter(str_detect(成分名, filter_anti_mrsa)) %>% 
  select(2) %>% 
  pull()
filter_anti_mrsa_iv_code <- str_c(anti_mrsa_iv, collapse = "|")

abx_use_change <- emr_drug_data %>% 
  select(1,3,4,8) %>% 
  rename(id = "患者ID",
         start = "開始日",
         code = "薬価コード",
         name = "薬剤名") %>% 
  distinct(id, name, .keep_all=TRUE) %>% 
  mutate(mrsa_tag = ifelse(str_detect(code, c(filter_anti_mrsa_oral_code, filter_anti_mrsa_iv_code)), 1, 0)) %>% 
  distinct(id, mrsa_tag, .keep_all=TRUE) %>% 
  group_by(id) %>% 
  filter(n() >= 2) %>% 
  mutate(lag_mrsa_tag = lag(mrsa_tag),
         lag_start = lag(start),
         diff = mrsa_tag - lag_mrsa_tag,
         diff_time = start - lag_start) %>% 
  #filter(0 < diff_time & diff_time <= 7) %>% 
  ungroup()

selected_abx_use_change <- abx_use_change %>%
  filter(diff == 1) %>% 
  distinct(id, .keep_all=TRUE) %>% # confirm no more than 2 change
  filter(0 < diff_time & diff_time <= 7) 

selected_abx_use_change_final <- inner_join(id_key, selected_abx_use_change, by = "id")

