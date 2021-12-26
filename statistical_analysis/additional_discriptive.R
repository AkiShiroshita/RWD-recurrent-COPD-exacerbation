
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

5095/6601


# Vasopressor

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

app <- read_excel("memo/applicant.xls")
laba <- read_excel("memo/laba.xlsx")

app %>% glimpse()
app %>% colnames()
laba %>% colnames()
laba <- laba %>% 
  pull(drug)
filter_laba <- str_c(laba, collapse = "|")
laba <- app %>% 
  filter(str_detect(`...8`, filter_laba)) %>% 
  select(2) %>% 
  pull()
filter_laba_code <- str_c(laba, collapse = "|")
laba_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_laba_code))
laba_use %>% glimpse()
laba_use %>% colnames()
laba_use1 <- laba_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         laba_end1 = "終了日",
         laba_code1 = "薬価コード",
         laba_name1 = "薬剤名",
         laba_dose1 = "用量",
         laba_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         laba_end1 = ymd(laba_end1)) %>% 
  distinct(id, adm, laba_code1, .keep_all=TRUE)
laba_use1 <- inner_join(key, laba_use1, by = c("id","adm")) 
laba_use1_wide <- laba_use1 %>% 
  select(id,adm,laba_code1) %>% 
  pivot_wider(names_from = laba_code1,
              values_from = laba_code1,
              names_prefix = "drug") %>% 
  unite(laba1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

laba_use2 <- laba_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         laba_end2 = "終了日",
         laba_code2 = "薬価コード",
         laba_name2 = "薬剤名",
         laba_dose2 = "用量",
         laba_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         laba_end2 = ymd(laba_end2)) %>% 
  distinct(id, adm, laba_code2, .keep_all=TRUE)
laba_use2 <- inner_join(key, laba_use2, by = c("id","adm")) 
laba_use2_wide <- laba_use2 %>% 
  select(id,adm,laba_code2) %>% 
  pivot_wider(names_from = laba_code2,
              values_from = laba_code2,
              names_prefix = "drug") %>% 
  unite(laba2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, laba_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, laba_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(laba, starts_with("laba"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# LAMA --------------------------------------------------------------------

app <- read_excel("memo/applicant.xls")
lama <- read_excel("memo/lama.xlsx")

app %>% glimpse()
app %>% colnames()
lama %>% colnames()
lama <- lama %>% 
  pull(drug)
filter_lama <- str_c(lama, collapse = "|")
lama <- app %>% 
  filter(str_detect(`...8`, filter_lama)) %>% 
  select(2) %>% 
  pull()
filter_lama_code <- str_c(lama, collapse = "|")
lama_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_lama_code))
lama_use %>% glimpse()
lama_use %>% colnames()
lama_use1 <- lama_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         lama_end1 = "終了日",
         lama_code1 = "薬価コード",
         lama_name1 = "薬剤名",
         lama_dose1 = "用量",
         lama_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         lama_end1 = ymd(lama_end1)) %>% 
  distinct(id, adm, lama_code1, .keep_all=TRUE)
lama_use1 <- inner_join(key, lama_use1, by = c("id","adm")) 
lama_use1_wide <- lama_use1 %>% 
  select(id,adm,lama_code1) %>% 
  pivot_wider(names_from = lama_code1,
              values_from = lama_code1,
              names_prefix = "drug") %>% 
  unite(lama1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

lama_use2 <- lama_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         lama_end2 = "終了日",
         lama_code2 = "薬価コード",
         lama_name2 = "薬剤名",
         lama_dose2 = "用量",
         lama_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         lama_end2 = ymd(lama_end2)) %>% 
  distinct(id, adm, lama_code2, .keep_all=TRUE)
lama_use2 <- inner_join(key, lama_use2, by = c("id","adm")) 
lama_use2_wide <- lama_use2 %>% 
  select(id,adm,lama_code2) %>% 
  pivot_wider(names_from = lama_code2,
              values_from = lama_code2,
              names_prefix = "drug") %>% 
  unite(lama2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, lama_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, lama_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(lama, starts_with("lama"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# ICS/LABA ----------------------------------------------------------------

app <- read_excel("memo/applicant.xls")
ics_laba <- read_excel("memo/ics_laba.xlsx")

app %>% glimpse()
app %>% colnames()
ics_laba %>% colnames()
ics_laba <- ics_laba %>% 
  pull(drug)
filter_ics_laba <- str_c(ics_laba, collapse = "|")
ics_laba <- app %>% 
  filter(str_detect(`...8`, filter_ics_laba)) %>% 
  select(2) %>% 
  pull()
filter_ics_laba_code <- str_c(ics_laba, collapse = "|")
ics_laba_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_ics_laba_code))
ics_laba_use %>% glimpse()
ics_laba_use %>% colnames()
ics_laba_use1 <- ics_laba_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         ics_laba_end1 = "終了日",
         ics_laba_code1 = "薬価コード",
         ics_laba_name1 = "薬剤名",
         ics_laba_dose1 = "用量",
         ics_laba_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         ics_laba_end1 = ymd(ics_laba_end1)) %>% 
  distinct(id, adm, ics_laba_code1, .keep_all=TRUE)
ics_laba_use1 <- inner_join(key, ics_laba_use1, by = c("id","adm")) 
ics_laba_use1_wide <- ics_laba_use1 %>% 
  select(id,adm,ics_laba_code1) %>% 
  pivot_wider(names_from = ics_laba_code1,
              values_from = ics_laba_code1,
              names_prefix = "drug") %>% 
  unite(ics_laba1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

ics_laba_use2 <- ics_laba_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         ics_laba_end2 = "終了日",
         ics_laba_code2 = "薬価コード",
         ics_laba_name2 = "薬剤名",
         ics_laba_dose2 = "用量",
         ics_laba_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         ics_laba_end2 = ymd(ics_laba_end2)) %>% 
  distinct(id, adm, ics_laba_code2, .keep_all=TRUE)
ics_laba_use2 <- inner_join(key, ics_laba_use2, by = c("id","adm")) 
ics_laba_use2_wide <- ics_laba_use2 %>% 
  select(id,adm,ics_laba_code2) %>% 
  pivot_wider(names_from = ics_laba_code2,
              values_from = ics_laba_code2,
              names_prefix = "drug") %>% 
  unite(ics_laba2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_laba_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_laba_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(ics_laba, starts_with("ics_laba"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# LABA/LAMA ---------------------------------------------------------------

app <- read_excel("memo/applicant.xls")
laba_lama <- read_excel("memo/laba_lama.xlsx")

app %>% glimpse()
app %>% colnames()
laba_lama %>% colnames()
laba_lama <- laba_lama %>% 
  pull(drug)
filter_laba_lama <- str_c(laba_lama, collapse = "|")
laba_lama <- app %>% 
  filter(str_detect(`...8`, filter_laba_lama)) %>% 
  select(2) %>% 
  pull()
filter_laba_lama_code <- str_c(laba_lama, collapse = "|")
laba_lama_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_laba_lama_code))
laba_lama_use %>% glimpse()
laba_lama_use %>% colnames()
laba_lama_use1 <- laba_lama_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         laba_lama_end1 = "終了日",
         laba_lama_code1 = "薬価コード",
         laba_lama_name1 = "薬剤名",
         laba_lama_dose1 = "用量",
         laba_lama_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         laba_lama_end1 = ymd(laba_lama_end1)) %>% 
  distinct(id, adm, laba_lama_code1, .keep_all=TRUE)
laba_lama_use1 <- inner_join(key, laba_lama_use1, by = c("id","adm")) 
laba_lama_use1_wide <- laba_lama_use1 %>% 
  select(id,adm,laba_lama_code1) %>% 
  pivot_wider(names_from = laba_lama_code1,
              values_from = laba_lama_code1,
              names_prefix = "drug") %>% 
  unite(laba_lama1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

laba_lama_use2 <- laba_lama_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         laba_lama_end2 = "終了日",
         laba_lama_code2 = "薬価コード",
         laba_lama_name2 = "薬剤名",
         laba_lama_dose2 = "用量",
         laba_lama_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         laba_lama_end2 = ymd(laba_lama_end2)) %>% 
  distinct(id, adm, laba_lama_code2, .keep_all=TRUE)
laba_lama_use2 <- inner_join(key, laba_lama_use2, by = c("id","adm")) 
laba_lama_use2_wide <- laba_lama_use2 %>% 
  select(id,adm,laba_lama_code2) %>% 
  pivot_wider(names_from = laba_lama_code2,
              values_from = laba_lama_code2,
              names_prefix = "drug") %>% 
  unite(laba_lama2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, laba_lama_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, laba_lama_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(laba_lama, starts_with("laba_lama"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

# ICS/LABA/LAMA -----------------------------------------------------------

app <- read_excel("memo/applicant.xls")
ics_laba_lama <- read_excel("memo/ics_laba_lama.xlsx")

app %>% glimpse()
app %>% colnames()
ics_laba_lama %>% colnames()
ics_laba_lama <- ics_laba_lama %>% 
  pull(drug)
filter_ics_laba_lama <- str_c(ics_laba_lama, collapse = "|")
ics_laba_lama <- app %>% 
  filter(str_detect(`...8`, filter_ics_laba_lama)) %>% 
  select(2) %>% 
  pull()
filter_ics_laba_lama_code <- str_c(ics_laba_lama, collapse = "|")
ics_laba_lama_use <- emr_drug_data %>% 
  filter(str_detect(薬価コード, filter_ics_laba_lama_code))
ics_laba_lama_use %>% glimpse()
ics_laba_lama_use %>% colnames()
ics_laba_lama_use1 <- ics_laba_lama_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         ics_laba_lama_end1 = "終了日",
         ics_laba_lama_code1 = "薬価コード",
         ics_laba_lama_name1 = "薬剤名",
         ics_laba_lama_dose1 = "用量",
         ics_laba_lama_department1 = "診療科") %>% 
  mutate(adm = ymd(adm),
         ics_laba_lama_end1 = ymd(ics_laba_lama_end1)) %>% 
  distinct(id, adm, ics_laba_lama_code1, .keep_all=TRUE)
ics_laba_lama_use1 <- inner_join(key, ics_laba_lama_use1, by = c("id","adm")) 
ics_laba_lama_use1_wide <- ics_laba_lama_use1 %>% 
  select(id,adm,ics_laba_lama_code1) %>% 
  pivot_wider(names_from = ics_laba_lama_code1,
              values_from = ics_laba_lama_code1,
              names_prefix = "drug") %>% 
  unite(ics_laba_lama1, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

ics_laba_lama_use2 <- ics_laba_lama_use %>% 
  select(1,3,4,5,7,8,9) %>% 
  rename(id = "患者ID",
         adm = "開始日", # for joining
         ics_laba_lama_end2 = "終了日",
         ics_laba_lama_code2 = "薬価コード",
         ics_laba_lama_name2 = "薬剤名",
         ics_laba_lama_dose2 = "用量",
         ics_laba_lama_department2 = "診療科") %>% 
  mutate(adm = ymd(adm),
         adm = adm - 1,
         ics_laba_lama_end2 = ymd(ics_laba_lama_end2)) %>% 
  distinct(id, adm, ics_laba_lama_code2, .keep_all=TRUE)
ics_laba_lama_use2 <- inner_join(key, ics_laba_lama_use2, by = c("id","adm")) 
ics_laba_lama_use2_wide <- ics_laba_lama_use2 %>% 
  select(id,adm,ics_laba_lama_code2) %>% 
  pivot_wider(names_from = ics_laba_lama_code2,
              values_from = ics_laba_lama_code2,
              names_prefix = "drug") %>% 
  unite(ics_laba_lama2, starts_with("drug"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_laba_lama_use1_wide, by = c("id","adm")) 
dpc_ef1_data_selected <- left_join(dpc_ef1_data_selected, ics_laba_lama_use2_wide, by = c("id","adm")) 

dpc_ef1_data_selected <- dpc_ef1_data_selected %>% 
  unite(ics_laba_lama, starts_with("ics_laba_lama"),
        sep = "_",
        remove = TRUE,
        na.rm = TRUE) 

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

# CCI ---------------------------------------------------------------------

dpc_ef1_data = fread("input/2021102512_71_DPC_FF1_data_2021_002_SCE.csv.gz")
cci <- dpc_ef1_data %>%
  filter(項目名 == "入院時併存症名に対するICD10コード") %>% 
  mutate(id = str_c(患者ID, 入院日, sep = "-")) %>% 
  rename(name = "項目名",
         code = "データ") %>% 
  select(id, code) %>% 
  arrange(id)
charlson <- comorbidity(x = cci, id = "id", code = "code", map = "charlson_icd10_quan", assign0 = FALSE)
charlson
score <- score(charlson, weights = "quan", assign0 = FALSE)
cci_id <- cci %>% 
  distinct(id, .keep_all=TRUE) %>% 
  arrange(id)
cci_df <- cbind(cci_id, score) %>% 
  select(-code) %>% 
  separate(col = id,
           into = c("id", "adm"),
           sep = "-") %>% 
  mutate(id = as.character(id),
         adm = ymd(adm))

df <- read_rds("output/cleaned_data.rds")
key <- df %>% distinct(id, adm) %>% mutate(id = as.character(id))

cci_df <- left_join(key, cci_df, by = c("id", "adm"))

# Person-day --------------------------------------------------------------


