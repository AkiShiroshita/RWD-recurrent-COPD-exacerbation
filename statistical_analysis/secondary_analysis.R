
df <- read_rds("output/cleaned_data.rds")
df %>% glimpse()

# Oral vancomycin ---------------------------------------------------------

oral_vanco <- oral_abx_use %>% 
  rename(id = "患者ID") %>% 
  filter(薬価コード == "6113001B1011" | 薬価コード == "6113001B1089" | 薬価コード == "6113001B1143")

# Oral metrolidazole ------------------------------------------------------

oral_metro <- oral_abx_use %>% 
  rename(id = "患者ID") %>% 
  filter(薬価コード == "6419002F1131")
oral_metro <- oral_abx_use

# IV metrolidazole --------------------------------------------------------

iv_metro <- iv_abx_use %>% 
  rename(id = "患者ID") %>% 
  filter(薬価コード == "6419401A1027" )
iv_metro_selected <- inner_join(df, iv_metro, by = "id")

# Disease code ------------------------------------------------------------

cdi_coded <- df %>% 
  filter(subs_code1 == "A047"|subs_code2 == "A047"|subs_code3 == "A047"|subs_code4 == "A047"|
           subs_code5 == "A047"|subs_code6 == "A047"|subs_code7 == "A047"|subs_code8 == "A047"|
           subs_code9 == "A047"|subs_code10 == "A047")
cdi_coded

# Join data ---------------------------------------------------------------

cdi <- full_join(iv_metro_selected, cdi_coded, key = c("id", "adm"))
cdi %>% glimpse()
cdi <- cdi %>% 
  select(-starts_with("com"), -starts_with("subs"), -main, -birthday, -main_code, -prep_code,
         -prep, -reso, -reso_code, -disc_adl, -adm_jcs, -hugh_johns, -severity, -adm_adl) %>% 
  arrange(id, adm)
cdi %>% distinct(id)
cdi %>% write_csv("output/secondary_analysis.csv")

# Recept data -------------------------------------------------------------

claim_disease_data = fread("input/2021102512_51_Claim_Disease_data_2021_002_SCE.csv.gz")
claim_disease_data <- claim_disease_data %>% 
  rename(id = "患者ID")
claim_disease_data_selected <- inner_join(claim_disease_data, key, by = "id")
claim_disease_data_selected_cdi <- claim_disease_data_selected %>% 
  filter(ICD10 == "A047" & 主傷病 == 1) %>% 
  arrange(id, adm)
claim_disease_data_selected_cdi %>% distinct(id)
claim_disease_data_selected_cdi %>% write_csv("output/secondary_analysis_sub.csv")