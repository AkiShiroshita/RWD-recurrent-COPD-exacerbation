
# Set-up packages ---------------------------------------------------------

rm(list=ls())
packages = c("tidyverse",
             "lubridate",
             "psych",
             "arsenal",
             "tableone",
             "naniar",
             "ggplot2",
             "ggplotgui",
             "cowplot")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
# Descriptive statistics --------------------------------------------------

getwd()
df <- read_rds("output/cleaned_data.rds")

df %>% distinct(id)
df %>% distinct(id, adm)

df %>% glimpse()
df %>% colnames()

df %>% filter(death24 == 1) 
df <- df %>% 
  filter(death24 == 0)

# COPD-AE
df %>% filter(main_code == "J440" | main_code == "J441" | prep_code == "J440" | prep_code == "J441"|
                reso_code == "J440" | reso_code == "J441") # 1200
df %>% filter(main_code == "J440" | main_code == "J441" | prep_code == "J440" | prep_code == "J441"|
                reso_code == "J440" | reso_code == "J441") %>% distinct(id) # 642

# Pneumonia
df %>% filter(str_detect(df$main_code,"J09") | str_detect(df$reso_code,"J09") | str_detect(df$prep_code,"J09") |
                str_detect(df$main_code,"J1") | str_detect(df$reso_code,"J1") | str_detect(df$prep_code,"J1"))
df %>% filter(str_detect(df$main_code,"J09") | str_detect(df$reso_code,"J09") | str_detect(df$prep_code,"J09") |
                str_detect(df$main_code,"J1") | str_detect(df$reso_code,"J1") | str_detect(df$prep_code,"J1")) %>% distinct(id)

# bronchitis
df %>% filter(str_detect(df$main_code,"J2") | str_detect(df$reso_code,"J2") | str_detect(df$prep_code,"J2"))
df %>% filter(str_detect(df$main_code,"J2") | str_detect(df$reso_code,"J2") | str_detect(df$prep_code,"J2")) %>% distinct(id)

# summary

df_summary <- df %>% 
  select(-starts_with("com"), -starts_with("sub"), -starts_with("main"), -starts_with("prep"),
         -starts_with("reso"), -severity, -amb, -nhcap) %>% 
  mutate(oral_abx = ifelse(str_detect(df$oral_abx, "\\d+"), 1, 0),
         iv_abx = ifelse(str_detect(df$iv_abx, "\\d+"), 1, 0),
         anti_pseudo_oral = ifelse(str_detect(df$anti_pseudo_oral, "\\d+"), 1, 0),
         anti_pseudo_iv = ifelse(str_detect(df$anti_pseudo_iv, "\\d+"), 1, 0),
         steroid_oral = ifelse(str_detect(df$steroid_oral, "\\d+"), 1, 0),
         steroid_iv = ifelse(str_detect(df$steroid_iv, "\\d+"), 1, 0),
         oxy = ifelse(str_detect(df$procedure, "酸素"), 1, 0),
         #adm_jcs = case_when(adm_jcs == 0 ~ 0,
         #                    adm_jcs == 1 | adm_jcs == 2 | adm_jcs == 3 ~ 1,
         #                    adm_jcs == 10 | adm_jcs == 20 | adm_jcs == 30 ~ 2,
         #                    adm_jcs == 100 | adm_jcs == 200 | adm_jcs == 300 ~ 3),
         adm_jcs = if_else(0 < adm_jcs, 1, 0),
         disc_jcs = case_when(disc_jcs == 0 ~ 0,
                              disc_jcs == 1 | disc_jcs == 2 | disc_jcs == 3 ~ 1,
                              disc_jcs == 10 | disc_jcs == 20 | disc_jcs == 30 ~ 2,
                              disc_jcs == 100 | disc_jcs == 200 | disc_jcs == 300 ~ 3),
         #adm_adl = case_when(adm_adl >= 85 ~ 0,
         #                    adm_adl == 0 ~ 2,
         #                    adm_adl > 0 | adm_adl < 85 ~ 1),
         adm_adl = if_else(20 <= adm_adl, 1, 0),
         #adm_adl = case_when(adm_adl == 100 ~ 0,
         #                    60 <= adm_adl &  adm_adl < 100 ~ 1,
         #                    40 <= adm_adl &  adm_adl < 60 ~ 2,
         #                    0 <= adm_adl &  adm_adl < 40 ~ 3),
         disc_adl = case_when(disc_adl >= 85 ~ 0,
                              disc_adl == 0 ~ 2,
                              disc_adl > 0 | disc_adl < 85 ~ 1),
         id = as.character(id),
         sex = as.factor(sex),
         birthday = str_replace_all(birthday, pattern = "1931年以下出生", replacement = "19310000"),
         birthday = as.numeric(birthday) + 101,
         birthday = ymd(birthday),
         age = trunc(time_length(interval(as.Date(birthday), as.Date(adm)),"year")),
         diff_time = as.numeric(diff_time),
         adm_adl = as.factor(adm_adl),
         disc_adl = as.factor(disc_adl),
         adm_jcs = as.factor(adm_jcs),
         disc_adl = as.factor(disc_adl),
         bmi = as.numeric(bmi),
         disc = ymd(disc),
         los = disc - adm + 1,
         los = as.numeric(los),
         death = ifelse(prognosis == 6 | prognosis == 7, 1, 0),
         death = as.factor(death),
         direct_death = ifelse(prognosis==6, 1, 0),
         direct_death = as.factor(direct_death),
         indirect_death = ifelse(prognosis==7, 1, 0),
         indirect_death = as.factor(indirect_death),
         anti_pseudo = ifelse(anti_pseudo_oral==1 | anti_pseudo_iv==1, 1, 0),
         anti_pseudo = as.factor(anti_pseudo),
         steroid = if_else(steroid_oral==1 | steroid_iv==1, 1, 0),
         steroid = as.factor(steroid),
         hugh_johns = as.numeric(hugh_johns),
         oxy = as.factor(oxy),
         wbc = as.numeric(wbc),
         alb = as.numeric(alb),
         bun = as.numeric(bun),
         crp = as.numeric(crp)) %>% 
  select(id, adm, los, death, diff_time, age, sex, bmi, adm_adl, adm_jcs,
         anti_pseudo, steroid, oxy, wbc, alb, bun, crp, hugh_johns, disc, direct_death, indirect_death) %>% 
  group_by(id) %>% 
  mutate(count = row_number()) %>% 
  ungroup()
df_summary %>% glimpse()
df_summary %>% colnames()

df_summary %>% filter(anti_pseudo == 0) %>% distinct(id)
df_summary %>% filter(anti_pseudo == 0) %>% distinct(id, adm)

df_summary %>% filter(anti_pseudo == 1) %>% distinct(id)
df_summary %>% filter(anti_pseudo == 1) %>% distinct(id, adm)

vars <- c("count", "age", "sex", "bmi", "adm_adl", "hugh-johns", "disc_adl", "adm_jcs",
          "disc_jcs", "oxy", "wbc", "alb", "bun", "crp", "anti_pseudo", "steroid", "los", "death")
factorVars <- c("death", "sex", "adm_adl", "hugh-johns", "disc_adl", "adm_jcs",
          "disc_jcs", "anti_pseudo", "steroid", "oxy", "count")
table1 <- CreateTableOne(vars = vars,
                         data = df_summary,
                         includeNA = FALSE,
                         factorVars = factorVars)
table1 %>% 
  print(nonnormal = c("wbc", "alb", "bun", "crp", "los"))

table2 <- CreateTableOne(vars = vars,
                         data = df_summary,
                         includeNA = FALSE,
                         factorVars = factorVars,
                         strata = "anti_pseudo")
table2 %>% 
  print(nonnormal = c("wbc", "alb", "bun", "crp", "los"))

#ggplot_shiny(data = df_summary)

df_summary %>% write_rds("output/df_summary.rds", compress = "gz")

df_summary <- df_summary %>% 
  mutate(anti_pseudo = factor(anti_pseudo,
                              levels = c(0, 1),
                              labels = c("Non-anti-pseudomonal antibiotics group", "Anti-pseudomonal antibiotics group")))

theme_set(theme_cowplot())
#graph1 <- ggplot(df_summary, aes(x = diff_time, fill = anti_pseudo)) +
#  geom_density(position = 'identity', alpha = 0.8, adjust = 1) +
#  labs(x = 'Interval between hospitalisations (days)', y = 'Density')+
#  labs(fill = '') +
#  theme_classic() +
#  theme(
#    axis.title = element_text(size = 13),
#    axis.text = element_text(size = 10),
#    legend.position = 'right'
#  )
#graph1

graph1 <- ggplot(df_summary, aes(x = diff_time, fill = anti_pseudo)) +
  geom_density(position = 'identity', alpha = 0.8, adjust = 1) +
  labs(x = 'Interval between hospitalisations (days)', y = 'Density')+
  labs(fill = '') +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette="Set1")
  
graph1

#graph2 <- ggplot(df_summary, aes(x = count, fill = anti_pseudo)) +
#  geom_histogram(position = 'identity', alpha = 0.8, binwidth = 1) +
#  labs(x = 'The number of recurrences', y = 'Count') +
#  labs(fill = '') +
#  theme_classic() +
#  theme(
#    axis.title = element_text(size = 12),
#    axis.text = element_text(size = 12),
#    text = element_text(family = 'Helvetica')
#  )
#graph2

graph2 <- ggplot(df_summary, aes(x = count, fill = anti_pseudo)) +
  geom_histogram(aes(y=c(..count..[..group..==1]/sum(..count..[..group..==1]),
                         ..count..[..group..==2]/sum(..count..[..group..==2]))*100),
                 position='dodge', binwidth = 1) +
  labs(x = 'The number of recurrences', y = 'Percentage') +
  labs(fill = '') +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    text = element_text(family = 'Helvetica')) +
  scale_fill_brewer(palette="Set1")
graph2

plot_grid(graph1, graph2, ncol = 1)

miss <- miss_var_summary(df_summary)
miss        

# Abx change --------------------------------------------------------------

emr_drug_data = fread("input/2021102512_3_EMR_Drug_data_2021_002_SCE.csv.gz")

id_key <- df %>% 
  distinct(id)

emr_drug_data %>% colnames()

abx_use_change <- emr_drug_data %>% 
  select(1,3,4,8) %>% 
  rename(id = "患者ID",
         start = "開始日",
         code = "薬価コード",
         name = "薬剤名") %>% 
  distinct(id, name, .keep_all=TRUE) %>% 
  mutate(pseudo_tag = ifelse(str_detect(code, c(filter_anti_pseudo_oral_code, filter_anti_pseudo_iv_code)), 1, 0)) %>% 
  distinct(id, pseudo_tag, .keep_all=TRUE) %>% 
  group_by(id) %>% 
  filter(n() >= 2) %>% 
  mutate(lag_pseudo_tag = lag(pseudo_tag),
         lag_start = lag(start),
         diff = pseudo_tag - lag_pseudo_tag,
         diff_time = start - lag_start) %>% 
  #filter(0 < diff_time & diff_time <= 7) %>% 
  ungroup()

selected_abx_use_change <- inner_join(id_key, abx_use_change, by = "id")
selected_abx_use_change %>%
  filter(diff == 1)

# Total procedure ---------------------------------------------------------

## mechanical ventilation

total_claim_procedure_venti <- claim_procedure_data %>% 
  rename(id = "患者ID",
         day = "対象日",
         code = "診療行為コード",
         name = "診療行為") %>% 
  group_by(id, code) %>% 
  mutate(id = as.character(id),
         day = ymd(day),
         lag_day = lag(day),
         diff_code = day -lag_day) %>% 
  filter(diff_code > 1) %>% 
  ungroup() %>% 
  select(id, day, code, name)

selected_total_claim_procedure_data_venti <- inner_join(total_claim_procedure_venti, df_summary, by = "id")
selected_total_claim_procedure_data_venti %>% glimpse()
selected_total_claim_procedure_data_venti %>% colnames()

ventilation <- selected_total_claim_procedure_data %>% 
  filter(name == "人工呼吸" | name == "人工呼吸（５時間超）" | name == "救命のための気管内挿管" | name == "人工呼吸（鼻マスク式人工呼吸器）" | 
           name == "人工呼吸（鼻マスク式人工呼吸器）（５時間超）" | name == "人工呼吸（閉鎖循環式麻酔装置）（５時間超）" | name == "人工呼吸（閉鎖循環式麻酔装置）" | 
           name == "ＣＰＡＰ" | name == "ＣＰＡＰ（５時間超）" | name == "ＩＭＶ（５時間超）") %>% 
  filter(adm < day & day < disc)

ventilation %>% glimpse()

## dialysis

total_claim_procedure_dial <- claim_procedure_data %>% 
  rename(id = "患者ID",
         day = "対象日",
         code = "診療行為コード",
         name = "診療行為") %>% 
  group_by(id, code) %>% 
  mutate(id = as.character(id),
         day = ymd(day),
         lag_day = lag(day),
         diff_code = day -lag_day) %>% 
  filter(diff_code > 3) %>% 
  ungroup() %>% 
  select(id, day, code, name)

selected_total_claim_procedure_data_dial <- inner_join(total_claim_procedure_dial, df_summary, by = "id")
selected_total_claim_procedure_data_dial %>% glimpse()
selected_total_claim_procedure_data_dial %>% colnames()

dialysis <- selected_total_claim_procedure_data_dial %>% 
  filter(name == "持続緩徐式血液濾過" | name == "障害者等加算（持続緩徐式血液濾過）" | name == "人工腎臓（その他）" | name == "透析液水質確保加算（人工腎臓）" | 
           name == "人工腎臓（導入期）加算"  | name == "人工腎臓（慢性維持透析）（４時間未満）" | name == "透析液水質確保加算２" | 
           name == "障害者等加算（人工腎臓）" | name == "人工腎臓（慢性維持透析１）（４時間未満）"  | name == "慢性維持透析濾過加算（人工腎臓）" |
           name == "人工腎臓（慢性維持透析１）（４時間以上５時間未満）" | name == "時間外・休日加算（人工腎臓）" | name ==  "導入期加算２（人工腎臓）" |
           name == "人工腎臓（慢性維持透析濾過）（複雑）" | name == "人工腎臓（慢性維持透析）（４時間以上５時間未満）" | name == "透析液水質確保加算１" |
           name == "人工腎臓（慢性維持透析）（５時間以上）" | name == "人工腎臓（慢性維持透析１）（４時間未満）（イを除く）" | name == "人工腎臓（慢性維持透析１）（５時間以上）" | name == "長時間加算（人工腎臓）") %>% 
  filter(adm < day & day < disc)
























