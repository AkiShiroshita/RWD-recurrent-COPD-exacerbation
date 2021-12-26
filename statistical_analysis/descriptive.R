
# Set-up packages ---------------------------------------------------------

rm(list=ls())
packages = c("tidyverse",
             "readxl",
             "data.table",
             "lubridate",
             "psych",
             "arsenal",
             "tableone",
             "naniar",
             "ggplot2",
             "ggplotgui",
             "cowplot",
             "survival")
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

# summary

df_summary <- df %>% 
  #select(-starts_with("com"), -starts_with("sub"), -starts_with("main"), -starts_with("prep"),
  #       -starts_with("reso"), -severity, -amb, -nhcap) %>% 
  mutate(oral_abx = ifelse(str_detect(df$oral_abx, "\\d+"), 1, 0),
         iv_abx = ifelse(str_detect(df$iv_abx, "\\d+"), 1, 0),
         anti_pseudo_oral = ifelse(str_detect(df$anti_pseudo_oral, "\\d+"), 1, 0),
         anti_pseudo_iv = ifelse(str_detect(df$anti_pseudo_iv, "\\d+"), 1, 0),
         steroid_oral = ifelse(str_detect(df$steroid_oral, "\\d+"), 1, 0),
         steroid_iv = ifelse(str_detect(df$steroid_iv, "\\d+"), 1, 0),
         oxy = ifelse(str_detect(df$procedure, "酸素"), 1, 0),
         adm_jcs = case_when(adm_jcs == 0 ~ 0,
                             adm_jcs == 1 | adm_jcs == 2 | adm_jcs == 3 ~ 1,
                             adm_jcs == 10 | adm_jcs == 20 | adm_jcs == 30 ~ 2,
                             adm_jcs == 100 | adm_jcs == 200 | adm_jcs == 300 ~ 3),
         #adm_jcs = if_else(0 < adm_jcs, 1, 0),
         disc_jcs = case_when(disc_jcs == 0 ~ 0,
                              disc_jcs == 1 | disc_jcs == 2 | disc_jcs == 3 ~ 1,
                              disc_jcs == 10 | disc_jcs == 20 | disc_jcs == 30 ~ 2,
                              disc_jcs == 100 | disc_jcs == 200 | disc_jcs == 300 ~ 3),
         adm_adl = case_when(adm_adl >= 85 ~ 0,
                             adm_adl == 0 ~ 2,
                             adm_adl > 0 | adm_adl < 85 ~ 1),
         #adm_adl = if_else(20 <= adm_adl, 1, 0),
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
         hugh_johns = na_if(hugh_johns, 0),
         oxy = as.factor(oxy),
         wbc = as.numeric(wbc),
         alb = as.numeric(alb),
         bun = as.numeric(bun),
         crp = as.numeric(crp),
         age_cci = case_when(age < 50 ~ 0,
                             50 <= age & age < 59 ~ 1,
                             60 <= age & age < 69 ~ 2,
                             70 <= age & age < 79 ~ 3,
                             80 <= age ~ 4),
         cci_score = cci_score + as.numeric(age_cci)) %>% 
  group_by(id) %>% 
  mutate(count = row_number()) %>% 
  ungroup()

df_summary %>% glimpse()
df_summary %>% colnames()

vars <- c("count", "age", "sex", "bmi", "adm_adl", "hugh_johns", "disc_adl", "adm_jcs",
          "disc_jcs", "cci_score", "oxy", "wbc", "alb", "bun", "crp", "anti_pseudo", "steroid", "los",
          "death", "direct_death", "indirect_death",
          "ventilation", "dialysis", "intubation")
factorVars <- c("death", "sex", "adm_adl", "hugh_johns", "disc_adl", "adm_jcs", 
                "disc_jcs", "anti_pseudo", "steroid", "oxy", "count", "direct_death",
                "indirect_death", "ventilation", "dialysis", "intubation")
table1 <- CreateTableOne(vars = vars,
                         data = df_summary,
                         includeNA = FALSE,
                         factorVars = factorVars)
table1 %>% 
  print(nonnormal = c("cci_score", "wbc", "alb", "bun", "crp", "los"))

table2 <- CreateTableOne(vars = vars,
                         data = df_summary,
                         includeNA = FALSE,
                         factorVars = factorVars,
                         strata = "anti_pseudo")
table2 %>% 
  print(nonnormal = c("cci_score", "wbc", "alb", "bun", "crp", "los"))

# comorbidities  

df_com <- df %>% 
  mutate(bronch = ifelse((str_detect(df$com_code1, "J47")) | (str_detect(df$com_code2, "J47")) |
                           (str_detect(df$com_code3, "J47")) | (str_detect(df$com_code4, "J47")) |
                           (str_detect(df$com_code5, "J47")) | (str_detect(df$com_code6, "J47")) |
                           (str_detect(df$com_code7, "J47")) | (str_detect(df$com_code8, "J47")) |
                           (str_detect(df$com_code9, "J47")) | (str_detect(df$com_code10, "J47")), 1, 0),
         
         asthma = ifelse((str_detect(df$com_code1, "J45")) | (str_detect(df$com_code2, "J45")) |
                           (str_detect(df$com_code3, "J45")) | (str_detect(df$com_code4, "J45")) |
                           (str_detect(df$com_code5, "J45")) | (str_detect(df$com_code6, "J45")) |
                           (str_detect(df$com_code7, "J45")) | (str_detect(df$com_code8, "J45")) |
                           (str_detect(df$com_code9, "J45")) | (str_detect(df$com_code10, "J45")) |
                           (str_detect(df$com_code1, "J46")) | (str_detect(df$com_code2, "J46")) |
                           (str_detect(df$com_code3, "J46")) | (str_detect(df$com_code4, "J46")) |
                           (str_detect(df$com_code5, "J46")) | (str_detect(df$com_code6, "J46")) |
                           (str_detect(df$com_code7, "J46")) | (str_detect(df$com_code8, "J46")) |
                           (str_detect(df$com_code9, "J46")) | (str_detect(df$com_code10, "J46")), 1, 0),
         anti_pseudo_oral = ifelse(str_detect(df$anti_pseudo_oral, "\\d+"), 1, 0),
         anti_pseudo_iv = ifelse(str_detect(df$anti_pseudo_iv, "\\d+"), 1, 0),
         anti_pseudo = ifelse(anti_pseudo_oral==1 | anti_pseudo_iv==1, 1, 0),
         anti_pseudo = as.factor(anti_pseudo)
         )

vars <- c("bronch", "asthma")
factorVars <- c("bronch", "asthma")
table1 <- CreateTableOne(vars = vars,
                         data = df_com,
                         includeNA = FALSE,
                         factorVars = factorVars)
table1 <- CreateTableOne(vars = vars,
                         data = df_com,
                         includeNA = FALSE,
                         factorVars = factorVars,
                         strata = "anti_pseudo")
table1 %>% 
  print()

#ggplot_shiny(data = df_summary)

# for analysis

df <- read_rds("output/cleaned_data.rds")

miss <- miss_var_summary(df)
miss       

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
         adm_jcs = if_else(0 < adm_jcs, 1, 0), # confirmed no missing 
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
         hugh_johns = na_if(hugh_johns, 0),
         hugh_johns = if_else(3 < hugh_johns, 1, 0),
         hugh_johns = as.factor(hugh_johns),
         steroid = as.factor(steroid),
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

df_summary %>% write_rds("output/df_summary.rds", compress = "gz")

df_summary %>% filter(anti_pseudo == 0) %>% distinct(id)
df_summary %>% filter(anti_pseudo == 1) %>% distinct(id)

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

ggsave("figures/figure2.tiff", dpi = 350)

miss <- miss_var_summary(df_summary)
miss        


# Abx strategies ----------------------------------------------------------

df_stra <- df %>% 
  select(id, adm, oral_abx, iv_abx) %>% 
  separate(oral_abx, c("oral_abx1", "oral_abx2",  "oral_abx3", "oral_abx4"), sep="_") %>% 
  separate(iv_abx, c("iv_abx1", "iv_abx2", "iv_abx3", "iv_abx4", "iv_abx5"), sep="_") 
df_stra %>% glimpse()

df_stra <- df_stra %>% 
  pivot_longer(cols = c(-id, -adm), names_to = "abx", values_to = "value")
tb <- table(df_stra$value) %>% as.data.frame()
tb <- tb %>% 
  mutate(Var1 = as.character(Var1))

oral <- read_excel("memo/oral.xlsx")
oral <- oral %>% 
  rename(Var1 = "薬価基準収載医薬品コード",
         ing = "成分名") %>% 
  select(Var1, ing)
iv <- read_excel("memo/iv.xlsx")
iv <- iv %>% 
  rename(Var1 = "薬価基準収載医薬品コード",
         ing = "成分名") %>% 
  select(Var1, ing)

tb <- left_join(tb, oral, by = "Var1")
tb <- left_join(tb, iv, by = "Var1")

tb %>% write.csv("output/abx_strategies.csv",
                 fileEncoding = "shift-jis")

# Abx change --------------------------------------------------------------

emr_drug_data = fread("input/2021102512_3_EMR_Drug_data_2021_002_SCE.csv.gz")

id_key <- df %>% 
  distinct(id)

emr_drug_data %>% colnames()

oral <- read_excel("memo/oral.xlsx")
iv <- read_excel("memo/iv.xlsx")
anti_pseudo <- read_excel("memo/anti_pseudo.xlsx")
anti_pseudo <- anti_pseudo %>% 
  pull(drug)
filter_anti_pseudo <- str_c(anti_pseudo, collapse = "|")
anti_pseudo_oral <- oral %>% 
  filter(str_detect(成分名, filter_anti_pseudo)) %>% 
  select(2) %>% 
  pull()
filter_anti_pseudo_oral_code <- str_c(anti_pseudo_oral, collapse = "|")

anti_pseudo <- read_excel("memo/anti_pseudo.xlsx")
anti_pseudo <- anti_pseudo %>% 
  pull(drug)
filter_anti_pseudo <- str_c(anti_pseudo, collapse = "|")
anti_pseudo_iv <- iv %>% 
  filter(str_detect(成分名, filter_anti_pseudo)) %>% 
  select(2) %>% 
  pull()
filter_anti_pseudo_iv_code <- str_c(anti_pseudo_iv, collapse = "|")

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

selected_abx_use_change <- abx_use_change %>%
  filter(diff == 1) %>% 
  distinct(id, .keep_all=TRUE) %>% # confirm no more than 2 change
  filter(0 < diff_time & diff_time <= 7) 

selected_abx_use_change_final <- inner_join(id_key, selected_abx_use_change, by = "id")

# Atypical change

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

# MRSA change

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

