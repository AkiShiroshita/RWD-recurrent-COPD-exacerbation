
# Set-up packages ---------------------------------------------------------

rm(list=ls())
packages = c("tidyverse",
             "data.table",
             "lubridate",
             "psych",
             "arsenal",
             "tableone",
             "naniar",
             "ggplot2",
             "ggplotgui",
             "cowplot",
             "survival",
             "comorbidity")
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

# comorbidities  

df_com <- df_summary %>% 
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
         malig = ifelse((str_detect(df$com_code1, "C")) | (str_detect(df$com_code2, "C")) |
                          (str_detect(df$com_code3, "C")) | (str_detect(df$com_code4, "C")) |
                          (str_detect(df$com_code5, "C")) | (str_detect(df$com_code6, "C")) |
                          (str_detect(df$com_code7, "C")) | (str_detect(df$com_code8, "C")) |
                          (str_detect(df$com_code9, "C")) | (str_detect(df$com_code10, "C"))|
                          
                          (str_detect(df$com_code1, "D3")) | (str_detect(df$com_code2, "D3")) |
                          (str_detect(df$com_code3, "D3")) | (str_detect(df$com_code4, "D3")) |
                          (str_detect(df$com_code5, "D3")) | (str_detect(df$com_code6, "D3")) |
                          (str_detect(df$com_code7, "D3")) | (str_detect(df$com_code8, "D3")) |
                          (str_detect(df$com_code9, "D3")) | (str_detect(df$com_code10, "D3")) |
                          
                          (str_detect(df$com_code1, "D4")) | (str_detect(df$com_code2, "D4")) |
                          (str_detect(df$com_code3, "D4")) | (str_detect(df$com_code4, "D4")) |
                          (str_detect(df$com_code5, "D4")) | (str_detect(df$com_code6, "D4")) |
                          (str_detect(df$com_code7, "D4")) | (str_detect(df$com_code8, "D4")) |
                          (str_detect(df$com_code9, "D4")) | (str_detect(df$com_code10, "D4")), 1, 0),
         anemia = ifelse((str_detect(df$com_code1, "D5")) | (str_detect(df$com_code2, "C")) |
                           (str_detect(df$com_code3, "D5")) | (str_detect(df$com_code4, "D5")) |
                           (str_detect(df$com_code5, "D5")) | (str_detect(df$com_code6, "D5")) |
                           (str_detect(df$com_code7, "D5")) | (str_detect(df$com_code8, "D5")) |
                           (str_detect(df$com_code9, "D5")) | (str_detect(df$com_code10, "D5"))|
                           
                           (str_detect(df$com_code1, "D6")) | (str_detect(df$com_code2, "D6")) |
                           (str_detect(df$com_code3, "D6")) | (str_detect(df$com_code4, "D6")) |
                           (str_detect(df$com_code5, "D6")) | (str_detect(df$com_code6, "D6")) |
                           (str_detect(df$com_code7, "D6")) | (str_detect(df$com_code8, "D6")) |
                           (str_detect(df$com_code9, "D6")) | (str_detect(df$com_code10, "D6")), 1, 0),
         endoc = ifelse((str_detect(df$com_code1, "E")) | (str_detect(df$com_code2, "J47")) |
                          (str_detect(df$com_code3, "E")) | (str_detect(df$com_code4, "E")) |
                          (str_detect(df$com_code5, "E")) | (str_detect(df$com_code6, "E")) |
                          (str_detect(df$com_code7, "E")) | (str_detect(df$com_code8, "E")) |
                          (str_detect(df$com_code9, "E")) | (str_detect(df$com_code10, "E")), 1, 0),
         psycho = ifelse((str_detect(df$com_code1, "F")) | (str_detect(df$com_code2, "J47")) |
                           (str_detect(df$com_code3, "F")) | (str_detect(df$com_code4, "F")) |
                           (str_detect(df$com_code5, "F")) | (str_detect(df$com_code6, "F")) |
                           (str_detect(df$com_code7, "F")) | (str_detect(df$com_code8, "F")) |
                           (str_detect(df$com_code9, "F")) | (str_detect(df$com_code10, "F")), 1, 0),
         neuro = ifelse((str_detect(df$com_code1, "G")) | (str_detect(df$com_code2, "G")) |
                          (str_detect(df$com_code3, "G")) | (str_detect(df$com_code4, "G")) |
                          (str_detect(df$com_code5, "G")) | (str_detect(df$com_code6, "G")) |
                          (str_detect(df$com_code7, "G")) | (str_detect(df$com_code8, "G")) |
                          (str_detect(df$com_code9, "G")) | (str_detect(df$com_code10, "G")), 1, 0),
         heart = ifelse((str_detect(df$com_code1, "I")) | (str_detect(df$com_code2, "I")) |
                          (str_detect(df$com_code3, "I")) | (str_detect(df$com_code4, "I")) |
                          (str_detect(df$com_code5, "I")) | (str_detect(df$com_code6, "I")) |
                          (str_detect(df$com_code7, "I")) | (str_detect(df$com_code8, "I")) |
                          (str_detect(df$com_code9, "I")) | (str_detect(df$com_code10, "I")), 1, 0), 
         digest = ifelse((str_detect(df$com_code1, "K")) | (str_detect(df$com_code2, "K")) |
                           (str_detect(df$com_code3, "K")) | (str_detect(df$com_code4, "K")) |
                           (str_detect(df$com_code5, "K")) | (str_detect(df$com_code6, "K")) |
                           (str_detect(df$com_code7, "K")) | (str_detect(df$com_code8, "K")) |
                           (str_detect(df$com_code9, "K")) | (str_detect(df$com_code10, "K")), 1, 0)) %>% 
  select(id, adm, bronch, asthma, malig, anemia, endoc, psycho, neuro, heart, digest, anti_pseudo)

vars <- c("bronch", "asthma", "malig", "anemia", "endoc", "psycho", "neuro", "heart", "digest")
factorVars <- c("bronch", "asthma", "malig", "anemia", "endoc", "psycho", "neuro", "heart", "digest")
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

vars <- c("count", "age", "sex", "bmi", "adm_adl", "hugh_johns", "disc_adl", "adm_jcs",
          "disc_jcs", "oxy", "wbc", "alb", "bun", "crp", "anti_pseudo", "steroid", "los", "death", "direct_death", "indirect_death")
factorVars <- c("death", "sex", "adm_adl", "hugh_johns", "disc_adl", "adm_jcs",
                "disc_jcs", "anti_pseudo", "steroid", "oxy", "count", "direct_death", "indirect_death")
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

# Total procedure ---------------------------------------------------------

claim_procedure_data = fread("input/2021102512_52_Claim_Procedure_data_2021_002_SCE.csv.gz")
claim_procedure_data <- claim_procedure_data %>% 
  rename(id = "患者ID",
         day = "対象日",
         code = "診療行為コード",
         name = "診療行為") %>% 
  mutate(id = as.character(id),
         day = ymd(day)) 

key <- df %>% 
  distinct(id, adm, disc)

id_key <- key %>% 
  distinct(id, .keep_all=TRUE)

count_key <- key %>% 
  group_by(id) %>% 
  mutate(count = row_number(),
         id = as.character(id),
         disc = ymd(disc))
max(count_key$count)

## mechanical ventilation

ventilation <- claim_procedure_data %>% 
  filter(name == "人工呼吸" | name == "人工呼吸（５時間超）" | name == "救命のための気管内挿管" | name == "人工呼吸（鼻マスク式人工呼吸器）" | 
           name == "人工呼吸（鼻マスク式人工呼吸器）（５時間超）" | name == "人工呼吸（閉鎖循環式麻酔装置）（５時間超）" | name == "人工呼吸（閉鎖循環式麻酔装置）" | 
           name == "ＣＰＡＰ" | name == "ＣＰＡＰ（５時間超）" | name == "ＩＭＶ（５時間超）") %>% 
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

df_summary <- left_join(df_summary, venti_use, by = c("id", "adm"))
df_summary <- df_summary %>% 
  mutate(ventilation = if_else(is.na(ventilation), 0, 1))

## dialysis

dialysis <- claim_procedure_data %>% 
  filter(name == "持続緩徐式血液濾過" | name == "障害者等加算（持続緩徐式血液濾過）" | name == "人工腎臓（その他）" | name == "透析液水質確保加算（人工腎臓）" | 
           name == "人工腎臓（導入期）加算"  | name == "人工腎臓（慢性維持透析）（４時間未満）" | name == "透析液水質確保加算２" | 
           name == "障害者等加算（人工腎臓）" | name == "人工腎臓（慢性維持透析１）（４時間未満）"  | name == "慢性維持透析濾過加算（人工腎臓）" |
           name == "人工腎臓（慢性維持透析１）（４時間以上５時間未満）" | name == "時間外・休日加算（人工腎臓）" | name ==  "導入期加算２（人工腎臓）" |
           name == "人工腎臓（慢性維持透析濾過）（複雑）" | name == "人工腎臓（慢性維持透析）（４時間以上５時間未満）" | name == "透析液水質確保加算１" |
           name == "人工腎臓（慢性維持透析）（５時間以上）" | name == "人工腎臓（慢性維持透析１）（４時間未満）（イを除く）" | name == "人工腎臓（慢性維持透析１）（５時間以上）" | name == "長時間加算（人工腎臓）") %>%  
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

df_summary <- left_join(df_summary, dialysis_use, by = c("id", "adm"))
df_summary <- df_summary %>% 
  mutate(dialysis = if_else(is.na(dialysis), 0, 1))

# intubation

intubation <- claim_procedure_data %>% 
  filter(name == "救命のための気管内挿管") %>% 
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

df_summary <- left_join(df_summary, intubation_use, by = c("id", "adm"))
df_summary <- df_summary %>% 
  mutate(intubation = if_else(is.na(intubation), 0, 1))

# output

df_summary %>% write_rds("output/df_summary.rds", compress = "gz")

factorVars <- vars <- c("ventilation", "dialysis", "intubation")
table1 <- CreateTableOne(vars = vars,
                         data = df_summary,
                         includeNA = FALSE,
                         factorVars = factorVars,
                         strata = "anti_pseudo")
table1 <- CreateTableOne(vars = vars,
                         data = df_summary,
                         includeNA = FALSE,
                         factorVars = factorVars)
table1 %>% 
  print()




















