
# Set-up packages ---------------------------------------------------------

rm(list=ls())
packages = c("tidyverse",
             "lubridate",
             "psych",
             "arsenal",
             "tableone",
             "ggplot2",
             "ggplotgui")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
# Descriptive statistics --------------------------------------------------

getwd()
df <- read_rds("output/cleaned_data.rds")
df %>% colnames()

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
         adm_jcs = case_when(adm_jcs == 0 ~ 0,
                             adm_jcs == 1 | adm_jcs == 2 | adm_jcs == 3 ~ 1,
                             adm_jcs == 10 | adm_jcs == 20 | adm_jcs == 30 ~ 2,
                             adm_jcs == 100 | adm_jcs == 200 | adm_jcs == 300 ~ 3),
         disc_jcs = case_when(disc_jcs == 0 ~ 0,
                              disc_jcs == 1 | disc_jcs == 2 | disc_jcs == 3 ~ 1,
                              disc_jcs == 10 | disc_jcs == 20 | disc_jcs == 30 ~ 2,
                              disc_jcs == 100 | disc_jcs == 200 | disc_jcs == 300 ~ 3),
         adm_adl = case_when(adm_adl == 100 ~ 0,
                             60 <= adm_adl &  adm_adl < 100 ~ 1,
                             40 <= adm_adl &  adm_adl < 60 ~ 2,
                             0 <= adm_adl &  adm_adl < 40 ~ 3),
         disc_adl = case_when(disc_adl == 100 ~ 0,
                             60 <= disc_adl &  adm_adl < 100 ~ 1,
                             40 <= disc_adl &  adm_adl < 60 ~ 2,
                             0 <= disc_adl &  adm_adl < 40 ~ 3),
         id = as.factor(id),
         sex = as.factor(sex),
         adm_adl = as.factor(adm_adl),
         disc_adl = as.factor(disc_adl),
         adm_jcs = as.factor(adm_jcs),
         disc_adl = as.factor(disc_adl),
         bmi = as.numeric(bmi),
         disc = ymd(disc),
         los = disc - adm + 1,
         los = as.numeric(los),
         death = ifelse(prognosis==6 | prognosis==7, 1, 0),
         death = as.factor(death),
         anti_pseudo = ifelse(anti_pseudo_oral==1 | anti_pseudo_iv==1, 1, 0),
         anti_pseudo = as.factor(anti_pseudo),
         steroid = if_else(steroid_oral==1 | steroid_iv==1, 1, 0),
         oxy = as.factor(oxy),
         wbc = as.numeric(wbc),
         alb = as.numeric(alb),
         bun = as.numeric(bun),
         crp = as.numeric(crp)) %>% 
  select(id, los, death, sex, bmi, adm_adl, disc_adl, adm_jcs, disc_jcs, anti_pseudo, steroid, oxy, wbc, alb, bun, crp) %>% 
  group_by(id) %>% 
  mutate(count = row_number()) %>% 
  ungroup()
df_summary %>% glimpse()
df_summary %>% colnames()

vars <- c("count", "sex", "bmi", "adm_adl", "disc_adl", "adm_jcs",
          "disc_jcs", "oxy", "wbc", "alb", "bun", "crp", "anti_pseudo", "steroid", "los", "death")
factorVars <- c("death", "sex", "adm_adl", "disc_adl", "adm_jcs",
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

ggplot_shiny(data = df_summary)

miss <- miss_var_summary(df_summary)
miss        