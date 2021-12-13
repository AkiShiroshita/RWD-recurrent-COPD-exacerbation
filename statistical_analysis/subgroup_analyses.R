# Set-up packages ---------------------------------------------------------

packages = c(
  "tidyverse",
  "lubridate",
  "exploratory",
  "multirich",
  "ggplot2",
  "knitr",
  "kableExtra",
  "gt",
  "survival",
  "mice",
  "norm2",
  "frailtyHL",
  "mitools",
  "lme4",
  "geepack"
)
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#install.packages("devtools")
#devtools::install_github("exploratory-io/exploratory_func")
library(exploratory)

df <- read_rds("output/cleaned_data.rds")


# COPD exacerbation -------------------------------------------------------

df_copd <- df %>%
  filter(main_code == "J440" | main_code == "J441" | prep_code == "J440" | prep_code == "J441"|
                reso_code == "J440" | reso_code == "J441" | main_code == "J440" | main_code == "J441" | 
                  prep_code == "J440" | prep_code == "J441"| reso_code == "J440" | reso_code == "J441") 

df_copd <- df_copd %>% 
  select(-starts_with("com"), -starts_with("sub"), -starts_with("main"), -starts_with("prep"),
         -starts_with("reso"), -severity, -amb, -nhcap) %>% 
  mutate(oral_abx = ifelse(str_detect(df_copd$oral_abx, "\\d+"), 1, 0),
         iv_abx = ifelse(str_detect(df_copd$iv_abx, "\\d+"), 1, 0),
         anti_pseudo_oral = ifelse(str_detect(df_copd$anti_pseudo_oral, "\\d+"), 1, 0),
         anti_pseudo_iv = ifelse(str_detect(df_copd$anti_pseudo_iv, "\\d+"), 1, 0),
         steroid_oral = ifelse(str_detect(df_copd$steroid_oral, "\\d+"), 1, 0),
         steroid_iv = ifelse(str_detect(df_copd$steroid_iv, "\\d+"), 1, 0),
         oxy = ifelse(str_detect(df_copd$procedure, "酸素"), 1, 0),
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

df_copd <- df_copd %>%
  drop_na(los) %>% 
  select(-adm, -disc, -direct_death, -indirect_death, -diff_time) 

# person-year
df_py <- df_copd %>% 
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py$total_los)/608

df_py1 <- df_copd %>%
  filter(anti_pseudo == 0) %>% 
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py1$total_los)/555

df_py2 <- df_copd %>% 
  filter(anti_pseudo == 1) %>%
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py2$total_los)/116

obj_py1 <- Surv(as.numeric(df_copd$los), as.numeric(df_copd$death))
fit_py11 <- survfit(obj_py1 ~ 1,
                    data = df_copd)
summary(fit_py11)
print(fit_py11)
pyears(obj_py1 ~ 1, scale = 1)

df_copd1 <- df_copd %>% 
  filter(anti_pseudo == 0)
obj_py12 <- Surv(as.numeric(df_copd1$los), as.numeric(df_copd1$death))
fit_py12 <- survfit(obj_py12 ~ 1,
                    data = df_copd1)
summary(fit_py12)
print(fit_py12)

df_copd2 <- df_copd %>% 
  filter(anti_pseudo == 1)
obj_py13 <- Surv(as.numeric(df_copd2$los), as.numeric(df_copd2$death))
fit_py13 <- survfit(obj_py13 ~ 1,
                    data = df_copd2)
summary(fit_py13)
print(fit_py13)

df_mi_copd <- df_copd 
df_mi_copd0 <- mice(df_mi_copd, maxit = 0)
df_mi_copd0$method
df_mi_copd0$predictorMatrix
predmt <- (1 - diag(1, ncol(df_mi_copd)))
predmt[1, ] <- predmt[, 1] <- 0
predmt
df_mi100_copd <- mice(df_mi_copd, m = 100, predictorMatrix = predmt, maxit = 20, printFlag = FALSE, seed = 1234)

df_mi100_stack_copd <- complete(df_mi100_copd, action="long") %>% 
  as_tibble()

df_mi100_stack_copd <- df_mi100_stack_copd %>% 
  group_by(.imp) %>% 
  mutate(bun = if_else(19 < bun, 1, 0),
         discharge = if_else(death == 0, 0, 1)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.)) %>% 
  ungroup()

res_fm_copd <- df_mi100_stack_copd %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + age + bmi + adm_adl + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm_copd
combined_res_fm_copd <- MIcombine(res_fm_copd$fit, call=NULL)

combined_res_fm_copd_sum <- summary(combined_res_fm_copd)
exp(combined_res_fm_copd_sum[, 1:4])


# Pneumonia ---------------------------------------------------------------

df_pneumo <-  df %>% 
  filter(str_detect(df$main_code,"J09") | str_detect(df$reso_code,"J09") | str_detect(df$prep_code,"J09") |
                str_detect(df$main_code,"J1") | str_detect(df$reso_code,"J1") | str_detect(df$prep_code,"J1") |
                  str_detect(df$main_code,"J09") | str_detect(df$reso_code,"J09") | str_detect(df$prep_code,"J09") |
                str_detect(df$main_code,"J1") | str_detect(df$reso_code,"J1") | str_detect(df$prep_code,"J1"))

df_pneumo <- df_pneumo %>% 
  select(-starts_with("com"), -starts_with("sub"), -starts_with("main"), -starts_with("prep"),
         -starts_with("reso"), -amb, -nhcap) %>% 
  mutate(severity = as.character(severity),
         oral_abx = ifelse(str_detect(df_pneumo$oral_abx, "\\d+"), 1, 0),
         iv_abx = ifelse(str_detect(df_pneumo$iv_abx, "\\d+"), 1, 0),
         anti_pseudo_oral = ifelse(str_detect(df_pneumo$anti_pseudo_oral, "\\d+"), 1, 0),
         anti_pseudo_iv = ifelse(str_detect(df_pneumo$anti_pseudo_iv, "\\d+"), 1, 0),
         steroid_oral = ifelse(str_detect(df_pneumo$steroid_oral, "\\d+"), 1, 0),
         steroid_iv = ifelse(str_detect(df_pneumo$steroid_iv, "\\d+"), 1, 0),
         oxy = ifelse(str_detect(df_pneumo$procedure, "酸素"), 1, 0),
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
  select(id, adm, los, death, diff_time, age, sex, bmi, adm_adl, adm_jcs, severity,
         anti_pseudo, steroid, oxy, wbc, alb, bun, crp, hugh_johns, disc, direct_death, indirect_death) %>% 
  group_by(id) %>% 
  mutate(count = row_number()) %>% 
  ungroup()

df_pneumo %>% glimpse()

list_to_text <- function(column, sep = ", ") {
  loadNamespace("stringr")
  ret <- sapply(column, function(x) {
    ret <- stringr::str_c(stringr::str_replace_na(x), collapse = sep)
    if(identical(ret, character(0))){
      # if it's character(0). Not too sure if this would still happen now that we do str_replace_na first.
      NA
    } else {
      ret
    }
  })
  as.character(ret)
}

df_pneumo <- df_pneumo %>% 
  mutate(severity = as.character(severity),
         severity = str_split(severity, pattern = ""),
         severity = list_to_text(severity, sep = ":")) %>% 
         separate(severity, into = c("severity1", "severity2", "severity3", "bp", "immunodef",
                              "severity6", "severity7"), sep = ":") %>% 
  mutate(bp = as.numeric(bp),
         immunodef = as.numeric(immunodef))

#df_pneumo$severity <- sapply(strsplit(df_pneumo$severity,""), function(x) sum(as.numeric(x))) 

df_pneumo <- df_pneumo %>%
  drop_na(los) %>% 
  select(-adm, -disc, -direct_death, -indirect_death, -diff_time,
         -severity1, -severity2, -severity3, -severity6, -severity7) 

# person-year
df_py <- df_pneumo %>% 
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py$total_los)/1042

df_py1 <- df_pneumo %>%
  filter(anti_pseudo == 0) %>% 
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py1$total_los)/861

df_py2 <- df_pneumo %>% 
  filter(anti_pseudo == 1) %>%
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py2$total_los)/451

obj_py1 <- Surv(as.numeric(df_pneumo$los), as.numeric(df_pneumo$death))
fit_py11 <- survfit(obj_py1 ~ 1,
                    data = df_pneumo)
summary(fit_py11)
print(fit_py11)
pyears(obj_py1 ~ 1, scale = 1)

df_pneumo1 <- df_pneumo %>% 
  filter(anti_pseudo == 0)
obj_py12 <- Surv(as.numeric(df_pneumo1$los), as.numeric(df_pneumo1$death))
fit_py12 <- survfit(obj_py12 ~ 1,
                    data = df_pneumo1)
summary(fit_py12)
print(fit_py12)

df_pneumo2 <- df_pneumo %>% 
  filter(anti_pseudo == 1)
obj_py13 <- Surv(as.numeric(df_pneumo2$los), as.numeric(df_pneumo2$death))
fit_py13 <- survfit(obj_py13 ~ 1,
                    data = df_pneumo2)
summary(fit_py13)
print(fit_py13)


df_mi_pneumo <- df_pneumo 
df_mi_pneumo0 <- mice(df_mi_pneumo, maxit = 0)
df_mi_pneumo0$method
df_mi_pneumo0$predictorMatrix
predmt <- (1 - diag(1, ncol(df_mi_pneumo)))
predmt[1, ] <- predmt[, 1] <- 0
predmt
df_mi100_pneumo <- mice(df_mi_pneumo, m = 100, predictorMatrix = predmt, maxit = 20, printFlag = FALSE, seed = 1234)

df_mi100_stack_pneumo <- complete(df_mi100_pneumo, action="long") %>% 
  as_tibble()

df_mi100_stack_pneumo <- df_mi100_stack_pneumo %>% 
  group_by(.imp) %>% 
  mutate(bun = if_else(19 < bun, 1, 0),
         discharge = if_else(death == 0, 0, 1)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.)) %>% 
  ungroup()

res_fm_pneumo <- df_mi100_stack_pneumo %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + age + bmi + adm_adl + hugh_johns + 
                                  adm_jcs + oxy + bun + steroid + count + bp + immunodef + 
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm_pneumo
combined_res_fm_pneumo <- MIcombine(res_fm_pneumo$fit, call=NULL)

combined_res_fm_pneumo_sum <- summary(combined_res_fm_pneumo)
exp(combined_res_fm_pneumo_sum[, 1:4])


