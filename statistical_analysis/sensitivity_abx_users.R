
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
             "survival",
             "mice",
             "norm2",
             "frailtyHL",
             "mitools",
             "lme4",
             "geepack")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

df <- read_rds("output/cleaned_data.rds")

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
  select(id, adm, los, death, diff_time, age, sex, bmi, adm_adl, adm_jcs, oral_abx, iv_abx, 
         anti_pseudo, steroid, oxy, wbc, alb, bun, crp, hugh_johns, disc, direct_death, indirect_death) %>% 
  group_by(id) %>% 
  mutate(count = row_number()) %>% 
  ungroup()
df_summary <- df_summary %>% 
  filter(oral_abx == 1 | iv_abx ==1)

df<- df_summary %>% 
  drop_na(los) %>% 
  select(-adm, -disc, -direct_death, -indirect_death, -diff_time, -oral_abx, -iv_abx)


set.seed(1234)

df %>% glimpse()
df_mi <- df 
df_mi0 <- mice(df_mi, maxit = 0)
df_mi0$method
df_mi0$predictorMatrix
predmt <- (1 - diag(1, ncol(df_mi)))
predmt[1, ] <- predmt[, 1] <- 0
predmt
df_mi100 <- mice(df_mi, m = 100, predictorMatrix = predmt, maxit = 20, printFlag = FALSE, seed = 1234)

plot(df_mi100)

df_mi100_1 <- complete(df_mi100, 1) %>% 
  glimpse()

df_mi100_stack <- complete(df_mi100, action="long") %>% 
  as_tibble()

df_mi100_stack <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  mutate(bun = if_else(19 < bun, 1, 0),
         discharge = if_else(death == 0, 1, 0)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.)) %>% 
  ungroup()

res_fm <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + age + bmi + adm_adl + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm
combined_res_fm <- MIcombine(res_fm$fit, call=NULL)

combined_res_fm_sum <- summary(combined_res_fm)
exp(combined_res_fm_sum[, 1:4])

# person- year

df_py <- df_summary %>% 
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py$total_los)/554

df_py1 <- df_summary %>%
  filter(anti_pseudo == 0) %>% 
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py1$total_los)/260

df_py2 <- df_summary %>% 
  filter(anti_pseudo == 1) %>%
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py2$total_los)/333

obj_py1 <- Surv(as.numeric(df_summary$los), as.numeric(df_summary$death))
fit_py11 <- survfit(obj_py1 ~ 1,
                    data = df_summary)
summary(fit_py11)
print(fit_py11)
pyears(obj_py1 ~ 1, scale = 1)

df_summary1 <- df_summary %>% 
  filter(anti_pseudo == 0)
obj_py12 <- Surv(as.numeric(df_summary1$los), as.numeric(df_summary1$death))
fit_py12 <- survfit(obj_py12 ~ 1,
                    data = df_summary1)
summary(fit_py12)
print(fit_py12)

df_summary2 <- df_summary %>% 
  filter(anti_pseudo == 1)
obj_py13 <- Surv(as.numeric(df_summary2$los), as.numeric(df_summary2$death))
fit_py13 <- survfit(obj_py13 ~ 1,
                    data = df_summary2)
summary(fit_py13)
print(fit_py13)
