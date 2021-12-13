# Set-up packages ---------------------------------------------------------

packages = c(
  "tidyverse",
  "lubridate",
  "ggplot2",
  "knitr",
  "kableExtra",
  "tableone",
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

df <- read_rds("output/cleaned_data.rds")


# risks for drug resistant organisms -------------------------------------------------------

df_dr <- df %>% 
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
         adm_adl_sel = if_else(85 <= adm_adl, 1, 0),
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
  select(id, adm, los, anti_pseudo, age, sex, bmi, adm_adl, adm_adl_sel, hugh_johns,
           adm_jcs, oxy, bun, steroid, death, route, pre_hos,
         pep, oral_abx90, iv_abx90, oral_steroid90, iv_steroid90,
         oral_immuno, procedure_code) %>% 
  group_by(id) %>% 
  mutate(count = row_number()) %>% 
  ungroup()

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

df_dr <- left_join(df_dr, dialysis_use, by = c("id", "adm"))
df_dr <- df_dr %>% 
  mutate(dialysis = if_else(is.na(dialysis), 0, 1),
         nh = if_else(route == 0 | route == 4 | route == 5, 1, 0),
         abx90 = if_else(oral_abx90 == 1 | iv_abx90 == 1, 1, 0),
         steroid90 = if_else(oral_steroid90 == 1 | iv_steroid90 == 1, 1, 0),
         immuno90 = if_else(oral_immuno == 1, 1, 0)
         )

factorVars <- vars <- c("dialysis", "nh", "pre_hos", "abx90", "steroid90", "immuno90", "adm_adl_sel", "pep")
table1 <- CreateTableOne(vars = vars,
                         data = df_dr,
                         includeNA = FALSE,
                         factorVars = factorVars)
table1 %>% 
  print(nonnormal = c("wbc", "alb", "bun", "crp", "los"))

table2 <- CreateTableOne(vars = vars,
                         data = df_dr,
                         includeNA = FALSE,
                         factorVars = factorVars,
                         strata = "anti_pseudo")
table2 %>% 
  print(nonnormal = c("wbc", "alb", "bun", "crp", "los"))

df_dr <- df_dr %>% 
  filter(nh == 1 | dialysis == 1 | pre_hos == 1 | abx90 == 1 | steroid90 == 1 |
           immuno90 == 1 | pep ==1)

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

# analysis ----------------------------------------------------------------

df_mi_dr <- df_dr %>% 
  select(id, los, death, anti_pseudo, age, sex, bmi, adm_adl, hugh_johns, adm_jcs, oxy, bun, steroid, count)
df_mi_dr0 <- mice(df_mi_dr, maxit = 0)
df_mi_dr0$method
df_mi_dr0$predictorMatrix
predmt <- (1 - diag(1, ncol(df_mi_dr)))
predmt[1, ] <- predmt[, 1] <- 0
predmt
df_mi100_dr <- mice(df_mi_dr, m = 100, predictorMatrix = predmt, maxit = 20, printFlag = FALSE, seed = 1234)

df_mi100_stack_dr <- complete(df_mi100_dr, action="long") %>% 
  as_tibble()

df_mi100_stack_dr <- df_mi100_stack_dr %>% 
  group_by(.imp) %>% 
  mutate(bun = if_else(19 < bun, 1, 0),
         discharge = if_else(death == 0, 0, 1)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.)) %>% 
  ungroup()

res_fm_dr <- df_mi100_stack_dr %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + age + bmi + adm_adl + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm_dr
combined_res_fm_dr <- MIcombine(res_fm_dr$fit, call=NULL)

combined_res_fm_dr_sum <- summary(combined_res_fm_dr)
exp(combined_res_fm_dr_sum[, 1:4])

# Interaction terms -------------------------------------------------------

df_mi_int <- df_dr %>% 
  select(id, los, death, anti_pseudo, age, sex, bmi, adm_adl, hugh_johns, adm_jcs, oxy, bun, steroid, count,
         nh, dialysis, pre_hos, abx90, steroid90, immuno90, pep)
df_mi_int0 <- mice(df_mi_int, maxit = 0)
df_mi_int0$method
df_mi_int0$predictorMatrix
predmt <- (1 - diag(1, ncol(df_mi_int)))
predmt[1, ] <- predmt[, 1] <- 0
predmt
df_mi100_int <- mice(df_mi_int, m = 100, predictorMatrix = predmt, maxit = 20, printFlag = FALSE, seed = 1234)

df_mi100_stack_int <- complete(df_mi100_int, action="long") %>% 
  as_tibble()

df_mi100_stack_int <- df_mi100_stack_int %>% 
  group_by(.imp) %>% 
  mutate(bun = if_else(19 < bun, 1, 0),
         discharge = if_else(death == 0, 0, 1)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.)) %>% 
  ungroup()

res_fm_int <- df_mi100_stack_int %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo:nh + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm_int
combined_res_fm_int <- MIcombine(res_fm_int$fit, call=NULL)

combined_res_fm_int_sum <- summary(combined_res_fm_int)
exp(combined_res_fm_int_sum[, 1:4])

res_fm_int <- df_mi100_stack_int %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo:dialysis + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm_int
combined_res_fm_int <- MIcombine(res_fm_int$fit, call=NULL)

combined_res_fm_int_sum <- summary(combined_res_fm_int)
exp(combined_res_fm_int_sum[, 1:4])

res_fm_int <- df_mi100_stack_int %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo:pre_hos + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm_int
combined_res_fm_int <- MIcombine(res_fm_int$fit, call=NULL)

combined_res_fm_int_sum <- summary(combined_res_fm_int)
exp(combined_res_fm_int_sum[, 1:4])

res_fm_int <- df_mi100_stack_int %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo:abx90 + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm_int
combined_res_fm_int <- MIcombine(res_fm_int$fit, call=NULL)

combined_res_fm_int_sum <- summary(combined_res_fm_int)
exp(combined_res_fm_int_sum[, 1:4])

res_fm_int <- df_mi100_stack_int %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo:steroid90 + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm_int
combined_res_fm_int <- MIcombine(res_fm_int$fit, call=NULL)

combined_res_fm_int_sum <- summary(combined_res_fm_int)
exp(combined_res_fm_int_sum[, 1:4])

res_fm_int <- df_mi100_stack_int %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo:immuno90 + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm_int
combined_res_fm_int <- MIcombine(res_fm_int$fit, call=NULL)

combined_res_fm_int_sum <- summary(combined_res_fm_int)
exp(combined_res_fm_int_sum[, 1:4])

res_fm_int <- df_mi100_stack_int %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo:pep + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm_int
combined_res_fm_int <- MIcombine(res_fm_int$fit, call=NULL)

combined_res_fm_int_sum <- summary(combined_res_fm_int)
exp(combined_res_fm_int_sum[, 1:4])
