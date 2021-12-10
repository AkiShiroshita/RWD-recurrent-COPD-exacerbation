
# Set-up packages ---------------------------------------------------------

packages = c(
  "tidyverse",
  "lubridate",
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

df_mi100 <- read_rds("output/df_mi100.rds")

df_mi100_stack <- complete(df_mi100, action="long") %>% 
  as_tibble()

df_mi100_stack <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  mutate(bun = if_else(19 < bun, 1, 0),
         discharge = if_else(death == 0, 0, 1)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.)) %>% 
  ungroup()

# Interaction terms -----------------------------------------------------------

# age

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*age + bmi + adm_adl + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# bmi

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*bmi + age + adm_adl + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# adl

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*adm_adl + age + bmi + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# jcs

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*adm_jcs + age + bmi + hugh_johns + adm_adl +
                                   oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# hugh_johns

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*hugh_johns + age + bmi + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# oxy

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*oxy + age + bmi + hugh_johns + adm_adl +
                                  adm_jcs + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# bun

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*bun + adm_adl + age + bmi + hugh_johns +
                                  adm_jcs + oxy + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# steroid

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*steroid + adm_adl + age + bmi + hugh_johns +
                                  adm_jcs + oxy + bun + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

# Interaction terms2 -----------------------------------------------------------

# age

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*age +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# bmi

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*bmi +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# adl

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*adm_adl +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# jcs

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*adm_jcs +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# hugh_johns

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*hugh_johns +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# oxy

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*oxy + 
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# bun

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*bun +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])

# steroid

res_int <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo*steroid +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_int
combined_res_int <- MIcombine(res_int$fit, call=NULL)

combined_res_int_sum <- summary(combined_res_int)
exp(combined_res_int_sum[, 1:4])
