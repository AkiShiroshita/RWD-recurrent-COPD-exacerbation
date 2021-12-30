
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

df_mi100_1 <- complete(df_mi100, 1) %>% 
  glimpse()

df_mi100_stack <- complete(df_mi100, action="long") %>% 
  as_tibble()

df_mi100_stack <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  mutate(bun = if_else(19 < bun, 1, 0),
         discharge = if_else(death == 0, 0, 1)) %>% 
  filter(count < 10) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.)) %>% 
  ungroup()


# Frailty model2 -----------------------------------------------------------

res_fm2 <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + count +
                                  frailty(id, dist = "gamma"),
                                data = .))) 
res_fm2
combined_res_fm2 <- MIcombine(res_fm2$fit, call=NULL)

combined_res_fm_sum2 <- summary(combined_res_fm2)
exp(combined_res_fm_sum2[, 1:4])

res_fm2 <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + age + bmi + adm_adl + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gamma"),
                                data = .))) 
res_fm2
combined_res_fm2 <- MIcombine(res_fm2$fit, call=NULL)

combined_res_fm_sum2 <- summary(combined_res_fm2)
exp(combined_res_fm_sum2[, 1:4])

# Cox proportional hazard model with robust standard error ----------------

res_fm2 <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + count + cluster(id),
                                data = .))) 
res_fm2
combined_res_fm2 <- MIcombine(res_fm2$fit, call=NULL)

combined_res_fm_sum2 <- summary(combined_res_fm2)
exp(combined_res_fm_sum2[, 1:4])

res_fm2 <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + age + bmi + adm_adl + steroid + adm_jcs +
                                  oxy + bun + count + cluster(id),
                                data = .))) 
res_fm2
combined_res_fm2 <- MIcombine(res_fm2$fit, call=NULL)

combined_res_fm_sum2 <- summary(combined_res_fm2)
exp(combined_res_fm_sum2[, 1:4])

# Fixed effects model -----------------------------------------------------------

res_fe <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + count +
                                  strata(id),
                                data = .))) 
res_fe
combined_res_fe <- MIcombine(res_fe$fit, call=NULL)

combined_res_fe_sum <- summary(combined_res_fe)
exp(combined_res_fe_sum[, 1:4])

# no convergence

# Logistic regression -----------------------------------------------------

# just for a exploratory
res_lg <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~geeglm(death ~ anti_pseudo + age + bmi + adm_adl + hugh_johns +
                                   adm_jcs + oxy + bun + steroid + count,
                                 id = id,
                                 corstr = "exchangeable",
                                 family = binomial(),
                                 data = .))) 
res_lg
combined_res_lg <- MIcombine(res_lg$fit, call=NULL)

combined_res_lg_sum <- summary(combined_res_lg)
exp(combined_res_lg_sum[, 1:4])
