
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
            discharge = if_else(death == 0, 1, 0)) %>% 
     group_by(id, .add = TRUE) %>% 
     mutate(manti_pseudo = mean(as.numeric(anti_pseudo))) %>% 
     ungroup(id) %>% 
     mutate(danti_pseudo = as.numeric(anti_pseudo) - manti_pseudo) %>% 
     mutate_all(.funs = ~ as.character(.)) %>% 
     mutate_all(.funs = ~ as.numeric(.)) %>% 
     ungroup(.imp)

# Frailty model (death) -----------------------------------------------------------

res_fm <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ danti_pseudo + manti_pseudo + age + bmi + adm_adl + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm
combined_res_fm <- MIcombine(res_fm$fit, call=NULL)

combined_res_fm_sum <- summary(combined_res_fm)
exp(combined_res_fm_sum[, 1:4])

# Frailty model (discharge)  -------------------------------------------------------------

res_gm <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, discharge) ~ manti_pseudo + danti_pseudo + age + bmi + adm_adl + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_gm
combined_res_gm <- MIcombine(res_gm$fit, call=NULL)

combined_res_gm_sum <- summary(combined_res_gm)
exp(combined_res_gm_sum[, 1:4])

# Time to next hospitalization --------------------------------------------

df <- read_rds("output/df_summary.rds")
df$diff_time

df_mi100_stack <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  mutate(bun = if_else(19 < bun, 1, 0),
         diff_time = df$diff_time,
         revisit = if_else(is.na(diff_time), 0, 1),
         diff_time = if_else(is.na(diff_time), los, diff_time)) %>% 
  group_by(id, .add = TRUE) %>% 
  mutate(manti_pseudo = mean(as.numeric(anti_pseudo))) %>% 
  ungroup(id) %>% 
  mutate(danti_pseudo = as.numeric(anti_pseudo) - manti_pseudo) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.)) %>% 
  ungroup(.imp)

res_fm <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(diff_time, revisit) ~ manti_pseudo + danti_pseudo + age + bmi + adm_adl + hugh_johns +
                                  adm_jcs + oxy + bun + steroid + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm
combined_res_fm <- MIcombine(res_fm$fit, call=NULL)

combined_res_fm_sum <- summary(combined_res_fm)
exp(combined_res_fm_sum[, 1:4])

