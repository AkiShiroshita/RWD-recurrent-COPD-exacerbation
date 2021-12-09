
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
  "mitools"
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
  mutate(bun = if_else(19 < bun, 1, 0)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.)) %>% 
  ungroup()

# Frailty model -----------------------------------------------------------

res_fm <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + sex + bmi + adm_adl +
                                  steroid + adm_jcs + oxy + crp + alb + bun + count +
                                  frailty(id, dist = "gauss"),
                                data = .))) 
res_fm
combined_res_fm <- MIcombine(res_fm$fit, call=NULL)

combined_res_fm_sum <- summary(combined_res_fm)
exp(combined_res_fm_sum[, 1:4])

# Gamma model -------------------------------------------------------------

res_gm <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~glm(los ~ anti_pseudo + sex + bmi + adm_adl +
                                  steroid + adm_jcs + oxy + crp + alb + bun + count +
                                  (1|id),
                              data = .,
                              family = Famma(link = "log")))) 
res_fm
combined_res_fm <- MIcombine(res_fm$fit, call=NULL)

combined_res_fm_sum <- summary(combined_res_fm)
exp(combined_res_fm_sum[, 1:4])
