
# Set-up packages ---------------------------------------------------------

packages = c("tidyverse",
             "lubridate",
             "survival",
             "mice",
             "norm2",
             "tidyverse",
             "lubridate",
             "ggplot2",
             "survminer")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Data set-up -------------------------------------------------------------

getwd()
df_summary <- read_rds("output/df_summary.rds")
df_summary %>% glimpse()
df_summary %>% colnames()

df_summary <- df_summary %>%
  select(id, los, death, anti_pseudo, age, bmi, adm_adl, hugh_johns,
  adm_jcs, oxy, bun, steroid, count) 

df_comp <- df_summary %>% 
  drop_na()
df_comp %>% glimpse()
df_comp <- df_comp %>% 
  mutate(bun = if_else(19 < bun, 1, 0)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.))

# Kaplan-Meier Method -----------------------------------------------------

df_comp %>% glimpse()
df_comp <- df_comp %>% 
  mutate(alive = ifelse(death == 1, 0, 1))
obj_km <- Surv(df_comp$los, df_comp$alive)
fit_km <- survfit(obj_km ~ 1,
                  data = df_comp)
summary(fit_km)
print(fit_km)

ggsurvplot(fit_km,
           dat = df_comp,
           legend = "none",
           size = 2,
           conf.int = F,
           xlab = "Days since admission",
           font.x = 14,
           ylab = "Survival probability",
           font.y = 14,
           xlim = c(0, 300),
           censor = F,
           ggtheme = theme_classic())

fit_km2 <- survfit(obj_km ~ anti_pseudo,
                   data = df_comp)
summary(fit_km2)
print(fit_km2)

temp <- ggsurvplot(fit_km2,
           dat = df_comp,
           legend.title = "",
           legend.labs = c("Anti-pseudomonal antibiotics group","Non-anti-pseudomonal antibiotics group"),
           legend = c(.8,.8),
           font.legend = 14,
           size = 2,
           conf.int = F,
           xlab = "Days since admission",
           font.x = 14,
           ylab = "Survival probability",
           font.y = 14,
           xlim = c(0, 300),
           censor = F,
           ggtheme = theme_classic())
temp

# Fixed effects model -----------------------------------------------------
# These fixed effects model did not converge

## count: continuous
## not converged

obj_fixed_comp <- Surv(df_comp$los, df_comp$death)
fit_fixed_comp <- coxph(obj_fixed_comp ~ anti_pseudo + sex + bmi + adm_adl + steroid + adm_jcs + oxy + crp + alb + bun + count + strata(id),
                        data = df_comp)
cox.zph(fit_fixed_comp)
scatter.smooth(residuals(fit_fixed_comp, type="deviance"))
abline(h=0,lty=3,col=2)

result_fixed <- summary(fit_fixed_comp)
exp(result_fixed[["coefficients"]][1,1])
exp(result_fixed[["coefficients"]][1,3])
exp(confint(fit_fixed_comp))

## count: categorical variable (1 < count < 6)
## not converged

df_comp_fixed <- df_comp_fixed %>% 
  filter(count < 6)

obj_fixed_comp <- Surv(df_comp$los, df_comp$death)
fit_fixed_comp <- coxph(obj_fixed_comp ~ anti_pseudo + sex + bmi + adm_adl + steroid + adm_jcs + oxy + crp + alb + bun + count + strata(id),
                        data = df_comp)
cox.zph(fit_fixed_comp)
scatter.smooth(residuals(fit_fixed_comp, type="deviance"))
abline(h=0,lty=3,col=2)

summary(fit_fixed_comp)
confint(fit_fixed_comp)

# Frailty model -----------------------------------------------------------

## count: continuous

obj_frailty_comp <- Surv(df_comp$los, df_comp$death)
fit_frailty_comp <- coxph(obj_frailty_comp ~ anti_pseudo + age + bmi + adm_adl + hugh_johns +
                            adm_jcs + oxy + bun + steroid + count + frailty(id),
                          data = df_comp)
result_frailty <- summary(fit_frailty_comp)
exp(result_frailty[["coefficients"]][1,1])
exp(result_frailty[["coefficients"]][1,3])
exp(confint(fit_frailty_comp))

# Cox proportional hazard model with robust standard error ----------------

df_comp %>% glimpse()
obj_cox_comp <- Surv(df_comp$los, df_comp$death)
fit_cox_comp <- coxph(obj_cox_comp ~ anti_pseudo + age + bmi + adm_adl + steroid + adm_jcs + oxy + bun + count + cluster(id),
                          data = df_comp)
result_frailty <- summary(fit_cox_comp)
result_frailty
exp(confint(fit_frailty_comp))
