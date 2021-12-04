
# Set-up packages ---------------------------------------------------------

packages = c("survival",
             "mice",
             "norm2",
             "tidyverse",
             "lubridate")
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

df_comp <- df_summary %>%
  select(-disc) %>% 
  drop_na()

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
## not converge

df_comp %>% glimpse()
df_comp_fixed <- df_comp %>% 
  mutate(death = as.integer(death),
         adm_jcs = as.integer(adm_jcs),
         adm_adl = as.integer(adm_adl)
         #adm_jcs = ifelse(adm_jcs == 0, 0, 1),
         #adm_adl = ifelse(adm_adl == 0 | adm_adl == 1, 0, 1)
         )

obj_fixed_comp <- Surv(df_comp_fixed$los, df_comp_fixed$death)
fit_fixed_comp <- coxph(obj_fixed_comp ~ anti_pseudo + sex + bmi + adm_adl + steroid + adm_jcs + oxy + crp + alb + bun + count + strata(id),
                        data = df_comp_fixed)
cox.zph(fit_fixed_comp)
scatter.smooth(residuals(fit_fixed_comp, type="deviance"))
abline(h=0,lty=3,col=2)

result_fixed <- summary(fit_fixed_comp)
exp(result_fixed[["coefficients"]][1,1])
exp(result_fixed[["coefficients"]][1,3])
exp(confint(fit_fixed_comp))

## count: categorical variable (1 < count < 6)
## not converge

df_comp_fixed <- df_comp_fixed %>% 
  filter(count < 6)

obj_fixed_comp <- Surv(df_comp_fixed$los, df_comp_fixed$death)
fit_fixed_comp <- coxph(obj_fixed_comp ~ anti_pseudo + sex + bmi + adm_adl + steroid + adm_jcs + oxy + crp + alb + bun + count + strata(id),
                        data = df_comp_fixed)
cox.zph(fit_fixed_comp)
scatter.smooth(residuals(fit_fixed_comp, type="deviance"))
abline(h=0,lty=3,col=2)

summary(fit_fixed_comp)
confint(fit_fixed_comp)

# Frailty model -----------------------------------------------------------

## count: continuous

df_comp %>% glimpse()
df_comp_random <- df_comp %>% 
  mutate(death = as.integer(death),
         #count = as.factor(count),
         #adm_jcs = as.integer(adm_jcs),
         #adm_adl = as.integer(adm_adl))
         #adm_jcs = ifelse(adm_jcs == 0, 0, 1),
         #adm_adl = ifelse(adm_adl == 0 | adm_adl == 1, 0, 1)
         )
obj_frailty_comp <- Surv(df_comp_random$los, df_comp_random$death)
fit_frailty_comp <- coxph(obj_frailty_comp ~ anti_pseudo + sex + bmi + adm_adl + steroid + adm_jcs + oxy + crp + alb + bun + count + frailty(id),
                          data = df_comp_random)
result_frailty <- summary(fit_frailty_comp)
exp(result_frailty[["coefficients"]][1,1])
exp(result_frailty[["coefficients"]][1,3])
exp(confint(fit_frailty_comp))

