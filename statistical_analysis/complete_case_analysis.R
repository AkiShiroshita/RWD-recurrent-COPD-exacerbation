
# Set-up packages ---------------------------------------------------------

packages = c("survival",
             "survminer",
             "naniar")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Data set-up -------------------------------------------------------------

df_summary %>% glimpse()
df_comp <- df_summary %>%
  select(-disc_adl, -disc_jcs)
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
           legend.title = "Legend",
           legend = c(.8,.8),
           size = 1,
           conf.int = F,
           xlab = "Days since admission",
           ylab = "Survival probability",
           censor = F,
           ggtheme = theme_bw())

fit_km2 <- survfit(obj_km ~ anti_pseudo,
                   data = df_comp)
summary(fit_km2)
print(fit_km2)

temp <- ggsurvplot(fit_km2,
           dat = df_comp,
           legend.title = "Legend",
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

obj_fixed_comp <- Surv(df_comp$los, df_comp$death)
fit_fixed_comp <- coxph(obj_fixed_comp ~ anti_pseudo + sex + bmi + adm_adl + steroid + adm_jcs + oxy + crp + alb + bun + count + strata(id),
                        data = df_comp)
cox.zph(fit_fixed_comp)
scatter.smooth(residuals(fit_fixed_comp, type="deviance"))
abline(h=0,lty=3,col=2)

summary(fit_fixed_comp)
confint(fit_fixed_comp)


# Frailty model -----------------------------------------------------------

obj_frailty_comp <- Surv(df_comp$los, df_comp$death)
fit_frailty_comp <- coxph(obj_frailty_comp ~ anti_pseudo + frailty(id),
                          data = df_comp)
summary(fit_frailty_comp)
