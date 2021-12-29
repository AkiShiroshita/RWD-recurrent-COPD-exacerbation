
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

# person- year

df_py <- df_summary %>% 
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py$total_los)/893

df_py1 <- df_summary %>%
  filter(anti_pseudo == 0) %>% 
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py1$total_los)/645

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

df_comp <- df_summary %>% 
  drop_na()
df_comp %>% glimpse()
df_comp <- df_comp %>% 
  mutate(bun = if_else(19 < bun, 1, 0)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.))

# Kaplan-Meier Method -----------------------------------------------------

df_comp %>% glimpse()

# person-year
df_py <- df_comp %>% 
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py$total_los)/641

df_py1 <- df_comp %>%
  filter(anti_pseudo == 0) %>% 
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py1$total_los)/449

df_py2 <- df_comp %>% 
  filter(anti_pseudo == 1) %>%
  group_by(id) %>% 
  summarise(total_los = sum(los)) %>% 
  ungroup() 
sum(df_py2$total_los)/250

obj_py1 <- Surv(as.numeric(df_comp$los), as.numeric(df_comp$death))
fit_py11 <- survfit(obj_py1 ~ 1,
                    data = df_comp)
summary(fit_py11)
print(fit_py11)
pyears(obj_py1 ~ 1, scale = 1)

df_comp1 <- df_comp %>% 
  filter(anti_pseudo == 0)
obj_py12 <- Surv(as.numeric(df_comp1$los), as.numeric(df_comp1$death))
fit_py12 <- survfit(obj_py12 ~ 1,
                    data = df_comp1)
summary(fit_py12)
print(fit_py12)

df_comp2 <- df_comp %>% 
  filter(anti_pseudo == 1)
obj_py13 <- Surv(as.numeric(df_comp2$los), as.numeric(df_comp2$death))
fit_py13 <- survfit(obj_py13 ~ 1,
                    data = df_comp2)
summary(fit_py13)
print(fit_py13)

# figure
obj_py2 <- Surv(as.numeric(df_comp$los), as.numeric(df_comp$death))
fit_py21 <- survfit(obj_py2 ~ 1,
                  data = df_comp)
summary(fit_py21)
print(fit_py21)
pyears(obj_py1 ~ 1)

fit_py22 <- survfit(obj_py2 ~ as.numeric(anti_pseudo),
                    data = df_comp)
summary(fit_py22)
print(fit_py22)
pyears(obj_py1 ~ as.numeric(df_comp$death))

ggsurvplot(fit_py21,
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

temp <- ggsurvplot(fit_py22,
           dat = df_comp,
           legend.title = "",
           legend.labs = c("Anti-pseudomonal antibiotics group","Non-anti-pseudomonal antibiotics group"),
           legend = c(.3,.4),
           font.legend = 14,
           size = 2,
           conf.int = T,
           xlab = "Days since admission",
           font.x = 14,
           ylab = "Survival probability",
           font.y = 14,
           xlim = c(0, 100),
           break.x.by = 20,
           censor = F,
           ggtheme = theme_classic())
temp

ggsave("figures/Figure3.tiff", dpi = 350)

# Fixed effects model -----------------------------------------------------
# These fixed effects model did not converge

## count: continuous
## not converged

obj_fixed_comp <- Surv(df_comp$los, df_comp$death)
fit_fixed_comp <- coxph(obj_fixed_comp ~ anti_pseudo + age + bmi + adm_adl + hugh_johns +
                          adm_jcs + oxy + bun + steroid + count + strata(id),
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
fit_fixed_comp <- coxph(obj_fixed_comp ~ anti_pseudo + age + bmi + adm_adl + hugh_johns +
                          adm_jcs + oxy + bun + steroid + count + strata(id),
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

