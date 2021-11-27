
# Set-up packages ---------------------------------------------------------

packages = c("survival",
             "naniar")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Data set-up -------------------------------------------------------------

df_stats <- df_summary %>% 
  mutate(disc = ymd(disc),
         los = disc - adm + 1,
         death = ifelse(prognosis==6 | prognosis==7, 1, 0),
         anti_pseudo = ifelse(anti_pseudo_oral==1 | anti_pseudo_iv==1, 1, 0),
         steroid = if_else(steroid_oral==1 | steroid_iv==1, 1, 0)) %>% 
  select(id, los, death, bmi, anti_pseudo, oxy, wbc, alb, bun, crp)

miss <- miss_var_summary(df_stats)
miss

df_comp <- df_stats %>% 
  drop_na()

# Fixed effects model -----------------------------------------------------

obj_fixed_comp <- Surv(df_comp$los, df_comp$death)
fit_fixed_comp <- coxph(obj_fixed_comp ~ anti_pseudo + strata(id),
                        data = df_comp)
summary(fit_fixed_comp)


# Frailty model -----------------------------------------------------------

obj_frailty_comp <- Surv(df_comp$los, df_comp$death)
fit_frailty_comp <- coxph(obj_frailty_comp ~ anti_pseudo + frailty(id),
                          data = df_comp)
summary(fit_frailty_comp)
