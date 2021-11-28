
# Set-up packages ---------------------------------------------------------

packages = c("survival",
             "mice",
             "norm2",
             "mitools",
             "tidyverse",
             "lubridate")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Multiple imputation -----------------------------------------------------

getwd()
df_summary <- read_rds("output/df_summary.rds")
df_summary %>% glimpse()
df_mi <- df_summary
df_mi0 <- mice(df_mi, maxit = 0)
df_mi0$method
df_mi0$predictorMatrix
predmt <- (1 - diag(1, ncol(df_mi)))
predmt[1, ] <- predmt[, 1] <- 0
predmt
df_mi100 <- mice(df_mi, m = 100, predictorMatrix = predmt, maxit = 20, printFlag = FALSE, seed = 1234)

df_mi100 %>% write_rds("output/df_mi100.rds", compress = "gz")

# Fixed effects model -----------------------------------------------------

# not converged

df_mi100 <- read_rds("output/df_mi100.rds")

est <- se <- vector(length = df_mi100$m, mode = "list")

for (i in 1:df_mi100$m) {
  com <- complete(df_mi100, i)
  com <- com %>% 
    mutate(death = as.integer(death)
    )
  
  obj_fixed_mi <- Surv(com$los, com$death)
  fit_fixed_mi <- coxph(obj_fixed_mi ~ anti_pseudo + sex + bmi + adm_adl + steroid + adm_jcs + oxy + crp + alb + bun + count + strata(id),
                          data = com)
  s <- summary(fit_fixed_mi)
  est[[i]] <- s[["coefficients"]][1, 1]
  se[[i]] <- s[["coefficients"]][1, 3]
}

miinf_fixed <- miInference(est, se)
print(miinf_fixed)
exp(miinf_fixed$est) 
exp(miinf_fixed$std.err) 

# Frailty model -----------------------------------------------------------

df_mi100 <- read_rds("output/df_mi100.rds")

est <- se <- vector(length = df_mi100$m, mode = "list")

for (i in 1:df_mi100$m) {
  com <- complete(df_mi100, i)
  com <- com %>% 
    mutate(death = as.integer(death)
    )
  
  obj_random_mi <- Surv(com$los, com$death)
  fit_random_mi <- coxph(obj_random_mi ~ anti_pseudo + sex + bmi + adm_adl + steroid + adm_jcs + oxy + crp + alb + bun + count + frailty(id),
                        data = com)
  s <- summary(fit_random_mi)
  est[[i]] <- s[["coefficients"]][1, 1]
  se[[i]] <- s[["coefficients"]][1, 3]
}

miinf_random <- miInference(est, se)
print(miinf_random)
# lower limit
lower <- miinf_random[["est"]] - 1.96 * miinf_random[["std.err"]]
# upper limit
upper <- miinf_random[["est"]] + 1.96 * miinf_random[["std.err"]]

exp(miinf_random$est) 
exp(lower) 
exp(upper)
