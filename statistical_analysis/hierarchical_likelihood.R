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
  "frailtyHL"
)
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


# Data preparation --------------------------------------------------------

getwd()
df <- read_rds("output/df_summary.rds")
df %>% glimpse()
df %>% colnames()

df <- df %>%
  drop_na(los) %>% 
  select(-adm, -disc, -direct_death, -indirect_death, -diff_time) 

df %>% write_csv("output/analysis_data.csv")

df_comp <- df %>% 
  drop_na()
df_comp %>% glimpse()
df_comp <- df_comp %>% 
  mutate(bun = if_else(19 < bun, 1, 0)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.))

# Complete case analyses -----------------------------------------------------------

## Time to in-hospital death
fit_1_comp <- frailtyHL(Surv(los, death) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                        RandDist = "Normal",
                        mord = 1, # Laplace approximations to fit the mean parameters
                        dord = 1,  # Laplace approximations to fit the variance parameters
                        Maxiter = 200, # maximum number of iterations
                        convergence = 10^-6, # tolerance of the convergence criterion
                        varfixed = FALSE, # value of the variance terms for the frailties is not fixed
                        varinit = 0.1, # initial value 
                        data = df_comp)

## Time to hospital discharge
### cause-specific hazard model
fit_2_cs1_comp <- jointmodeling(Model = "mean",
                                RespDist = "FM", # frailty model
                                Link = "log",
                                LinPred = Surv(los, death == 0) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                                RandDist = "gaussian")
fit_2_cs2_comp <- jointmodeling(Model = "mean",
                                RespDist = "FM",
                                Link = "log",
                                LinPred = Surv(los, death == 1) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                                RandDist = "gaussian")
data_conti <- df_comp
data_surv <- df_comp
res_2_cs_comp <- jmfit(fit_2_cs1_comp,
                       fit_2_cs2_comp,
                       data_conti,
                       data_surv,
                       Maxiter = 200)

res_2_cs_comp[["F.Est"]][1,1]
res_2_cs_comp[["F.Est"]][1,2]
res_2_cs_comp[["F.Est"]][2,1]
res_2_cs_comp[["F.Est"]][2,2]

### subdistribution hazard
beta.init <- rep(0, 9)
q <- length(unique(df_comp$id))
v.init = rep(0, q)
theta.init = 0.05
fit_2_sd_comp <- hlike.frailty(CmpRsk(los, death) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                               data = df_comp,
                               inits = list(beta = beta.init,
                                            theta = theta.init,
                                            v = v.init
                               ),
                               order = 1,
                               frailty.cov = "none",
                               subHazard = TRUE,
                               MAX.ITER = 300)
summary(fit_2_sd_comp)
fit_2_sd_comp[["beta"]][["Estimate"]][1]
fit_2_sd_comp[["beta"]][["SE"]][1]

# Multiple imputation -----------------------------------------------------

## mice
df %>% glimpse()
df_mi <- df 
df_mi0 <- mice(df_mi, maxit = 0)
df_mi0$method
df_mi0$predictorMatrix
predmt <- (1 - diag(1, ncol(df_mi)))
predmt[1, ] <- predmt[, 1] <- 0
predmt
df_mi100 <- mice(df_mi, m = 100, predictorMatrix = predmt, maxit = 20, printFlag = FALSE, seed = 1234)

df_mi100 %>% write_rds("output/df_mi100.rds", compress = "gz")

## Time to in-hospital death

df_mi100 <- read_rds("output/df_mi100.rds")

com1 <- complete(df_mi100, 1)
com1 %>% glimpse()
com1 <- com1 %>% 
  mutate(bun = if_else(19 < bun, 1, 0)) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.))
fit_1_imp1 <- frailtyHL(Surv(los, death) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                        RandDist = "Normal",
                        mord = 1, # Laplace approximations to fit the mean parameters
                        dord = 1,  # Laplace approximations to fit the variance parameters
                        Maxiter = 200, # maximum number of iterations
                        convergence = 10^-6, # tolerance of the convergence criterion
                        varfixed = FALSE, # value of the variance terms for the frailties is not fixed
                        varinit = 0.1, # initial value 
                        data = com1)
fit_1_imp1[["FixCoef"]][1,1]
fit_1_imp1[["FixCoef"]][1,2]

fit_2_cs1_comp <- jointmodeling(Model = "mean",
                                RespDist = "FM", # frailty model
                                Link = "log",
                                LinPred = Surv(los, death == 0) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                                RandDist = "gaussian")
fit_2_cs2_comp <- jointmodeling(Model = "mean",
                                RespDist = "FM",
                                Link = "log",
                                LinPred = Surv(los, death == 1) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                                RandDist = "gaussian")
data_conti <- com1
data_surv <- com1
res_2_cs_comp <- jmfit(fit_2_cs1_comp,
                       fit_2_cs2_comp,
                       data_conti,
                       data_surv,
                       Maxiter = 200)

beta.init <- rep(0, 9)
q <- length(unique(com1$id))
v.init = rep(0, q)
theta.init = 0.05
fit_2_sd_comp <- hlike.frailty(CmpRsk(los, death) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                               data = com1,
                               inits = list(beta = beta.init,
                                            theta = theta.init,
                                            v = v.init
                               ),
                               order = 1,
                               frailty.cov = "none",
                               subHazard = TRUE,
                               MAX.ITER = 300)
summary(fit_2_sd_comp)
fit_2_sd_comp[["beta"]][["Estimate"]][1]
fit_2_sd_comp[["beta"]][["SE"]][1]

## Main analyses

est_imp_frail <- se_imp_frail <- vector(length = df_mi100$m, mode = "list")

for (i in 1:df_mi100$m) {
  com <- complete(df_mi100, i)
  com <- com %>% 
    mutate(bun = if_else(19 < bun, 1, 0)) %>% 
    mutate_all(.funs = ~ as.character(.)) %>% 
    mutate_all(.funs = ~ as.numeric(.))
  fit_1_imp <- frailtyHL(Surv(los, death) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                         RandDist = "Normal",
                         mord = 1, # Laplace approximations to fit the mean parameters
                         dord = 1,  # Laplace approximations to fit the variance parameters
                         Maxiter = 200, # maximum number of iterations
                         convergence = 10^-6, # tolerance of the convergence criterion
                         varfixed = FALSE, # value of the variance terms for the frailties is not fixed
                         varinit = 0.1, # initial value 
                         data = com)
  est_imp_frail[[i]] <- fit_1_imp[["FixCoef"]][1,1]
  se_imp_frail[[i]] <- fit_1_imp[["FixCoef"]][1,2]
}

miinf_imp_frail <- miInference(est_imp_frail, se_imp_frail)
print(miinf_imp_frail)

# 95% lower limit
lower <- miinf_random[["est"]] - 1.96 * miinf_random[["std.err"]]

# 95% upper limit
upper <- miinf_random[["est"]] + 1.96 * miinf_random[["std.err"]]

exp(miinf_random$est) 
exp(lower) 
exp(upper)

## Time to hospital discharge
### cause-specific hazard model
est_imp_cox1 <- se_imp_cox1 <- est_imp_cox2 <- se_imp_cox2 <- vector(length = df_mi100$m, mode = "list")

for (i in 1:df_mi100$m) {
  com <- complete(df_mi100, i)
  com <- com %>% 
    mutate(bun = if_else(19 < bun, 1, 0)) %>% 
    mutate_all(.funs = ~ as.character(.)) %>% 
    mutate_all(.funs = ~ as.numeric(.))
  data_conti <- com
  data_surv <- com
  fit_2_cs1_imp <- jointmodeling(Model = "mean",
                                 RespDist = "FM",
                                 Ling = "log",
                                 LinPred = Surv(los, event == 1) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                                 RandDist = "gaussian")
  fit_2_cs2_imp <- jointmodeling(Model = "mean",
                                 RespDist = "FM",
                                 Ling = "log",
                                 LinPred = Surv(los, event == 2) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                                 RandDist = "gaussian")
  res_2_cs_imp <- jmfit(fit_2_cs1_imp,
                        fit_2_cs2_comp,
                        data_conti,
                        data_surv,
                        Maxiter = 200)
  fit_1_imp <- frailtyHL(Surv(los, death) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                         RandDist = "Normal",
                         mord = 1, # Laplace approximations to fit the mean parameters
                         dord = 1,  # Laplace approximations to fit the variance parameters
                         Maxiter = 200, # maximum number of iterations
                         convergence = 10^-6, # tolerance of the convergence criterion
                         varfixed = FALSE, # value of the variance terms for the frailties is not fixed
                         varinit = 0.1, # initial value 
                         data = com)
  est_imp_cox1[[i]]  <- res_2_cs_comp[["F.Est"]][1,1]
  se_imp_cox1[[i]] <- res_2_cs_comp[["F.Est"]][1,2]
  est_imp_cox2[[i]]  <- res_2_cs_comp[["F.Est"]][2,1]
  se_imp_cox2[[i]] <- res_2_cs_comp[["F.Est"]][2,2]
}

miinf_imp_cox1 <- miInference(est_imp_cox1, se_imp_cox1)
miinf_imp_cox2 <- miInference(est_imp_cox2, se_imp_cox2)

print(miinf_imp_cox1)
print(miinf_imp_cox2)

# 95% lower limit
lower1 <- miinf_imp_cox1[["est"]] - 1.96 * miinf_imp_cox1[["std.err"]]
lower1 <- miinf_imp_cox2[["est"]] - 1.96 * miinf_imp_cox2[["std.err"]]

# 95% upper limit
upper2 <- miinf_imp_cox1[["est"]] + 1.96 * miinf_imp_cox1[["std.err"]]
upper2 <- miinf_imp_cox2[["est"]] + 1.96 * miinf_imp_cox2[["std.err"]]

exp(miinf_imp_cox1$est) 
exp(miinf_imp_cox2$est) 

exp(lower1) 
exp(upper1)
exp(lower2) 
exp(upper2)

### subdistribution hazard

est_imp_sub <- se_imp_sub <- vector(length = df_mi100$m, mode = "list")

for (i in 1:df_mi100$m) {
  com <- complete(df_mi100, i)
  com <- com %>% 
    mutate(bun = if_else(19 < bun, 1, 0)) %>% 
    mutate_all(.funs = ~ as.character(.)) %>% 
    mutate_all(.funs = ~ as.numeric(.))
  beta.init <- rep(0,9)
  v.int = rep(0, q)
  theta.init = 0.05
  fit_2_sd_comp <- hlike.frailty(CmpRsk(los, event) ~ anti_pseudo + count + age + bmi + adm_adl + adm_jcs + steroid + oxy + bun + (1|id),
                                 data = com,
                                 inits = list(beta = beta.init,
                                              theta = theta.init,
                                              v = v.init
                                 ),
                                 order = 1,
                                 frailty.cov = "none",
                                 subHazard = TRUE,
                                 MAX.ITER = 300)
  est_imp_sub[[i]] <- fit_2_sd_comp[["beta"]][["Estimate"]][1]
  se_imp_sub[[i]] <- fit_2_sd_comp[["beta"]][["SE"]][1]
}

miinf_imp_sub <- miInference(est_imp_sub, se_imp_sub)
print(miinf_imp_sub)

# 95% lower limit
lower <- miinf_imp_sub[["est"]] - 1.96 * miinf_imp_sub[["std.err"]]

# 95% upper limit
upper <- miinf_imp_sub[["est"]] + 1.96 * miinf_imp_sub[["std.err"]]

exp(miinf_imp_sub$est) 
exp(lower) 
exp(upper)
