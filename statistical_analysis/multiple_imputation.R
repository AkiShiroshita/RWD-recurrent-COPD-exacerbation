
# Set-up packages ---------------------------------------------------------

packages = c("survival",
             "mice")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Multiple imputation -----------------------------------------------------

df_stats %>% glimpse()
df_mi <- df_stats %>%
  mutate(id = as.factor(id),
         los = as.numeric(los),
         death = as.factor(death),
         bmi = as.numeric(bmi),
         anti_pseudo = as.factor(anti_pseudo),
         oxy = as.factor(oxy),
         wbc = as.numeric(wbc),
         alb = as.numeric(alb),
         bun = as.numeric(bun),
         crp = as.numeric(crp))
df_mi0 <- mice(df_mi, maxit = 0)
df_mi0$method
df_mi0$predictorMatrix
predmt <- (1 - diag(1, ncol(df_mi)))
predmt[1, ] <- predmt[, 1] <- 0
predmt
df_mi100 <- mice(df_mi, m = 100, predictorMatrix = predmt, maxit = 20, printFlag = FALSE, seed = 1234)

# Fixed effects model -----------------------------------------------------

est <- se <- vector(length = df_mi100$m, mode = "list")
for (i in 1:df_mi100$m) {
  com <- complete(df_mi100, i)
  com <- com %>% 
    mutate(bmi = weight/(height/100)^2) %>% 
    mutate(hr = if_else(hr >= 109, 1, 0))
  fit <- glmer(los ~ broad + age + bmi + count + o2 + hot + adl + ams + rr + hr + steroid + hospital + (1|id),
               com,
               family = Gamma(link = "log"))
  s <- summary(fit)
  est[[i]] <- s[["coefficients"]][2, 1]
  se[[i]] <- s[["coefficients"]][2, 2]
}
miinf <- miInference(est, se)
print(miinf)
exp(miinf$est) 
exp(miinf$std.err) 


# Frailty model -----------------------------------------------------------


