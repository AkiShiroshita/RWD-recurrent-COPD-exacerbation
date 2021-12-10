
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
         discharge = if_else(death == 0, 0, 1)) %>% 
  filter(count < 10) %>% 
  mutate_all(.funs = ~ as.character(.)) %>% 
  mutate_all(.funs = ~ as.numeric(.)) %>% 
  ungroup()

# Fixed effects model -----------------------------------------------------------

res_fe <- df_mi100_stack %>% 
  group_by(.imp) %>% 
  nest() %>% 
  mutate(fit = map(data, ~coxph(Surv(los, death) ~ anti_pseudo + count +
                                  strata(id),
                                data = .))) 
res_fe
combined_res_fe <- MIcombine(res_fe$fit, call=NULL)

combined_res_fe_sum <- summary(combined_res_fe)
exp(combined_res_fe_sum[, 1:4])

# no convergence
