
# Set-up packages ---------------------------------------------------------

rm(list=ls())
packages = c("devtools",
             "usethis",
             "here",
             "readr",
             "data.table",
             "readxl",
             "tidyverse",
             "tidylog",
             "lubridate",
             "psych",
             "arsenal",
             "tableone",
             "ggplot2",
             "tidylog",
             "ggplotgui",
             "ggthemes",
             "broom",
             "lme4",
             "parameters",
             "ggeffects",
             "performance",
             "plm",
             "mice")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
# Descriptive statistics --------------------------------------------------

getwd()
df <- read_rds("output/cleaned_data.rds")
df %>% colnames()

df_summary <- df %>% 
  select(-starts_with("com"), -starts_with("sub"), -starts_with("main"), -starts_with("prep"),
         -starts_with("reso"), -severity, -amb, -nhcap) %>% 
  mutate(oral_abx = ifelse(str_detect(df$oral_abx, "\\d+"), 1, 0),
         iv_abx = ifelse(str_detect(df$iv_abx, "\\d+"), 1, 0),
         anti_pseudo_oral = ifelse(str_detect(df$anti_pseudo_oral, "\\d+"), 1, 0),
         anti_pseudo_iv = ifelse(str_detect(df$anti_pseudo_iv, "\\d+"), 1, 0),
         steroid_oral = ifelse(str_detect(df$steroid_oral, "\\d+"), 1, 0),
         steroid_iv = ifelse(str_detect(df$steroid_iv, "\\d+"), 1, 0),
         oxy = ifelse(str_detect(df$procedure, "酸素"), 1, 0))

vars <- c("sex", "adm_adl", "disc_adl", "bmi", "disc_to", "prognosis", "death24", "route", "oral_abx", "iv_abx",
          "anti_pseudo_oral", "anti_pseudo_iv", "steroid_oral", "steroid_iv", "wbc", "alb", "bun", "crp", "difwbc", "oxy")
factorVars <- c("sex", "adm_adl", "disc_adl", "disc_to", "prognosis", "death24", "route", "oral_abx", "iv_abx",
                "anti_pseudo_oral", "anti_pseudo_iv", "steroid_oral", "steroid_iv", "oxy")
table1 <- CreateTableOne(vars = vars,
                         data = df_summary,
                         includeNA = FALSE,
                         factorVars = factorVars)
table1 %>% 
  print()
        