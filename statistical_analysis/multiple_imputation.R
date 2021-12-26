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

# Multiple imputation -----------------------------------------------------

## mice

set.seed(1234)

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

plot(df_mi100)
