
# Set-up ------------------------------------------------------------------

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
             "ggplot2",
             "ggplotgui",
             "ggthemes",
             "arsenal",
             "survival")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

df <- read_rds("output/cleaned_data.rds")
df %>% glimpse()

emr_drug_data = fread("input/2021102512_3_EMR_Drug_data_2021_002_SCE.csv.gz")

# Oral vancomycin ---------------------------------------------------------

oral_vanco <- emr_drug_data %>% 
  rename(id = "患者ID",
         day = "開始日") %>%
  mutate(day = ymd(day)) %>% 
  filter(薬価コード == "6113001B1011" | 薬価コード == "6113001B1089" | 
                薬価コード == "6113001B1143")

# nobody used oral vancomycin

# Oral metrolidazole ------------------------------------------------------

oral_metro <- emr_drug_data %>% 
  rename(id = "患者ID",
         day = "開始日") %>%
  mutate(day = ymd(day)) %>% 
  filter(薬価コード == "6419002F1131" )

# nobody used oral metrolidazole

# IV metrolidazole --------------------------------------------------------

iv_metro <- emr_drug_data %>% 
  rename(id = "患者ID",
         day = "開始日",
         code = "薬価コード") %>%
  mutate(id = as.character(id),
         day = ymd(day)) %>% 
  filter(code == "6419401A1027" )

iv_metro_use <- c()

for(i in 1:17) {
  filter_key <- count_key %>% 
    filter(count == i)
  id1 <- filter_key$id
  id2 <- iv_metro$id
  y1 <- filter_key$adm
  y2 <- iv_metro$day
  iv_metro_filter <- neardate(id1, id2, y1, y2) 
  iv_metro_filter <- ifelse((iv_metro$day[iv_metro_filter] - filter_key$adm) >= 3 & (iv_metro$day[iv_metro_filter] - filter_key$adm) < 90, NA, iv_metro_filter)
  iv_metro_before <- iv_metro[iv_metro_filter, ] %>% 
    drop_na(id) %>% 
    distinct(id, .keep_all=TRUE)
  count <- count_key %>% 
    filter(count == i)  
  iv_metro_use_append <- left_join(count, iv_metro_before, by = "id")
  iv_metro_use <- bind_rows(iv_metro_use, iv_metro_use_append)
}

iv_metro_use <- iv_metro_use %>% 
  arrange(id, adm) %>% 
  drop_na(code) %>% 
  rename(iv_metro = "code") %>% 
  select(id, adm, iv_metro) %>% 
  mutate(id = as.integer(id))

df <- left_join(df, iv_metro_use, by = c("id","adm")) 
df <- df %>% 
  mutate(iv_metro = if_else(is.na(iv_metro), 0, 1))

df %>% filter(iv_metro == 1)

# Disease code ------------------------------------------------------------

df <- df %>% 
  mutate(cdi_code = if_else(subs_code1 == "A047"|subs_code2 == "A047"|subs_code3 == "A047"|subs_code4 == "A047"|
           subs_code5 == "A047"|subs_code6 == "A047"|subs_code7 == "A047"|subs_code8 == "A047"|
           subs_code9 == "A047"|subs_code10 == "A047", 1, 0))

# total ---------------------------------------------------------------

df <- df %>% 
  mutate(cdi = if_else(iv_metro == 1 | cdi_code == 1, 1, 0),
         anti_pseudo_oral = ifelse(str_detect(df$anti_pseudo_oral, "\\d+"), 1, 0),
         anti_pseudo_iv = ifelse(str_detect(df$anti_pseudo_iv, "\\d+"), 1, 0),
         anti_pseudo = ifelse(anti_pseudo_oral == 1 | anti_pseudo_iv == 1, 1, 0))

review_cdi <- df %>% filter(cdi == 1) 

factorVars <- vars <- c("cdi")
table1 <- CreateTableOne(vars = vars,
                         data = df,
                         includeNA = FALSE,
                         factorVars = factorVars,
                         strata = "anti_pseudo")
table1 <- CreateTableOne(vars = vars,
                         data = df,
                         includeNA = FALSE,
                         factorVars = factorVars)
table1 %>% 
  print()
