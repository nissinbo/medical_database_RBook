library(tidyverse)
library(data.table)
library(comorbidity)
library(wakefield)

set.seed(1234)

n <- 10

patient <- data.frame(id = 1:n) %>% 
  mutate(gender = sample(x = c("F", "M"), size = n, replace = TRUE)) %>% 
  mutate(start_date = sample(seq(as.Date("2010-01-01"), as.Date("2020/12/31"), "day"), n))


drug_row_n <- sample(n:(n * 100), 1)

drug <- data.frame(id = c(1:n, sample(1:n, drug_row_n, replace = TRUE))) %>% 
  left_join(patient %>% select(id, start_date), "id") %>% 
  mutate(date = sample(seq(as.Date("2010-01-01"), as.Date("2020/12/31"), "day"), nrow(.))) %>% 
  filter(start_date <= date) %>% 
  select(!start_date) %>% 
  arrange(id, date) %>% 
  mutate(atccode = paste0(sample(LETTERS[1:5], nrow(.), replace = TRUE), sample(rep(0:3) %>% str_pad(2, pad ="0"), nrow(.), replace = TRUE))) %>% 
  mutate(dose_amount = sample(1:90, nrow(.), replace = TRUE))


disease_row_n <- sample(n:(n * 100), 1)

disease <- data.frame(id = c(1:n, sample(1:n, disease_row_n, replace = TRUE))) %>% 
  left_join(patient %>% select(id, start_date), "id") %>% 
  mutate(date = sample(seq(as.Date("2010-01-01"), as.Date("2020/12/31"), "day"), nrow(.))) %>% 
  filter(start_date <= date) %>% 
  select(!start_date) %>% 
  arrange(id, date) %>% 
  mutate(., icd10code = sample_diag(nrow(.))) %>% 
  r_na(2)


fwrite(patient, "data/patient.csv")
fwrite(drug, "data/drug.csv")
fwrite(disease, "data/disease.csv")


# data_gh <- "https://raw.githubusercontent.com/nissinbo/medical_database_RBook/master/data/"

# patient <- read.csv(paste0(data_gh, "patient.csv"))
# drug <- read.csv(paste0(data_gh, "drug.csv"))
# disease <- read.csv(paste0(data_gh, "disease.csv"))
