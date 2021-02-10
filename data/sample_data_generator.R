library(tidyverse)
library(data.table)

set.seed(1234)

n <- 10

sample_patient <- data.frame(id = 1:n) %>% 
  mutate(gender = sample(x = c("F", "M"), size = n, replace = TRUE)) %>% 
  mutate(start_date = sample(seq(as.Date("2010-01-01"), as.Date("2020/12/31"), "day"), n))


drug_row_n <- sample(n:(n * 100), 1)

sample_drug <- data.frame(id = c(1:n, sample(1:n, drug_row_n, replace = TRUE))) %>% 
  left_join(sample_patient %>% select(id, start_date), "id") %>% 
  mutate(date = sample(seq(as.Date("2010-01-01"), as.Date("2020/12/31"), "day"), nrow(.))) %>% 
  filter(start_date <= date) %>% 
  select(!start_date) %>% 
  arrange(id, date) %>% 
  mutate(atccode = paste0(sample(LETTERS[1:5], nrow(.), replace = TRUE), sample(rep(0:3) %>% str_pad(2, pad ="0"), nrow(.), replace = TRUE))) %>% 
  mutate(dose_amount = sample(1:90, nrow(.), replace = TRUE))


disease_row_n <- sample(n:(n * 100), 1)

sample_disease <- data.frame(id = c(1:n, sample(1:n, disease_row_n, replace = TRUE))) %>% 
  left_join(sample_patient %>% select(id, start_date), "id") %>% 
  mutate(date = sample(seq(as.Date("2010-01-01"), as.Date("2020/12/31"), "day"), nrow(.))) %>% 
  filter(start_date <= date) %>% 
  select(!start_date) %>% 
  arrange(id, date) %>% 
  mutate(icd10code = paste0(sample(LETTERS[1:3], nrow(.), replace = TRUE), sample(rep(0:10) %>% str_pad(2, pad ="0"), nrow(.), replace = TRUE)))


fwrite(sample_patient, "sample_patient.csv")
fwrite(sample_drug, "sample_drug.csv")
fwrite(sample_disease, "sample_disease.csv")