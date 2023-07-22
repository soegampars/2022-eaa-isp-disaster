library("pacman")
p_load(rJava, tabulizer, ggplot2, readr, dplyr, pdftools, janitor, stringr, tidyr, tesseract, pdftools, tidyverse, magick)

setwd("D:/database/idbt/keadaanangkatankerja/extract")

dki_list <- list.files(pattern = "dki_2*.*pdf")

kodekabkot <- c("3101", "3171", "3172", "3173", "3174", "3175")

dki_2011 <- as.data.frame(extract_areas(dki_list[[1]], output = "data.frame", guess = FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  mutate_at(vars(2:12), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(2:12), as.double) %>%
  rename(kabkot = 1, lf_non1 = 2,	lf_non2 = 3, lf_sd	= 4,
         lf_smp1 = 5, lf_smp2 = 6, lf_sma1 = 7, lf_sma2 = 8,
         lf_tert11 = 9, lf_tert12 = 10,
         lf_tert2 = 11, lf = 12) %>%
    mutate(prov = 31, year = 2011, lf_non = lf_non1+lf_non2,
           lf_smp = lf_smp1+lf_smp2, lf_sma = lf_sma1+lf_sma2,
           lf_tert1 = lf_tert11+lf_tert12, lf_tert = lf_tert1+lf_tert2)

  dki_2011$kabkot_kode <- kodekabkot

dki_2012 <- as.data.frame(extract_areas(dki_list[[2]], output = "data.frame", guess = FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  mutate_at(vars(2:12), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(2:12), as.double) %>%
  rename(kabkot = 1, lf_non1 = 2,	lf_non2 = 3, lf_sd	= 4,
         lf_smp1 = 5, lf_smp2 = 6, lf_sma1 = 7, lf_sma2 = 8,
         lf_tert11 = 9, lf_tert12 = 10,
         lf_tert2 = 11, lf = 12) %>%
  mutate(prov = 31, year = 2012, lf_non = lf_non1+lf_non2,
         lf_smp = lf_smp1+lf_smp2, lf_sma = lf_sma1+lf_sma2,
         lf_tert1 = lf_tert11+lf_tert12, lf_tert = lf_tert1+lf_tert2)

dki_2012$kabkot_kode <- kodekabkot

dki_2013 <- as.data.frame(extract_areas(dki_list[[3]], output = "data.frame", guess = FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  mutate_at(vars(2:12), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(2:12), str_replace_all, "[-]", "0") %>%
  mutate_at(vars(2:12), as.double) %>%
  rename(kabkot = 1, lf_non1 = 2,	lf_non2 = 3, lf_sd	= 4,
         lf_smp1 = 5, lf_smp2 = 6, lf_sma1 = 7, lf_sma2 = 8,
         lf_tert11 = 9, lf_tert12 = 10,
         lf_tert2 = 11, lf = 12) %>%
  mutate(prov = 31, year = 2013, lf_non = lf_non1+lf_non2,
         lf_smp = lf_smp1+lf_smp2, lf_sma = lf_sma1+lf_sma2,
         lf_tert1 = lf_tert11+lf_tert12, lf_tert = lf_tert1+lf_tert2)

dki_2013$kabkot_kode <- kodekabkot

dki_2014 <- as.data.frame(extract_areas(dki_list[[4]], output = "data.frame", guess = FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  mutate_at(vars(2:12), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(2:12), str_replace_all, "[-]", "0") %>%
  mutate_at(vars(2:12), as.double) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(kabkot = 1, lf_non1 = 2,	lf_non2 = 3, lf_sd	= 4,
         lf_smp1 = 5, lf_smp2 = 6, lf_sma1 = 7, lf_sma2 = 8,
         lf_tert11 = 9, lf_tert12 = 10,
         lf_tert2 = 11, lf = 12) %>%
  mutate(prov = 31, year = 2014, lf_non = lf_non1+lf_non2,
         lf_smp = lf_smp1+lf_smp2, lf_sma = lf_sma1+lf_sma2,
         lf_tert1 = lf_tert11+lf_tert12, lf_tert = lf_tert1+lf_tert2)

dki_2014$kabkot_kode <- kodekabkot

# dki_2015 <- as.data.frame(extract_areas(dki_list[[5]], output = "data.frame", guess = FALSE))
dki_2015 <- read_csv("dki_2015_manual.csv") %>%
  mutate_at(vars(2:12), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(2:12), as.double) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(kabkot = 1, lf_non1 = 2,	lf_non2 = 3, lf_sd	= 4,
         lf_smp1 = 5, lf_smp2 = 6, lf_sma1 = 7, lf_sma2 = 8,
         lf_tert11 = 9, lf_tert12 = 10,
         lf_tert2 = 11, lf = 12) %>%
  mutate(prov = 31, year = 2015, lf_non = lf_non1+lf_non2,
         lf_smp = lf_smp1+lf_smp2, lf_sma = lf_sma1+lf_sma2,
         lf_tert1 = lf_tert11+lf_tert12, lf_tert = lf_tert1+lf_tert2)

dki_2015$kabkot_kode <- kodekabkot

dki_2017 <- as.data.frame(extract_areas(dki_list[[6]], output = "data.frame", guess = FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  mutate_at(vars(2:11), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:11), str_replace_all, "[-]", "0") %>%
  mutate_at(vars(2:11), as.double) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(kabkot = 1, lf_non1 = 2,	lf_non2 = 3, lf_sd	= 4,
         lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7,
         lf_tert11 = 8, lf_tert12 = 9,
         lf_tert2 = 10, lf = 11) %>%
  mutate(prov = 31, year = 2017, lf_non = lf_non1+lf_non2,
         lf_sma = lf_sma1+lf_sma2,lf_tert1 = lf_tert11+lf_tert12,
         lf_tert = lf_tert1+lf_tert2)

dki_2017$kabkot_kode <- kodekabkot

dki_2018 <- as.data.frame(extract_areas(dki_list[[7]], output = "data.frame", guess = FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  mutate_at(vars(2:11), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:11), str_replace_all, "[-]", "0") %>%
  mutate_at(vars(2:11), as.double) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(kabkot = 1, lf_non1 = 2,	lf_non2 = 3, lf_sd	= 4,
         lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7,
         lf_tert11 = 8, lf_tert12 = 9,
         lf_tert2 = 10, lf = 11) %>%
  mutate(prov = 31, year = 2018, lf_non = lf_non1+lf_non2,
         lf_sma = lf_sma1+lf_sma2,lf_tert1 = lf_tert11+lf_tert12,
         lf_tert = lf_tert1+lf_tert2)

dki_2018$kabkot_kode <- kodekabkot

dki_2019 <- as.data.frame(extract_areas(dki_list[[8]], output = "data.frame", guess = FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  mutate_at(vars(2:11), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:11), str_replace_all, "[-]", "0") %>%
  mutate_at(vars(2:11), as.double) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(kabkot = 1, lf_non1 = 2,	lf_non2 = 3, lf_sd	= 4,
         lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7,
         lf_tert11 = 8, lf_tert12 = 9,
         lf_tert2 = 10, lf = 11) %>%
  mutate(prov = 31, year = 2019, lf_non = lf_non1+lf_non2,
         lf_sma = lf_sma1+lf_sma2,lf_tert1 = lf_tert11+lf_tert12,
         lf_tert = lf_tert1+lf_tert2)

dki_2019$kabkot_kode <- kodekabkot

compile_dki <- bind_rows(dki_2011, dki_2012, dki_2013,
                           dki_2014, dki_2015, dki_2017,
                           dki_2018, dki_2019) %>%
  write_csv("dki_2011_2019.csv")

