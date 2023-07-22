library("pacman")
p_load(rJava, tabulizer, ggplot2, readr, dplyr, pdftools, janitor, stringr, tidyr)

setwd("D:/database/idbt/keadaanangkatankerja/tert")

jatim_list <- list.files(pattern = "jatim-*.*pdf")

jatim_2011 <- as.data.frame(extract_areas(jatim_list[[1]], guess = FALSE)) %>%
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert11 = 8, lf_tert12 = 9, lf_tert2 = 10, lf = 11) %>%
  mutate_at(vars(2:10), str_replace_all, "[,.]", "") %>%
  mutate_at(vars(2:10), as.integer) %>%
  drop_na() %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2,  lf_tert1 = lf_tert11+lf_tert12, lf_tert = lf_tert1+lf_tert2, prov = 35, year = 2011)

jatim_2011$kabkot[1:29] <- paste0("Kabupaten ", jatim_2011$kabkot[1:29])
jatim_2011$kabkot[30:38] <- paste0("Kota ", jatim_2011$kabkot[30:38])
jatim_2011$kabkot_kode <- 3501:3538
jatim_2011$kabkot_kode[30:38] <- 3571:3579

jatim_2012 <- as.data.frame(extract_areas(jatim_list[[2]], output = "data.frame", guess = FALSE)) %>%
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert1 = 8, lf_tert2 = 9, lf = 10) %>%
  mutate_at(vars(2:10), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:10), as.integer) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2, lf_tert = lf_tert1+lf_tert2, prov = 35, year = 2012) %>%
  mutate_at(vars(1), str_replace, "[ ]", "") %>%
  mutate_at(vars(1), str_replace_all, "[1234567890,.]", "")

jatim_2012$kabkot[1:29] <- paste0("Kabupaten ", jatim_2012$kabkot[1:29])
jatim_2012$kabkot_kode <- 3501:3538
jatim_2012$kabkot_kode[30:38] <- 3571:3579

jatim_2013 <- as.data.frame(extract_areas(jatim_list[[3]], output = "data.frame", guess = FALSE)) %>%
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert1 = 8, lf_tert2 = 9, lf = 10) %>%
  mutate_at(vars(2:10), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:10), as.integer) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2, lf_tert = lf_tert1+lf_tert2, prov = 35, year = 2013) %>%
  mutate_at(vars(1), str_replace, "[ ]", "") %>%
  mutate_at(vars(1), str_replace_all, "[1234567890,.]", "")

jatim_2013$kabkot[1:29] <- paste0("Kabupaten ", jatim_2013$kabkot[1:29])
jatim_2013$kabkot_kode <- 3501:3538
jatim_2013$kabkot_kode[30:38] <- 3571:3579

jatim_2014 <- as.data.frame(extract_areas(jatim_list[[4]], output = "data.frame", guess = FALSE)) %>%
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert1 = 8, lf_tert2 = 9, lf = 10) %>%
  mutate_at(vars(2:10), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:10), as.integer) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2, lf_tert = lf_tert1+lf_tert2, prov = 35, year = 2014) %>%
  mutate_at(vars(1), str_replace, "[ ]", "") %>%
  mutate_at(vars(1), str_replace_all, "[1234567890,.]", "")

jatim_2014$kabkot[1:29] <- paste0("Kabupaten ", jatim_2014$kabkot[1:29])
jatim_2014$kabkot_kode <- 3501:3538
jatim_2014$kabkot_kode[30:38] <- 3571:3579

jatim_2015 <- as.data.frame(extract_areas(jatim_list[[5]], output = "data.frame", guess = FALSE)) %>%
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert1 = 8, lf_tert2 = 9, lf = 10) %>%
  mutate_at(vars(2:10), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:10), as.integer) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2, lf_tert = lf_tert1+lf_tert2, prov = 35, year = 2015) %>%
  mutate_at(vars(1), str_replace, "[ ]", "") %>%
  mutate_at(vars(1), str_replace_all, "[1234567890,.]", "")

jatim_2015$kabkot[1:29] <- paste0("Kabupaten ", jatim_2015$kabkot[1:29])
jatim_2015$kabkot_kode <- 3501:3538
jatim_2015$kabkot_kode[30:38] <- 3571:3579

jatim_2017 <- as.data.frame(extract_areas(jatim_list[[6]], output = "data.frame", guess = FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert1 = 8, lf_tert2 = 9, lf = 10) %>%
  mutate_at(vars(2:10), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:10), as.integer) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2, lf_tert = lf_tert1+lf_tert2, prov = 35, year = 2017) %>%
  mutate_at(vars(1), str_replace, "[ ]", "") %>%
  mutate_at(vars(1), str_replace_all, "[1234567890,.]", "")

jatim_2017$kabkot[1:29] <- paste0("Kabupaten ", jatim_2017$kabkot[1:29])
jatim_2017$kabkot_kode <- 3501:3538
jatim_2017$kabkot_kode[30:38] <- 3571:3579

jatim_2018 <- as.data.frame(extract_areas(jatim_list[[7]], output = "data.frame", guess = FALSE)) %>%
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert1 = 8, lf_tert2 = 9, lf = 10) %>%
  mutate_at(vars(2:10), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:10), as.integer) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2, lf_tert = lf_tert1+lf_tert2, prov = 35, year = 2018) %>%
  mutate_at(vars(1), str_replace, "[ ]", "") %>%
  mutate_at(vars(1), str_replace_all, "[1234567890,.]", "")

jatim_2018$kabkot[1:29] <- paste0("Kabupaten ", jatim_2018$kabkot[1:29])
jatim_2018$kabkot_kode <- 3501:3538
jatim_2018$kabkot_kode[30:38] <- 3571:3579

jatim_2019 <- as.data.frame(extract_areas(jatim_list[[8]], output = "data.frame", guess = FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert1 = 8, lf_tert2 = 9, lf = 10) %>%
  mutate_at(vars(2:10), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:10), as.integer) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2, lf_tert = lf_tert1+lf_tert2, prov = 35, year = 2019) %>%
  mutate_at(vars(1), str_replace, "[ ]", "") %>%
  mutate_at(vars(1), str_replace_all, "[1234567890,.]", "")

jatim_2019$kabkot[1:29] <- paste0("Kabupaten ", jatim_2019$kabkot[1:29])
jatim_2019$kabkot_kode <- 3501:3538
jatim_2019$kabkot_kode[30:38] <- 3571:3579

jatim_2020 <- as.data.frame(extract_areas(jatim_list[[9]], output = "data.frame", guess=FALSE)) %>%
  select(where(~!all(is.na(.x))))
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert1 = 8, lf_tert2 = 9, lf = 10) %>%
  mutate_at(vars(2:10), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:10), as.integer) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2, lf_tert = lf_tert1+lf_tert2, prov = 35, year = 2020) %>%
  mutate_at(vars(1), str_replace, "[ ]", "") %>%
  mutate_at(vars(1), str_replace_all, "[1234567890,.]", "")

jatim_2020$kabkot[1:29] <- paste0("Kabupaten ", jatim_2020$kabkot[1:29])
jatim_2020$kabkot_kode <- 3501:3538
jatim_2020$kabkot_kode[30:38] <- 3571:3579

datasetjatim <- bind_rows(jatim2019, jatim2018, jatim2017, jatim2015, jatim2014, jatim2013, jatim2012, jatim2011) %>%
  write_csv("compiled_lf_jatim.csv")
