library("pacman")
p_load(rJava, tabulizer, ggplot2, readr, dplyr, pdftools, janitor, stringr, tidyr)

setwd("D:/database/idbt/keadaanangkatankerja/tert")

# DATASET JAWA TIMUR ------------------------------------------------------

jatim_list <- list.files(pattern = "jatim-*.*pdf")

jatim_2011 <- as.data.frame(extract_areas(jatim_list[[1]], guess = FALSE)) %>%
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert11 = 8, lf_tert12 = 9, lf_tert2 = 10, lf = 11) %>%
  mutate_at(vars(2:11), str_replace_all, "[,.]", "") %>%
  mutate_at(vars(2:11), as.integer) %>%
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
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2, lf_tert = lf_tert1+lf_tert2, prov = 35, year = 2019) %>%
  mutate_at(vars(1), str_replace, "[ ]", "") %>%
  mutate_at(vars(1), str_replace_all, "[1234567890,.]", "")

jatim_2019$kabkot[1:29] <- paste0("Kabupaten ", jatim_2019$kabkot[1:29])
jatim_2019$kabkot_kode <- 3501:3538
jatim_2019$kabkot_kode[30:38] <- 3571:3579

jatim_2020 <- as.data.frame(extract_areas(jatim_list[[9]], output = "data.frame", guess=FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
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


# OTHER PROVINCES ---------------------------------------------------------

data2020 <- list.files(pattern = "*-2020.*pdf")

#Non: non1 (tidak pernah sekolah), non2(tidak lulus SD)
##SMA: sma1 (umum), sma2 (smk),
##Tersier: tert1 (diploma), tert2(univ)

banten_2020 <- as.data.frame(extract_areas(data2020[[1]], output = "data.frame", guess=FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  rename(kabkot = 1, lf_non = 2, lf_sd = 3, lf_smp = 4, lf_sma = 5, lf_tert = 6, lf = 7) %>%
  mutate_at(vars(2:7), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:7), as.integer) %>%
  mutate_at(vars(1), str_to_title) %>%
  mutate(prov = 36, year = 2020)

  banten_2020$kabkot[1:4] <- paste0("Kabupaten ", banten_2020$kabkot[1:4])
  banten_2020$kabkot_kode <- 3601:3608
  banten_2020$kabkot_kode[5:8] <- 3671:3674

diy_2020 <- as.data.frame(extract_areas(data2020[[2]], output = "data.frame", guess=FALSE)) %>%
  rename(kabkot = 1, lf_non = 2, lf_sd = 3, lf_smp = 4, lf_sma1 = 5, lf_sma2 = 6, lf_tert1 = 7, lf_tert2 = 8, lf = 9) %>%
  mutate_at(vars(2:9), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:9), as.integer) %>%
  mutate(lf_tert = lf_tert1+lf_tert2, lf_sma = lf_sma1+lf_sma2, prov = 34, year = 2020)

  diy_2020$kabkot[1:4] <- paste0("Kabupaten ", diy_2020$kabkot[1:4])
  diy_2020$kabkot_kode <- 3401:3405
  diy_2020$kabkot_kode[5] <- 3471

dki_2020 <- as.data.frame(extract_areas(data2020[[3]], output = "data.frame", guess=FALSE)) %>%
  rename(kabkot = 1, lf_non = 2, lf_sd = 3, lf_smp = 4, lf_sma1 = 5, lf_sma2 = 6, lf_tert1 = 7, lf_tert2 = 8, lf = 9) %>%
  mutate_at(vars(2:9), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:9), as.integer) %>%
  mutate(lf_tert = lf_tert1+lf_tert2, lf_sma = lf_sma1+lf_sma2, prov = 31, year = 2020) %>%
  mutate_at(vars(1), str_to_title)

  dki_2020$kabkot[1] <- paste0("Kabupaten ", dki_2020$kabkot[1])
  dki_2020$kabkot[2:6] <- paste0("Kota ", dki_2020$kabkot[2:6])
  dki_2020$kabkot_kode <- 3101
  dki_2020$kabkot_kode[2:6] <- 3171:3175

jabar_2020 <- as.data.frame(extract_areas(data2020[[4]], output = "data.frame", guess=FALSE)) %>%
  select(1,3,5,7,9,11,13,14) %>%
  rename(kabkot = 1, lf_non = 2, lf_sd = 3, lf_smp = 4, lf_sma1 = 5, lf_sma2 = 6, lf_tert = 7, lf = 8) %>%
  mutate_at(vars(2:8), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:8), as.integer) %>%
  mutate(lf_sma = lf_sma1+lf_sma2, prov = 32, year = 2020)

  jabar_2020$kabkot_kode <- 3201:3227
  jabar_2020$kabkot_kode[19:27] <- 3271:3279

jateng_2020 <- as.data.frame(extract_areas(data2020[[5]], output = "data.frame", guess=FALSE)) %>%
  rename(kabkot = 1, lf_non1 = 2, lf_non2 = 3, lf_sd = 4, lf_smp = 5, lf_sma1 = 6, lf_sma2 = 7, lf_tert1 = 8, lf_tert2 = 9, lf = 10) %>%
  mutate_at(vars(2:10), str_replace_all, "[,. ]", "") %>%
  mutate_at(vars(2:10), as.integer) %>%
  mutate(lf_non = lf_non1+lf_non2, lf_sma = lf_sma1+lf_sma2, lf_tert = lf_tert1+lf_tert2, prov = 33, year = 2020) %>%
  mutate_at(vars(1), str_replace_all, "Kab ", "Kabupaten")

  jateng_2020$kabkot_kode <- 3301:3335
  jateng_2020$kabkot_kode[30:35] <- 3371:3376

compile <- bind_rows(jatim_2011, jatim_2012, jatim_2013, jatim_2014,
                     jatim_2015, jatim_2017, jatim_2018, jatim_2019,
                     jatim_2020, jateng_2020, jabar_2020, dki_2020,
                     diy_2020, banten_2020) %>%
  select(kabkot, kabkot_kode, prov, year, lf_non1, lf_non2, lf_non, lf_sd,
         lf_smp, lf_sma1, lf_sma2, lf_sma, lf_tert11, lf_tert12, lf_tert1, lf_tert2,
         lf_tert, lf) %>%
  write_csv("20210903_quarcsupdate_lf.csv") %>%
  write_csv("idbt_district_lf_educ.csv")
