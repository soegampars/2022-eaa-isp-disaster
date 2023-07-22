library("pacman")
p_load(rJava, tabulizer, ggplot2, readr, dplyr, pdftools, janitor, stringr, tidyr, tesseract, pdftools, tidyverse, magick)

setwd("D:/database/idbt/pdrbpengeluaran/extract/extension")

ext_list <- list.files(pattern = "pdrbexp*.*pdf")

grdp_3528_1014 <- as.data.frame(extract_areas(ext_list[[1]], output = "data.frame", guess = FALSE)) %>%
  select(where(~!all(is.na(.x)))) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2010 = 1, grdp2011 = 2, grdp2012 = 3, grdp2013 = 4, grdp2014 = 5) %>%
  mutate(grdp2010 = grdp2010/1000, grdp2011 = grdp2011/1000, grdp2012 = grdp2012/1000,
         grdp2013 = grdp2013/1000, grdp2014 = grdp2014/1000, kabkot_kode = 3528)

grdp_3528_1014$cat <- cat21

grdp_3528_1418 <- as.data.frame(extract_areas(ext_list[[2]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2014 = 1, grdp2015 = 2, grdp2016 = 3, grdp2017 = 4, grdp2018 = 5) %>%
  mutate(grdp2014 = grdp2014/1000, grdp2015 = grdp2015/1000, grdp2016 = grdp2016/1000,
         grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000, kabkot_kode = 3528)

grdp_3528_1418$cat <- cat7

grdp_3528_ext <-
  grdp_3528_1014 %>%
  select(!c("grdp2014")) %>%
  left_join(grdp_3528_1418, by = c("cat", "kabkot_kode")) %>%
  select(!grdp2018)

grdp_3101_ext <- read_csv("extension_3101.csv") %>%
  mutate(grdp2019 = grdp2019/1000, grdp2018 = grdp2018/1000, grdp2017 = grdp2017/1000,
         grdp2016 = grdp2016/1000, grdp2015 = grdp2015/1000, grdp2014 = grdp2014/1000, grdp2013 = grdp2013/1000,
         grdp2012 = grdp2012/1000, grdp2011 = grdp2011/1000, grdp2010 = grdp2010/1000, kabkot_kode = 3101)

  grdp_3101_ext$cat <- cat6

grdp_3526_ext <- read_csv("extension_3526.csv") %>%
  mutate(grdp2020 = grdp2020/1000, grdp2019 = grdp2019/1000, grdp2018 = grdp2018/1000, grdp2017 = grdp2017/1000,
         grdp2016 = grdp2016/1000, grdp2015 = grdp2015/1000, grdp2014 = grdp2014/1000, grdp2013 = grdp2013/1000,
         grdp2012 = grdp2012/1000, grdp2011 = grdp2011/1000, grdp2010 = grdp2010/1000, kabkot_kode = 3526)

  grdp_3526_ext$cat <- cat6

grdp_3527_ext <- read_csv("extension_3527.csv") %>%
  mutate(grdp2020 = grdp2020/1000, grdp2019 = grdp2019/1000, grdp2018 = grdp2018/1000, grdp2017 = grdp2017/1000,
         grdp2016 = grdp2016/1000, grdp2015 = grdp2015/1000, grdp2014 = grdp2014/1000, grdp2013 = grdp2013/1000,
         grdp2012 = grdp2012/1000, grdp2011 = grdp2011/1000, grdp2010 = grdp2010/1000, kabkot_kode = 3527)

  grdp_3527_ext$cat <- cat6

grdp_3529_ext <- read_csv("extension_3529.csv") %>%
  mutate(grdp2020 = grdp2020/1000, grdp2019 = grdp2019/1000, grdp2018 = grdp2018/1000, grdp2017 = grdp2017/1000,
         grdp2016 = grdp2016/1000, grdp2015 = grdp2015/1000, grdp2014 = grdp2014/1000, grdp2013 = grdp2013/1000,
         grdp2012 = grdp2012/1000, grdp2011 = grdp2011/1000, grdp2010 = grdp2010/1000, kabkot_kode = 3529)

  grdp_3529_ext$cat <- cat6

compile_extension <-
  grdp_3528_ext %>%
  bind_rows(grdp_3101_ext) %>%
  bind_rows(grdp_3526_ext) %>%
  bind_rows(grdp_3527_ext) %>%
  bind_rows(grdp_3529_ext) %>%
  select(kabkot_kode, cat, grdp2010, grdp2011, grdp2012, grdp2013, grdp2014, grdp2015) %>%
  write_csv("20210922_pdrbpengeluaran_distextension.csv")

setwd("D:/database/idbt/pdrbpengeluaran/extract/compiled")

data_31 <- read_csv("compile_31.csv")
data_32 <- read_csv("compile_32.csv")
data_33 <- read_csv("compile_33.csv")
data_34 <- read_csv("compile_34.csv")
data_35 <- read_csv("compile_35.csv")
data_36 <- read_csv("compile_36.csv")

compile_main <- data_31 %>%
  bind_rows(data_32) %>%
  bind_rows(data_33) %>%
  bind_rows(data_34) %>%
  bind_rows(data_35) %>%
  bind_rows(data_36) %>%
  left_join(compile_extension, by = c("kabkot_kode", "cat")) %>%
  select(kabkot_kode, cat, grdp2010, grdp2011, grdp2012, grdp2013, grdp2014, grdp2015, grdp2016, grdp2017, grdp2018, grdp2019, grdp2020) %>%
  write_csv("idbt_pdrbpengeluaran_kabkotjawa_20210920.csv")
