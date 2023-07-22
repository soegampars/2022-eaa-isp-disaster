library("pacman")
p_load(rJava, tabulizer, ggplot2, readr, dplyr, pdftools, janitor, stringr, tidyr, tesseract, pdftools, tidyverse, magick)

setwd("D:/database/idbt/pdrbpengeluaran/extract")


# CATEGORIZATION ----------------------------------------------------------

cat <- c("1. Konsumsi RT", "1a. Makanan, Minuman, dan Rokok",
         "1b. Pakaian dan Alas Kaki", "1c. Perumahan, Perkakas, Perlengkapan dan Penyelenggaraan RT",
         "1d. Kesehatan dan Pendidikan", "1e. Transportasi, Komunikasi, Rekreasi, dan Budaya",
         "1f. Hotel dan Restoran", "1g. Lainnya", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
         "4. PMTB", "4a. Bangunan", "4b. Non-Bangunan", "5. Perubahan Inventori",
         "6. Ekspor", "7. Impor")

cat_short <- c("1. Konsumsi RT", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
               "4. PMTB", "5. Perubahan Inventori","6. Ekspor", "7. Impor")

cat_short2 <- c("1. Konsumsi RT", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
               "4. PMTB", "5. Perubahan Inventori","6x. Net Ekspor")

cat20 <- c("1. Konsumsi RT", "1a. Makanan dan Minuman Non Beralkohol",
           "1b. Minuman Beralkohol dan Rokok",
           "1c. Pakaian",
           "1d. Perumahan, Air, Listrik, Gas dan Bahan Bakar Lainnya",
           "1e. Perabot, Peralatan rumah tangga dan Pemeliharaan Rutin Rumah",
           "1f. Kesehatan",
           "1g. Transportasi/Angkutan",
           "1h. Komunikasi",
           "1i. Rekreasi dan Budaya",
           "1j. Pendidikan",
           "1k. Penginapan dan Hotel",
           "1.l. Barang Pribadi dan Jasa Perorangan",
           "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
           "4. PMTB", "4a. Bangunan", "4b. Non-Bangunan", "5. Perubahan Inventori",
           "6x. Net Ekspor")

cat21 <- c("1. Konsumsi RT", "1a. Makanan dan Minuman Non Beralkohol",
           "1b. Minuman Beralkohol dan Rokok",
           "1c. Pakaian",
           "1d. Perumahan, Air, Listrik, Gas dan Bahan Bakar Lainnya",
           "1e. Perabot, Peralatan rumah tangga dan Pemeliharaan Rutin Rumah",
           "1f. Kesehatan",
           "1g. Transportasi/Angkutan",
           "1h. Komunikasi",
           "1i. Rekreasi dan Budaya",
           "1j. Pendidikan",
           "1k. Penginapan dan Hotel",
           "1.l. Barang Pribadi dan Jasa Perorangan",
           "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
           "4. PMTB", "4a. Bangunan", "4b. Non-Bangunan", "5. Perubahan Inventori",
           "6. Ekspor", "7. Impor")

cat23 <- c("1. Konsumsi RT",
           "1a. Makanan, Minuman, dan Rokok",
           "1b. Pakaian dan Alas Kaki",
           "1c. Perumahan, Perkakas, Perlengkapan dan Penyelenggaraan RT",
           "1d. Kesehatan dan Pendidikan",
           "1e. Transportasi, Komunikasi, Rekreasi, dan Budaya",
           "1f. Hotel dan Restoran",
           "1g. Lainnya",
           "2. Konsumsi LNPRT",
           "3. Konsumsi Pemerintah",
           "3a. Konsumsi Kolektif",
           "3b. Konsumsi Individu",
           "4. PMTB",
           "4a. Bangunan",
           "4b. Non-Bangunan",
           "5. Perubahan Inventori",
           "6. Ekspor",
           "6a. Ekspor Barang",
           "6b. Ekspor Jasa",
           "7. Impor",
           "7a. Impor Barang",
           "7b. Impor Jasa",
           "6x. Net Ekspor")

cat26 <- c("1. Konsumsi RT",
           "1a. Makanan, Minuman, dan Rokok",
           "1b. Pakaian dan Alas Kaki",
           "1c. Perumahan, Perkakas, Perlengkapan dan Penyelenggaraan RT",
           "1d. Kesehatan dan Pendidikan",
           "1e. Transportasi, Komunikasi, Rekreasi, dan Budaya",
           "1f. Hotel dan Restoran",
           "1g. Lainnya",
           "2. Konsumsi LNPRT",
           "3. Konsumsi Pemerintah",
           "3a. Konsumsi Kolektif",
           "3b. Konsumsi Individu",
           "4. PMTB",
           "4a. Bangunan",
           "4b. Mesin dan Perlengkapan",
           "4c. Kendaraan",
           "4d. Peralatan Lainnya",
           "4e. CBR",
           "4f. Produk Kekayaan Intelektual",
           "5. Perubahan Inventori",
           "6. Ekspor",
           "6a. Ekspor Barang",
           "6b. Ekspor Jasa",
           "7. Impor",
           "7a. Impor Barang",
           "7b. Impor Jasa")

cat30 <- c("1. Konsumsi RT",
           "1a. Makanan, Minuman, dan Rokok",
           "1b. Pakaian dan Alas Kaki",
           "1c. Perumahan, Perkakas, Perlengkapan dan Penyelenggaraan RT",
           "1d. Kesehatan dan Pendidikan",
           "1e. Transportasi, Komunikasi, Rekreasi, dan Budaya",
           "1f. Hotel dan Restoran",
           "1g. Lainnya",
           "2. Konsumsi LNPRT",
           "3. Konsumsi Pemerintah",
           "3a. Konsumsi Kolektif",
           "3b. Konsumsi Individu",
           "4. PMTB",
           "4a. Bangunan",
           "4b. Mesin dan Perlengkapan",
           "4c. Kendaraan",
           "4d. Peralatan Lainnya",
           "4e. CBR",
           "4f. Produk Kekayaan Intelektual",
           "5. Perubahan Inventori",
           "6. Ekspor",
           "6a. Ekspor Barang",
           "6aa. Ekspor Barang Non-Migas",
           "6ab. Ekspor Barang Migas",
           "6b. Ekspor Jasa",
           "7. Impor",
           "7a. Impor Barang",
           "7aa. Impor Barang Non-Migas",
           "7ab. Impor Barang Migas",
           "7b. Impor Jasa")

cat17 <- c("1. Konsumsi RT", "1a. Makanan, Minuman, dan Rokok",
           "1b. Pakaian dan Alas Kaki", "1c. Perumahan, Perkakas, Perlengkapan dan Penyelenggaraan RT",
           "1d. Kesehatan dan Pendidikan", "1e. Transportasi, Komunikasi, Rekreasi, dan Budaya",
           "1f. Hotel dan Restoran", "1g. Lainnya", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
           "4. PMTB", "4a. Bangunan", "4b. Non-Bangunan", "5. Perubahan Inventori",
           "6. Ekspor", "7. Impor", "6x. Net Ekspor")

cat16 <- c("1. Konsumsi RT", "1a. Makanan, Minuman, dan Rokok",
         "1b. Pakaian dan Alas Kaki", "1c. Perumahan, Perkakas, Perlengkapan dan Penyelenggaraan RT",
         "1d. Kesehatan dan Pendidikan", "1e. Transportasi, Komunikasi, Rekreasi, dan Budaya",
         "1f. Hotel dan Restoran", "1g. Lainnya", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
         "4. PMTB", "4a. Bangunan", "4b. Non-Bangunan", "5. Perubahan Inventori",
         "6. Ekspor", "7. Impor")

cat15 <- c("1. Konsumsi RT", "1a. Makanan, Minuman, dan Rokok",
           "1b. Pakaian dan Alas Kaki", "1c. Perumahan, Perkakas, Perlengkapan dan Penyelenggaraan RT",
           "1d. Kesehatan dan Pendidikan", "1e. Transportasi, Komunikasi, Rekreasi, dan Budaya",
           "1f. Hotel dan Restoran", "1g. Lainnya", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
           "4. PMTB", "4a. Bangunan", "4b. Non-Bangunan", "5. Perubahan Inventori",
           "6x. Net Ekspor")

cat14 <- c("1. Konsumsi RT", "1a. Makanan, Minuman, dan Rokok",
           "1b. Pakaian dan Alas Kaki", "1c. Perumahan, Perkakas, Perlengkapan dan Penyelenggaraan RT",
           "1d. Kesehatan dan Pendidikan", "1e. Transportasi, Komunikasi, Rekreasi, dan Budaya",
           "1f. Hotel dan Restoran", "1g. Lainnya", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
           "4. PMTB", "5. Perubahan Inventori", "6. Ekspor", "7. Impor")

cat13 <- c("1. Konsumsi RT", "1a. Makanan, Minuman, dan Rokok",
           "1b. Pakaian dan Alas Kaki", "1c. Perumahan, Perkakas, Perlengkapan dan Penyelenggaraan RT",
           "1d. Kesehatan dan Pendidikan", "1e. Transportasi, Komunikasi, Rekreasi, dan Budaya",
           "1f. Hotel dan Restoran", "1g. Lainnya", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
           "4. PMTB", "5. Perubahan Inventori", "6x. Net Ekspor")

cat10_lumajang <- c("1. Konsumsi RT", "1a. Makanan, Minuman, dan Rokok",
                    "1ax. Non-Makanan","2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
                    "4. PMTB", "5. Perubahan Inventori", "6x. Net Ekspor", "6. Ekspor", "7. Impor")

cat8 <- c("1. Konsumsi RT", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
          "4. PMTB", "5. Perubahan Inventori","6. Ekspor", "7. Impor", "6x. Net Ekspor")

cat7 <- c("1. Konsumsi RT", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
               "4. PMTB", "5. Perubahan Inventori","6. Ekspor", "7. Impor")

cat6 <- c("1. Konsumsi RT", "2. Konsumsi LNPRT", "3. Konsumsi Pemerintah",
                "4. PMTB", "5. Perubahan Inventori","6x. Net Ekspor")

# 34 - D.I. YOGYAKARTA ----------------------------------------------------

list_diy <- list.files(pattern = "pdrbexp_1620_340*.*pdf")

grdp_3401 <- as.data.frame(extract_areas(list_diy[[1]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[, ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3401)

  grdp_3401$cat <- cat

grdp_3402 <- as.data.frame(extract_areas(list_diy[[2]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3402)

  grdp_3402$cat <- cat

grdp_3403 <- as.data.frame(extract_areas(list_diy[[3]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[ ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3403)

  grdp_3403$cat <- cat

grdp_3404 <- read_csv("pdrbexp_1620_3404.csv") %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3404)

  grdp_3404$cat <- cat

grdp_3471 <- as.data.frame(extract_areas(list_diy[[5]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000,kabkot_kode = 3471)

grdp_3471$cat <- cat

compile_diy <- bind_rows(grdp_3401, grdp_3402, grdp_3403, grdp_3404, grdp_3471) %>%
  select(kabkot_kode, cat, grdp2016, grdp2017, grdp2018, grdp2019, grdp2020) %>%
  write_csv("compiled/compile_34.csv")

# 36 - BANTEN -------------------------------------------------------------

list_banten <- list.files(pattern = "pdrbexp_1620_360*.*pdf")

grdp_3601 <- as.data.frame(extract_areas(list_banten[[1]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3601)

  grdp_3601$cat <- cat_short

grdp_3602 <- as.data.frame(extract_areas(list_banten[[2]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3602)

  grdp_3602$cat <- cat_short

# png_3603 <- pdf_convert(list_banten[[3]], dpi=600)
# num_only <- tesseract::tesseract(
#   options = list(tessedit_char_whitelist = c(".0123456789 "))
# )
# text <- ocr(png_3603, engine = num_only) %>%
#   str_split(pattern = "\n") %>%
#   unlist() %>%
#   tibble(data = .)
#
# png_3603 <- image_read("pdrbexp_1620_3603_1.png")
# png_3603 %>%
#   image_quantize(colorspace = "gray") %>%
#   image_transparent(color = "white", fuzz=20) %>%
#   image_background("white") %>%
#   image_negate() %>%
#   image_ggplot()
#
# ocr_3603 <- png_3603 %>%
#   image_ocr()
#
# raw_tibble <- ocr_3603 %>%
#   str_split(pattern = "\n") %>%
#   unlist() %>%
#   tibble(data = .) %>%
#   write_csv("3603_text.csv")

grdp_3603 <- read_csv("pdrbexp_1620_3603.csv") %>%
  mutate_at(vars(1:4), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:4), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3603)

grdp_3603$cat <- cat_short

grdp_3604 <- as.data.frame(extract_areas(list_banten[[4]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3604)

  grdp_3604$cat <- cat_short

grdp_3671 <- as.data.frame(extract_areas(list_banten[[5]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3671)

grdp_3671$cat <- cat

grdp_3672 <- as.data.frame(extract_areas(list_banten[[6]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3672)

  grdp_3672$cat <- cat_short

grdp_3673 <- as.data.frame(extract_areas(list_banten[[7]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3673)

  grdp_3673$cat <- cat_short

grdp_3674 <- as.data.frame(extract_areas(list_banten[[8]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3674)

  grdp_3674$cat <- cat

compile_banten <- bind_rows(grdp_3601, grdp_3602, grdp_3603, grdp_3604,
                            grdp_3671, grdp_3672, grdp_3673, grdp_3674) %>%
    select(kabkot_kode, cat, grdp2016, grdp2017, grdp2018, grdp2019, grdp2020) %>%
    write_csv("compiled/compile_36.csv")

# 31 - DKI JAKARTA --------------------------------------------------------

  list_dki <- list.files(pattern = "pdrbexp_1620_310*.*pdf")

  # png_3101 <- image_read("pdrbexp_1620_3101_1.png")
  # png_3101 %>%
  #   image_quantize(colorspace = "gray") %>%
  #   image_transparent(color = "white", fuzz=20) %>%
  #   image_background("white") %>%
  #   image_negate() %>%
  #   image_ggplot()
  #
  # ocr_3101 <- png_3101 %>%
  #   image_ocr()
  #
  # raw_tibble <- ocr_3101 %>%
  #   str_split(pattern = "\n") %>%
  #   unlist() %>%
  #   tibble(data = .) %>%
  #   write_csv("3101_text.csv")

# grdp_3101 <- as.data.frame(extract_areas(list_dki[[1]], output = "data.frame", guess = FALSE))
grdp_3101 <- read_csv("pdrbexp_1620_3101.csv") %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3101)

  grdp_3101$cat <- cat_short2

grdp_3171 <- as.data.frame(extract_areas(list_dki[[2]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[ )]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3171)

  grdp_3171$cat <- cat_short2

grdp_3172 <- as.data.frame(extract_areas(list_dki[[3]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3172)

  grdp_3172$cat <- cat_short2

grdp_3173 <- as.data.frame(extract_areas(list_dki[[4]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016*1000, grdp2017 = grdp2017*1000, grdp2018 = grdp2018*1000,
         grdp2019 = grdp2019*1000, grdp2020 = grdp2020*1000, kabkot_kode = 3173)

  grdp_3173$cat <- cat_short2

grdp_3174 <- as.data.frame(extract_areas(list_dki[[5]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[)]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[ ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016*1000, grdp2017 = grdp2017*1000, grdp2018 = grdp2018*1000,
         grdp2019 = grdp2019*1000, grdp2020 = grdp2020*1000, kabkot_kode = 3174)

  grdp_3174$cat <- cat_short2

grdp_3175 <- as.data.frame(extract_areas(list_dki[[6]], guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3175)

  grdp_3175$cat <- cat_short2

compile_dki <- bind_rows(grdp_3101, grdp_3171, grdp_3172, grdp_3173,
                         grdp_3174, grdp_3175) %>%
    select(kabkot_kode, cat, grdp2016, grdp2017, grdp2018, grdp2019, grdp2020) %>%
    write_csv("compiled/compile_31.csv")


# 32 - JABAR --------------------------------------------------------------

list_jabar <- list.files(pattern = "pdrbexp_1620_320*.*pdf")

grdp_3201 <- as.data.frame(extract_areas(list_jabar[[1]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3201)

grdp_3201$cat <- cat15

grdp_3202 <- as.data.frame(extract_areas(list_jabar[[2]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3202)

grdp_3202$cat <- cat15

grdp_3203 <- as.data.frame(extract_areas(list_jabar[[3]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3203)

grdp_3203$cat <- cat15

grdp_3204 <- as.data.frame(extract_areas(list_jabar[[4]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3204)

grdp_3204$cat <- cat15

grdp_3205 <- as.data.frame(extract_areas(list_jabar[[5]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3205)

grdp_3205$cat <- cat15

grdp_3206 <- as.data.frame(extract_areas(list_jabar[[6]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3206)

grdp_3206$cat <- cat15

grdp_3207 <- as.data.frame(extract_areas(list_jabar[[7]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3207)

grdp_3207$cat <- cat13

grdp_3208 <- as.data.frame(extract_areas(list_jabar[[8]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3208)

grdp_3208$cat <- cat6

grdp_3209 <- as.data.frame(extract_areas(list_jabar[[9]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[. ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3209)

grdp_3209$cat <- cat15

grdp_3210 <- as.data.frame(extract_areas(list_jabar[[10]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3210)

grdp_3210$cat <- cat15

grdp_3211 <- as.data.frame(extract_areas(list_jabar[[11]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[), ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3211)

grdp_3211$cat <- cat15

grdp_3212 <- as.data.frame(extract_areas(list_jabar[[12]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3212)

grdp_3212$cat <- cat15

grdp_3213 <- as.data.frame(extract_areas(list_jabar[[13]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3213)

grdp_3213$cat <- cat15

grdp_3214 <- as.data.frame(extract_areas(list_jabar[[14]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3214)

grdp_3214$cat <- cat15

grdp_3215 <- as.data.frame(extract_areas(list_jabar[[15]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3215)

grdp_3215$cat <- cat15

# grdp_3216 <- as.data.frame(extract_areas(list_jabar[[16]], output = "data.frame", guess = FALSE)) ##FLATTENED
grdp_3216 <- read_csv("manual_3216.csv") %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3216)

grdp_3216$cat <- cat15

grdp_3217 <- as.data.frame(extract_areas(list_jabar[[17]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3217)

grdp_3217$cat <- cat15

grdp_3218 <- as.data.frame(extract_areas(list_jabar[[18]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3218)

grdp_3218$cat <- cat13

grdp_3271 <- as.data.frame(extract_areas(list_jabar[[19]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3271)

grdp_3271$cat <- cat13

grdp_3272 <- as.data.frame(extract_areas(list_jabar[[20]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3272)

grdp_3272$cat <- cat15

grdp_3273 <- as.data.frame(extract_areas(list_jabar[[21]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3273)

grdp_3273$cat <- cat15

grdp_3274 <- as.data.frame(extract_areas(list_jabar[[22]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3274)

grdp_3274$cat <- cat15

grdp_3275 <- as.data.frame(extract_areas(list_jabar[[23]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3275)

grdp_3275$cat <- cat20

grdp_3276 <- as.data.frame(extract_areas(list_jabar[[24]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3276)

grdp_3276$cat <- cat15

grdp_3277 <- as.data.frame(extract_areas(list_jabar[[25]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3277)

grdp_3277$cat <- cat15

grdp_3278 <- as.data.frame(extract_areas(list_jabar[[26]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3278)

grdp_3278$cat <- cat15

grdp_3279 <- as.data.frame(extract_areas(list_jabar[[27]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3279)

grdp_3279$cat <- cat15

dflist_jabar <- objects(pattern = "grdp_320*")

# grdp_3201, grdp_3202, grdp_3203, grdp_3204, grdp_3205, grdp_3206, grdp_3207,
# grdp_3208, grdp_3209, grdp_3210, grdp_3211, grdp_3212, grdp_3213, grdp_3214,
# grdp_3215, grdp_3216, grdp_3217, grdp_3218, grdp_3271, grdp_3272, grdp_3273,
# grdp_3274, grdp_3275, grdp_3276, grdp_3277, grdp_3278, grdp_3279

compile_jabar <- bind_rows(grdp_3201, grdp_3202, grdp_3203, grdp_3204, grdp_3205, grdp_3206, grdp_3207,
                           grdp_3208, grdp_3209, grdp_3210, grdp_3211, grdp_3212, grdp_3213, grdp_3214,
                           grdp_3215, grdp_3216, grdp_3217, grdp_3218, grdp_3271, grdp_3272, grdp_3273,
                           grdp_3274, grdp_3275, grdp_3276, grdp_3277, grdp_3278, grdp_3279) %>%
  select(kabkot_kode, cat, grdp2016, grdp2017, grdp2018, grdp2019, grdp2020) %>%
  write_csv("compiled/compile_32.csv")

# 33 - JAWA TENGAH --------------------------------------------------------

list_jateng <- list.files(pattern = "pdrbexp_1620_330*.*pdf")

grdp_3301 <- as.data.frame(extract_areas(list_jateng[[1]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3301)

grdp_3301$cat <- cat6

# grdp_3302 <- as.data.frame(extract_areas(list_jateng[[2]], output = "data.frame", guess = FALSE)) #FLATTENED
grdp_3302 <- read_csv("manual_3302.csv") %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3302)

grdp_3302$cat <- cat6

grdp_3303 <- as.data.frame(extract_areas(list_jateng[[3]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3303)

grdp_3303$cat <- cat6

grdp_3304 <- as.data.frame(extract_areas(list_jateng[[4]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3304)

grdp_3304$cat <- cat6

grdp_3305 <- as.data.frame(extract_areas(list_jateng[[5]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3305)

grdp_3305$cat <- cat17

grdp_3306 <- as.data.frame(extract_areas(list_jateng[[6]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3306)

grdp_3306$cat <- cat6

grdp_3307 <- as.data.frame(extract_areas(list_jateng[[7]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3307)

grdp_3307$cat <- cat6

grdp_3308 <- as.data.frame(extract_areas(list_jateng[[8]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3308)

grdp_3308$cat <- cat6

grdp_3309 <- as.data.frame(extract_areas(list_jateng[[9]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3309)

grdp_3309$cat <- cat6

grdp_3310 <- as.data.frame(extract_areas(list_jateng[[10]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:2), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:2), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:2), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:2), as.double) %>%
  rename(grdp2019 = 1, grdp2020 = 2) %>%
  mutate(kabkot_kode = 3310)

grdp_3310$cat <- cat17

grdp_3311 <- as.data.frame(extract_areas(list_jateng[[11]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3311)

grdp_3311$cat <- cat15

grdp_3312 <- as.data.frame(extract_areas(list_jateng[[12]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3312)

grdp_3312$cat <- cat6

#3313 ada kemungkinan kesalahan satuan (DECLARED: juta Rupiah; EXISTING: as Miliar rupiah)
grdp_3313 <- as.data.frame(extract_areas(list_jateng[[13]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3313)

grdp_3313$cat <- cat7

grdp_3314 <- as.data.frame(extract_areas(list_jateng[[14]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[), ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3314)

grdp_3314$cat <- cat14

grdp_3315 <- as.data.frame(extract_areas(list_jateng[[15]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3315)

grdp_3315$cat <- cat6

grdp_3316 <- as.data.frame(extract_areas(list_jateng[[16]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3316)

grdp_3316$cat <- cat16

grdp_3317 <- as.data.frame(extract_areas(list_jateng[[17]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3317)

grdp_3317$cat <- cat6

grdp_3318 <- as.data.frame(extract_areas(list_jateng[[18]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3318)

grdp_3318$cat <- cat6

grdp_3319 <- as.data.frame(extract_areas(list_jateng[[19]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3319)

grdp_3319$cat <- cat13

grdp_3320 <- as.data.frame(extract_areas(list_jateng[[20]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:2), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:2), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:2), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:2), as.double) %>%
  rename(grdp2019 = 1, grdp2020 = 2) %>%
  mutate(kabkot_kode = 3320)

grdp_3320$cat <- cat6

grdp_3321 <- as.data.frame(extract_areas(list_jateng[[21]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3321)

grdp_3321$cat <- cat6

grdp_3322 <- as.data.frame(extract_areas(list_jateng[[22]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3322)

grdp_3322$cat <- cat6

grdp_3323 <- as.data.frame(extract_areas(list_jateng[[23]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3323)

grdp_3323$cat <- cat6

grdp_3324 <- as.data.frame(extract_areas(list_jateng[[24]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[), ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3324)

grdp_3324$cat <- cat13

grdp_3325 <- as.data.frame(extract_areas(list_jateng[[25]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3325)

grdp_3325$cat <- cat6

grdp_3326 <- as.data.frame(extract_areas(list_jateng[[26]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3326)

grdp_3326$cat <- cat6

grdp_3327 <- as.data.frame(extract_areas(list_jateng[[27]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3327)

grdp_3327$cat <- cat6

grdp_3328 <- as.data.frame(extract_areas(list_jateng[[28]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3328)

grdp_3328$cat <- cat8

grdp_3329 <- as.data.frame(extract_areas(list_jateng[[29]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3329)

grdp_3329$cat <- cat6

grdp_3371 <- as.data.frame(extract_areas(list_jateng[[30]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3371)

grdp_3371$cat <- cat6

grdp_3372 <- as.data.frame(extract_areas(list_jateng[[31]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3372)

grdp_3372$cat <- cat7

grdp_3373 <- as.data.frame(extract_areas(list_jateng[[32]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3373)

grdp_3373$cat <- cat6

grdp_3374 <- as.data.frame(extract_areas(list_jateng[[33]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3374)

grdp_3374$cat <- cat6

grdp_3375 <- as.data.frame(extract_areas(list_jateng[[34]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[), ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3375)

grdp_3375$cat <- cat6

grdp_3376 <- as.data.frame(extract_areas(list_jateng[[35]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3376)

grdp_3376$cat <- cat6


dflist_jateng <- objects(pattern = "grdp_330*")

# grdp_3301, grdp_3302, grdp_3303, grdp_3304, grdp_3305, grdp_3306, grdp_3307,
# grdp_3308, grdp_3309, grdp_3310, grdp_3311, grdp_3312, grdp_3313, grdp_3314,
# grdp_3315, grdp_3316, grdp_3317, grdp_3318, grdp_3319, grdp_3320, grdp_3321,
# grdp_3322, grdp_3323, grdp_3324, grdp_3325, grdp_3326, grdp_3327, grdp_3328,
# grdp_3329, grdp_3371, grdp_3372, grdp_3373, grdp_3374, grdp_3375, grdp_3376

compile_jateng <- bind_rows(grdp_3301, grdp_3302, grdp_3303, grdp_3304, grdp_3305, grdp_3306, grdp_3307,
                            grdp_3308, grdp_3309, grdp_3310, grdp_3311, grdp_3312, grdp_3313, grdp_3314,
                            grdp_3315, grdp_3316, grdp_3317, grdp_3318, grdp_3319, grdp_3320, grdp_3321,
                            grdp_3322, grdp_3323, grdp_3324, grdp_3325, grdp_3326, grdp_3327, grdp_3328,
                            grdp_3329, grdp_3371, grdp_3372, grdp_3373, grdp_3374, grdp_3375, grdp_3376) %>%
  select(kabkot_kode, cat, grdp2016, grdp2017, grdp2018, grdp2019, grdp2020) %>%
  write_csv("compiled/compile_33.csv")

# 35 - JAWA TIMUR ---------------------------------------------------------

list_jatim <- list.files(pattern = "pdrbexp_1620_350*.*pdf")

# 3516 flattened
# 3527 flattened

grdp_3501 <- as.data.frame(extract_areas(list_jatim[[1]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3501)

grdp_3501$cat <- cat6

grdp_3502 <- as.data.frame(extract_areas(list_jatim[[2]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3502)

grdp_3502$cat <- cat15

grdp_3503 <- as.data.frame(extract_areas(list_jatim[[3]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3503)

grdp_3503$cat <- cat6

grdp_3504 <- as.data.frame(extract_areas(list_jatim[[4]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3504)

grdp_3504$cat <- cat6

grdp_3505 <- as.data.frame(extract_areas(list_jatim[[5]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[), ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3505)

grdp_3505$cat <- cat15

grdp_3506 <- as.data.frame(extract_areas(list_jatim[[6]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3506)

grdp_3506$cat <- cat15

grdp_3507 <- as.data.frame(extract_areas(list_jatim[[7]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3507)

grdp_3507$cat <- cat15

grdp_3508 <- as.data.frame(extract_areas(list_jatim[[8]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[), ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3508)

grdp_3508$cat <- cat10_lumajang

grdp_3509 <- as.data.frame(extract_areas(list_jatim[[9]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[), ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3509)

grdp_3509$cat <- cat6

grdp_3510 <- as.data.frame(extract_areas(list_jatim[[10]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3510)

grdp_3510$cat <- cat13

grdp_3511 <- as.data.frame(extract_areas(list_jatim[[11]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[), ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3511)

grdp_3511$cat <- cat6

grdp_3512 <- as.data.frame(extract_areas(list_jatim[[12]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3512)

grdp_3512$cat <- cat6

grdp_3513 <- as.data.frame(extract_areas(list_jatim[[13]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3513)

grdp_3513$cat <- cat15

grdp_3514 <- as.data.frame(extract_areas(list_jatim[[14]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3514)

grdp_3514$cat <- cat16

grdp_3515 <- as.data.frame(extract_areas(list_jatim[[15]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3515)

grdp_3515$cat <- cat15

# grdp_3516 <- as.data.frame(extract_areas(list_jatim[[16]], output = "data.frame", guess = FALSE)) #Flattened
grdp_3516 <- read_csv("manual_3516.csv") %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3516)

grdp_3516$cat <- cat7

grdp_3517 <- as.data.frame(extract_areas(list_jatim[[17]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), as.character) %>%
  mutate_at(vars(1:5), str_replace_all, "[.]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3517)

grdp_3517$cat <- cat7

grdp_3518 <- as.data.frame(extract_areas(list_jatim[[18]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3518)

grdp_3518$cat <- cat21

grdp_3519 <- as.data.frame(extract_areas(list_jatim[[19]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3519)

grdp_3519$cat <- cat16

grdp_3520 <- as.data.frame(extract_areas(list_jatim[[20]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3520)

grdp_3520$cat <- cat6

grdp_3521 <- as.data.frame(extract_areas(list_jatim[[21]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3521)

grdp_3521$cat <- cat13

grdp_3522 <- as.data.frame(extract_areas(list_jatim[[22]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3522)

grdp_3522$cat <- cat6

grdp_3523 <- as.data.frame(extract_areas(list_jatim[[23]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[), ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3523)

grdp_3523$cat <- cat6

grdp_3524 <- as.data.frame(extract_areas(list_jatim[[24]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[), ]", "") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3524)

grdp_3524$cat <- cat15

grdp_3525 <- as.data.frame(extract_areas(list_jatim[[25]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3525)

grdp_3525$cat <- cat15

grdp_3526 <- as.data.frame(extract_areas(list_jatim[[26]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3526)

grdp_3526$cat <- cat15

# grdp_3527 <- as.data.frame(extract_areas(list_jatim[[27]], output = "data.frame", guess = FALSE))
grdp_3527 <- read_csv("manual_3527.csv") %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3527)

grdp_3527$cat <- cat6

grdp_3528 <- as.data.frame(extract_areas(list_jatim[[28]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3528)

grdp_3528$cat <- cat16

grdp_3529 <- as.data.frame(extract_areas(list_jatim[[29]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3529)

grdp_3529$cat <- cat16

grdp_3571 <- as.data.frame(extract_areas(list_jatim[[30]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3571)

grdp_3571$cat <- cat6

grdp_3572 <- as.data.frame(extract_areas(list_jatim[[31]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3572)

grdp_3572$cat <- cat7

grdp_3573 <- as.data.frame(extract_areas(list_jatim[[32]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3573)

grdp_3573$cat <- cat16

grdp_3574 <- as.data.frame(extract_areas(list_jatim[[33]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3574)

grdp_3574$cat <- cat17

grdp_3575 <- as.data.frame(extract_areas(list_jatim[[34]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3575)

grdp_3575$cat <- cat15

grdp_3576 <- as.data.frame(extract_areas(list_jatim[[35]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3576)

grdp_3576$cat <- cat15

grdp_3577 <- as.data.frame(extract_areas(list_jatim[[36]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3577)

grdp_3577$cat <- cat15

grdp_3578 <- as.data.frame(extract_areas(list_jatim[[37]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(kabkot_kode = 3578)

grdp_3578$cat <- cat15

grdp_3579 <- as.data.frame(extract_areas(list_jatim[[38]], output = "data.frame", guess = FALSE)) %>%
  mutate_at(vars(1:5), str_replace_all, "[(]", "-") %>%
  mutate_at(vars(1:5), str_replace_all, "[). ]", "") %>%
  mutate_at(vars(1:5), str_replace_all, "[,]", ".") %>%
  mutate_at(vars(1:5), as.double) %>%
  rename(grdp2016 = 1, grdp2017 = 2, grdp2018 = 3, grdp2019 = 4, grdp2020 = 5) %>%
  mutate(grdp2016 = grdp2016/1000, grdp2017 = grdp2017/1000, grdp2018 = grdp2018/1000,
         grdp2019 = grdp2019/1000, grdp2020 = grdp2020/1000, kabkot_kode = 3579)

grdp_3579$cat <- cat15

dflist_jatim <- objects(pattern = "grdp_350*")

# grdp_3501, grdp_3502, grdp_3503, grdp_3504, grdp_3505, grdp_3506, grdp_3507,
# grdp_3508, grdp_3509, grdp_3510, grdp_3511, grdp_3512, grdp_3513, grdp_3514,
# grdp_3515, grdp_3516, grdp_3517, grdp_3518, grdp_3519, grdp_3520, grdp_3521,
# grdp_3522, grdp_3523, grdp_3524, grdp_3525, grdp_3526, grdp_3527, grdp_3528,
# grdp_3529, grdp_3571, grdp_3572, grdp_3573, grdp_3574, grdp_3575, grdp_3576,
# grdp_3577, grdp_3578, grdp_3579

compile_jatim <- bind_rows(grdp_3501, grdp_3502, grdp_3503, grdp_3504, grdp_3505, grdp_3506, grdp_3507,
                           grdp_3508, grdp_3509, grdp_3510, grdp_3511, grdp_3512, grdp_3513, grdp_3514,
                           grdp_3515, grdp_3516, grdp_3517, grdp_3518, grdp_3519, grdp_3520, grdp_3521,
                           grdp_3522, grdp_3523, grdp_3524, grdp_3525, grdp_3526, grdp_3527, grdp_3528,
                           grdp_3529, grdp_3571, grdp_3572, grdp_3573, grdp_3574, grdp_3575, grdp_3576,
                           grdp_3577, grdp_3578, grdp_3579) %>%
  select(kabkot_kode, cat, grdp2016, grdp2017, grdp2018, grdp2019, grdp2020) %>%
  write_csv("compiled/compile_35.csv")
