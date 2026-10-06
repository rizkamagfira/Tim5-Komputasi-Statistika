# ============================================================
# PROYEK KELOMPOK
# KOMPUTASI STATISTIKA
# ANALISIS KEMISKINAN DAN TPT SULAWESI SELATAN
# ============================================================


# 1. PACKAGES YANG DIGUNAKAN
library(readxl)
library(dplyr)
library(writexl)
library(sf)
library(ggplot2)

# ============================================================

# 2. IMPORT DATA
data_kemiskinan <- read_excel( "D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data Mentah Kemiskinan.xlsx")
data_kemiskinan

data_tpt <- read_excel("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data Mentah Tingkat Pengangguran Terbuka.xlsx")
data_tpt

peta <- st_read("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/geoBoundaries-IDN-ADM2_simplified.geojson")

# ============================================================

# 3. PEMBERSIHAN DATA
# Data kemiskinan
names(data_kemiskinan) <- c("Kab.Kota", "Kemiskinan.2023", "Kemiskinan.2024", "Kemiskinan.2025")

# Data TPT
names(data_tpt) <- c("Kab.Kota", "TPT.2023", "TPT.2024","TPT.2025")

# Membersihkan spasi pada nama wilayah
data_kemiskinan$Kab.Kota <- trimws(data_kemiskinan$Kab.Kota)
data_tpt$Kab.Kota <- trimws(data_tpt$Kab.Kota)

# ============================================================

# 4. CEK DATA
print(data_kemiskinan)
print(data_tpt)

# Cek missing value
colSums(is.na(data_kemiskinan))
colSums(is.na(data_tpt))

# ============================================================

# 5. PENGGABUNGAN DATA KEMISKINAN DAN TPT
data_gabungan <- left_join(data_kemiskinan, data_tpt, by = "Kab.Kota")
print(data_gabungan)

# ============================================================

# 6. MEMILIH WILAYAH SULAWESI SELATAN
peta_sulsel <- peta %>% filter(shapeName %in% data_gabungan$Kab.Kota)
print(peta_sulsel)

# Mengecek jumlah wilayah
nrow(peta_sulsel)

# ============================================================

# 7. MEMBUAT KODE WILAYAH
peta_sulsel <- peta_sulsel %>% mutate(Kode_Wilayah = case_when(
  tolower(trimws(shapeName)) == "kepulauan selayar" ~ "7301",
  tolower(trimws(shapeName)) == "bulukumba" ~ "7302",
  tolower(trimws(shapeName)) == "bantaeng" ~ "7303",
  tolower(trimws(shapeName)) == "jeneponto" ~ "7304",
  tolower(trimws(shapeName)) == "takalar" ~ "7305",
  tolower(trimws(shapeName)) == "gowa" ~ "7306",
  tolower(trimws(shapeName)) == "sinjai" ~ "7307",
  tolower(trimws(shapeName)) == "maros" ~ "7308",
  tolower(trimws(shapeName)) == "pangkajene dan kepulauan" ~ "7309",
  tolower(trimws(shapeName)) == "barru" ~ "7310",
  tolower(trimws(shapeName)) == "bone" ~ "7311",
  tolower(trimws(shapeName)) == "soppeng" ~ "7312",
  tolower(trimws(shapeName)) == "wajo" ~ "7313",
  tolower(trimws(shapeName)) == "sidenreng rappang" ~ "7314",
  tolower(trimws(shapeName)) == "pinrang" ~ "7315",
  tolower(trimws(shapeName)) == "enrekang" ~ "7316",
  tolower(trimws(shapeName)) == "luwu" ~ "7317",
  tolower(trimws(shapeName)) == "tana toraja" ~ "7318",
  tolower(trimws(shapeName)) == "luwu utara" ~ "7322",
  tolower(trimws(shapeName)) == "luwu timur" ~ "7325",
  tolower(trimws(shapeName)) == "toraja utara" ~ "7326",
  tolower(trimws(shapeName)) == "kota makassar" ~ "7371",
  tolower(trimws(shapeName)) == "kota parepare" ~ "7372",
  tolower(trimws(shapeName)) == "kota palopo" ~ "7373",
  TRUE ~ NA_character_
  )
  )

# ============================================================

# 8. MENGGABUNGKAN BATAS PETA + DATA STATISTIK
data_peta <- peta_sulsel %>%
  select(Kode_Wilayah, Kab.Kota = shapeName, geometry) %>%
  left_join(data_gabungan, by = "Kab.Kota") %>%
  select(Kode_Wilayah, Kab.Kota, Kemiskinan.2023, Kemiskinan.2024,
         Kemiskinan.2025, TPT.2023, TPT.2024, TPT.2025, geometry)

# ============================================================

# 9. PEMERIKSAAN HASIL
data_peta %>% st_drop_geometry() %>% print()

# Cek apakah ada kode wilayah NA
data_peta %>% st_drop_geometry() %>%
  filter(is.na(Kode_Wilayah)) %>%
  select(Kode_Wilayah, Kab.Kota)

# Cek apakah ada data kemiskinan/TPT NA
data_peta %>% st_drop_geometry() %>%
  filter(is.na(Kemiskinan.2023) | is.na(Kemiskinan.2024) |
           is.na(Kemiskinan.2025) | is.na(TPT.2023) | is.na(TPT.2024) |
           is.na(TPT.2025)) %>%
  select(Kode_Wilayah, Kab.Kota, Kemiskinan.2023, Kemiskinan.2024,
         Kemiskinan.2025, TPT.2023, TPT.2024, TPT.2025)

# ============================================================

# 10. URUTKAN DATA BERDASARKAN KODE WILAYAH
data_peta <- data_peta %>% arrange(Kode_Wilayah)
print(data_peta)

# ============================================================

# 11. SIMPAN DATA BERSIH KE EXCEL
write_xlsx(data_peta %>% st_drop_geometry(), "D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.xlsx")

# ============================================================

# 12. SIMPAN DATA SPASIAL KE RDS
saveRDS(data_peta, "D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.rds")

# ============================================================

# 13. CEK FILE HASIL
file.exists("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.xlsx")

file.exists("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.rds")

# ============================================================

# 14. MEMBACA KEMBALI FILE RDS
data_bersih <- readRDS("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.rds")
print(data_bersih)

# ============================================================

# 15. PETA KEMISKINAN TAHUN 2025
peta_kemiskinan <- ggplot(data_bersih) + geom_sf(aes(fill = Kemiskinan.2025),
                                                 color = "white", linewidth = 0.4) +
  scale_fill_gradientn(colors = c("lightyellow", 
                                  "gold",
                                  "orange", 
                                  "orangered",
                                  m"red"),
                       name = "Kemiskinan (%)") +
  labs(title = "Persentase Kemiskinan Kabupaten/Kota", 
       subtitle = "Sulawesi Selatan Tahun 2025") +
  theme_minimal() + theme(plot.title = element_text(face = "bold",
                                                    size = 15),
                          plot.subtitle = element_text(size = 11),
                          legend.title = element_text(face = "bold")
                          )
print(peta_kemiskinan)

# ============================================================

# 16. PETA TPT TAHUN 2025
peta_tpt <- ggplot(data_bersih) + geom_sf(aes(fill = TPT.2025), color = "white",
                                          linewidth = 0.4) + 
  scale_fill_gradientn(colors = c("lightblue",
                                  "deepskyblue",
                                  "mediumslateblue",
                                  "purple",
                                  "darkred"),
                       name = "TPT (%)") +
  labs(title = "Tingkat Pengangguran Terbuka (TPT)",
       subtitle = "Sulawesi Selatan Tahun 2025") +
  theme_minimal() + theme(plot.title = element_text(face = "bold", 
                                                    size = 15),
                          plot.subtitle = element_text(size = 11),
                          legend.title = element_text(face = "bold")
                          )
print(peta_tpt)
