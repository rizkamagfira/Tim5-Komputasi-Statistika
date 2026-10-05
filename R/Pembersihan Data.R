# PROYEK KELOMPOK
# KOMPUTASI STATISTIKA

# Package
library(readxl)
library(dplyr)
library(writexl)
library(sf)
library(ggplot2)

# ============================================================
# 1. IMPORT DATA
# ============================================================

data_kemiskinan <- read_excel(
  "D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data Mentah Kemiskinan.xlsx"
)

data_tpt <- read_excel(
  "D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data Mentah Tingkat Pengangguran Terbuka.xlsx"
)

# ============================================================
# 2. CLEANING DATA
# ============================================================

names(data_kemiskinan) <- c(
  "Kab.Kota",
  "Kemiskinan.2023",
  "Kemiskinan.2024",
  "Kemiskinan.2025"
)

names(data_tpt) <- c(
  "Kab.Kota",
  "TPT.2023",
  "TPT.2024",
  "TPT.2025"
)

# Cek missing value
colSums(is.na(data_kemiskinan))
colSums(is.na(data_tpt))

# ============================================================
# 3. GABUNGKAN DATA KEMISKINAN DAN TPT
# ============================================================

data_gabungan <- left_join(
  data_kemiskinan,
  data_tpt,
  by = "Kab.Kota"
)

# Cek hasil
data_gabungan

# ============================================================
# 4. IMPORT GEOboundaries
# ============================================================

peta <- st_read(
  "D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/geoBoundaries-IDN-ADM2_simplified.geojson"
)

# Melihat kolom geoBoundaries
names(peta)

# ============================================================
# 5. PILIH WILAYAH YANG SESUAI DENGAN DATA
# ============================================================

peta_sulsel <- peta %>%
  filter(shapeName %in% data_gabungan$Kab.Kota)

# Cek jumlah wilayah
nrow(peta_sulsel)

# ============================================================
# 6. GABUNGKAN DATA DENGAN KODE WILAYAH DAN BATAS PETA
# ============================================================

data_peta <- peta_sulsel %>%
  left_join(
    data_gabungan,
    by = c("shapeName" = "Kab.Kota")
  )

# Lihat hasil penggabungan
data_peta

# ============================================================
# 7. CEK DATA HASIL PENGGABUNGAN
# ============================================================

data_peta %>%
  st_drop_geometry() %>%
  select(
    shapeID,
    shapeName,
    Kemiskinan.2023,
    Kemiskinan.2024,
    Kemiskinan.2025,
    TPT.2023,
    TPT.2024,
    TPT.2025
  )

# Cek wilayah yang tidak memiliki data
data_peta %>%
  st_drop_geometry() %>%
  filter(is.na(Kemiskinan.2023)) %>%
  select(shapeID, shapeName)

# ============================================================
# 8. SIMPAN DATA BERSIH YANG SUDAH TERGABUNG DENGAN PETA
# ============================================================

saveRDS(
  data_peta,
  "Data_Bersih_Gabungan_Geoboundaries.rds"
)

# Simpan juga dalam Excel tanpa geometry
write_xlsx(
  data_peta %>% st_drop_geometry(),
  "Data_Bersih_Gabungan_Geoboundaries.xlsx"
)

# ============================================================
# 9. MEMBUAT PETA 
# ============================================================
peta_kemiskinan <- ggplot(data_bersih) +
  geom_sf(
    aes(fill = Kemiskinan.2025),
    color = "white",
    linewidth = 0.4
  ) +
  scale_fill_gradientn(
    colors = c(
      "lightyellow",
      "gold",
      "orange",
      "orangered",
      "red"
    ),
    name = "Kemiskinan (%)"
  ) +
  labs(
    title = "Persentase Kemiskinan Kabupaten/Kota",
    subtitle = "Sulawesi Selatan Tahun 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    legend.title = element_text(
      face = "bold"
    )
  )

peta_kemiskinan

peta_tpt <- ggplot(data_bersih) +
  geom_sf(
    aes(fill = TPT.2025),
    color = "white",
    linewidth = 0.4
  ) +
  scale_fill_gradientn(
    colors = c(
      "lightblue",
      "deepskyblue",
      "mediumslateblue",
      "purple",
      "darkred"
    ),
    name = "TPT (%)"
  ) +
  labs(
    title = "Tingkat Pengangguran Terbuka (TPT)",
    subtitle = "Sulawesi Selatan Tahun 2025"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    legend.title = element_text(
      face = "bold"
    )
  )

peta_tpt


