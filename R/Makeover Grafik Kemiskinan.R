# ============================================================
# MAKEOVER: PERSENTASE PENDUDUK MISKIN
# KAB/KOTA SULSEL 2023-2025
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- 1. Impor data ----
data <- read_excel("D:/Data Kemiskinan.xlsx")

# ---- 2. Format long (satu baris = satu wilayah-tahun) ----
data_long <- data %>%
  pivot_longer(
    cols      = starts_with("Persentase"),
    names_to  = "Tahun",
    values_to = "Persentase"
  ) %>%
  mutate(Tahun = gsub("[^0-9]", "", Tahun))   # "Persentase 2023 (%)" -> "2023"

# Pemeriksaan: harus 72 baris (24 x 3) dan tanpa NA
table(data_long$Tahun)
stopifnot(nrow(data_long) == 72, !anyNA(data_long$Persentase))

# ---- 3. Urutkan wilayah menurut nilai 2025 ----
urutan <- data %>%
  arrange(`Persentase 2025 (%)`) %>%
  pull(`Kabupaten/Kota`)

data_long <- data_long %>%
  mutate(`Kabupaten/Kota` = factor(`Kabupaten/Kota`, levels = urutan))

# ---- 4. Grafik berwarna ----
# - Warna mengikuti nilai (kuning = rendah, merah tua = tinggi)
# - Batas skala warna dikunci (4-14) agar sebanding antarpanel
# - Batang horizontal: nama wilayah terbaca tanpa dimiringkan
makeover_warna <- ggplot(data_long, aes(x = Persentase, y = `Kabupaten/Kota`)) +
  geom_col(aes(fill = Persentase), width = 0.7) +
  geom_text(
    aes(label = gsub("\\.", ",", sprintf("%.1f", Persentase))),
    hjust = -0.2, size = 2.4, color = "grey20"
  ) +
  facet_wrap(~ Tahun, nrow = 1) +
  scale_fill_gradientn(
    colors = c("#FFD166", "#F4845F", "#C0392B", "#5B1A2B"),
    limits = c(4, 14),
    name   = "Persentase (%)",
    labels = function(v) gsub("\\.", ",", v)
  ) +
  scale_x_continuous(
    limits = c(0, 15),
    breaks = seq(0, 12, 4),
    labels = function(v) paste0(v, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title    = "Persentase penduduk miskin kabupaten/kota, 2023-2025",
    subtitle = "Sulawesi Selatan, diurutkan menurut nilai 2025",
    x        = "Persentase penduduk miskin",
    y        = NULL,
    caption  = paste(
      "Sumber: BPS (Susenas).",
      "Angka adalah estimasi survei; interval kepercayaan belum ditampilkan.",
      sep = "\n")
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",              # judul rata dengan tepi kiri gambar
    plot.title          = element_text(face = "bold", size = 13),
    plot.subtitle       = element_text(color = "grey35"),
    plot.caption        = element_text(hjust = 0, color = "grey40"),
    strip.text          = element_text(face = "bold", size = 11),
    axis.text.y         = element_text(size = 8),
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank(),
    panel.spacing       = unit(1, "lines"),
    legend.position     = "bottom",
    legend.key.width    = unit(3, "cm"),
    legend.key.height   = unit(0.35, "cm")
  )

makeover_warna

# ---- 5. Simpan dari kode ----
ggsave("D:/Makeover_After_Kemiskinan_Warna.png", makeover_warna,
       width = 11, height = 7.5, dpi = 300, bg = "white")
