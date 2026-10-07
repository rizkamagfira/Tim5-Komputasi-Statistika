# KOMPUTASI STATISTIKA - KELOMPOK 5
# GALERI GRAFIK STATISTIK 
# ===========================================================

library(readxl)
library(patchwork)
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(readxl)
library(tidyr)

# ===========================================================
# LOKASI FILE
# ===========================================================
path_data  <- "D:/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.xlsx"
folder_out <- "D:/Galeri_Sulsel"     # semua gambar disimpan di sini

stopifnot("File data tidak ditemukan, cek path_data" = file.exists(path_data))
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

# ===========================================================
# 1. TEMA & PALET 
# ===========================================================
warna_tim <- c("#0072B2",
               "#D55E00",
               "#009E73")

pal_indikator <- c("Kemiskinan" = "#0072B2",
                   "TPT" = "#D55E00")

theme_tim <- function() {theme_minimal(base_family = "sans") +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 11),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          legend.text = element_text(size = 9),
          legend.title = element_text(face = "bold", size = 9),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")}

# Aturan warna: biru = Kemiskinan, oranye = TPT, hijau = sorotan
biru   <- unname(pal_indikator["Kemiskinan"])
oranye <- unname(pal_indikator["TPT"])
hijau  <- warna_tim[3]
abu    <- "grey60"
pal_tipe <- c("Kabupaten" = abu, "Kota" = hijau)
sumber   <- "Sumber: BPS, diolah Kelompok 5"

# ===========================================================
# 2. DATA: baca, rapikan (wide -> long)
# ===========================================================
data_wide <- read_excel(path_data,
                        col_types = c("text", "text", rep("numeric", 6)))

dat <- data_wide |>
  rename(kode = Kode_Wilayah, wilayah = `Kab.Kota`) |>
  pivot_longer(-c(kode, wilayah),
               names_to = c("indikator", "tahun"),
               names_sep = "\\.",
               values_to = "nilai") |>
  mutate(tahun = as.integer(tahun)) |>
  pivot_wider(names_from = indikator, values_from = nilai) |>
  rename(kemiskinan = Kemiskinan, tpt = TPT) |>
  mutate(tipe    = if_else(str_starts(kode, "737"), "Kota", "Kabupaten"),
         wilayah = str_remove(wilayah, "^Kota "))

saveRDS(dat, file.path(folder_out, "data_bersih.rds"))
glimpse(dat)    # cek: harus 72 baris; kolom kode, wilayah, tahun, kemiskinan, tpt, tipe

# ===========================================================
# GRAFIK 1. Sebaran kemiskinan per tahun (violin + boxplot + titik)
# ===========================================================
g1 <- ggplot(dat, aes(x = factor(tahun), y = kemiskinan)) +
  geom_violin(fill = biru, alpha = 0.25, colour = NA) +
  geom_boxplot(width = 0.12, outlier.shape = NA, colour = biru,
               fill = "white", linewidth = 0.5) +
  geom_jitter(aes(colour = tipe), width = 0.07, size = 2, alpha = 0.9) +
  scale_colour_manual(values = pal_tipe, name = "Tipe wilayah") +
  labs(title = "Sebaran kemiskinan bergeser turun",
       subtitle = "24 kabupaten/kota di Sulawesi Selatan, 2023-2025",
       x = "Tahun", y = "Kemiskinan (%)", caption = sumber) +
  theme_tim()

# ===========================================================
# GRAFIK 2. Tren waktu, small multiples 24 wilayah
# ===========================================================
dat_tren <- dat |>
  pivot_longer(c(kemiskinan, tpt),
               names_to = "indikator", values_to = "nilai") |>
  mutate(indikator = recode(indikator, kemiskinan = "Kemiskinan", tpt = "TPT"))

g2 <- ggplot(dat_tren, aes(x = tahun, y = nilai, colour = indikator)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.6) +
  facet_wrap(vars(wilayah), ncol = 6) +
  scale_colour_manual(values = pal_indikator, name = "Indikator") +
  scale_x_continuous(breaks = 2023:2025) +
  labs(title = "Tren kemiskinan dan TPT per kabupaten/kota",
       subtitle = "2023-2025 (satuan: %)",
       x = NULL, y = "Persen (%)", caption = sumber) +
  theme_tim() +
  theme(strip.text = element_text(face = "bold", size = 8),
        axis.text.x = element_text(size = 7))

# ===========================================================
# GRAFIK 3. Scatterplot TPT vs Kemiskinan + regresi + pita SK 95%
# Semua tahun (2023-2025), satu panel per tahun.
# ===========================================================
dat_sc <- dat |> mutate(sorot = wilayah == "Makassar")

g3 <- ggplot(dat_sc, aes(x = kemiskinan, y = tpt)) +
  geom_smooth(method = "lm", formula = y ~ x, level = 0.95,
              colour = hijau, fill = hijau, alpha = 0.18, linewidth = 1) +
  geom_smooth(data = filter(dat_sc, !sorot), method = "lm", formula = y ~ x,
              se = FALSE, colour = "grey30", linetype = "dashed",
              linewidth = 0.7) +
  geom_point(aes(colour = sorot), size = 2.4) +
  geom_text(data = filter(dat_sc, sorot), aes(label = wilayah),
            hjust = -0.15, vjust = 0.4, size = 3, fontface = "bold") +
  facet_wrap(vars(tahun)) +
  scale_colour_manual(values = c(`FALSE` = abu, `TRUE` = hijau), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  labs(title = "Kemiskinan dan TPT, 2023-2025",
       subtitle = "Hijau: regresi + pita SK 95%. Putus-putus: tanpa Makassar",
       x = "Kemiskinan (%)", y = "TPT (%)", caption = sumber) +
  theme_tim()

# ===========================================================
# GRAFIK 4 (bebas). Dumbbell: kemiskinan 2023, 2024, 2025 per wilayah
# Diurutkan berdasarkan kemiskinan 2025 (tertinggi di atas)
# ===========================================================
urutan <- dat |> filter(tahun == 2025) |> arrange(kemiskinan) |> pull(wilayah)

dat_db <- dat |>
  mutate(wilayah = factor(wilayah, levels = urutan),
         tahun   = factor(tahun))

rentang <- dat_db |>
  group_by(wilayah) |>
  summarise(mn = min(kemiskinan), mx = max(kemiskinan), .groups = "drop")

g4 <- ggplot() +
  geom_segment(data = rentang,
               aes(x = mn, xend = mx, y = wilayah, yend = wilayah),
               colour = "grey75", linewidth = 1) +
  geom_point(data = dat_db,
             aes(x = kemiskinan, y = wilayah, colour = tahun), size = 2.6) +
  scale_colour_manual(values = c("2023" = abu, "2024" = "#8FC1E3", "2025" = biru),
                      name = "Tahun") +
  labs(title = "Perubahan kemiskinan 2023-2025",
       subtitle = "Diurutkan dari kemiskinan 2025 tertinggi di atas",
       x = "Kemiskinan (%)", y = NULL, caption = sumber) +
  theme_tim() +
  theme(panel.grid.major.y = element_line(colour = "grey92"))

# ===========================================================
# GRAFIK 5. Komposisi multipanel (patchwork)
# Kolom kiri: g1 (sebaran) di atas g3 (scatter 3 tahun). Kolom kanan: g4 (dumbbell)
# ===========================================================
g5 <- ((g1 / g3) | g4) +
  plot_layout(widths = c(1.4, 1)) +
  plot_annotation(
    title = "Kemiskinan dan pengangguran di Sulawesi Selatan, 2023-2025",
    tag_levels = "A",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

# ===========================================================
# SIMPAN SEMUA GRAFIK ke D:/Galeri_Sulsel
# ===========================================================
ggsave(file.path(folder_out, "g1_sebaran.png"),   g1, width = 7,  height = 5,  dpi = 300, bg = "white")
ggsave(file.path(folder_out, "g2_tren.png"),      g2, width = 12, height = 8,  dpi = 300, bg = "white")
ggsave(file.path(folder_out, "g3_scatter.png"),   g3, width = 11, height = 5,  dpi = 300, bg = "white")
ggsave(file.path(folder_out, "g4_dumbbell.png"),  g4, width = 7,  height = 8,  dpi = 300, bg = "white")
ggsave(file.path(folder_out, "g5_patchwork.png"), g5, width = 15, height = 11, dpi = 300, bg = "white")

# Lihat di RStudio:
g1; g2; g3; g4; g5