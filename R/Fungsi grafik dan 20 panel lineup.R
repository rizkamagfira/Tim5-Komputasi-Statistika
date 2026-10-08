# KOMPUTASI STATISTIKA - KELOMPOK 5
# HARI 7: FUNGSI GRAFIK {{ }} + LINEUP 20 PANEL
# ===========================================================

library(tidyverse)
library(readxl)
library(patchwork)
library(nullabor)

# ===========================================================
# LOKASI FILE
# ===========================================================
path_data  <- "D:/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.xlsx"
folder_out <- "D:/Lineup_Sulsel"

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
sumber <- "Sumber: BPS, diolah Kelompok 5"

# ===========================================================
# 2. DATA: baca Excel, rapikan (wide -> long)
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

glimpse(dat)    # cek: harus 72 baris; kolom kode, wilayah, tahun, kemiskinan, tpt, tipe

# ===========================================================
# 3. FUNGSI GRAFIK DENGAN {{ }}  
# ===========================================================

# --- Fungsi 1: tren waktu, satu panel per wilayah ---------------------
plot_tren <- function(data, indikator, warna = biru,
                      judul = NULL, label_y = NULL, ncol = 6) {
  ggplot(data, aes(x = tahun, y = {{ indikator }})) +
    geom_line(colour = warna, linewidth = 0.8) +
    geom_point(colour = warna, size = 1.6) +
    facet_wrap(vars(wilayah), ncol = ncol,
               labeller = labeller(wilayah = label_wrap_gen(12))) +
    scale_x_continuous(breaks = sort(unique(data$tahun))) +
    labs(title = judul, x = NULL, y = label_y, caption = sumber) +
    theme_tim() +
    theme(strip.text = element_text(face = "bold", size = 8),
          axis.text.x = element_text(size = 7))
}

# --- Fungsi 2: scatterplot + regresi + pita SK 95% --------------------
plot_scatter <- function(data, x, y, sorot = "Makassar",
                         judul = NULL, label_x = NULL, label_y = NULL) {
  data <- data |> mutate(.sorot = wilayah %in% sorot)
  ggplot(data, aes(x = {{ x }}, y = {{ y }})) +
    geom_smooth(method = "lm", formula = y ~ x, level = 0.95,
                colour = hijau, fill = hijau, alpha = 0.18, linewidth = 1) +
    geom_point(aes(colour = .sorot), size = 2.4) +
    geom_text(data = filter(data, .sorot), aes(label = wilayah),
              hjust = -0.15, vjust = 0.4, size = 3, fontface = "bold") +
    facet_wrap(vars(tahun)) +
    scale_colour_manual(values = c(`FALSE` = abu, `TRUE` = hijau), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.3))) +
    labs(title = judul, x = label_x, y = label_y, caption = sumber) +
    theme_tim()
}

# --- Fungsi 3: sebaran per tahun (violin + boxplot + titik) -----------
plot_sebaran <- function(data, indikator, warna = biru,
                         judul = NULL, label_y = NULL) {
  ggplot(data, aes(x = factor(tahun), y = {{ indikator }})) +
    geom_violin(fill = warna, alpha = 0.25, colour = NA) +
    geom_boxplot(width = 0.12, outlier.shape = NA, colour = warna,
                 fill = "white", linewidth = 0.5) +
    geom_jitter(width = 0.07, size = 2, colour = warna, alpha = 0.8) +
    labs(title = judul, x = "Tahun", y = label_y, caption = sumber) +
    theme_tim()
}

# ===========================================================
# 4. UJI FUNGSI: ganti indikator hanya dengan mengubah satu argumen
# ===========================================================
p_tren_kem <- plot_tren(dat, kemiskinan, warna = biru,
                        judul = "Tren kemiskinan per kabupaten/kota",
                        label_y = "Kemiskinan (%)")

p_tren_tpt <- plot_tren(dat, tpt, warna = oranye,
                        judul = "Tren TPT per kabupaten/kota",
                        label_y = "TPT (%)")

p_scatter  <- plot_scatter(dat, x = kemiskinan, y = tpt,
                           judul = "Kemiskinan dan TPT",
                           label_x = "Kemiskinan (%)", label_y = "TPT (%)")

p_sebaran  <- plot_sebaran(dat, tpt, warna = oranye,
                           judul = "Sebaran TPT per tahun",
                           label_y = "TPT (%)")

p_tren_kem; p_tren_tpt; p_scatter; p_sebaran

ggsave(file.path(folder_out, "tren_kemiskinan.png"), p_tren_kem, width = 12, height = 8, dpi = 200, bg = "white")
ggsave(file.path(folder_out, "tren_tpt.png"),        p_tren_tpt, width = 12, height = 8, dpi = 200, bg = "white")
ggsave(file.path(folder_out, "scatter.png"),         p_scatter,  width = 10, height = 5, dpi = 200, bg = "white")
ggsave(file.path(folder_out, "sebaran_tpt.png"),     p_sebaran,  width = 7,  height = 5, dpi = 200, bg = "white")

# ===========================================================
# 5. LINEUP 20 PANEL (1 data asli + 19 data permutasi)
# H0: tidak ada hubungan antara kemiskinan dan TPT (TPT diacak)
# ===========================================================
set.seed(2026)                      

dat25 <- dat |> filter(tahun == 2025)

pos_asli <- sample(20, 1)            # posisi panel data asli (RAHASIA)

dat_lineup <- lineup(null_permute("tpt"), dat25, n = 20, pos = pos_asli)

g_lineup <- ggplot(dat_lineup, aes(x = kemiskinan, y = tpt)) +
  geom_smooth(method = "lm", formula = y ~ x, level = 0.95,
              colour = hijau, fill = hijau, alpha = 0.18, linewidth = 0.8) +
  geom_point(aes(shape = tipe), colour = biru, size = 2, alpha = 0.9) +
  scale_shape_manual(values = c("Kabupaten" = 16, "Kota" = 15),
                     name = "Jenis wilayah") +
  facet_wrap(vars(.sample), ncol = 5) +
  labs(title = "Manakah panel yang paling berbeda dari yang lain?",
       subtitle = "Dari 20 panel ini, satu memuat data asli. Pilih nomor panel yang menurut Anda paling berbeda.",
       x = "Kemiskinan", y = "TPT") +
  theme_tim() +
  theme(axis.text  = element_blank(),     # angka sumbu disembunyikan agar tidak jadi petunjuk
        axis.ticks = element_blank(),
        panel.border     = element_rect(colour = "grey40", fill = NA, linewidth = 0.6),
        panel.spacing    = unit(0.8, "lines"),
        panel.grid       = element_blank(),
        strip.background = element_rect(fill = "grey93", colour = "grey40"),
        strip.text       = element_text(face = "bold", size = 12))

g_lineup

# Gambar lineup (INI yang dibagikan ke pengamat)
ggsave(file.path(folder_out, "lineup_20panel.png"), g_lineup,
       width = 12, height = 9, dpi = 200, bg = "white")

# KUNCI JAWABAN (jangan dibagikan ke pengamat!)
writeLines(c(paste("Panel data asli  :", pos_asli),
             "Seed            : 2026",
             "Data            : tahun 2025, 24 kabupaten/kota",
             "H0              : TPT diacak (tidak ada hubungan dengan kemiskinan)"),
           file.path(folder_out, "KUNCI_lineup_RAHASIA.txt"))

saveRDS(dat_lineup, file.path(folder_out, "data_lineup.rds"))
cat("Kunci jawaban tersimpan. Jangan dibagikan ke pengamat.\n")
