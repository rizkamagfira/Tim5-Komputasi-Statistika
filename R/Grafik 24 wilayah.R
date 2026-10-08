# KOMPUTASI STATISTIKA - KELOMPOK 5
# 24 PROFIL KABUPATEN/KOTA OTOMATIS (purrr::walk + ggsave)
# ===========================================================

library(tidyverse)
library(readxl)
library(patchwork)
library(sf)

# ===========================================================
# LOKASI FILE
# ===========================================================
path_data     <- "D:/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.xlsx"
path_batas    <- "D:/geoBoundaries-IDN-ADM2.geojson"
folder_profil <- "D:/Profil_Sulsel/profil"

stopifnot("File data tidak ditemukan, cek path_data"   = file.exists(path_data),
          "File batas tidak ditemukan, cek path_batas" = file.exists(path_batas))
dir.create(folder_profil, recursive = TRUE, showWarnings = FALSE)

# ===========================================================
# 1. TEMA & PALET (dari teman: bagian 1)
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
sumber <- "Sumber: BPS dan geoBoundaries, diolah Kelompok 5"

# ===========================================================
# 2. DATA (wide -> long)
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
  rename(kemiskinan = Kemiskinan, tpt = TPT)

glimpse(dat)    # cek: harus 72 baris

# Rata-rata sederhana 24 kabupaten/kota per tahun (pembanding)
rata <- dat |>
  group_by(tahun) |>
  summarise(kemiskinan = mean(kemiskinan), tpt = mean(tpt), .groups = "drop")

# ===========================================================
# 3. BATAS WILAYAH (untuk peta mini)
# ===========================================================
norm <- function(x) {
  x |> str_to_lower() |> str_remove("^kota |^kabupaten ") |> str_squish()
}

batas <- st_read(path_batas, quiet = TRUE) |> mutate(kunci = norm(shapeName))

peta <- batas |>
  inner_join(dat |> distinct(wilayah) |> mutate(kunci = norm(wilayah)),
             by = "kunci")
cat("Jumlah wilayah di peta:", nrow(peta), "(harus 24)\n")

# ===========================================================
# 4. FUNGSI PANEL TREN: wilayah vs rata-rata Sulsel ({{ }})
# ===========================================================
panel_tren <- function(d, r, y, warna, nama, label_y) {
  gab <- bind_rows(
    d |> transmute(tahun, seri = nama, nilai = {{ y }}),
    r |> transmute(tahun, seri = "Rata-rata Sulsel", nilai = {{ y }})
  )
  seri <- c(nama, "Rata-rata Sulsel")
  
  ggplot(gab, aes(tahun, nilai, colour = seri, linetype = seri)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.2) +
    scale_colour_manual(values = setNames(c(warna, "grey55"), seri), name = NULL) +
    scale_linetype_manual(values = setNames(c("solid", "dashed"), seri), name = NULL) +
    scale_x_continuous(breaks = sort(unique(gab$tahun))) +
    labs(x = NULL, y = label_y) +
    theme_tim()
}

# ===========================================================
# 5. FUNGSI PROFIL SATU WILAYAH (menyimpan 1 gambar)
#    Untuk demo: ubah `tahun_acuan` (misalnya 2024)
# ===========================================================
profil_wilayah <- function(nama, data = dat, rerata = rata,
                           peta_sf = peta, tahun_acuan = 2025,
                           folder = folder_profil) {
  
  d <- data |> filter(wilayah == nama)
  
  # Panel A dan B: tren
  gA <- panel_tren(d, rerata, kemiskinan, biru,   nama, "Kemiskinan (%)") +
    labs(title = "Tren kemiskinan")
  gB <- panel_tren(d, rerata, tpt,        oranye, nama, "TPT (%)") +
    labs(title = "Tren TPT")
  
  # Panel C: posisi di antara 24 wilayah (tahun acuan)
  pa <- data |>
    filter(tahun == tahun_acuan) |>
    mutate(sorot = wilayah == nama,
           wilayah = fct_reorder(wilayah, kemiskinan))
  
  gC <- ggplot(pa, aes(kemiskinan, wilayah, colour = sorot, size = sorot)) +
    geom_point() +
    scale_colour_manual(values = c(`FALSE` = "grey70", `TRUE` = hijau), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 2, `TRUE` = 3.5), guide = "none") +
    labs(title = paste("Posisi di antara 24 wilayah,", tahun_acuan),
         x = "Kemiskinan (%)", y = NULL) +
    theme_tim() +
    theme(axis.text.y = element_text(size = 7))
  
  # Panel D: peta mini, wilayah disorot
  gD <- ggplot(peta_sf) +
    geom_sf(aes(fill = wilayah == nama), colour = "white", linewidth = 0.2) +
    scale_fill_manual(values = c(`FALSE` = "grey85", `TRUE` = hijau), guide = "none") +
    labs(title = "Lokasi") +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 14))
  
  # Angka ringkas untuk subjudul
  k <- d$kemiskinan[d$tahun == tahun_acuan]
  t <- d$tpt[d$tahun == tahun_acuan]
  peringkat <- sum(pa$kemiskinan > k) + 1      # 1 = kemiskinan tertinggi
  
  g <- (gA | gB) / (gC | gD) +
    plot_layout(heights = c(1, 1.3)) +
    plot_annotation(
      title = nama,
      subtitle = sprintf("%d: kemiskinan %.2f%% (peringkat %d dari 24, 1 = tertinggi), TPT %.2f%%",
                         tahun_acuan, k, peringkat, t),
      caption = sumber,
      theme = theme(plot.title = element_text(face = "bold", size = 18),
                    plot.subtitle = element_text(size = 12))
    )
  
  berkas <- file.path(folder, paste0("profil_", str_replace_all(nama, " ", "_"), ".png"))
  ggsave(berkas, g, width = 12, height = 9, dpi = 150, bg = "white")
  invisible(berkas)
}

# ===========================================================
# 6. UJI SATU WILAYAH DULU (lihat hasilnya di folder profil)
# ===========================================================
profil_wilayah("Bone")

# ===========================================================
# 7. SATU PERINTAH: 24 PROFIL SEKALIGUS
# ===========================================================
walk(sort(unique(dat$wilayah)), profil_wilayah)

cat(length(list.files(folder_profil, pattern = "\\.png$")),
    "file profil tersimpan di", folder_profil, "\n")   # harus 24