# KOMPUTASI STATISTIKA - KELOMPOK 5
# PETA KOROPLET + UJI BUTA WARNA
# ===========================================================

library(tidyverse)
library(readxl)
library(patchwork)
library(sf)
library(colorspace)

# ===========================================================
# LOKASI FILE
# ===========================================================
path_data  <- "D:/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.xlsx"
path_batas <- "D:/geoBoundaries-IDN-ADM2.geojson"
folder_out <- "D:/Peta_Sulsel"

stopifnot("File data tidak ditemukan, cek path_data"   = file.exists(path_data),
          "File batas tidak ditemukan, cek path_batas" = file.exists(path_batas))
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

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

biru   <- unname(pal_indikator["Kemiskinan"])
sumber <- "Sumber: BPS dan geoBoundaries, diolah Kelompok 5"

# ===========================================================
# 2. DATA KEMISKINAN (wide -> long)
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

# ===========================================================
# 3. BATAS WILAYAH + PENGGABUNGAN LEWAT NAMA
# ===========================================================
batas <- st_read(path_batas, quiet = TRUE)

norm <- function(x) {
  x |> str_to_lower() |> str_remove("^kota |^kabupaten ") |> str_squish()
}

dat25 <- dat |> filter(tahun == 2025) |> mutate(kunci = norm(wilayah))
batas <- batas |> mutate(kunci = norm(shapeName))

# Wilayah di DATA yang tidak ketemu di batas (HARUS kosong)
cat("Wilayah yang belum cocok:\n")
print(anti_join(dat25, st_drop_geometry(batas), by = "kunci") |> select(wilayah))

peta <- batas |> inner_join(dat25, by = "kunci")
cat("Jumlah wilayah di peta:", nrow(peta), "(harus 24)\n")

# ===========================================================
# 4. FUNGSI PETA + UJI BUTA WARNA
# ===========================================================
pal7 <- colorRampPalette(c("#DCEBF7", "#0072B2"))(7)   # muda -> tua

peta_kor <- function(data, nilai, palet, judul = NULL, label = NULL) {
  ggplot(data) +
    geom_sf(aes(fill = {{ nilai }}), colour = "white", linewidth = 0.3) +
    scale_fill_gradientn(colours = palet, name = label) +
    labs(title = judul, caption = sumber) +
    theme_tim() +
    theme(axis.text = element_blank(), panel.grid = element_blank())
}

# Peta utama
p_utama <- peta_kor(peta, kemiskinan, pal7,
                    "Kemiskinan di Sulawesi Selatan, 2025", "Kemiskinan (%)")
p_utama

# Uji buta warna: normal vs deuteranopia vs protanopia
p_normal <- peta_kor(peta, kemiskinan, pal7,
                     "Penglihatan normal", "Kemiskinan (%)")
p_deutan <- peta_kor(peta, kemiskinan, deutan(pal7),
                     "Simulasi deuteranopia", "Kemiskinan (%)")
p_protan <- peta_kor(peta, kemiskinan, protan(pal7),
                     "Simulasi protanopia", "Kemiskinan (%)")

p_uji <- p_normal | p_deutan | p_protan
p_uji

# Gambar palet (normal vs deutan vs protan)
swatchplot(list(Normal = pal7, Deutan = deutan(pal7), Protan = protan(pal7)))

# ===========================================================
# 5. SIMPAN
# ===========================================================
ggsave(file.path(folder_out, "peta_kemiskinan.png"), p_utama,
       width = 7, height = 8, dpi = 200, bg = "white")
ggsave(file.path(folder_out, "peta_uji_butawarna.png"), p_uji,
       width = 15, height = 7, dpi = 200, bg = "white")