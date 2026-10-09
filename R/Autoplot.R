# ==========================================
# FUNGSI AUTOPLOT DENGAN S3
# TIM 5
# ==========================================

library(readxl)
library(ggplot2)

# 1. Membaca hasil bootstrap
hasil_bootstrap <- readRDS("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Keluaran/hasil_bootstrap.rds")

# 2. Membaca hasil lineup
hasil_lineup <- readRDS("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Keluaran/hasil_lineup.rds")

# 3. Menggabungkan hasil analisis
analisis_tim <- list(bootstrap = hasil_bootstrap,
                     lineup = hasil_lineup)

# 4. Memberikan class S3
class(analisis_tim) <- "analisis_tim"

# 5. Dokumentasi fungsi autoplot
# Nama fungsi: autoplot.analisis_tim
# Tujuan: Menampilkan rata-rata kemiskinan
# dan interval kepercayaan 95% hasil bootstrap.
# Input: Objek analisis_tim.
# Output: Grafik titik dan garis interval kepercayaan.

# 6. Membuat fungsi autoplot khusus
autoplot.analisis_tim <- function(object, ...) {
  
  ggplot(
    object$bootstrap,
    aes(
      x = reorder(Wilayah, Rata_rata),
      y = Rata_rata
    )
  ) +
    geom_point(
      color = "#0072B2",
      size = 2
    ) +
    geom_errorbar(
      aes(
        ymin = Batas_bawah,
        ymax = Batas_atas
      ),
      color = "#D55E00",
      width = 0.2
    ) +
    coord_flip() +
    labs(
      title = "Hasil Bootstrap Kemiskinan",
      x = "Kabupaten/Kota",
      y = "Rata-rata Kemiskinan"
    ) +
    theme_minimal()
}

# 7. Memeriksa metode S3
print(getS3method("autoplot", 
                  "analisis_tim"))

# 8. Membuat grafik melalui autoplot
grafik_s3 <- autoplot(analisis_tim)

print(grafik_s3)

# 9. Menyimpan grafik
ggsave("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Keluaran/grafik_s3.png",
       grafik_s3,
       width = 9,
       height = 7)

# 10. Menampilkan pesan selesai
cat("Fungsi autoplot selesai dijalankan.\n")

