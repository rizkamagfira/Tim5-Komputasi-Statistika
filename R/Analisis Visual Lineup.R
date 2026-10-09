# ==========================================
# ANALISIS VISUAL LINEUP
# TIM 5
# ==========================================

# 1. Memanggil package
library(readxl)
library(ggplot2)

# 2. Membaca file Excel jawaban responden
jawaban <- read_excel("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Jawaban Responden.xlsx")

# 3. Memeriksa data responden
print(names(jawaban))
print(head(jawaban))

# 4. Mengambil kolom pilihan panel
# Kolom ke-3 adalah Pilihan Panel
pilihan <- as.character(jawaban[[3]])

# Mengubah jawaban menjadi angka
pilihan <- as.numeric(gsub("[^0-9]", "", pilihan))

# 5. Membaca file kunci lineup
kunci <- readLines("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Lineup_Sulsel/KUNCI_lineup_RAHASIA.txt")

# 6. Mengambil posisi panel asli dari file kunci
baris_panel <- grep("Panel data asli", kunci,
                    value = TRUE)

if (length(baris_panel) == 0) {
  stop("Panel data asli tidak ditemukan pada file kunci.")
}

pos_asli <- as.numeric(
  sub(".*:\\s*", "", baris_panel[1])
)

# 7. Menghitung jawaban benar dan salah
jumlah_pengamat <- sum(!is.na(pilihan))

jumlah_benar <- sum(pilihan == pos_asli,na.rm = TRUE)

jumlah_salah <- jumlah_pengamat - jumlah_benar

proporsi_benar <- jumlah_benar / jumlah_pengamat

# 8. Menampilkan ringkasan jawaban
cat("\n===== HASIL VISUAL LINEUP =====\n")
cat("Posisi panel asli:", pos_asli, "\n")
cat("Jumlah responden:", jumlah_pengamat, "\n")
cat("Jawaban benar:", jumlah_benar, "\n")
cat("Jawaban salah:", jumlah_salah, "\n")
cat("Proporsi benar:", proporsi_benar, "\n")

# 9. Melakukan uji binomial
if (jumlah_pengamat == 0) {
  stop("Pilihan panel belum terbaca. Periksa file Excel.")
}

uji <- binom.test(jumlah_benar,
                  jumlah_pengamat,
                  p = 1/20,
                  alternative = "greater")

cat("Nilai p:", uji$p.value, "\n")
print(uji)

# 10. Membuat tabel ringkasan
ringkasan <- data.frame(Kategori = c("Benar", "Salah"),
                        Jumlah = c(jumlah_benar, jumlah_salah))
print(ringkasan)

# 11. Membuat grafik
grafik_lineup <- ggplot(
  ringkasan,
  aes(x = Kategori, y = Jumlah, fill = Kategori)
) +
  geom_col(width = 0.6) +
  scale_fill_manual(
    values = c(
      "Benar" = "#009E73",
      "Salah" = "#D55E00"
    )
  ) +
  labs(
    title = "Hasil Pengamatan Visual Lineup",
    subtitle = paste(
      "Jawaban benar:", jumlah_benar,
      "dari", jumlah_pengamat, "responden"
    ),
    x = "Kategori Jawaban",
    y = "Jumlah Responden"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(grafik_lineup)

# 12. Menyimpan grafik
ggsave("keluaran/grafik_lineup.png",
       grafik_lineup,
       width = 7,
       height = 5)

# 13. Menyimpan tabel ringkasan
write.csv(ringkasan, "keluaran/ringkasan_lineup.csv", row.names = FALSE)

# 14. Menyimpan hasil analisis
hasil_lineup <- list(posisi_panel_asli = pos_asli, 
                     jumlah_pengamat = jumlah_pengamat,
                     jumlah_benar = jumlah_benar,
                     jumlah_salah = jumlah_salah,
                     proporsi_benar = proporsi_benar,
                     nilai_p = uji$p.value)

saveRDS(hasil_lineup, "keluaran/hasil_lineup.rds")

cat("\nAnalisis selesai. Hasil tersimpan di folder keluaran.\n")
