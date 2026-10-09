# ==================================================
# BOOTSTRAP INTERVAL KEPERCAYAAN 95%
# TIM 5
# ==================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Membaca data Excel
data <- read_excel("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.xlsx")

# 2. Melihat nama kolom
names(data)

# 3. Mengambil kolom kemiskinan
# Pastikan nama kolom wilayah dan kemiskinan sesuai hasil names(data)
data_kemiskinan <- data %>%
  select(`Kab.Kota`, starts_with("Kemiskinan"))

# 4. Mengubah data menjadi format panjang
data_long <- data_kemiskinan %>%
  pivot_longer(
    cols = starts_with("Kemiskinan"),
    names_to = "Tahun",
    values_to = "Kemiskinan"
  ) %>%
  filter(!is.na(Kemiskinan))

# 5. Menyiapkan bootstrap
set.seed(2026)
B <- 1999

hasil <- data.frame()

# 6. Menghitung bootstrap untuk setiap kabupaten/kota
for (wilayah in unique(data_long$`Kab.Kota`)) {
  
  nilai <- data_long %>%
    filter(`Kab.Kota` == wilayah) %>%
    pull(Kemiskinan)
  
  rata_rata <- mean(nilai)
  
  bootstrap <- replicate(
    B,
    mean(sample(nilai, size = length(nilai), replace = TRUE))
  )
  
  batas_bawah <- quantile(bootstrap, 0.025)
  batas_atas <- quantile(bootstrap, 0.975)
  
  hasil <- rbind(
    hasil,
    data.frame(
      Wilayah = wilayah,
      Rata_rata = rata_rata,
      Batas_bawah = as.numeric(batas_bawah),
      Batas_atas = as.numeric(batas_atas)
    )
  )
}

# 7. Melihat hasil bootstrap
print(hasil)

# 8. Menyimpan hasil
write.csv(hasil, "keluaran/hasil_bootstrap.csv", row.names = FALSE)

saveRDS(hasil, "keluaran/hasil_bootstrap.rds")

# 9. Membuat grafik interval kepercayaan
grafik_bootstrap <- ggplot(
  hasil,
  aes(x = reorder(Wilayah, Rata_rata), y = Rata_rata)
) +
  geom_point(color = "#0072B2", size = 2) +
  geom_errorbar(
    aes(ymin = Batas_bawah, ymax = Batas_atas),
    color = "#D55E00",
    width = 0.2
  ) +
  coord_flip() +
  labs(
    title = "Rata-rata Kemiskinan dan Interval Kepercayaan 95%",
    x = "Kabupaten/Kota",
    y = "Rata-rata Kemiskinan"
  ) +
  theme_minimal()

print(grafik_bootstrap)

ggsave("keluaran/grafik_bootstrap.png",
       grafik_bootstrap,
       width = 9,
       height = 7)

