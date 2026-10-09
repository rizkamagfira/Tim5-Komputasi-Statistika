# ==========================================
# BONUS GGPROTO BOOTSTRAP
# TIM 5
# ==========================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Membaca data Excel
data <- read_excel("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.xlsx")

# 2. Mengambil data kemiskinan
data_kemiskinan <- data %>% select(`Kab.Kota`, starts_with("Kemiskinan"))

# 3. Mengubah data menjadi format panjang
data_long <- data_kemiskinan %>%pivot_longer(cols = starts_with("Kemiskinan"),
                                             names_to = "Tahun",
                                             values_to = "Kemiskinan") %>%
  filter(!is.na(Kemiskinan))

# 4. Menyiapkan bootstrap
set.seed(2026)

# 5. Membuat statistik bootstrap dengan ggproto
StatSKBootstrap <- ggproto("StatSKBootstrap",
  Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales,
                           B = 1999, conf = 0.95) {
    
    nilai <- data$y
    nilai <- nilai[!is.na(nilai)]
    
    if (length(nilai) == 0) {
      return(data.frame())
    }
    
    # Menghitung rata-rata
    rata_rata <- mean(nilai)
    
    # Bootstrap sebanyak B kali
    hasil_bootstrap <- replicate(
      B,
      mean(sample(
        nilai,
        size = length(nilai),
        replace = TRUE
      ))
    )
    
    # Menghitung interval kepercayaan
    alpha <- 1 - conf
    
    batas <- quantile(
      hasil_bootstrap,
      probs = c(alpha / 2, 1 - alpha / 2),
      names = FALSE
    )
    
    # Menghasilkan titik dan batas interval
    data.frame(
      x = data$x[1],
      y = rata_rata,
      ymin = batas[1],
      ymax = batas[2]
    )
  }
)

# 6. Membuat fungsi stat_sk_bootstrap()
stat_sk_bootstrap <- function(mapping = NULL,
                              data = NULL,
                              B = 1999,
                              conf = 0.95,
                              ...) {
  
  layer(
    stat = StatSKBootstrap,
    geom = "pointrange",
    data = data,
    mapping = mapping,
    position = "identity",
    params = list(
      B = B,
      conf = conf,
      ...
    )
  )
}

# 7. Membuat grafik bootstrap menggunakan ggproto
grafik_ggproto <- ggplot(
  data_long,
  aes(
    x = `Kab.Kota`,
    y = Kemiskinan
  )
) +
  stat_sk_bootstrap(
    B = 1999,
    conf = 0.95,
    color = "#0072B2"
  ) +
  coord_flip() +
  labs(
    title = "Interval Kepercayaan Bootstrap Kemiskinan",
    subtitle = "Statistik khusus ggproto, interval kepercayaan 95%",
    x = "Kabupaten/Kota",
    y = "Rata-rata Kemiskinan"
  ) +
  theme_minimal()

# 8. Menampilkan grafik
print(grafik_ggproto)

# 9. Menyimpan hasil
dir.create("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Keluaran",
           showWarnings = FALSE)

ggsave("D:/S2 UNHAS/SEMESTER 1/KOMPUTASI STATISTIKA LANJUT/Proyek Kelompok/Keluaran/grafik_ggproto.png",
       grafik_ggproto,
       width = 9,
       height = 7)

cat("Bonus ggproto selesai dijalankan.\n")
