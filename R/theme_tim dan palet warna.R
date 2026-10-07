# KOMPUTASI STATISTIKA
# KELOMPOK 5
# ===========================================================

library(ggplot2)

# 1. PALET WARNA
warna_tim <- c("#0072B2",
               "#D55E00",
               "#009E73")

# ============================================================

# 2. PALET BERDASARKAN INDIKATOR
pal_indikator <- c("Kemiskinan" = "#0072B2",
                   "TPT" = "#D55E00")

# ============================================================

# 3. THEME TIM
theme_tim <- function() {theme_minimal(base_family = "sans") +
    theme(plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 11),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 9),
          legend.text = element_text(size = 9),
          legend.title = element_text(face = "bold", size = 9),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")}
