# Atlas Visual Sulawesi Selatan

Kelompok 5, Komputasi Statistika Lanjut (S2 Statistika FMIPA Unhas), Semester Ganjil 2026/2027
Anggota: Ilfani Magfirli, Rizka Magfira

Kemiskinan (indikator utama) dan Tingkat Pengangguran Terbuka/TPT (indikator pendamping) di 24
kabupaten/kota Sulawesi Selatan, 2023-2025. Seluruh grafik dihasilkan dari kode.

## Struktur repositori

```
├── data/                 Data_Bersih_Gabungan_Kemiskinan_TPT_Sulsel.xlsx, geoBoundaries-IDN-ADM2.geojson
├── R/
│   ├── theme_tim.R       tema dan palet warna tim
│   ├── fungsi_grafik.R   fungsi data, grafik ({{ }}), peta, uji buta warna, 24 profil
│   ├── autoplot.R        metode S3 autoplot() untuk objek analisis_tim
│   ├── bootstrap.R       SK bootstrap 95% (menghasilkan keluaran/hasil_bootstrap.rds)
│   ├── lineup.R          lineup 20 panel (menghasilkan keluaran/lineup_20panel.png)
│   └── buat_infografis.py  skrip pembuat infografis.pdf (Python)
├── keluaran/
│   ├── profil/           24 profil kabupaten/kota
│   ├── hasil_bootstrap.rds, hasil_lineup.rds
│   └── lineup_20panel.png, makeover_before.jpg, makeover_after.png
├── atlas.qmd             sumber atlas (dirender ke HTML)
├── infografis.pdf        infografis kebijakan 1 halaman
└── README.md
```

## Prasyarat

- R dan [Quarto](https://quarto.org) terpasang.
- Paket R (sekali saja):
  ```r
  install.packages(c("tidyverse", "readxl", "patchwork", "sf", "colorspace", "nullabor"))
  ```
- Hanya untuk membuat ulang `infografis.pdf`: Python 3 dengan paket `pandas`, `numpy`, `matplotlib`, `openpyxl`.

## Cara merender ulang

Jalankan dari folder proyek (buka folder ini sebagai RStudio Project).

1. Render atlas:
   ```
   quarto render atlas.qmd
   ```
   Hasilnya `atlas.html` (satu berkas, gambar sudah tertanam).
2. Buat 24 profil kabupaten/kota dengan satu perintah:
   ```r
   source("R/theme_tim.R"); source("R/fungsi_grafik.R")
   dat <- muat_data(); peta <- muat_peta(dat, 2025)
   buat_semua_profil(dat, peta, tahun_acuan = 2025)
   ```
   Hasilnya 24 gambar di `keluaran/profil/`.
3. Buat ulang lineup (opsional): `source("R/lineup.R")`.
4. Buat ulang infografis (opsional): `python R/buat_infografis.py .`

## Demo: ubah satu argumen

- **Tahun:** ubah `tahun_acuan` di chunk `setup` pada `atlas.qmd` (atau argumen `tahun_acuan` pada
  `buat_semua_profil()`), lalu render ulang.
- **Indikator:** ganti nama kolom pada fungsi, misalnya `plot_tren(dat, kemiskinan)` menjadi
  `plot_tren(dat, tpt)`.

## Fungsi utama

| Fungsi | Kegunaan |
|---|---|
| `plot_tren()`, `plot_scatter()`, `plot_sebaran()`, `plot_dumbbell()` | Grafik statistik dengan `{{ }}` |
| `peta_kor()`, `uji_butawarna()` | Peta koroplet dan simulasi deuteranopia/protanopia |
| `profil_wilayah()`, `buat_semua_profil()` | Satu profil, dan 24 profil dari satu perintah |
| `analisis_tim()`, `autoplot()` | Objek analisis S3 dan grafik SK bootstrap |

## Sumber data

BPS (kemiskinan dan TPT) dan geoBoundaries (batas wilayah ADM2).

## Catatan

- Rata-rata pada profil adalah rata-rata sederhana 24 wilayah, bukan angka resmi provinsi.
- Selang bootstrap dihitung atas tiga nilai tahunan per wilayah, sehingga mencerminkan variasi antartahun.
- Berkas kunci jawaban lineup (`KUNCI_lineup_RAHASIA.txt`) sengaja tidak diunggah ke repositori.
- Penggunaan AI dideklarasikan pada lampiran atlas.
