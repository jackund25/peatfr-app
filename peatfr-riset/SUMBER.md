# Sumber kode di folder ini

File di folder `R/` adalah **salinan tanpa perubahan** dari paket Peatfr:

- Repositori: <https://github.com/mellygsln/peatfr>
- Commit: `a968760d2007a26ac9dbace0085e1961c4c10028` (12 November 2025)
- Lisensi: MIT (lihat `DESCRIPTION`)
- Paper: Mahdiyasa, A. W., Melly, M., Pasaribu, U. S., Taufik, M. & Muljadi, B. P. (2025). *Peatfr: An R package to forecast tropical peatland fire risk with stochastic, machine learning, and optimisation methods.* Ecological Informatics, 92, 103532. <https://doi.org/10.1016/j.ecoinf.2025.103532>

Hak cipta kode tetap milik para penulis Peatfr.

## Yang disalin

Imputasi (`KNN`, `Linear`, `Loess`, `Spline`), peramalan ARIMA (`Auto Predict ARIMA`), dan indeks PFVI (`Fire Predict`).

Fungsi LSTM dan GRU **tidak disalin**, karena membutuhkan TensorFlow yang tidak tersedia di server shinyapps.io dan akan mencoba memasangnya otomatis. Saat dijalankan di laptop dengan folder clone lengkap di `../peatfr`, aplikasi memakai folder clone itu, termasuk LSTM dan GRU.

Fungsi `autopeatfr()` juga tidak disalin karena aplikasi memanggil tiap tahap secara terpisah.
