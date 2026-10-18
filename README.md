# Peatfr App

Aplikasi web (R Shiny) untuk menjalankan model prediksi risiko kebakaran gambut **Peatfr** dari awal sampai akhir: unggah data harian, periksa kualitasnya, jalankan model, lalu lihat dan unduh hasilnya.

Aplikasi ini **membungkus kode riset tanpa mengubahnya**. Semua fungsi model (imputasi, peramalan ARIMA/LSTM/GRU, dan indeks PFVI) dipanggil langsung dari repositori asli:

- Kode riset: <https://github.com/mellygsln/peatfr>
- Paper: Mahdiyasa, A. W., Melly, M., Pasaribu, U. S., Taufik, M. & Muljadi, B. P. (2025). *Peatfr: An R package to forecast tropical peatland fire risk with stochastic, machine learning, and optimisation methods.* Ecological Informatics, 92, 103532.

Status: **prototipe** untuk eksplorasi internal VeloCT.

---

## Alur aplikasi

| Tab | Isi |
| --- | --- |
| 1. Data | Unggah CSV, pemetaan kolom otomatis, grafik empat variabel, pemeriksaan kualitas data |
| 2. Jalankan model | Pilih metode imputasi, model peramalan, jumlah hari ramalan, dan R0 |
| 3. Hasil | Kelas risiko, grafik PFVI dengan pita kelas, tabel ramalan, jumlah hari per kelas |
| 4. Evaluasi model | Akurasi peramalan dibanding tebakan naif, uji Ljung-Box, parameter PFVI hasil kalibrasi |
| 5. Unduh | Hasil per hari, evaluasi model, dan temuan kualitas data dalam CSV |

## Yang ditambahkan aplikasi di luar kode riset

Supaya transparan, berikut bagian yang **bukan** berasal dari kode riset (semuanya ada di `R/pipeline.R`):

1. **Pembacaan data fleksibel.** Mendukung koma desimal (`-0,43`), pemisah `;` atau `,`, dan menebak kolom dari namanya.
2. **Pemeriksaan kualitas data.** Data kosong, nilai di luar kewajaran, dan dugaan gangguan sensor: kelembapan tanah yang menyimpang jauh dari median 21 hari sekitarnya (z-skor robust > 6) **saat muka air tanah stabil**. Hujan sungguhan menggerakkan keduanya; gangguan sensor hanya satu.
3. **Data kosong di ujung deret** diisi dengan nilai terakhir yang ada. Interpolasi bawaan Peatfr hanya bisa mengisi bagian tengah dan akan error kalau data hari terakhir kosong, situasi yang lazim pada data real-time.
4. **Opsi mengecualikan** titik yang diduga gangguan sensor sebelum imputasi.
5. **Baseline naif** (nilai terakhir diteruskan) dengan pembagian latih/uji 80/20 yang sama dengan kode riset, sebagai pembanding akurasi.
6. **Ringkasan operasional**: jumlah hari per kelas, rangkaian hari Ekstrem berturut-turut, dan jumlah hari faktor hujan aktif (> 5,1 mm).
7. **Ringkasan untuk pengambil keputusan**: lima butir dalam bahasa biasa (status risiko, lama kondisi Ekstrem, kualitas data, keandalan ramalan, batasan), ditulis otomatis dari hasil setiap run.

Kalibrasi PFVI tetap mengikuti kode riset: parameter dioptimasi ulang setiap kali dijalankan, memakai data observasi ditambah data ramalan.

---

## Menjalankan di laptop (VS Code)

### 1. Susunan folder

Taruh folder aplikasi ini **bersebelahan** dengan folder hasil clone Peatfr:

```
code/
├── peatfr/        <- hasil clone github.com/mellygsln/peatfr
└── peatfr-app/    <- folder ini
```

Aplikasi otomatis mencari kode riset di `../peatfr`. Kalau lokasinya lain, atur variabel lingkungan `PEATFR_DIR`, atau pasang paketnya dengan `remotes::install_github("mellygsln/peatfr")`.

### 2. Pasang paket (sekali saja)

Di terminal R:

```r
install.packages(c("shiny", "bslib", "ggplot2", "forecast", "zoo", "VIM"),
                 repos = "https://cloud.r-project.org")
```

### 3. Jalankan

Buka folder `peatfr-app` di VS Code, buat terminal R (Ctrl+Shift+P, **R: Create R terminal**), lalu:

```r
shiny::runApp()
```

Aplikasi terbuka di browser. Coba tombol **"Atau pakai data contoh"**, atau unggah dataset Sabangau dari paper (bit.ly/DatasetSabangau, file CSV dari Google Sheets bisa langsung diunggah).

Tahap kalibrasi PFVI butuh 30 detik sampai beberapa menit, tergantung data.

---

## Membagikan lewat shinyapps.io

Supaya Pak Ivan dan rekan lain bisa membuka aplikasi lewat browser tanpa memasang apa pun.

Folder `peatfr-riset/` berisi salinan kode riset yang dibutuhkan (lihat `peatfr-riset/SUMBER.md`), jadi aplikasi tetap jalan di server walaupun folder clone `../peatfr` tidak ikut terunggah. LSTM dan GRU tidak tersedia di versi server.

### 1. Siapkan akun

1. Daftar di <https://www.shinyapps.io>.
2. Di dashboard, buka menu akun, pilih **Tokens**, klik **Show**, lalu **Copy to clipboard**. Yang tersalin adalah satu perintah `rsconnect::setAccountInfo(...)` berisi nama akun, token, dan secret.

Token dan secret itu seperti kata sandi: jangan dibagikan atau disimpan di repo GitHub.

### 2. Hubungkan R dengan akun (sekali saja)

Di terminal R:

```r
install.packages("rsconnect", repos = "https://cloud.r-project.org")
```

Lalu tempel perintah `rsconnect::setAccountInfo(...)` yang tadi disalin, dan tekan Enter.

### 3. Unggah

Pastikan semua paket di langkah "Pasang paket" sudah terpasang di laptop, karena shinyapps.io meniru versi paket dari laptop Anda. Lalu, dari folder `peatfr-app`:

```r
rsconnect::deployApp(appName = "peatfr-app")
```

Proses pertama butuh beberapa menit karena server memasang semua paket. Setelah selesai, browser terbuka ke alamat seperti `https://<nama-akun>.shinyapps.io/peatfr-app/`. Alamat itulah yang dibagikan.

Untuk memperbarui setelah ada perubahan kode, jalankan perintah `deployApp` yang sama.

### Catatan

- Siapa pun yang punya link bisa membuka aplikasi. Data yang diunggah pengguna tidak disimpan setelah sesi ditutup, tetapi **jangan unggah data instansi yang bersifat rahasia** ke layanan publik ini.
- Paket gratis shinyapps.io membatasi jam pemakaian per bulan; cukup untuk demo dan uji coba.
- Untuk pemakaian dengan data klien, pindahkan ke server AWS milik VeloCT.

---

## Format data

Satu baris per hari. Nama kolom bebas; aplikasi menebak dan pengguna bisa mengubahnya.

| Variabel | Satuan | Catatan |
| --- | --- | --- |
| Muka air tanah | meter | **Negatif** = di bawah permukaan, misalnya `-0.45` |
| Kelembapan tanah | % | Rumus kalibrasi memakai 40% = kering, 70% = jenuh |
| Curah hujan | mm/hari | Rumus PFVI hanya menghitung hujan di atas 5,1 mm |
| Suhu udara | °C | Paper memakai suhu maksimum harian |
| Tanggal | opsional | `YYYY-MM-DD`, `DD/MM/YYYY`, atau `DD-MM-YYYY` |

Disarankan minimal 50 hari data.

---

## LSTM dan GRU

Pilihan LSTM dan GRU hanya muncul kalau TensorFlow (Python) sudah terpasang dan bisa diakses dari R lewat paket `keras3` dan `reticulate`. Aplikasi sengaja tidak memasangnya otomatis karena ukurannya besar. Kode riset sendiri akan mencoba memasangnya saat fungsi LSTM/GRU pertama kali dipanggil.

Jalur LSTM/GRU di aplikasi ini **belum diuji**, karena TensorFlow tidak tersedia di lingkungan pengembangan prototipe.

---

## Struktur

```
peatfr-app/
├── app.R              Antarmuka dan logika Shiny
├── R/
│   ├── pipeline.R     Pembacaan data, pemeriksaan kualitas, pemanggilan kode riset, ringkasan otomatis
│   └── grafik.R       Grafik ggplot2
├── peatfr-riset/      Salinan kode riset Peatfr untuk server (tanpa LSTM/GRU), lihat SUMBER.md
└── contoh/
    └── contoh_data_gambut.csv   Data sintetis 100 hari (bukan data lapangan)

Saat dijalankan, aplikasi memakai `../peatfr` (folder clone lengkap) kalau ada, dan baru memakai `peatfr-riset/` kalau tidak ada. Panel kiri tab Data menampilkan sumber yang sedang dipakai.
```

## Keterbatasan prototipe

- Satu pengguna dalam satu waktu; selama model berjalan, aplikasi menunggu sampai selesai.
- Satu lokasi per unggahan. Belum ada peta atau perbandingan antarlokasi.
- Belum ada penyimpanan riwayat hasil (basis data).
- Pemeriksaan gangguan sensor bersifat penanda, bukan koreksi otomatis.

## Pengembangan berikutnya

- Hosting untuk tim: shinyapps.io, Posit Connect, atau kontainer Docker di AWS.
- Banyak lokasi sekaligus dengan peringkat risiko antarlokasi.
- Penyimpanan hasil ke PostgreSQL atau BigQuery, lalu dashboard Looker Studio.
- Validasi terhadap titik panas (NASA FIRMS, SiPongi): tingkat deteksi, alarm palsu, dan waktu tenggang.
