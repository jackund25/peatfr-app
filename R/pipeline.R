# =============================================================================
# Lapisan pipeline Peatfr App
#
# Prinsip: kode riset Peatfr (github.com/mellygsln/peatfr) dipanggil apa adanya,
# tidak diubah. Semua tambahan (pembacaan data, pemeriksaan kualitas, penanganan
# data kosong di ujung, baseline, ringkasan kelas) ada di file ini.
# =============================================================================

if (!exists("%||%", baseenv())) `%||%` <- function(a, b) if (is.null(a)) b else a

VARIABEL <- c(WT = "Muka air tanah (m)", SM = "Kelembapan tanah (%)",
              Rf = "Curah hujan (mm)", Temp = "Suhu udara (°C)")

# Batas kelas PFVI, sama dengan yang dipakai di autopeatfr()
KELAS_BATAS <- c(-Inf, 75, 150, 225, Inf)
KELAS_NAMA  <- c("Rendah", "Sedang", "Tinggi", "Ekstrem")

# -----------------------------------------------------------------------------
# 1. Memuat kode riset
# -----------------------------------------------------------------------------

#' Memuat fungsi Peatfr ke dalam environment tersendiri.
#' Urutan: PEATFR_DIR (bawaan "../peatfr", folder clone lengkap di laptop),
#' lalu salinan di "peatfr-riset" (dipakai saat di-deploy ke shinyapps.io),
#' lalu paket peatfr yang terpasang lewat remotes::install_github().
muat_peatfr <- function(src_dir = Sys.getenv("PEATFR_DIR", "../peatfr")) {
  env <- new.env(parent = globalenv())
  if (!dir.exists(file.path(src_dir, "R")) && dir.exists("peatfr-riset/R")) src_dir <- "peatfr-riset"
  folder_r <- file.path(src_dir, "R")
  if (dir.exists(folder_r)) {
    berkas <- list.files(folder_r, pattern = "\\.[Rr]$", full.names = TRUE)
    for (f in berkas) sys.source(f, envir = env)
    attr(env, "sumber") <- paste("Folder kode riset:", normalizePath(src_dir, winslash = "/"))
  } else if (requireNamespace(nama_paket <- "peatfr", quietly = TRUE)) {
    # Nama paket lewat variabel supaya pemindai dependensi tidak
    # mencoba memasang peatfr dari CRAN saat deploy (peatfr tidak ada di CRAN)
    ns <- asNamespace(nama_paket)
    for (n in c("linear_interpolation", "spline_interpolation", "loess_interpolation",
                "knn_imputation", "autopredictarima", "autopredictlstm",
                "autopredictgru", "firepredict")) {
      if (exists(n, envir = ns, inherits = FALSE)) assign(n, get(n, envir = ns), envir = env)
    }
    attr(env, "sumber") <- "Paket peatfr terpasang"
  } else {
    stop("Kode Peatfr tidak ditemukan. Clone github.com/mellygsln/peatfr ke folder ",
         "'../peatfr', atur PEATFR_DIR, atau pasang paketnya dengan ",
         "remotes::install_github('mellygsln/peatfr').")
  }
  env
}

#' LSTM/GRU butuh TensorFlow. Kode riset akan memasangnya otomatis kalau
#' belum ada, jadi aplikasi mengecek dulu agar tidak memicu pemasangan besar.
tersedia_ml <- function() {
  # Nama paket ditulis sebagai variabel supaya pemindai dependensi
  # (renv / rsconnect) tidak menganggapnya wajib saat deploy.
  paket <- c("keras3", "reticulate")
  if (!all(vapply(paket, requireNamespace, logical(1), quietly = TRUE))) return(FALSE)
  cek_python <- getExportedValue(paket[2], "py_module_available")
  isTRUE(tryCatch(cek_python("tensorflow"), error = function(e) FALSE))
}

# -----------------------------------------------------------------------------
# 2. Membaca data
# -----------------------------------------------------------------------------

#' Mengubah teks menjadi angka. Mendukung koma desimal ("0,43") dan titik.
ke_angka <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "-", "null", "NULL")] <- NA
  # Kalau ada koma tapi tidak ada titik, anggap koma sebagai desimal
  koma_desimal <- grepl(",", x) & !grepl("\\.", x)
  x[koma_desimal] <- gsub(",", ".", x[koma_desimal])
  # Sisa koma dianggap pemisah ribuan
  x <- gsub(",", "", x)
  suppressWarnings(as.numeric(x))
}

baca_csv <- function(path) {
  baris1 <- readLines(path, n = 1, warn = FALSE)
  sep <- if (lengths(regmatches(baris1, gregexpr(";", baris1))) >
             lengths(regmatches(baris1, gregexpr(",", baris1)))) ";" else ","
  read.csv(path, sep = sep, colClasses = "character", check.names = FALSE,
           strip.white = TRUE, fileEncoding = "UTF-8-BOM")
}

#' Menebak kolom untuk tiap variabel dari nama kolom.
tebak_kolom <- function(nama) {
  pola <- list(
    WT   = "water|^wt$|tma|muka|gwl|table",
    SM   = "soil|^sm$|lembap|moist|swc",
    Rf   = "rain|^rf$|hujan|curah|precip|^ch$",
    Temp = "temp|suhu|^t$|tmax",
    Tanggal = "date|tanggal|tgl|waktu"
  )
  sapply(pola, function(p) {
    i <- grep(p, nama, ignore.case = TRUE)
    if (length(i)) nama[i[1]] else ""
  })
}

#' Membentuk data standar: tanggal (opsional), WT, SM, Rf, Temp.
siapkan_data <- function(raw, kolom) {
  d <- data.frame(
    WT   = ke_angka(raw[[kolom[["WT"]]]]),
    SM   = ke_angka(raw[[kolom[["SM"]]]]),
    Rf   = ke_angka(raw[[kolom[["Rf"]]]]),
    Temp = ke_angka(raw[[kolom[["Temp"]]]])
  )
  tgl <- NULL
  if (nzchar(kolom[["Tanggal"]]) && kolom[["Tanggal"]] %in% names(raw)) {
    x <- raw[[kolom[["Tanggal"]]]]
    for (f in c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%m/%d/%Y", "%Y/%m/%d")) {
      cobaan <- as.Date(x, format = f)
      if (mean(!is.na(cobaan)) > 0.9) { tgl <- cobaan; break }
    }
  }
  d$waktu <- if (is.null(tgl)) seq_len(nrow(d)) else tgl
  attr(d, "pakai_tanggal") <- !is.null(tgl)
  d
}

# -----------------------------------------------------------------------------
# 3. Pemeriksaan kualitas data
# -----------------------------------------------------------------------------

#' Z-skor robust tiap titik terhadap median bergerak (jendela k hari).
#' Median bergerak tahan terhadap gangguan beberapa hari, jadi titik yang
#' menyimpang dari pola sekitarnya akan menonjol.
z_robust <- function(x, k = 21, abaikan = integer(0)) {
  ok <- !is.na(x)
  dasar <- ok; dasar[abaikan] <- FALSE
  if (sum(dasar) < k) return(rep(0, length(x)))
  isi <- stats::approx(which(dasar), x[dasar], xout = seq_along(x), rule = 2)$y
  # Jendela parsial di ujung supaya titik dekat akhir data tetap terdeteksi
  m <- zoo::rollapply(isi, k, stats::median, partial = TRUE, align = "center")
  r <- x - m
  s <- 1.4826 * stats::mad(r[dasar], na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  z <- r / s
  z[!ok] <- 0
  z
}

#' Titik kelembapan yang menyimpang saat muka air stabil. Diulang dengan
#' mengeluarkan titik yang sudah tertandai, supaya gangguan beberapa hari
#' berturut-turut tidak ikut menggeser median dan memicu tanda palsu.
deteksi_gangguan_sm <- function(sm, wt, ambang = 6) {
  z_wt <- z_robust(wt)
  tanda <- integer(0)
  for (i in 1:5) {
    z_sm <- z_robust(sm, abaikan = tanda)
    baru <- which(abs(z_sm) > ambang & abs(z_wt) < 3)
    if (identical(baru, tanda)) break
    tanda <- baru
  }
  tanda
}

#' Mengembalikan daftar temuan per titik dan pesan ringkas.
cek_kualitas <- function(d) {
  n <- nrow(d)
  temuan <- list()
  tambah <- function(idx, var, alasan, tingkat) {
    if (length(idx)) temuan[[length(temuan) + 1]] <<- data.frame(
      baris = idx, variabel = var, nilai = d[[var]][idx], alasan = alasan,
      tingkat = tingkat, stringsAsFactors = FALSE)
  }
  pesan <- character(0)

  # Data kosong
  for (v in names(VARIABEL)) {
    na <- which(is.na(d[[v]]))
    tambah(na, v, "Data kosong", "info")
    ujung <- 0
    while (ujung < n && is.na(d[[v]][n - ujung])) ujung <- ujung + 1
    if (ujung > 0) pesan <- c(pesan, sprintf(
      "%s kosong di %d hari terakhir. Interpolasi Peatfr tidak bisa mengisi ujung data, jadi aplikasi meneruskan nilai terakhir yang ada.",
      VARIABEL[[v]], ujung))
  }

  # Nilai di luar kisaran wajar
  tambah(which(d$WT > 0.5 | d$WT < -3), "WT", "Di luar kisaran wajar (-3 s.d. 0,5 m)", "peringatan")
  tambah(which(d$SM < 0 | d$SM > 100), "SM", "Di luar 0 s.d. 100%", "peringatan")
  tambah(which(d$Rf < 0), "Rf", "Curah hujan negatif", "peringatan")
  tambah(which(d$Temp < 10 | d$Temp > 45), "Temp", "Di luar kisaran wajar (10 s.d. 45 °C)", "peringatan")

  # Penyimpangan dari pola sekitar; kelembapan dicek silang dengan muka air.
  # Hujan sungguhan menggerakkan keduanya; gangguan sensor hanya satu.
  sensor <- deteksi_gangguan_sm(d$SM, d$WT)
  tambah(sensor, "SM", "Kelembapan menyimpang jauh saat muka air stabil: kemungkinan gangguan sensor", "peringatan")

  # Pesan tingkat dataset
  if (n < 50) pesan <- c(pesan, sprintf(
    "Data hanya %d hari. Disarankan minimal 50 hari; grafik ARIMA bawaan Peatfr butuh minimal 25.", n))
  rf_max <- suppressWarnings(max(d$Rf, na.rm = TRUE))
  if (is.finite(rf_max) && rf_max < 5.1) pesan <- c(pesan, sprintf(
    "Curah hujan maksimal hanya %s, di bawah ambang 5,1 mm di rumus PFVI. Faktor hujan tidak akan pernah aktif. Periksa satuan (mm?) atau periode datanya.",
    format(round(rf_max, 4), decimal.mark = ",")))
  luar_sm <- mean(d$SM < 40 | d$SM > 70, na.rm = TRUE)
  if (is.finite(luar_sm) && luar_sm > 0.05) pesan <- c(pesan, sprintf(
    "%s%% data kelembapan di luar 40 s.d. 70%%. Rumus kalibrasi PFVI memakai batas tetap 40%% (kering) dan 70%% (jenuh), jadi nilai di luar rentang ini terpotong.",
    format(round(100 * luar_sm), decimal.mark = ",")))

  tabel <- if (length(temuan)) do.call(rbind, temuan) else
    data.frame(baris = integer(0), variabel = character(0), nilai = numeric(0),
               alasan = character(0), tingkat = character(0))
  tabel <- tabel[order(tabel$baris, tabel$variabel), ]
  list(temuan = tabel, pesan = pesan)
}

# -----------------------------------------------------------------------------
# 4. Menjalankan Peatfr
# -----------------------------------------------------------------------------

kelas_pfvi <- function(p) as.character(cut(p, KELAS_BATAS, labels = KELAS_NAMA))

#' Mengisi data kosong di awal/akhir deret dengan nilai terdekat (tambahan
#' aplikasi; interpolasi Peatfr hanya mengisi bagian tengah).
isi_ujung <- function(x) {
  ok <- which(!is.na(x))
  if (!length(ok)) return(x)
  if (ok[1] > 1) x[seq_len(ok[1] - 1)] <- x[ok[1]]
  akhir <- ok[length(ok)]
  if (akhir < length(x)) x[(akhir + 1):length(x)] <- x[akhir]
  x
}

#' Mengambil metrik uji yang dicetak (bukan dikembalikan) oleh fungsi riset.
ambil_metrik <- function(log) {
  ambil <- function(label) {
    baris <- grep(paste0("Test Set ", label, ":"), log, value = TRUE)
    as.numeric(sub(paste0(".*Test Set ", label, ":\\s*"), "", baris))
  }
  rmse <- ambil("RMSE"); mae <- ambil("MAE")
  if (length(rmse) != 4 || length(mae) != 4) return(NULL)
  data.frame(variabel = names(VARIABEL), RMSE = rmse, MAE = mae)
}

#' Baseline naif: nilai terakhir data latih diteruskan ke seluruh periode uji,
#' dengan pembagian latih/uji yang sama seperti autopredictarima() (80/20).
baseline_naif <- function(x) {
  n <- length(x); nt <- floor(n * 0.2); tr <- n - nt
  pred <- rep(x[tr], nt); aktual <- x[(tr + 1):n]
  c(RMSE = sqrt(mean((pred - aktual)^2)), MAE = mean(abs(pred - aktual)))
}

rangkaian_akhir <- function(kelas, target = "Ekstrem") {
  r <- rle(kelas); k <- length(r$values)
  if (r$values[k] == target) r$lengths[k] else 0L
}

teks_waktu <- function(w) if (inherits(w, "Date")) format(w, "%d %b %Y") else paste("hari ke-", w, sep = "")
angka_id <- function(x, d = 0) format(round(x, d), nsmall = d, decimal.mark = ",", big.mark = ".")

#' Ringkasan otomatis dalam bahasa biasa untuk pengambil keputusan.
#' Mengembalikan daftar butir: judul, isi, tingkat (bahaya/perhatian/baik/info).
ringkasan_temuan <- function(r, kualitas) {
  h <- r$hasil
  obs <- h[h$status == "Observasi", ]
  ram <- h[h$status == "Ramalan", ]
  butir <- list()
  tambah <- function(judul, isi, tingkat) butir[[length(butir) + 1]] <<- list(judul = judul, isi = isi, tingkat = tingkat)

  # 1. Status risiko
  kelas_ram <- unique(ram$kelas)
  wt_cm <- -obs$WT[nrow(obs)] * 100
  posisi_wt <- if (wt_cm > 60) "melewati ambang 60 cm, kedalaman di mana kebakaran terverifikasi terjadi"
               else if (wt_cm > 40) "melewati ambang bahaya 40 cm"
               else "masih di atas ambang bahaya 40 cm"
  tambah("Status risiko",
         sprintf("Ramalan %d hari ke depan berada di kelas %s (PFVI %s sampai %s). Muka air tanah terakhir %s cm di bawah permukaan, %s.",
                 nrow(ram), paste(kelas_ram, collapse = " dan "),
                 angka_id(min(ram$PFVI), 1), angka_id(max(ram$PFVI), 1), angka_id(wt_cm), posisi_wt),
         if ("Ekstrem" %in% kelas_ram) "bahaya" else if ("Tinggi" %in% kelas_ram) "perhatian" else "baik")

  # 2. Lama kondisi Ekstrem
  if (r$ekstrem_beruntun > 0) {
    mulai <- obs$waktu[nrow(obs) - r$ekstrem_beruntun + 1]
    tambah("Lama kondisi Ekstrem",
           paste0(sprintf("Lokasi ini sudah berada di kelas Ekstrem %d hari berturut-turut sejak %s.",
                          r$ekstrem_beruntun, teks_waktu(mulai)),
                  if (r$ekstrem_beruntun >= 14) " Kalau dipakai sebagai peringatan harian, status yang sama terus-menerus berisiko diabaikan petugas. Peringkat antarlokasi dibutuhkan untuk memilih prioritas." else ""),
           if (r$ekstrem_beruntun >= 14) "perhatian" else "info")
  }

  # 3. Kualitas data
  t <- kualitas$temuan
  n_sensor <- sum(t$tingkat == "peringatan" & grepl("gangguan sensor", t$alasan))
  n_kosong <- sum(r$data_kosong)
  isi_kualitas <- sprintf("%d nilai kosong diisi otomatis.", n_kosong)
  if (n_sensor) isi_kualitas <- paste(isi_kualitas, sprintf(
    "%d titik kelembapan tanah diduga gangguan sensor (%s). Kalibrasi PFVI memakai kelembapan sebagai acuan, jadi titik ini ikut memengaruhi indeks.",
    n_sensor, if (length(r$catatan)) "sudah dikecualikan pada run ini" else "belum dikecualikan; bisa dicoba lewat opsi di tab 2"))
  if (r$hari_hujan_aktif == 0) isi_kualitas <- paste(isi_kualitas,
    "Tidak ada hari dengan hujan di atas 5,1 mm, sehingga faktor hujan di rumus PFVI tidak pernah aktif; satuan data hujan perlu dipastikan.")
  tambah("Kualitas data", isi_kualitas, if (n_sensor || r$hari_hujan_aktif == 0) "perhatian" else "baik")

  # 4. Keandalan ramalan
  e <- r$evaluasi
  if (!is.null(e$lebih_baik_dari_naif)) {
    kalah <- e$keterangan[e$lebih_baik_dari_naif == "Tidak"]
    isi <- if (length(kalah)) sprintf(
      "Pada data uji, model %s kalah akurat dibanding tebakan paling sederhana (nilai terakhir diteruskan) untuk: %s.",
      toupper(r$pengaturan$model), paste(tolower(sub(" \\(.*", "", kalah)), collapse = " dan "))
    else sprintf("Model %s lebih akurat dari tebakan sederhana untuk keempat variabel.", toupper(r$pengaturan$model))
    if (!is.null(e$orde_ARIMA) && e$orde_ARIMA[e$variabel == "WT"] == "(0,1,0)") isi <- paste(isi,
      "Ramalan muka air tanah praktis sama dengan nilai hari terakhir (random walk), sehingga ramalan ini lebih tepat dibaca sebagai \"kondisi bertahan\".")
    tambah("Keandalan ramalan", isi, if (length(kalah)) "perhatian" else "baik")
  }

  # 5. Batasan
  tambah("Batasan",
         "Hasil ini mewakili satu titik pengukuran dan belum dibandingkan dengan kejadian titik panas di lokasi ini. Gunakan sebagai bahan pertimbangan, bukan pengganti verifikasi lapangan.",
         "info")
  butir
}

#' Pipeline lengkap: kecualikan titik terpilih -> isi ujung -> imputasi Peatfr
#' -> peramalan Peatfr -> PFVI Peatfr -> ringkasan.
jalankan_peatfr <- function(env, d, imputasi = "linear", model = "arima", h = 4,
                            R0 = 3000, look_back = 12, unit = 32, epochs = 100,
                            kecualikan = integer(0), progres = function(v, ket) NULL) {
  WT <- d$WT; SM <- d$SM; Rf <- d$Rf; Temp <- d$Temp
  catatan <- character(0)

  if (length(kecualikan)) {
    SM[kecualikan] <- NA
    catatan <- c(catatan, sprintf("%d titik kelembapan yang ditandai dikecualikan lalu diimputasi.",
                                  length(kecualikan)))
  }
  awal_na <- sapply(list(WT, SM, Rf, Temp), function(x) sum(is.na(x)))
  WT <- isi_ujung(WT); SM <- isi_ujung(SM); Rf <- isi_ujung(Rf); Temp <- isi_ujung(Temp)

  # Grafik bawaan kode riset diarahkan ke perangkat kosong
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  log <- character(0)

  progres(0.1, "Mengisi data kosong")
  if (anyNA(c(WT, SM, Rf, Temp))) {
    fungsi <- switch(imputasi, linear = env$linear_interpolation, spline = env$spline_interpolation,
                     loess = env$loess_interpolation, knn = env$knn_imputation)
    log <- c(log, utils::capture.output(hasil <- fungsi(WT, SM, Rf, Temp)))
    WT <- hasil$Water_Table; SM <- hasil$Soil_Moisture
    Rf <- hasil$Rainfall; Temp <- hasil$Temperature
  }
  terisi <- data.frame(WT = WT, SM = SM, Rf = Rf, Temp = Temp)

  progres(0.3, sprintf("Meramal %d langkah dengan %s", h, toupper(model)))
  if (model == "arima") {
    log <- c(log, utils::capture.output(ramal <- env$autopredictarima(WT, SM, Rf, Temp, h)))
  } else if (model == "lstm") {
    log <- c(log, utils::capture.output(ramal <- env$autopredictlstm(
      WT, SM, Rf, Temp, h, look_back = look_back, lstm_layer_units = unit, epochs = epochs)))
  } else {
    log <- c(log, utils::capture.output(ramal <- env$autopredictgru(
      WT, SM, Rf, Temp, h, look_back = look_back, gru_layer_units = unit, epochs = epochs)))
  }
  pr <- ramal$predictions
  names(pr) <- c("WT", "SM", "Rf", "Temp")

  progres(0.6, "Mengkalibrasi dan menghitung PFVI (Nelder-Mead)")
  waktu_mulai <- Sys.time()
  log <- c(log, utils::capture.output(fp <- env$firepredict(
    WT = c(WT, pr$WT), SM = c(SM, pr$SM), Rf = c(Rf, pr$Rf), Temp = c(Temp, pr$Temp),
    R0 = R0, dt = 1, h = h)))
  durasi <- as.numeric(difftime(Sys.time(), waktu_mulai, units = "secs"))

  n <- nrow(d)
  if (inherits(d$waktu, "Date")) {
    waktu <- c(d$waktu, max(d$waktu) + seq_len(h))
  } else {
    waktu <- c(d$waktu, n + seq_len(h))
  }
  hasil <- data.frame(
    waktu = waktu,
    status = c(rep("Observasi", n), rep("Ramalan", h)),
    rbind(terisi, pr),
    PFVI = fp$PFVI,
    DIobs = fp$DIobs
  )
  hasil$kelas <- kelas_pfvi(hasil$PFVI)

  # Evaluasi peramalan dan baseline
  metrik <- ambil_metrik(log)
  naif <- t(sapply(terisi, baseline_naif))
  evaluasi <- data.frame(variabel = names(VARIABEL), keterangan = unname(VARIABEL),
                         RMSE_naif = naif[, "RMSE"], MAE_naif = naif[, "MAE"])
  if (!is.null(metrik)) {
    evaluasi$RMSE_model <- metrik$RMSE
    evaluasi$MAE_model <- metrik$MAE
    evaluasi$lebih_baik_dari_naif <- ifelse(metrik$RMSE < evaluasi$RMSE_naif, "Ya", "Tidak")
  }
  if (!is.null(ramal$LjungBoxTest)) {
    evaluasi$LjungBox_p <- unlist(ramal$LjungBoxTest)[names(VARIABEL)]
  }
  if (!is.null(ramal$model_orders)) {
    evaluasi$orde_ARIMA <- sapply(ramal$model_orders[names(VARIABEL)],
                                  function(o) paste0("(", paste(o, collapse = ","), ")"))
  }

  obs <- hasil[hasil$status == "Observasi", ]
  list(
    hasil = hasil,
    evaluasi = evaluasi,
    parameter = setNames(fp$TransformationParameters, c("aH", "bH", "n", "alpha")),
    hari_per_kelas = table(factor(obs$kelas, levels = KELAS_NAMA)),
    ekstrem_beruntun = rangkaian_akhir(obs$kelas),
    hari_hujan_aktif = sum(Rf > 5.1),
    data_kosong = setNames(awal_na, names(VARIABEL)),
    catatan = catatan,
    durasi_pfvi = durasi,
    log = log,
    pengaturan = list(imputasi = imputasi, model = model, h = h, R0 = R0)
  )
}
