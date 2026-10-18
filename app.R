# =============================================================================
# Peatfr App: prediksi risiko kebakaran gambut dari data harian
# Membungkus kode riset Peatfr (Mahdiyasa dkk., 2025) tanpa mengubahnya.
# Jalankan: shiny::runApp()  (dari folder ini)
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

source("R/pipeline.R")
source("R/grafik.R")
source("R/presentasi.R")

# Hasil Sabangau yang sudah dihitung (presentasi/hitung_sabangau.R) dan titik panas FIRMS
PRESENTASI <- muat_presentasi()
FIRMS <- muat_firms()
AWAL_RAMALAN <- if (!is.null(PRESENTASI)) min(PRESENTASI$asli$hasil$waktu[PRESENTASI$asli$hasil$status == "Ramalan"]) else NULL
RADIUS_KM <- 25

PEATFR <- tryCatch(muat_peatfr(), error = function(e) e)
# LSTM/GRU hanya ditawarkan kalau TensorFlow ada DAN fungsinya ikut dimuat
# (salinan untuk shinyapps.io sengaja tidak menyertakan LSTM/GRU)
ML_TERSEDIA <- !inherits(PEATFR, "error") &&
  exists("autopredictlstm", envir = PEATFR, inherits = FALSE) && tersedia_ml()

fmt <- function(x, d = 2) format(round(x, d), nsmall = d, decimal.mark = ",", big.mark = ".")

pilihan_model <- if (ML_TERSEDIA) {
  c("ARIMA (stokastik)" = "arima", "LSTM (machine learning)" = "lstm", "GRU (machine learning)" = "gru")
} else {
  c("ARIMA (stokastik)" = "arima")
}

WARNA_BUTIR <- c(bahaya = "#C0392B", perhatian = "#E3B23C", baik = "#4E9A5B", info = "#8A939B")
tampil_butir <- function(butir) {
  tagList(lapply(butir, function(b) {
    div(style = sprintf("border-left: 4px solid %s; padding: 4px 0 4px 14px; margin-bottom: 12px;", WARNA_BUTIR[[b$tingkat]]),
        tags$b(b$judul), br(), span(b$isi))
  }))
}

belum_dihitung <- function() div(class = "alert alert-warning mt-4",
  "Hasil Sabangau belum dihitung. Jalankan: Rscript presentasi/hitung_sabangau.R")

# -----------------------------------------------------------------------------
# Halaman presentasi
# -----------------------------------------------------------------------------
halaman_ringkasan <- function() {
  if (is.null(PRESENTASI)) return(belum_dihitung())
  a <- PRESENTASI$asli; b <- PRESENTASI$bersih
  obs <- a$hasil[a$hasil$status == "Observasi", ]; ram <- a$hasil[a$hasil$status == "Ramalan", ]
  mulai_ekstrem <- obs$waktu[nrow(obs) - a$ekstrem_beruntun + 1]
  geser <- abs(tail(a$hasil$PFVI, 1) - tail(b$hasil$PFVI, 1))
  n_dekat <- if (!is.null(FIRMS)) sum(FIRMS$jarak_km <= RADIUS_KM & FIRMS$tanggal >= min(ram$waktu) & FIRMS$tanggal <= max(ram$waktu)) else NA

  kotak_titik <- if (!is.na(n_dekat)) {
    value_box(sprintf("Titik panas satelit ≤ %d km, %s", RADIUS_KM, paste(format(range(ram$waktu), "%d"), collapse = "–")),
              n_dekat, p("Oktober 2023, data NASA FIRMS"), theme = if (n_dekat > 0) "danger" else NULL)
  } else {
    value_box("Hari hujan di atas 5,1 mm", a$hari_hujan_aktif, p("faktor hujan tidak pernah aktif"), theme = "warning")
  }

  butir_ringkasan <- ringkasan_temuan(a, PRESENTASI$kualitas)
  if (!is.na(n_dekat) && n_dekat > 0) {
    n <- length(butir_ringkasan)
    butir_ringkasan[[n]]$isi <- sprintf(paste(
      "Satelit NASA mencatat %d titik panas dalam radius %d km pada %s, sejalan dengan kelas Ekstrem.",
      "Namun titik panas sudah terdeteksi sejak %s (awal data satelit yang diunduh), sebelum periode ramalan, jadi ini belum membuktikan kemampuan memberi peringatan lebih awal.",
      "Perlu pengujian di banyak lokasi dan musim, termasuk tingkat alarm palsu. Hasil mewakili satu titik pengukuran dan bukan pengganti verifikasi lapangan."),
      n_dekat, RADIUS_KM, paste(tgl_id(min(ram$waktu), tahun = FALSE), "s.d.", tgl_id(max(ram$waktu))),
      tgl_id(min(FIRMS$tanggal[FIRMS$jarak_km <= RADIUS_KM])))
  }

  tagList(
    div(class = "hero",
        h1("Memprediksi risiko kebakaran gambut sebelum api muncul"),
        p("Studi kasus Sabangau, Kalimantan Tengah · data harian 1 April s.d. 9 Oktober 2023 · model Peatfr (ITB, IPB, University of Nottingham)")),
    layout_columns(
      value_box(paste("Kelas ramalan", paste(format(range(ram$waktu), "%d"), collapse = "–"), "Okt 2023"),
                ram$kelas[nrow(ram)], p(paste("PFVI", angka_id(min(ram$PFVI), 1), "s.d.", angka_id(max(ram$PFVI), 1))), theme = "danger"),
      value_box("Muka air tanah terakhir", paste(angka_id(-obs$WT[nrow(obs)] * 100), "cm"),
                p("di bawah permukaan; ambang bahaya 40 cm")),
      value_box("Kelas Ekstrem berturut-turut", paste(a$ekstrem_beruntun, "hari"),
                p(paste("sejak", tgl_id(mulai_ekstrem)))),
      kotak_titik
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Tiga pesan utama"), card_body(
        div(class = "pesan", tags$b("1. Ilmunya kuat dan terbuka"),
            "Hasil paper berhasil kami reproduksi dengan dataset resmi penulis. Kodenya terbuka, sehingga bisa diaudit dan dikembangkan.",
            if (!is.na(n_dekat) && n_dekat > 0) sprintf(
              " Pada hari-hari yang diramal Ekstrem, satelit NASA mencatat %d titik panas dalam radius %d km dari stasiun.",
              n_dekat, RADIUS_KM)),
        div(class = "pesan", style = "border-color:#E3B23C;", tags$b("2. Tantangan utamanya data, bukan rumus"),
            sprintf("Beberapa hari data sensor yang diduga rusak menggeser indeks sekitar %s poin. Hujan tidak pernah cukup besar untuk dihitung. Validasi baru satu stasiun dan satu musim kemarau.", angka_id(geser))),
        div(class = "pesan", style = "border-color:#4E9A5B;", tags$b("3. Datanya sudah ada, tapi belum terhubung"),
            "Muka air dan kelembapan tanah (BPGMN, eks-BRGM), muka air di areal konsesi (KLH), cuaca (BMKG), titik panas (Kemenhut, BRIN, NASA). Integrasi data dan operasionalisasi adalah langkah berikutnya.")
      )),
      card(card_header("Ringkasan otomatis dari hasil model"), card_body(tampil_butir(butir_ringkasan)))
    ),
    card(card_header("Indeks kerawanan harian dan ramalan 4 hari ke depan"),
         card_body(plotOutput("pres_plot", height = "440px"),
                   tableOutput("pres_tabel_skenario"),
                   p(class = "catatan-kecil",
                     "Garis abu-abu putus-putus menunjukkan hasil bila data kelembapan yang diduga gangguan sensor (3 s.d. 8 Oktober 2023) dikeluarkan lalu diisi ulang."))),
    card(card_header("Bagaimana model bekerja"),
         card_body(tags$img(src = "ilustrasi_peatfr.svg", class = "ilustrasi",
                            alt = "Penampang gambut dan alur kerja Peatfr"))),
    p(class = "catatan-kecil mt-2",
      "Sumber: Mahdiyasa dkk. (2025), Ecological Informatics 92, 103532; kode github.com/mellygsln/peatfr; dataset Sabangau dari tautan resmi di paper. Prototipe VeloCT, bukan sistem operasional.")
  )
}

halaman_peta <- function() {
  if (is.null(PRESENTASI)) return(belum_dihitung())
  ringkas <- if (!is.null(FIRMS)) tabel_titik_panas(FIRMS, RADIUS_KM) else NULL
  panel_kanan <- if (is.null(FIRMS)) {
    p(class = "catatan-kecil", "Data titik panas NASA FIRMS belum dimuat. Simpan CSV unduhan FIRMS di folder data/firms lalu muat ulang aplikasi.")
  } else if (is.null(ringkas)) {
    p(sprintf("Tidak ada titik panas dalam radius %d km dari stasiun pada periode data.", RADIUS_KM))
  } else {
    tagList(tabel_html(ringkas, kanan = setdiff(names(ringkas), "Tanggal")),
            p(class = "catatan-kecil", "Jumlah titik panas per hari dalam radius ", RADIUS_KM, " km dari stasiun, menurut sensor satelit."))
  }
  tagList(
    div(class = "hero",
        h1("Apa yang terlihat dari satelit"),
        p("Model meramal kelas Ekstrem untuk 10 s.d. 13 Oktober 2023. Titik merah: titik panas yang terdeteksi satelit pada hari-hari tersebut; titik kuning: hari-hari sebelumnya.")),
    layout_columns(
      col_widths = c(8, 4),
      card(card_body(peta_ui("peta_sabangau", PRESENTASI$asli, FIRMS, RADIUS_KM, AWAL_RAMALAN))),
      card(card_header("Titik panas di sekitar stasiun"), card_body(
        panel_kanan,
        hr(),
        div(class = "catatan-kecil",
            tags$b("Cara membaca"), br(),
            span(style = "color:#1F5F8B;", "●"), " Stasiun pemantauan (perkiraan lokasi dari paper)", br(),
            span(style = "color:#C0392B;", "●"), " Titik panas pada hari yang diramal Ekstrem", br(),
            span(style = "color:#E3B23C;", "●"), " Titik panas pada hari sebelumnya", br(),
            "Lingkaran putus-putus: radius ", RADIUS_KM, " km", br(), br(),
            "Titik panas adalah anomali suhu yang terdeteksi satelit, belum tentu semuanya kebakaran; tetap perlu verifikasi lapangan.")
      ))
    )
  )
}

halaman_kl <- function() {
  baris <- data.frame(
    Data = c("Tinggi muka air tanah (stasiun restorasi)", "Tinggi muka air tanah (areal konsesi)", "Kelembapan tanah",
             "Curah hujan", "Suhu udara", "Indeks bahaya cuaca", "Titik panas", "Peta Kesatuan Hidrologis Gambut",
             "Izin konsesi dan tutupan lahan", "Batas desa dan kependudukan"),
    `Fungsi di model` = c("Input utama", "Memperapat jaringan pemantauan", "Input dan acuan kalibrasi", "Input", "Input",
                          "Pembanding", "Validasi model", "Unit wilayah peta risiko", "Konteks dan penanggung jawab lahan",
                          "Sasaran peringatan dini"),
    `Pemegang data` = c("BPGMN (warisan SIPALAGA BRGM)", "KLH", "BPGMN (SIPALAGA)", "BPGMN, BMKG", "BMKG", "BMKG (SPARTAN)",
                        "Kemenhut (SiPongi), BRIN, NASA FIRMS", "KLH", "Kemenhut, Kementan, ATR/BPN", "BIG, Kemendagri, BPS"),
    Catatan = c("Sumber data di paper Peatfr; sensor lapangan, bukan satelit", "Hampir 11.000 alat pantau di areal 332 perusahaan (Nov 2024)",
                "Data Sabangau berisi beberapa hari yang diduga gangguan sensor", "Satuan perlu diseragamkan antarinstansi",
                "Sumber data di paper Peatfr", "Hanya berbasis cuaca, tanpa muka air tanah", "Paper memvalidasi dengan FIRMS",
                "Sekitar 500 KHG akan dipetakan BPGMN", "Dibutuhkan untuk memasukkan faktor aktivitas manusia",
                "Untuk menurunkan peringatan sampai tingkat desa"),
    check.names = FALSE)
  tagList(
    div(class = "hero",
        h1("Datanya sudah ada, tersebar di banyak instansi"),
        p("Semua input model sudah dimiliki pemerintah. Tantangannya adalah menghubungkan, menyeragamkan, dan menjaga kualitasnya.")),
    card(card_body(tabel_html(baris))),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("1. Inventarisasi data"), card_body(
        "Memetakan data per instansi: apa, di mana, seberapa sering, formatnya, kualitasnya, dan hak aksesnya. Langkah pertama yang berisiko rendah dan langsung berguna.")),
      card(card_header("2. Standar dan pertukaran data"), card_body(
        "Kamus data dan satuan yang seragam, pemeriksaan kualitas otomatis, serta perjanjian pertukaran data, di bawah payung Satu Data Indonesia.")),
      card(card_header("3. Platform dan dashboard"), card_body(
        "Menjalankan model secara terjadwal, peringkat risiko antarlokasi, validasi rutin dengan titik panas, dan pelatihan untuk daerah."))
    ),
    p(class = "catatan-kecil mt-2", "Usulan peran VeloCT sebagai mitra teknologi, melengkapi tim peneliti. Sumber: Antara (19 Nov 2024), BMKG (Jun 2026), Kemenhut via Sindonews (Agu 2026), Setneg (Okt 2026).")
  )
}

# -----------------------------------------------------------------------------
# Antarmuka
# -----------------------------------------------------------------------------
ui <- page_navbar(
  id = "main",
  title = "Peatfr App",
  theme = bs_theme(version = 5, primary = "#1F5F8B", success = "#4E9A5B",
                   danger = "#C0392B", base_font = font_collection("system-ui", "sans-serif")),
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "leaflet/leaflet.css"),
    tags$script(src = "leaflet/leaflet.js"),
    tags$style(HTML("
      .hero { padding: 28px 4px 8px; }
      .hero h1 { font-size: 1.9rem; font-weight: 700; margin-bottom: .4rem; }
      .hero p { color: #5B6670; font-size: 1.05rem; margin-bottom: 0; }
      .pesan { border-left: 4px solid #1F5F8B; padding: 6px 0 6px 14px; margin-bottom: 14px; }
      .pesan b { display: block; font-size: 1.02rem; }
      .ilustrasi { width: 100%; max-width: 980px; display: block; margin: 0 auto; }
      .catatan-kecil { color: #5B6670; font-size: .85rem; }
    "))
  ),

  nav_panel("Ringkasan", halaman_ringkasan()),
  nav_panel("Peta", halaman_peta()),
  nav_panel("Data lintas K/L", halaman_kl()),

  nav_menu(
  "Coba sendiri",

  nav_panel(
    "1. Data",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        fileInput("berkas", "Unggah CSV data harian", accept = c(".csv", "text/csv"),
                  buttonLabel = "Pilih file", placeholder = "Belum ada file"),
        actionButton("pakai_sabangau", "Pakai data Sabangau (paper)", class = "btn-outline-primary btn-sm mb-2 w-100"),
        actionButton("pakai_contoh", "Atau pakai data contoh sintetis", class = "btn-outline-secondary btn-sm w-100"),
        hr(),
        p(tags$b("Pemetaan kolom"), br(),
          tags$small("Ditebak otomatis dari nama kolom. Ubah jika keliru.")),
        selectInput("kol_WT", "Muka air tanah (m, negatif = di bawah permukaan)", NULL),
        selectInput("kol_SM", "Kelembapan tanah (%)", NULL),
        selectInput("kol_Rf", "Curah hujan (mm/hari)", NULL),
        selectInput("kol_Temp", "Suhu udara (°C, maksimum harian)", NULL),
        selectInput("kol_Tanggal", "Tanggal (opsional)", NULL),
        hr(),
        tags$small(textOutput("sumber_kode"))
      ),
      uiOutput("ringkasan_data"),
      uiOutput("pesan_kualitas"),
      card(card_body(plotOutput("plot_variabel", height = "480px"))),
      card(card_header("Titik yang perlu diperiksa"), card_body(tableOutput("tabel_temuan")))
    )
  ),

  nav_panel(
    "2. Jalankan model",
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Pengaturan"),
        selectInput("imputasi", "Metode mengisi data kosong",
                    c("Interpolasi linear" = "linear", "Spline" = "spline",
                      "LOESS" = "loess", "k-nearest neighbour" = "knn")),
        selectInput("model", "Model peramalan", pilihan_model),
        if (!ML_TERSEDIA) tags$small(class = "text-muted",
          "LSTM dan GRU butuh TensorFlow (Python), yang belum terpasang di komputer ini. Lihat README."),
        conditionalPanel(
          "input.model != 'arima'",
          numericInput("look_back", "Jendela ke belakang (hari)", 12, min = 3, max = 60),
          numericInput("unit", "Jumlah unit jaringan", 32, min = 4, max = 256),
          numericInput("epochs", "Epoch pelatihan", 100, min = 10, max = 1000)
        ),
        sliderInput("h", "Hari yang diramal ke depan", min = 1, max = 14, value = 4),
        numericInput("R0", "Curah hujan tahunan representatif, R0 (mm)", 3000, min = 500, max = 6000, step = 100),
        checkboxInput("kecualikan", "Kecualikan titik kelembapan yang diduga gangguan sensor", FALSE),
        uiOutput("info_kecualikan"),
        actionButton("jalan", "Jalankan Peatfr", class = "btn-primary btn-lg w-100")
      ),
      card(
        card_header("Cara kerja"),
        tags$ol(
          tags$li(tags$b("Isi data kosong."), " Kode riset mengisi bagian tengah deret. Aplikasi menambahkan: data kosong di ujung diisi nilai terakhir, karena interpolasi tidak bisa mengisi ujung data."),
          tags$li(tags$b("Ramal empat variabel"), " beberapa hari ke depan, masing-masing dari pola masa lalunya sendiri."),
          tags$li(tags$b("Hitung PFVI."), " Parameter indeks dikalibrasi otomatis dengan optimisasi Nelder-Mead terhadap indeks kekeringan dari kelembapan tanah (40% = kering, 70% = jenuh).")
        ),
        p(class = "text-muted", "Tahap 3 bisa memakan 30 detik sampai beberapa menit, tergantung data."),
        p(class = "text-muted",
          "Kode riset: ", tags$a(href = "https://github.com/mellygsln/peatfr", target = "_blank", "github.com/mellygsln/peatfr"),
          ". Paper: Mahdiyasa dkk. (2025), Ecological Informatics 92, 103532.")
      )
    )
  ),

  nav_panel(
    "3. Hasil",
    uiOutput("kotak_hasil"),
    card(card_header("Ringkasan untuk pengambil keputusan"), card_body(uiOutput("ringkasan"))),
    card(card_body(plotOutput("plot_pfvi", height = "440px"))),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Ramalan"), card_body(tableOutput("tabel_ramalan"))),
      card(card_header("Jumlah hari per kelas (periode observasi)"), card_body(tableOutput("tabel_kelas")))
    )
  ),

  nav_panel(
    "4. Evaluasi model",
    card(
      card_header("Akurasi peramalan dibanding tebakan naif"),
      card_body(
        p("Data dibagi 80% latih dan 20% uji, sama seperti kode riset. Tebakan naif = nilai terakhir data latih diteruskan ke seluruh periode uji. Model yang baik seharusnya lebih akurat dari tebakan naif."),
        tableOutput("tabel_evaluasi"),
        tags$small(class = "text-muted",
          "Uji Ljung-Box: p di bawah 0,05 berarti pola data belum tertangkap model. Orde (0,1,0) adalah random walk, yaitu ramalan kurang lebih sama dengan nilai terakhir.")
      )
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(card_header("Parameter PFVI hasil kalibrasi"), card_body(tableOutput("tabel_parameter"),
        tags$small(class = "text-muted", "Dikalibrasi ulang setiap kali model dijalankan, memakai data observasi ditambah data ramalan."))),
      card(card_header("Log kode riset"), card_body(
        tags$details(tags$summary("Tampilkan keluaran teks dari fungsi Peatfr"),
                     verbatimTextOutput("log_riset"))))
    )
  ),

  nav_panel(
    "5. Unduh",
    card(
      card_header("Unduh hasil"),
      p("Hasil lengkap per hari: data yang sudah diisi, status observasi atau ramalan, PFVI, dan kelasnya."),
      downloadButton("unduh_hasil", "Hasil per hari (CSV)", class = "btn-primary"),
      br(), br(),
      downloadButton("unduh_evaluasi", "Evaluasi model (CSV)"),
      downloadButton("unduh_temuan", "Temuan kualitas data (CSV)")
    )
  ),

  ),

  nav_spacer(),
  nav_item(tags$small(class = "text-muted", "Prototipe VeloCT"))
)

# -----------------------------------------------------------------------------
# Logika
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  output$sumber_kode <- renderText({
    if (inherits(PEATFR, "error")) paste("Kode Peatfr belum ditemukan:", conditionMessage(PEATFR))
    else paste("Kode Peatfr dimuat dari:", attr(PEATFR, "sumber"))
  })

  data_mentah <- reactiveVal(NULL)

  observeEvent(input$berkas, {
    raw <- tryCatch(baca_csv(input$berkas$datapath), error = function(e) {
      showNotification(paste("File tidak bisa dibaca:", conditionMessage(e)), type = "error"); NULL
    })
    data_mentah(raw)
  })
  observeEvent(input$pakai_contoh, data_mentah(baca_csv("contoh/contoh_data_gambut.csv")))
  observeEvent(input$pakai_sabangau, data_mentah(baca_csv("data/Dataset_Sabangau.csv")))

  observeEvent(data_mentah(), {
    raw <- data_mentah(); req(raw)
    tebak <- tebak_kolom(names(raw))
    for (v in c("WT", "SM", "Rf", "Temp")) {
      updateSelectInput(session, paste0("kol_", v), choices = names(raw), selected = tebak[[v]])
    }
    updateSelectInput(session, "kol_Tanggal", choices = c("(tidak ada)" = "", names(raw)),
                      selected = tebak[["Tanggal"]])
  })

  data_siap <- reactive({
    raw <- data_mentah()
    validate(need(raw, "Unggah file CSV atau pakai data contoh untuk mulai."))
    kol <- c(WT = input$kol_WT, SM = input$kol_SM, Rf = input$kol_Rf,
             Temp = input$kol_Temp, Tanggal = input$kol_Tanggal %||% "")
    validate(need(all(nzchar(kol[1:4])) && all(kol[1:4] %in% names(raw)), "Pilih kolom untuk keempat variabel."))
    d <- siapkan_data(raw, kol)
    kosong <- sapply(d[names(VARIABEL)], function(x) mean(is.na(x)))
    validate(need(all(kosong < 0.5), paste(
      "Lebih dari separuh nilai kosong atau bukan angka di kolom:",
      paste(names(kosong)[kosong >= 0.5], collapse = ", "), ". Periksa pemetaan kolom.")))
    d
  })

  kualitas <- reactive(cek_kualitas(data_siap()))
  titik_sensor <- reactive({
    t <- kualitas()$temuan
    t$baris[t$tingkat == "peringatan" & grepl("gangguan sensor", t$alasan)]
  })

  output$ringkasan_data <- renderUI({
    d <- data_siap(); q <- kualitas()
    kosong <- sum(is.na(d[names(VARIABEL)]))
    periode <- if (attr(d, "pakai_tanggal")) paste(format(min(d$waktu), "%d %b %Y"), "s.d.", format(max(d$waktu), "%d %b %Y"))
               else "Tanpa kolom tanggal (hari ke-1, 2, ...)"
    layout_columns(
      value_box("Jumlah hari", nrow(d), p(periode)),
      value_box("Nilai kosong", kosong, p("diisi saat model dijalankan")),
      value_box("Titik perlu diperiksa", sum(q$temuan$tingkat == "peringatan"),
                p("gangguan sensor atau di luar kewajaran"),
                theme = if (any(q$temuan$tingkat == "peringatan")) "warning" else "success")
    )
  })

  output$pesan_kualitas <- renderUI({
    p <- kualitas()$pesan
    if (!length(p)) return(NULL)
    div(class = "alert alert-warning", tags$b("Catatan kualitas data"), tags$ul(lapply(p, tags$li)))
  })

  output$plot_variabel <- renderPlot(grafik_variabel(data_siap(), kualitas()$temuan), res = 96)

  output$tabel_temuan <- renderTable({
    t <- kualitas()$temuan
    t <- t[t$tingkat == "peringatan", ]
    validate(need(nrow(t) > 0, "Tidak ada titik yang mencurigakan."))
    d <- data_siap()
    data.frame(Waktu = format(d$waktu[t$baris]), Variabel = unname(VARIABEL[t$variabel]),
               Nilai = fmt(t$nilai), Alasan = t$alasan)
  })

  output$info_kecualikan <- renderUI({
    n <- length(titik_sensor())
    tags$small(class = "text-muted", if (n) paste(n, "titik akan dikosongkan lalu diisi ulang lewat imputasi.")
               else "Tidak ada titik yang terdeteksi sebagai gangguan sensor.")
  })

  # Menjalankan model -----------------------------------------------------
  hasil <- eventReactive(input$jalan, {
    if (inherits(PEATFR, "error")) {
      showNotification(conditionMessage(PEATFR), type = "error", duration = NULL); return(NULL)
    }
    d <- data_siap()
    kecuali <- if (isTRUE(input$kecualikan)) titik_sensor() else integer(0)
    withProgress(message = "Menjalankan Peatfr", value = 0, {
      tryCatch(
        jalankan_peatfr(PEATFR, d, imputasi = input$imputasi, model = input$model, h = input$h,
                        R0 = input$R0, look_back = input$look_back, unit = input$unit,
                        epochs = input$epochs, kecualikan = kecuali,
                        progres = function(v, ket) setProgress(v, detail = ket)),
        error = function(e) {
          showNotification(paste("Gagal menjalankan model:", conditionMessage(e)),
                           type = "error", duration = NULL)
          NULL
        })
    })
  })

  observeEvent(hasil(), if (!is.null(hasil())) nav_select("main", "3. Hasil", session = session))

  perlu_hasil <- function() {
    validate(need(hasil(), "Belum ada hasil. Jalankan model di tab 2."))
    hasil()
  }

  output$kotak_hasil <- renderUI({
    r <- perlu_hasil(); h <- r$hasil
    ram <- h[h$status == "Ramalan", ]
    akhir <- ram$kelas[nrow(ram)]
    tema <- c(Rendah = "success", Sedang = "warning", Tinggi = "warning", Ekstrem = "danger")[[akhir]]
    layout_columns(
      value_box("Kelas di akhir ramalan", akhir, p(paste("PFVI", fmt(ram$PFVI[nrow(ram)], 1))), theme = tema),
      value_box("Hari Ekstrem berturut-turut", r$ekstrem_beruntun, p("sampai akhir data observasi"),
                theme = if (r$ekstrem_beruntun >= 14) "danger" else NULL),
      value_box("Muka air terakhir", paste(fmt(-h$WT[sum(h$status == "Observasi")] * 100, 0), "cm"),
                p("di bawah permukaan; ambang bahaya 40 cm")),
      value_box("Hari hujan > 5,1 mm", r$hari_hujan_aktif, p("hari faktor hujan aktif di PFVI"),
                theme = if (r$hari_hujan_aktif == 0) "warning" else NULL)
    )
  })

  output$ringkasan <- renderUI(tampil_butir(ringkasan_temuan(perlu_hasil(), kualitas())))

  # Halaman presentasi ------------------------------------------------------
  output$pres_plot <- renderPlot({
    req(PRESENTASI)
    grafik_pfvi_banding(PRESENTASI$asli, PRESENTASI$bersih)
  }, res = 96)
  output$pres_tabel_skenario <- renderTable({
    req(PRESENTASI)
    tabel_skenario(PRESENTASI$asli, PRESENTASI$bersih)
  })

  output$plot_pfvi <- renderPlot(grafik_pfvi(perlu_hasil()$hasil), res = 96)

  output$tabel_ramalan <- renderTable({
    h <- perlu_hasil()$hasil; ram <- h[h$status == "Ramalan", ]
    tabel <- data.frame(format(ram$waktu), fmt(ram$WT, 3), fmt(ram$SM, 1), fmt(ram$Rf, 3),
                        fmt(ram$Temp, 1), fmt(ram$PFVI, 1), ram$kelas)
    names(tabel) <- c("Waktu", "Muka air (m)", "Kelembapan (%)", "Hujan (mm)",
                      "Suhu (°C)", "PFVI", "Kelas")
    tabel
  })

  output$tabel_kelas <- renderTable({
    k <- perlu_hasil()$hari_per_kelas
    data.frame(Kelas = names(k), `Jumlah hari` = as.integer(k),
               Persen = paste0(fmt(100 * as.integer(k) / sum(k), 0), "%"), check.names = FALSE)
  })

  output$tabel_evaluasi <- renderTable({
    e <- perlu_hasil()$evaluasi
    keluar <- data.frame(Variabel = e$keterangan, check.names = FALSE)
    if (!is.null(e$orde_ARIMA)) keluar$`Orde ARIMA` <- e$orde_ARIMA
    if (!is.null(e$RMSE_model)) {
      keluar$`RMSE model` <- fmt(e$RMSE_model, 4)
      keluar$`RMSE naif` <- fmt(e$RMSE_naif, 4)
      keluar$`Lebih baik dari naif?` <- e$lebih_baik_dari_naif
    } else {
      keluar$`RMSE naif` <- fmt(e$RMSE_naif, 4)
    }
    if (!is.null(e$LjungBox_p)) keluar$`Ljung-Box p` <- ifelse(e$LjungBox_p < 0.001, "< 0,001", fmt(e$LjungBox_p, 3))
    keluar
  })

  output$tabel_parameter <- renderTable({
    p <- perlu_hasil()$parameter
    data.frame(Parameter = c("aH (konstanta kalibrasi)", "bH (konstanta kalibrasi)",
                             "n (sifat hidraulik gambut)", "α (sifat hidraulik gambut)"),
               Nilai = fmt(p, 3))
  })

  output$log_riset <- renderText(paste(perlu_hasil()$log, collapse = "\n"))

  # Unduhan -----------------------------------------------------------------
  output$unduh_hasil <- downloadHandler(
    filename = function() paste0("peatfr_hasil_", format(Sys.Date()), ".csv"),
    content = function(file) write.csv(perlu_hasil()$hasil, file, row.names = FALSE)
  )
  output$unduh_evaluasi <- downloadHandler(
    filename = function() paste0("peatfr_evaluasi_", format(Sys.Date()), ".csv"),
    content = function(file) write.csv(perlu_hasil()$evaluasi, file, row.names = FALSE)
  )
  output$unduh_temuan <- downloadHandler(
    filename = function() paste0("peatfr_kualitas_data_", format(Sys.Date()), ".csv"),
    content = function(file) write.csv(kualitas()$temuan, file, row.names = FALSE)
  )
}

shinyApp(ui, server)
