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

# Hasil stasiun Sabangau yang sudah dihitung (presentasi/hitung_sabangau.R) dan titik panas FIRMS
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
# Halaman pemantauan (hasil tersimpan untuk stasiun Sabangau)
# -----------------------------------------------------------------------------
lencana_kelas <- function(k) span(class = "lencana", style = sprintf("background:%s;", WARNA_KELAS[[k]]), k)

halaman_pemantauan <- function() {
  if (is.null(PRESENTASI)) return(belum_dihitung())
  a <- PRESENTASI$asli
  obs <- a$hasil[a$hasil$status == "Observasi", ]; ram <- a$hasil[a$hasil$status == "Ramalan", ]
  mulai_ekstrem <- obs$waktu[nrow(obs) - a$ekstrem_beruntun + 1]
  akhir <- ram$kelas[nrow(ram)]
  tema <- c(Rendah = "success", Sedang = "warning", Tinggi = "warning", Ekstrem = "danger")[[akhir]]
  rentang_ram <- paste(tgl_id(min(ram$waktu), singkat = TRUE, tahun = FALSE), "–", tgl_id(max(ram$waktu), singkat = TRUE))
  n_dekat <- if (!is.null(FIRMS)) sum(FIRMS$jarak_km <= RADIUS_KM & FIRMS$tanggal >= min(ram$waktu) & FIRMS$tanggal <= max(ram$waktu)) else NA
  model <- toupper(a$pengaturan$model %||% "arima")

  tabel_ram <- tags$table(class = "table table-sm align-middle mb-0",
    tags$thead(tags$tr(tags$th("Tanggal"), tags$th("PFVI", style = "text-align:right"), tags$th("Kelas"))),
    tags$tbody(lapply(seq_len(nrow(ram)), function(i) tags$tr(
      tags$td(tgl_id(ram$waktu[i], singkat = TRUE)),
      tags$td(angka_id(ram$PFVI[i], 1), style = "text-align:right"),
      tags$td(lencana_kelas(ram$kelas[i]))))))

  ringkas_titik <- if (!is.null(FIRMS)) tabel_titik_panas(FIRMS, RADIUS_KM) else NULL
  isi_titik <- if (is.null(FIRMS)) {
    p(class = "catatan-kecil", "Data titik panas belum dimuat (folder data/firms).")
  } else if (is.null(ringkas_titik)) {
    p(class = "catatan-kecil", sprintf("Tidak ada titik panas dalam radius %d km.", RADIUS_KM))
  } else tabel_html(ringkas_titik, kanan = setdiff(names(ringkas_titik), "Tanggal"))

  tagList(
    div(class = "kepala",
        div(h4("Stasiun BRGM Sabangau"),
            span(class = "text-muted", "Kalimantan Tengah · observasi s.d. ", tgl_id(max(obs$waktu)),
                 " · ramalan ", nrow(ram), " hari · model ", model)),
        span(class = "badge text-bg-light border", "Contoh data: Sabangau 2023")),
    layout_columns(
      value_box("Kelas ramalan", akhir, p(rentang_ram, " · PFVI ", angka_id(max(ram$PFVI), 1)), theme = tema),
      value_box("Muka air tanah terakhir", paste(angka_id(-obs$WT[nrow(obs)] * 100), "cm"),
                p("di bawah permukaan · ambang 40 cm")),
      value_box("Ekstrem berturut-turut", paste(a$ekstrem_beruntun, "hari"), p("sejak ", tgl_id(mulai_ekstrem))),
      value_box(sprintf("Titik panas ≤ %d km", RADIUS_KM), if (is.na(n_dekat)) "–" else n_dekat,
                p("selama periode ramalan · NASA FIRMS"), theme = if (!is.na(n_dekat) && n_dekat > 0) "danger" else NULL)
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Peta stasiun dan titik panas"),
           card_body(peta_ui("peta_sabangau", a, FIRMS, RADIUS_KM, AWAL_RAMALAN),
                     div(class = "catatan-kecil mt-2",
                         span(style = "color:#1F5F8B;", "●"), " Stasiun   ",
                         span(style = "color:#C0392B;", "●"), " Titik panas selama periode ramalan   ",
                         span(style = "color:#E3B23C;", "●"), " Titik panas sebelumnya   ",
                         "- - - radius ", RADIUS_KM, " km"))),
      div(
        card(card_header("Ramalan per hari"), card_body(tabel_ram)),
        card(card_header(sprintf("Titik panas per hari (≤ %d km)", RADIUS_KM)), card_body(isi_titik))
      )
    ),
    card(card_body(plotOutput("pantau_plot", height = "400px"))),
    p(class = "catatan-kecil mt-1",
      "Sumber data: dataset Sabangau (Mahdiyasa dkk., 2025); titik panas NASA FIRMS (MODIS, VIIRS). ",
      "Lokasi stasiun perkiraan. Titik panas adalah anomali suhu dari satelit dan perlu verifikasi lapangan.")
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
      .kepala { display: flex; justify-content: space-between; align-items: flex-start; gap: 12px; padding: 18px 2px 4px; flex-wrap: wrap; }
      .kepala h4 { font-weight: 700; margin-bottom: 2px; }
      .lencana { color: #fff; border-radius: 4px; padding: 1px 8px; font-size: .82rem; font-weight: 600; }
      .catatan-kecil { color: #5B6670; font-size: .85rem; }
    "))
  ),

  nav_panel("Pemantauan", halaman_pemantauan()),

  nav_panel(
    "Data",
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
    "Jalankan model",
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
    "Hasil",
    uiOutput("kotak_hasil"),
    card(card_header("Interpretasi otomatis"), card_body(uiOutput("ringkasan"))),
    card(card_body(plotOutput("plot_pfvi", height = "440px"))),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Ramalan"), card_body(tableOutput("tabel_ramalan"))),
      card(card_header("Jumlah hari per kelas (periode observasi)"), card_body(tableOutput("tabel_kelas")))
    )
  ),

  nav_panel(
    "Evaluasi model",
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
    "Unduh",
    card(
      card_header("Unduh hasil"),
      p("Hasil lengkap per hari: data yang sudah diisi, status observasi atau ramalan, PFVI, dan kelasnya."),
      downloadButton("unduh_hasil", "Hasil per hari (CSV)", class = "btn-primary"),
      br(), br(),
      downloadButton("unduh_evaluasi", "Evaluasi model (CSV)"),
      downloadButton("unduh_temuan", "Temuan kualitas data (CSV)")
    )
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

  observeEvent(hasil(), if (!is.null(hasil())) nav_select("main", "Hasil", session = session))

  perlu_hasil <- function() {
    validate(need(hasil(), "Belum ada hasil. Jalankan model di tab Jalankan model."))
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

  # Halaman pemantauan -----------------------------------------------------
  output$pantau_plot <- renderPlot({
    req(PRESENTASI)
    grafik_pfvi(PRESENTASI$asli$hasil)
  }, res = 96)

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
