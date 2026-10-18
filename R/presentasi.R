# =============================================================================
# Bahan halaman presentasi: hasil Sabangau yang sudah dihitung, titik panas
# FIRMS, grafik perbandingan skenario, dan peta (Leaflet JS, tanpa paket R).
# =============================================================================

# Perkiraan lokasi stasiun dari peta Gambar 3 di paper Peatfr (Mahdiyasa dkk., 2025).
# Ganti dengan koordinat resmi kalau sudah dikonfirmasi.
STASIUN <- list(lat = -2.28, lon = 113.87,
                nama = "Stasiun pemantauan BRGM, Sabangau",
                catatan = "Perkiraan lokasi dari peta di paper (Gambar 3), belum koordinat resmi")

muat_presentasi <- function(path = "presentasi/hasil_sabangau.rds") {
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

#' Jarak (km) dua titik dengan rumus haversine.
jarak_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  dlat <- (lat2 - lat1) * rad; dlon <- (lon2 - lon1) * rad
  a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
  6371 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

#' Membaca semua CSV FIRMS di folder data/firms (unduhan API area/csv).
muat_firms <- function(folder = "data/firms") {
  berkas <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  if (!length(berkas)) return(NULL)
  semua <- lapply(berkas, function(f) {
    x <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(x) || !all(c("latitude", "longitude", "acq_date") %in% names(x)) || !nrow(x)) return(NULL)
    sensor <- if ("instrument" %in% names(x)) x$instrument else sub("_.*", "", basename(f))
    data.frame(lat = as.numeric(x$latitude), lon = as.numeric(x$longitude),
               tanggal = as.Date(x$acq_date), sensor = toupper(sensor),
               frp = if ("frp" %in% names(x)) as.numeric(x$frp) else NA_real_,
               keyakinan = if ("confidence" %in% names(x)) as.character(x$confidence) else NA_character_)
  })
  titik <- do.call(rbind, semua)
  if (is.null(titik) || !nrow(titik)) return(data.frame(lat = numeric(0), lon = numeric(0),
    tanggal = as.Date(character(0)), sensor = character(0), frp = numeric(0), keyakinan = character(0)))
  titik$jarak_km <- jarak_km(STASIUN$lat, STASIUN$lon, titik$lat, titik$lon)
  titik
}

#' Grafik PFVI dua skenario dalam satu gambar.
grafik_pfvi_banding <- function(asli, bersih) {
  a <- asli$hasil; b <- bersih$hasil
  a$skenario <- "Data asli"; b$skenario <- "Tanpa data sensor yang diduga rusak"
  g <- rbind(a[, c("waktu", "status", "PFVI", "skenario")], b[, c("waktu", "status", "PFVI", "skenario")])
  g$skenario <- factor(g$skenario, levels = c("Data asli", "Tanpa data sensor yang diduga rusak"))
  pita <- data.frame(xmin = min(g$waktu), xmax = max(g$waktu),
                     ymin = c(0, 75, 150, 225), ymax = c(75, 150, 225, 300),
                     kelas = factor(KELAS_NAMA, levels = KELAS_NAMA))
  akhir_obs <- max(a$waktu[a$status == "Observasi"])
  mulai_ekstrem <- a$waktu[a$status == "Observasi"][sum(a$status == "Observasi") - asli$ekstrem_beruntun + 1]

  ggplot() +
    geom_rect(data = pita, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = kelas), alpha = 0.13) +
    scale_fill_manual(values = WARNA_KELAS, name = NULL) +
    geom_vline(xintercept = akhir_obs, linetype = "dashed", colour = "grey45") +
    annotate("text", x = akhir_obs, y = 20, label = "ramalan →", hjust = -0.05, size = 3.3, colour = "grey30") +
    annotate("segment", x = mulai_ekstrem, xend = akhir_obs, y = 292, yend = 292, colour = WARNA_KELAS[["Ekstrem"]],
             linewidth = 0.8, arrow = grid::arrow(ends = "both", length = grid::unit(0.12, "cm"))) +
    annotate("text", x = mulai_ekstrem + (akhir_obs - mulai_ekstrem) / 2, y = 278,
             label = paste(asli$ekstrem_beruntun, "hari Ekstrem berturut-turut"), size = 3.4, colour = WARNA_KELAS[["Ekstrem"]]) +
    geom_line(data = g[g$status == "Observasi", ], aes(waktu, PFVI, colour = skenario, linetype = skenario), linewidth = 0.9) +
    geom_point(data = g[g$status == "Ramalan", ], aes(waktu, PFVI, colour = skenario), size = 2.2) +
    scale_colour_manual(values = c("Data asli" = WARNA_GARIS, "Tanpa data sensor yang diduga rusak" = "grey50"), name = NULL) +
    scale_linetype_manual(values = c("Data asli" = "solid", "Tanpa data sensor yang diduga rusak" = "22"), name = NULL) +
    scale_y_continuous(limits = c(0, 300), breaks = c(0, 75, 150, 225, 300), expand = c(0, 0)) +
    scale_x_date(breaks = seq(as.Date(format(min(g$waktu), "%Y-%m-01")), max(g$waktu), by = "1 month"),
                 labels = function(x) paste(BULAN_ID_SINGKAT[as.integer(format(x, "%m"))], format(x, "%Y"))) +
    labs(x = NULL, y = "PFVI (indeks kerawanan)") +
    tema_app() +
    theme(legend.box = "vertical", legend.spacing.y = grid::unit(0, "pt")) +
    guides(fill = guide_legend(order = 2, override.aes = list(alpha = 0.4)), colour = guide_legend(order = 1),
           linetype = guide_legend(order = 1))
}

tabel_skenario <- function(asli, bersih) {
  baris <- function(nama, r) {
    ram <- r$hasil[r$hasil$status == "Ramalan", ]
    data.frame(Skenario = nama,
               `PFVI ramalan` = paste(angka_id(min(ram$PFVI), 1), "s.d.", angka_id(max(ram$PFVI), 1)),
               `Kelas ramalan` = paste(unique(ram$kelas), collapse = ", "),
               `Hari Ekstrem (dari 192)` = as.integer(r$hari_per_kelas[["Ekstrem"]]),
               `Ekstrem berturut-turut` = r$ekstrem_beruntun, check.names = FALSE)
  }
  rbind(baris("Data asli", asli), baris("Tanpa data sensor yang diduga rusak", bersih))
}

#' Ringkasan titik panas per tanggal dalam radius tertentu dari stasiun.
tabel_titik_panas <- function(titik, radius = 25) {
  dekat <- titik[titik$jarak_km <= radius, ]
  if (!nrow(dekat)) return(NULL)
  tab <- as.data.frame.matrix(table(dekat$tanggal, dekat$sensor))
  data.frame(Tanggal = tgl_id(as.Date(rownames(tab)), singkat = TRUE), tab, check.names = FALSE, row.names = NULL)
}

#' Tabel HTML statis (untuk halaman yang tidak memakai render dari server).
tabel_html <- function(df, kanan = character(0)) {
  tags$table(class = "table table-sm align-middle",
    tags$thead(tags$tr(lapply(names(df), function(n) tags$th(n, style = if (n %in% kanan) "text-align:right")))),
    tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(names(df), function(n)
      tags$td(as.character(df[[n]][i]), style = if (n %in% kanan) "text-align:right"))))))
}

#' Peta Leaflet (JavaScript) dengan stasiun dan titik panas.
peta_ui <- function(id, hasil, titik, radius = 25, awal_ramalan = NULL, prioritas = NULL) {
  obs <- hasil$hasil[hasil$hasil$status == "Observasi", ]
  ram <- hasil$hasil[hasil$hasil$status == "Ramalan", ]
  popup_stasiun <- sprintf(
    "<b>%s</b><br>Muka air terakhir: %s cm di bawah permukaan<br>Kelas ramalan %s: <b>%s</b> (PFVI %s)<br><small>%s</small>",
    STASIUN$nama, angka_id(-obs$WT[nrow(obs)] * 100),
    paste(tgl_id(min(ram$waktu), singkat = TRUE, tahun = FALSE), "–", tgl_id(max(ram$waktu), singkat = TRUE)),
    ram$kelas[nrow(ram)], angka_id(ram$PFVI[nrow(ram)], 1), STASIUN$catatan)
  pts <- if (is.null(titik) || !nrow(titik)) "[]" else {
    dalam <- !is.null(awal_ramalan) & titik$tanggal >= awal_ramalan
    paste0("[", paste(sprintf('{"lat":%.5f,"lon":%.5f,"t":"%s","s":"%s","r":%s,"jarak":%.1f}',
      titik$lat, titik$lon, tgl_id(titik$tanggal, singkat = TRUE), titik$sensor,
      ifelse(dalam, "true", "false"), titik$jarak_km), collapse = ","), "]")
  }
  pri <- if (is.null(prioritas) || !nrow(prioritas)) "[]" else
    paste0("[", paste(sprintf('{"lat":%.5f,"lon":%.5f,"no":%d}', prioritas$lat, prioritas$lon, prioritas$no), collapse = ","), "]")
  js <- sprintf("
(function(){
  var peta = null;
  function buat(){
    if (peta || typeof L === 'undefined') return;
    var el = document.getElementById('%s'); if (!el) return;
    peta = L.map(el, {scrollWheelZoom: false}).setView([%f, %f], 10);
    var citra = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
      {maxZoom: 18, attribution: 'Citra: Esri, Maxar, Earthstar Geographics'}).addTo(peta);
    var osm = L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
      {maxZoom: 18, attribution: '&copy; kontributor OpenStreetMap'});
    L.control.layers({'Citra satelit': citra, 'Peta jalan': osm}, {}, {collapsed: false}).addTo(peta);
    L.circle([%f, %f], {radius: %d * 1000, color: '#ffffff', weight: 1.5, dashArray: '6 6', fill: false}).addTo(peta)
      .bindTooltip('Radius %d km dari stasiun');
    var titik = %s;
    titik.forEach(function(p){
      L.circleMarker([p.lat, p.lon], {radius: p.r ? 6 : 4, color: '#ffffff', weight: 1,
        fillColor: p.r ? '#C0392B' : '#E3B23C', fillOpacity: 0.95})
       .bindPopup('<b>Titik panas ' + p.s + '</b><br>' + p.t + '<br>' + p.jarak.toFixed(1) + ' km dari stasiun')
       .addTo(peta);
    });
    %s.forEach(function(q){
      L.marker([q.lat, q.lon], {icon: L.divIcon({className: 'nomor-prioritas', html: String(q.no), iconSize: [22, 22]}),
        zIndexOffset: 1000}).bindTooltip('Lokasi prioritas ' + q.no).addTo(peta);
    });
    L.circleMarker([%f, %f], {radius: 10, color: '#ffffff', weight: 3, fillColor: '#1F5F8B', fillOpacity: 1})
      .bindPopup(%s, {minWidth: 260, maxWidth: 320})
      .bindTooltip('Stasiun (klik untuk detail)', {permanent: true, direction: 'left', offset: [-10, 0]}).addTo(peta);
  }
  function segarkan(){ buat(); if (peta) setTimeout(function(){ peta.invalidateSize(); }, 150); }
  document.addEventListener('shown.bs.tab', segarkan);
  if (document.readyState !== 'loading') setTimeout(segarkan, 300); else document.addEventListener('DOMContentLoaded', function(){ setTimeout(segarkan, 300); });
})();",
    id, STASIUN$lat, STASIUN$lon, STASIUN$lat, STASIUN$lon, radius, radius, pts, pri,
    STASIUN$lat, STASIUN$lon, jsonlite_kutip(popup_stasiun))
  tagList(div(id = id, style = "height: 600px; border-radius: 8px;"), tags$script(HTML(js)))
}

#' Teks HTML aman untuk disisipkan sebagai string JavaScript.
jsonlite_kutip <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x); x <- gsub("'", "\\\\'", x); x <- gsub("\n", " ", x)
  paste0("'", x, "'")
}

#' Lokasi prioritas verifikasi: titik panas selama periode ramalan dalam radius,
#' dikelompokkan per sel ±3 km supaya deteksi MODIS dan VIIRS atas api yang sama digabung.
titik_prioritas <- function(titik, awal, akhir, radius = 25, n = 6) {
  if (is.null(titik) || !nrow(titik)) return(NULL)
  d <- titik[titik$jarak_km <= radius & titik$tanggal >= awal & titik$tanggal <= akhir, ]
  if (!nrow(d)) return(NULL)
  sel <- paste(round(d$lat / 0.03), round(d$lon / 0.03))  # sel ±3 km
  g <- do.call(rbind, lapply(split(d, sel), function(x) data.frame(
    lat = mean(x$lat), lon = mean(x$lon), deteksi = nrow(x), hari = length(unique(x$tanggal)),
    terakhir = max(x$tanggal), jarak_km = min(x$jarak_km))))
  g <- g[order(-g$deteksi, -g$hari, g$jarak_km), ]
  g <- head(g, n); g$no <- seq_len(nrow(g)); rownames(g) <- NULL
  g
}

#' Contoh langkah tindak lanjut per kelas risiko (disesuaikan dengan SOP instansi).
LANGKAH_KELAS <- list(
  Rendah = c("Pemantauan rutin data stasiun", "Pastikan sensor dan pengiriman data berfungsi"),
  Sedang = c("Tinjau tren mingguan bersama tim terkait", "Periksa kondisi sekat kanal dan muka air",
             "Siapkan rencana patroli bila tren terus naik"),
  Tinggi = c("Review hasil bersama tim terkait", "Patroli terjadwal di sekitar stasiun",
             "Periksa sekat kanal; lakukan pembasahan ulang bila muka air lebih dari 40 cm",
             "Siagakan regu pemadam setempat"),
  Ekstrem = c("Review hasil bersama tim terkait hari ini", "Verifikasi lapangan di lokasi prioritas (tabel di samping)",
              "Patroli harian dan kesiagaan regu pemadam dalam radius pemantauan",
              "Pembasahan ulang dan pemeriksaan sekat kanal di area terdekat",
              "Catat hasil verifikasi dan keputusan sebagai umpan balik model")
)
