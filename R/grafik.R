# =============================================================================
# Grafik Peatfr App (ggplot2)
# =============================================================================

WARNA_KELAS <- c(Rendah = "#4E9A5B", Sedang = "#E3B23C", Tinggi = "#E07A2F", Ekstrem = "#C0392B")
WARNA_GARIS <- "#1F5F8B"
WARNA_RAMAL <- "#C0392B"

tema_app <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          legend.position = "top",
          strip.text = element_text(face = "bold", hjust = 0))
}

#' Empat variabel input dalam satu gambar; titik yang ditandai diberi warna.
grafik_variabel <- function(d, temuan) {
  panjang <- do.call(rbind, lapply(names(VARIABEL), function(v) {
    data.frame(waktu = d$waktu, nilai = d[[v]], variabel = VARIABEL[[v]], kode = v,
               baris = seq_len(nrow(d)))
  }))
  panjang$variabel <- factor(panjang$variabel, levels = unname(VARIABEL))
  tanda <- temuan[temuan$tingkat == "peringatan", c("baris", "variabel")]
  names(tanda)[2] <- "kode"
  titik <- merge(panjang, tanda, by = c("baris", "kode"))

  ggplot(panjang, aes(waktu, nilai)) +
    geom_line(colour = WARNA_GARIS, linewidth = 0.6, na.rm = TRUE) +
    geom_point(data = titik, colour = WARNA_RAMAL, size = 2.2, na.rm = TRUE) +
    facet_wrap(~variabel, ncol = 2, scales = "free_y") +
    labs(x = NULL, y = NULL,
         title = "Data input",
         subtitle = if (nrow(titik)) "Titik merah: kemungkinan gangguan sensor atau nilai di luar kewajaran" else NULL) +
    tema_app()
}

#' PFVI historis dan ramalan dengan pita warna kelas risiko.
grafik_pfvi <- function(hasil) {
  obs <- hasil[hasil$status == "Observasi", ]
  ram <- hasil[hasil$status == "Ramalan", ]
  # Batas x pita mengikuti tipe sumbu (angka atau tanggal) supaya tidak bentrok
  pita <- data.frame(xmin = min(hasil$waktu), xmax = max(hasil$waktu),
                     ymin = c(0, 75, 150, 225), ymax = c(75, 150, 225, 300),
                     kelas = factor(KELAS_NAMA, levels = KELAS_NAMA))
  batas_ramal <- obs$waktu[nrow(obs)]

  g <- ggplot() +
    geom_rect(data = pita, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = kelas),
              alpha = 0.13) +
    scale_fill_manual(values = WARNA_KELAS, name = "Kelas PFVI") +
    geom_vline(xintercept = batas_ramal, linetype = "dashed", colour = "grey45") +
    geom_line(data = obs, aes(waktu, PFVI), colour = WARNA_GARIS, linewidth = 0.9) +
    geom_point(data = ram, aes(waktu, PFVI), colour = WARNA_RAMAL, size = 2.6) +
    scale_y_continuous(limits = c(0, 300), breaks = c(0, 75, 150, 225, 300), expand = c(0, 0)) +
    labs(x = NULL, y = "PFVI",
         title = "Indeks kerawanan kebakaran gambut (PFVI)",
         subtitle = "Garis biru: hasil hitung dari data observasi. Titik merah: ramalan. Garis putus-putus: akhir data observasi.") +
    tema_app() +
    guides(fill = guide_legend(override.aes = list(alpha = 0.4)))
  # Label bulan berbahasa Indonesia kalau sumbu berupa tanggal
  if (inherits(hasil$waktu, "Date")) {
    g <- g + scale_x_date(
      breaks = seq(as.Date(format(min(hasil$waktu), "%Y-%m-01")), max(hasil$waktu), by = "1 month"),
      labels = function(x) paste(BULAN_ID_SINGKAT[as.integer(format(x, "%m"))], format(x, "%Y")))
  }
  g
}
