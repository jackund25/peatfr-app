# Menghitung ulang hasil Sabangau untuk halaman presentasi.
# Jalankan dari folder peatfr-app:  Rscript presentasi/hitung_sabangau.R
source("R/pipeline.R")
env <- muat_peatfr()
raw <- baca_csv("data/Dataset_Sabangau.csv")
d <- siapkan_data(raw, tebak_kolom(names(raw)))
d$waktu <- as.Date("2023-04-01") + seq_len(nrow(d)) - 1   # periode menurut paper
attr(d, "pakai_tanggal") <- TRUE
q <- cek_kualitas(d)
sensor <- q$temuan$baris[q$temuan$tingkat == "peringatan" & grepl("gangguan sensor", q$temuan$alasan)]
cat("Menjalankan skenario asli...\n")
asli <- jalankan_peatfr(env, d, h = 4)
cat("Menjalankan skenario tanpa gangguan sensor...\n")
bersih <- jalankan_peatfr(env, d, h = 4, kecualikan = sensor)
asli$log <- NULL; bersih$log <- NULL
saveRDS(list(data = d, kualitas = q, asli = asli, bersih = bersih,
             dibuat = Sys.time(), sumber = attr(env, "sumber")),
        "presentasi/hasil_sabangau.rds")
cat("Selesai. PFVI ramalan asli:", round(tail(asli$hasil$PFVI, 4), 1),
    "| tanpa gangguan:", round(tail(bersih$hasil$PFVI, 4), 1), "\n")
cat("Ekstrem beruntun asli/bersih:", asli$ekstrem_beruntun, bersih$ekstrem_beruntun, "\n")
print(asli$hari_per_kelas); print(bersih$hari_per_kelas)
