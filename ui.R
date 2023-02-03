library(shiny)
library(tidyverse)
library(plotly)

kategori_kemiskinan <- c(
  "Indeks Kedalaman Kemiskinan",
  "Indeks Keparahan Kemiskinan",
  "Persentase Penduduk Miskin"
)

kategori_konsumsi <- c(
  "Bahan Makanan Lainnya",
  "Bahan Minuman",
  "Buah-Buahan",
  "Bumbu-Bumbuan",
  "Daging",
  "Ikan",
  "Kacang-Kacangan",
  "Makanan Minuman Jadi",
  "Minyak dan Kelapa",
  "Padi-Padian",
  "Rokok dan Tembakau",
  "Sayur-Sayuran",
  "Telur dan Susu",
  "Umbi-Umbian"
)

provinsi <- c("semua (gabungan)", "semua",
              "ACEH","SUMATERA UTARA","SUMATERA BARAT",
              "RIAU","JAMBI","SUMATERA SELATAN",
              "BENGKULU","LAMPUNG","KEP. BANGKA BELITUNG",
              "KEPULAUAN RIAU","DKI JAKARTA","JAWA BARAT",
              "JAWA TENGAH","D I YOGYAKARTA","JAWA TIMUR",
              "BANTEN","BALI","NUSA TENGGARA BARAT",
              "NUSA TENGGARA TIMUR","KALIMANTAN BARAT","KALIMANTAN TENGAH",
              "KALIMANTAN SELATAN","KALIMANTAN TIMUR","KALIMANTAN UTARA",
              "SULAWESI UTARA","SULAWESI TENGAH","SULAWESI SELATAN",
              "SULAWESI TENGGARA","GORONTALO","SULAWESI BARAT",
              "MALUKU","MALUKU UTARA","PAPUA BARAT",
              "PAPUA")

indeks_infrastruktur <- c("rasio_luas_wilayah_panjang_jalan",
                          "rasio_jumlah_penduduk_panjang_jalan")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Miskin dan Boros Akibat Kesenjangan Infrastruktur"),
  
  # Background story
  fluidRow(
    column(12,
           p(style = "text-align: justify;",
           "Secara logis, sering diasumsikan bahwa masyarakat yang hidup di bawah 
           garis kemiskinan adalah mereka yang berpenghasilan rendah, sehingga cenderung 
           memiliki rata-rata pengeluaran untuk konsumsi yang juga bernilai rendah. 
           Namun, asumsi ini terpatahkan oleh berita tentang masyarakat di Papua yang
           -- walaupun berada di bawah garis kemiskinan -- membeli komoditas dengan 
           harga jauh lebih mahal ketimbang daerah lain di Indonesia. Salah satu 
           penyebab disparitas harga komoditas tersebut adalah", 
           strong("rendahnya tingkat aksesibilitas daerah,"), 
           "sehingga biaya logistik pun membengkak."
             ),
           p(style = "text-align: justify;",
             "Tingkat aksesibilitas daerah dapat diukur secara kuantitatif dari
             rasio luas wilayahnya terhadap panjang jalan yang tersedia. Semakin besar
             nilai rasionya, maka semakin besar kesenjangan infrastruktur daerah tersebut,
             yang berarti tingkat aksesibilitasnya rendah. Selain itu, bisa juga
             digunakan rasio jumlah penduduk suatu daerah terhadap panjang jalan yang tersedia.
             "
           ),
           p(style = "text-align: justify;",
             "Dilansir dari BPS, Indeks Kedalaman Kemiskinan adalah ukuran rata-rata 
             kesenjangan pengeluaran tiap penduduk miskin terhadap garis kemiskinan.
             Sedangkan Indeks Keparahan Kemiskinan memberikan gambaran mengenai 
             penyebaran pengeluaran di antara penduduk miskin. Pengeluaran rata-rata 
             per kapita adalah biaya yang dikeluarkan untuk konsumsi semua anggota 
             rumah tangga selama sebulan dibagi dengan banyaknya anggota rumah 
             tangga dalam rumah tangga tersebut.
             "
           ),
           p(style = "text-align: justify;",
           "Berikut adalah visualisasi untuk keterkaitan antara kemiskinan,
           rata-rata pengeluaran untuk konsumsi, dan indeks kesenjangan infrastruktur suatu 
           daerah kabupaten/kota di Indonesia. Sebagai contoh, untuk konsumsi daging di Indonesia,
           rata-rata pengeluaran terkorelasi positif dengan indeks kedalaman kemiskinan.
           Keduanya juga terkorelasi positif dengan indeks kesenjangan infrastruktur."
             )
           )
  ),
  
  # Input dari user
  fluidRow(
    column(4,
           "Ukuran Kemiskinan",
           selectInput(inputId = "kategori_kemiskinan",
                       label = "Pilih ukuran kemiskinan",
                       choices = kategori_kemiskinan)),
    column(8,
           "Rata-rata Pengeluaran per Kapita Seminggu",
           selectInput(inputId = "kategori_konsumsi",
                       label = "Pilih kategori konsumsi",
                       choices = kategori_konsumsi,
                       selected = "Daging"))
  ),
  
  # Output scatterplot ukuran kemiskinan (X) vs subkategori rata-rata pengeluaran (Y_i) 
  sidebarLayout(
    # Panel untuk input subkategori konsumsi, filter provinsi, filter indeks infrastruktur
    sidebarPanel( uiOutput("select_subkategori_konsumsi"),
                  selectInput(inputId = "filter_provinsi",
                              label = "Pilih provinsi",
                              choices = provinsi,
                              multiple = T,
                              selected = provinsi[1]),
                  selectInput(inputId = "filter_indeks_jalan",
                              label = "Pilih Indeks Kesenjangan Infrastruktur",
                              choices = indeks_infrastruktur)
    ),
    # Scatterplot
    mainPanel(
      h3("Rata-Rata Pengeluaran per Kapita Seminggu"),
      h5("Berdasarkan data per kabupaten/kota tahun 2021"),
      plotlyOutput("scatterplot")
    )
  ),
  
  # Output barplot/lolipop nilai korelasi X dan Y_i for all i
  sidebarLayout(
    # Panel untuk input filter provinsi
    sidebarPanel(selectInput(inputId = "filter_provinsi_2",
                              label = "Pilih provinsi",
                              choices = provinsi[-1],
                             multiple = T,
                             selected = "semua")
    ),
    # Barplot
    mainPanel(
      h3("Korelasi Kemiskinan dengan Rata-rata Pengeluaran"),
      h5("Berdasarkan data per kabupaten/kota tahun 2021"),
      plotOutput("barplot_X_Yi")
    )
  ),
  
  # Output barplot/lolipop nilai korelasi X dan indeks infrastruktur (Z)
  sidebarLayout(
    # Panel untuk input filter provinsi
    sidebarPanel(selectInput(inputId = "filter_provinsi_3",
                             label = "Pilih provinsi",
                             choices = provinsi[-1],
                             multiple = T,
                             selected = "semua")
    ),
    # Barplot
    mainPanel(
      h3("Korelasi Kemiskinan dengan Indeks Kesenjangan Infrastruktur"),
      h5("Indeks Kesenjangan  Infrasktruktur dibuat berdasarkan data per kabupaten/kota tahun 2018 - 2021"),
      plotOutput("barplot_X_Z")
    )
  ),
  
  # Output barplot/lolipop nilai korelasi Y_i dan indeks infrastruktur (Z)
  sidebarLayout(
    # Panel untuk input subkategori konsumsi dan filter provinsi
    sidebarPanel(selectInput(inputId = "filter_indeks_jalan_2",
                             label = "Pilih Indeks Kesenjangan Infrastruktur",
                             choices = indeks_infrastruktur), 
                 selectInput(inputId = "filter_provinsi_4",
                             label = "Pilih provinsi",
                             choices = provinsi[-1],
                             multiple = T,
                             selected = "semua")
    ),
    # Barplot
    mainPanel(
      h3("Korelasi Rata-rata Pengeluaran dengan Indeks Kesenjangan Infrastruktur"),
      h5("Indeks Kesenjangan Infrasktruktur dibuat berdasarkan data per kabupaten/kota tahun 2018 - 2021"),
      plotOutput("barplot_Yi_Z")
    )
  )
  
  
  # fluidRow(
  #   h3("Korelasi"),
  #   plotOutput("barplot")
  # )
)
