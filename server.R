# Load package
library(shiny)
library(tidyverse)
library(janitor)
library(ggcorrplot)

# Define needed function
korelasi <- function(v1, v2, df) {
  cor_value <- cor(
    df %>% select(v1, v2),
    use = "complete.obs"
  )[1,2]
  
  return(cor_value)
}

signif_korelasi <- function(v1, v2, df, a=0.05) {
  cor_pval <- cor_pmat(
    df %>% select(v1, v2),
    use = "complete.obs"
  )[1,2]
  
  if(cor_pval < a){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

# Load fixed dataset
df_indeks_jalan <- read.csv("./indeks_infrastruktur_menurut_kab_kota.csv")

# Define server logic required
server <- function(input, output) {
   
    #####checking 
    # output$output1 <- renderPrint(input$filter_provinsi)
    # output$output2 <- renderPrint(input$kategori_konsumsi)
    # output$output3 <- renderTable(head(df_konsumsi()))
    # output$output4 <- renderTable(head(df_kemiskinan()))
    
    # Import tabel dataset
    df_konsumsi <- reactive({
        read.csv(
            paste("./ratarata_pengeluaran_konsumsi_perkapita_perminggu_",
                  str_replace_all(str_to_lower(input$kategori_konsumsi), 
                                  "[ -]", "_"),
                  ".csv", sep="")) %>%
            select(-X)
    }) 
    
    df_kemiskinan <- reactive({
        read.csv(
            paste("./",
                  str_replace_all(str_to_lower(input$kategori_kemiskinan), 
                                  "[ -]", "_"),
                  ".csv", sep = "")) %>%
            select(-X)
    })
    
    # Hasilkan select-box subkategori konsumsi
    observe({
        output$select_subkategori_konsumsi <- renderUI({
            selectInput(inputId = "subkategori_konsumsi",
                        label = "Pilih Subkategori Konsumsi",
                        choices = colnames(df_konsumsi()[-1])
            )  
        })
    })
    
    # Hasilkan select-box subkategori_konsumsi_2
    observe({
      output$select_subkategori_konsumsi_2 <- renderUI({
        selectInput(inputId = "subkategori_konsumsi_2",
                    label = "Pilih Subkategori Konsumsi",
                    choices = colnames(df_konsumsi()[-1])
        )  
      })
    })
    
    # Hasilkan scatterplot X vs Y_i 
    output$scatterplot <- renderPlotly({
  
      df_joined <-
        inner_join(df_konsumsi(), df_kemiskinan(),
                    by = c("kabupaten_kota"="nama_wilayah")) %>%
        inner_join(df_indeks_jalan, by = c("kabupaten_kota", "provinsi"))
      
      df_scatter <-
        df_joined %>% 
        pivot_longer(colnames(df_konsumsi())[-1], 
                     names_to = "kategori", 
                     values_to = "rata_rata_pengeluaran") %>%
        select(kabupaten_kota,
               provinsi,
               value_kemiskinan = contains(c("indeks","persentase")),
               kategori,
               rata_rata_pengeluaran, 
               indeks_infrastruktur = matches(input$filter_indeks_jalan)) %>%
        filter(kategori == input$subkategori_konsumsi) 
      
      
      if(!grepl("semua",input$filter_provinsi)) {
        df_scatter <- df_scatter %>% 
          filter(provinsi %in% input$filter_provinsi)
      }
      
      plt_scatter <- df_scatter %>%
        ggplot(mapping = aes(x = value_kemiskinan,
                             y = rata_rata_pengeluaran,
                             kabupaten_kota = kabupaten_kota,
                             size = indeks_infrastruktur
                             )
               ) +
        geom_point(stroke = NA) +
        geom_smooth(inherit.aes = FALSE, 
                    aes(value_kemiskinan,rata_rata_pengeluaran), 
                    method = "lm", se=F) +
        labs(x=input$kategori_kemiskinan,
             y="(Rp/kapita/minggu)",
             fill = "Rasio jumlah penduduk \nterhadap panjang jalan",
             caption = paste0("untuk konsumsi ",
                            input$subkategori_konsumsi)
             )
      
      if(input$filter_provinsi == "semua") {
        plt_scatter <- plt_scatter + 
          facet_wrap(~provinsi)
      }
      
      ggplotly(plt_scatter, tooltip = c("x","y","kabupaten_kota",
                                        "indeks_infrastruktur"
                                        )
               ) 
    })
    
    # Hasilkan barplot nilai korelasi indeks kemiskinan terhadap rata-rata pengeluaran
    # per subkategori konsumsi
    output$barplot_X_Yi <- renderPlot({

      subkategori_konsumsi <- colnames(df_konsumsi())[-1]

      df_joined <-
        inner_join(df_konsumsi(), df_kemiskinan(),
                   by = c("kabupaten_kota"="nama_wilayah")) %>%
        select(kabupaten_kota,
               provinsi,
               colnames(df_konsumsi())[-1],
               value_kemiskinan = contains(c("indeks","persentase")))
      
      if(!grepl("semua",input$filter_provinsi_2)){
        df_joined <- df_joined %>% 
          filter(provinsi %in% input$filter_provinsi_2)
      }

      corr_value <- c()
      signif_corr_value <- c()
      for(i in subkategori_konsumsi) {
        corr_value <- append(corr_value,
                             korelasi(i,"value_kemiskinan", df_joined))
        signif_corr_value <- append(signif_corr_value,
                                    signif_korelasi(i,"value_kemiskinan", df_joined))
      }
      df_bar <- data.frame(subkategori_konsumsi, corr_value, signif_corr_value)
      # df_bar
      
      plt_bar <- df_bar %>%
        mutate(subkategori_konsumsi = fct_reorder(subkategori_konsumsi, corr_value)) %>%
        ggplot(aes(x=corr_value, y=subkategori_konsumsi)) +
        # geom_col() +
        geom_segment(aes(x = 0, xend = corr_value, 
                         y = subkategori_konsumsi, yend = subkategori_konsumsi,
                         colour = signif_corr_value),
                     size = 2) +
        geom_point(shape = 20, size = 4) +
        geom_vline(xintercept = 0) +
        geom_vline(xintercept = 0.5, linetype = 3) +
        geom_vline(xintercept = -0.5, linetype = 3) +
        labs(y="Subkategori Konsumsi",
             x=paste0("Nilai korelasi terhadap ", input$kategori_kemiskinan),
             colour = "Signifikansi korelasi \n(p-value < 5%)") +
        xlim(-1,1) +
        # theme_ipsum(grid = F)
        theme_classic() +
        theme(axis.line.y = element_blank(),
              axis.ticks.y = element_blank())

      plt_bar
    })
    
    # Hasilkan barplot nilai korelasi indeks kemiskinan terhadap indeks infrastruktur
    output$barplot_X_Z <- renderPlot({
      
      df_joined <-
        inner_join(df_indeks_jalan, df_kemiskinan(),
                  by = c("kabupaten_kota"="nama_wilayah", "provinsi")) %>%
        select(kabupaten_kota,
               provinsi,
               value_kemiskinan = contains(c("indeks","persentase")),
               contains("rasio"))
      
      if(!grepl("semua", input$filter_provinsi_3)){
        df_joined <- df_joined %>% 
          filter(provinsi %in% input$filter_provinsi_3)
      }
      # head(df_joined)
      
      indeks_infrastruktur <- c("rasio_jumlah_penduduk_panjang_jalan","rasio_luas_wilayah_panjang_jalan")
      corr_value <- c()
      signif_corr_value <- c()
      for(i in indeks_infrastruktur) {
        corr_value <- append(corr_value,
                             korelasi(i, "value_kemiskinan", df_joined))
        signif_corr_value <- append(signif_corr_value,
                                    signif_korelasi(i,"value_kemiskinan", df_joined))
      }
      
      df_bar <- data.frame(indeks_infrastruktur, corr_value, signif_corr_value)
      # df_bar
      
      plt_bar <- df_bar %>%
        ggplot(aes(x=corr_value, y=indeks_infrastruktur)) +
        # geom_col() +
        geom_segment(aes(x = 0, xend = corr_value, 
                         y = indeks_infrastruktur, yend = indeks_infrastruktur,
                         colour = signif_corr_value),
                     size = 2) +
        geom_point(shape = 20, size = 4) +
        geom_vline(xintercept = 0) +
        geom_vline(xintercept = 0.5, linetype = 3) +
        geom_vline(xintercept = -0.5, linetype = 3) +
        labs(y="Indeks Kesenjangan Infrastruktur",
             x=paste0("Nilai korelasi terhadap ", input$kategori_kemiskinan),
             colour = "Signifikansi korelasi \n(p-value < 5%)") +
        xlim(-1,1) +
        # theme_ipsum(grid = F)
        theme_classic() +
        theme(axis.line.y = element_blank(),
              axis.ticks.y = element_blank())
      
      plt_bar
    })
    
    # Hasilkan barplot nilai korelasi indeks kemiskinan terhadap rata-rata pengeluaran
    # per subkategori konsumsi
    output$barplot_Yi_Z <- renderPlot({
      
      subkategori_konsumsi <- colnames(df_konsumsi())[-1]
      
      df_joined <-
        inner_join(df_konsumsi(), df_indeks_jalan,
                    by = "kabupaten_kota") %>%
        select(kabupaten_kota,
               provinsi,
               colnames(df_konsumsi())[-1],
               indeks_infrastruktur = matches(input$filter_indeks_jalan_2))
      
      if(!input$filter_provinsi_4 %in% c("semua")){
        df_joined <- df_joined %>% 
          filter(provinsi == input$filter_provinsi_4)
      }
      
      corr_value <- c()
      signif_corr_value <- c()
      for(i in subkategori_konsumsi) {
        corr_value <- append(corr_value,
                             korelasi(i,"indeks_infrastruktur", df_joined))
        signif_corr_value <- append(signif_corr_value,
                                    signif_korelasi(i,"indeks_infrastruktur", df_joined))
      }
      df_bar <- data.frame(subkategori_konsumsi, corr_value, signif_corr_value)
      # df_bar
      
      plt_bar <- df_bar %>%
        mutate(subkategori_konsumsi = fct_reorder(subkategori_konsumsi, corr_value)) %>%
        ggplot(aes(x=corr_value, y=subkategori_konsumsi)) +
        # geom_col() +
        geom_segment(aes(x = 0, xend = corr_value, 
                         y = subkategori_konsumsi, yend = subkategori_konsumsi,
                         colour = signif_corr_value),
                     size = 2) +
        geom_point(shape = 20, size = 4) +
        geom_vline(xintercept = 0) +
        geom_vline(xintercept = 0.5, linetype = 3) +
        geom_vline(xintercept = -0.5, linetype = 3) +
        labs(y="Subkategori Konsumsi",
             x=paste0("Nilai korelasi terhadap ", input$filter_indeks_jalan_2),
             colour = "Signifikansi korelasi \n(p-value < 5%)") +
        xlim(-1,1) +
        # theme_ipsum(grid = F)
        theme_classic() +
        theme(axis.line.y = element_blank(),
              axis.ticks.y = element_blank())
      
      plt_bar
    })
    
}

# # Run the application 
# shinyApp(ui = ui, server = server)
