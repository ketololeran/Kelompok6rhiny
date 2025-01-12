# Server
server <- function(input, output, session) {
  # Load dataset
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Update variable choices
  observe({
    data <- dataset()
    updateSelectInput(session, "var_normal", choices = names(data))
  })
  
  # Uji Normalitas - Histogram
  output$plot_normality <- renderPlot({
    req(input$var_normal)
    data <- dataset()
    
    ggplot(data, aes_string(x = input$var_normal)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "#3498db", color = "black", alpha = 0.7) +
      geom_density(color = "red", size = 1.2) +  # Garis density merah
      labs(title = paste("Histogram dan Kurva Kepadatan untuk", input$var_normal), 
           x = input$var_normal, y = "Density") +
      theme_minimal(base_size = 14) +  # Tema minimalis
      theme(
        plot.title = element_text(hjust = 0.5, color = "#2c3e50", size = 16, face = "bold"),
        axis.title = element_text(size = 14, color = "#2c3e50"),
        axis.text = element_text(size = 12, color = "#7f8c8d"),
        panel.grid.major = element_line(color = "#ecf0f1"),
        panel.grid.minor = element_line(color = "#ecf0f1", linetype = "dashed")
      )
  })
  
  # Uji Normalitas - Q-Q Plot
  output$qq_plot <- renderPlot({
    req(input$var_normal)
    data <- dataset()
    
    qqnorm(data[[input$var_normal]], main = paste("Q-Q Plot untuk", input$var_normal), col = "#e74c3c", pch = 16)
    qqline(data[[input$var_normal]], col = "blue", lwd = 2)
  })
  
  # Shapiro-Wilk Test
  output$shapiro_test <- renderPrint({
    req(input$var_normal)
    data <- dataset()
    result <- shapiro.test(data[[input$var_normal]])
    cat("Shapiro-Wilk Test:\n")
    print(result)
    if (result$p.value > 0.05) {
      cat("\nKesimpulan: Data berdistribusi normal.\n")
      cat("Alasan: Nilai p-value lebih besar dari 0.05, sehingga tidak ada cukup bukti untuk menolak 
        hipotesis nol (H0). H0 menyatakan bahwa data berasal dari distribusi normal.\n")
    } else {
      cat("\nKesimpulan: Data tidak berdistribusi normal.\n")
      cat("Alasan: Nilai p-value lebih kecil dari 0.05, sehingga hipotesis nol (H0) ditolak. Ini 
        menunjukkan bahwa data tidak berasal dari distribusi normal.\n")
    }
  })
  
  # Lilliefors Test
  output$lilliefors_test <- renderPrint({
    req(input$var_normal)
    data <- dataset()
    result <- lillie.test(data[[input$var_normal]])
    cat("Lilliefors Test:\n")
    print(result)
    if (result$p.value > 0.05) {
      cat("\nKesimpulan: Data berdistribusi normal.\n")
      cat("Alasan: Nilai p-value lebih besar dari 0.05, sehingga tidak ada cukup bukti untuk menolak 
        hipotesis nol (H0). H0 menyatakan bahwa data berasal dari distribusi normal.\n")
    } else {
      cat("\nKesimpulan: Data tidak berdistribusi normal.\n")
      cat("Alasan: Nilai p-value lebih kecil dari 0.05, sehingga hipotesis nol (H0) ditolak. Data 
        menunjukkan pola yang tidak konsisten dengan distribusi normal.\n")
    }
  })
  
  # Goodness of Fit Test (Chi-Square)
  output$goodness_of_fit_test <- renderPrint({
    req(input$var_normal)
    data <- dataset()
    result <- chisq.test(table(data[[input$var_normal]]))
    cat("Goodness of Fit (Chi-Square) Test:\n")
    print(result)
    if (result$p.value > 0.05) {
      cat("\nKesimpulan: Data berdistribusi normal (sesuai dengan distribusi yang diharapkan).\n")
      cat("Alasan: Nilai p-value lebih besar dari 0.05, sehingga tidak ada cukup bukti untuk menolak 
        hipotesis nol (H0). Data sesuai dengan distribusi normal yang diharapkan.\n")
    } else {
      cat("\nKesimpulan: Data tidak berdistribusi normal (perbedaan signifikan).\n")
      cat("Alasan: Nilai p-value lebih kecil dari 0.05, sehingga hipotesis nol (H0) ditolak. Data 
        menunjukkan adanya perbedaan signifikan dari distribusi normal yang diharapkan.\n")
    }
  })
  
  # Kolmogorov-Smirnov Test
  output$ks_test <- renderPrint({
    req(input$var_normal)
    data <- dataset()
    result <- ks.test(data[[input$var_normal]], "pnorm")
    cat("Kolmogorov-Smirnov Test:\n")
    print(result)
    if (result$p.value > 0.05) {
      cat("\nKesimpulan: Data berdistribusi normal.\n")
      cat("Alasan: Nilai p-value lebih besar dari 0.05, sehingga tidak ada cukup bukti untuk menolak 
        hipotesis nol (H0). Data tidak menunjukkan perbedaan signifikan dari distribusi normal.\n")
    } else {
      cat("\nKesimpulan: Data tidak berdistribusi normal.\n")
      cat("Alasan: Nilai p-value lebih kecil dari 0.05, sehingga hipotesis nol (H0) ditolak. Ini 
        menunjukkan adanya perbedaan signifikan antara data dan distribusi normal.\n")
    }
  })
}