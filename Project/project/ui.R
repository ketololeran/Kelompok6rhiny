library(shiny)
library(shinythemes)
library(ggplot2)
library(nortest)
library(shinyjs)
library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title= tagList(tags$img(src = "Logo_Aplikasi.png", height = "40px", style = "margin-right: 10px;"),"Aplikasi Uji Normalitas"), titleWidth = 650, 
                  tags$li(class="dropdown",tags$a(href="https://www.youtube.com/", icon("youtube"), "Tutorial", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://www.youtube.com/", icon("book"), "Guidebook", target="_blank"))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),  # Make sure tabName is 'welcome'
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Uji Normalitas", tabName = "normality", icon = icon("check-circle"))
    )
  ),
  dashboardBody(
    useShinyjs(),  # Memungkinkan penggunaan shinyjs untuk kontrol tampilan
    theme = shinytheme("slate"),  # Tema biru muda
    
    tabItems(
      # Tab Welcome
      tabItem(
        tabName = "welcome",
        fluidRow(
          column(
            width = 12,
            box(
              title = tagList(icon("smile"),"Selamat Datang!"), status = "primary", solidHeader = TRUE, width = 12,
              tags$h2("Selamat datang di Aplikasi Uji Normalitas!", style = "color: #2c3e50; font-weight: bold; text-align: center;"),
              tags$div(
                style = "text-align: center; margin-top: 20px;",
                tags$img(src = "Logo_Aplikasi.png", alt = "Logo Aplikasi", width = "300px")
              ),
              tags$p(
                "Aplikasi ini dirancang untuk mempermudah Anda dalam melakukan analisis statistik, khususnya uji normalitas. 
                Anda dapat mengunggah data dalam format CSV, memilih variabel yang ingin diuji, dan melakukan berbagai uji normalitas 
                dengan hasil yang langsung dapat dipahami.",
                style = "font-size: 16px; color: #34495e; text-align: justify;"
              ),
              tags$h4("Pengembang Aplikasi:", style = "color: #7f8c8d; text-align: center;"),
              tags$ul(
                tags$li("Innes Septian Amelia Putri (212313144)",style = "font-size: 16px; color: #7f8c8d;"),
                tags$li("Lya Karimatul Fitri (212313180)", style = "font-size: 16px; color: #7f8c8d;"),
                tags$li("Lydia Aushaf Ozora Siregar (212313181)", style = "font-size: 16px; color: #7f8c8d;"),
                tags$li("Juan Stevan Rehatta (212313154)", style = "font-size: 16px; color: #7f8c8d;"),
                tags$li("Muhammad Lazuardi Said Ahimsa B (212313247)", style = "font-size: 16px; color: #7f8c8d;")
              ),
              tags$p(
                "Kami berharap aplikasi ini dapat memberikan manfaat dan kemudahan dalam melakukan analisis statistik. 
                Selamat mencoba dan semoga berhasil!",
                style = "font-size: 16px; color: #34495e; text-align: justify;"
              )
            )
          )
        )
      ),
      
      # Tab About
      tabItem(
        tabName = "about",
        box(
          title = tagList(icon("address-card"),"Tentang Aplikasi"), status = "primary", solidHeader = TRUE, width = 12,
          tags$p("Aplikasi Uji Normalitas ini dirancang untuk membantu pengguna melakukan pengujian distribusi normal terhadap data numerik. Aplikasi ini mendukung berbagai jenis uji statistik seperti Shapiro-Wilk, Lilliefors, Goodness of Fit (Chi-Square), Kolmogorov-Smirnov."),
          tags$p("Pengguna hanya perlu mengunggah file CSV yang berisi data numerik, memilih variabel yang akan diuji, dan memilih jenis uji normalitas yang diinginkan."),
          tags$h5("Fitur utama aplikasi meliputi:"),
          tags$ul(
            tags$li("Visualisasi data menggunakan histogram dan Q-Q plot."),
            tags$li("Hasil uji statistik dengan interpretasi langsung.")
          ),
          tags$h5("Cara Penggunaan:"),
          tags$ol(
            tags$li("Unggah file CSV yang berisi data numerik melalui panel sebelah kiri."),
            tags$li("Pilih variabel yang akan diuji dari dropdown menu."),
            tags$li("Centang jenis uji normalitas yang ingin dilakukan."),
            tags$li("Lihat hasil uji di tab 'Uji Normalitas', yang mencakup grafik histogram, Q-Q plot, dan hasil uji statistik.")
          ),
          tags$h5("Catatan Penting:"),
          tags$ul(
            tags$li("Pastikan file CSV memiliki header dan kolom berisi data numerik."),
            tags$li("Hasil uji statistik hanya akurat jika data yang dimasukkan valid."),
            tags$li("Setiap uji statistik memiliki asumsi dan metode yang berbeda, sehingga disarankan untuk membaca hasil dengan hati-hati.")
          ),
          tags$p("Aplikasi ini dikembangkan menggunakan R dan paket Shiny untuk memudahkan analisis data statistik.")
        )
      ),
      
      # Tab Uji Normalitas
      tabItem(
        tabName = "normality",
        fluidRow(
          column(
            width = 4,
            box(
              title = tagList(icon("wrench"),"Pengaturan"),status = "primary", solidHeader = TRUE, width = 12,
              fileInput("file", "Unggah File CSV", accept = ".csv"),
              selectInput("var_normal", "Pilih Variabel untuk Uji Normalitas", choices = NULL),
              checkboxGroupInput(
                "selected_tests", 
                "Pilih Uji Normalitas yang Ingin Dilakukan:", 
                choices = c("Shapiro-Wilk", "Lilliefors", "Goodness of Fit (Chi-Square)", "Kolmogorov-Smirnov"),
                selected = c("Shapiro-Wilk", "Lilliefors", "Goodness of Fit (Chi-Square)", "Kolmogorov-Smirnov")
              )
            )
          ),
          column(
            width = 8,
            box(
              title = tagList(icon("chart-line"),"Hasil Uji Normalitas"), status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(6, plotOutput("plot_normality", height = "400px")),
                column(6, plotOutput("qq_plot", height = "400px"))
              ),
              conditionalPanel(
                condition = "input.selected_tests.includes('Shapiro-Wilk')",
                wellPanel(
                  style = "background-color: #f7f7f7; border-radius: 10px; padding: 15px; border: 1px solid #ddd;",
                  h5("Shapiro-Wilk Test", style = "color: #2c3e50;"),
                  verbatimTextOutput("shapiro_test")
                )
              ),
              conditionalPanel(
                condition = "input.selected_tests.includes('Lilliefors')",
                wellPanel(
                  style = "background-color: #f7f7f7; border-radius: 10px; padding: 15px; border: 1px solid #ddd;",
                  h5("Lilliefors Test", style = "color: #2c3e50;"),
                  verbatimTextOutput("lilliefors_test")
                )
              ),
              conditionalPanel(
                condition = "input.selected_tests.includes('Goodness of Fit (Chi-Square)')",
                wellPanel(
                  style = "background-color: #f7f7f7; border-radius: 10px; padding: 15px; border: 1px solid #ddd;",
                  h5("Goodness of Fit Test (Chi-Square)", style = "color: #2c3e50;"),
                  verbatimTextOutput("goodness_of_fit_test")
                )
              ),
              conditionalPanel(
                condition = "input.selected_tests.includes('Kolmogorov-Smirnov')",
                wellPanel(
                  style = "background-color: #f7f7f7; border-radius: 10px; padding: 15px; border: 1px solid #ddd;",
                  h5("Kolmogorov-Smirnov Test", style = "color: #2c3e50;"),
                  verbatimTextOutput("ks_test")
                )
              )
            )
          )
        )
      )
    )
  )
)
