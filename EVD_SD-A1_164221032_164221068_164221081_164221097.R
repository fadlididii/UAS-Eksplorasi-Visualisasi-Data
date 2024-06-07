# load the shinydashboard package
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)
library(MASS)
library(dashboardthemes)
library(Rdimtools)

dataset <- read.csv("data_mobile_legends.csv", header = TRUE, stringsAsFactors = FALSE)
hilang_na = dataset
hilang_na[is.na(hilang_na)] <- 0

total_hero <- nrow(dataset)
buff_count <- sum(!is.na(dataset$buff))
blink_count <- sum(!is.na(dataset$blink))

skill_columns <- colnames(dataset)[2:ncol(dataset)]

ui <- dashboardPage(
  dashboardHeader(title = "Menu"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Pendahuluan", tabName = "pendahuluan", icon = icon("info-circle")),
      menuItem("Data", tabName = "pengenalan", icon = icon("cog")),
      menuItem("Visualisasi", tabName = "visualisasi", icon = icon("bar-chart")),
      menuItem("Analisis", tabName = "analisis", icon = icon("sitemap"))
    )
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "purple_gradient"
    ),

    tags$head(
      tags$style(HTML("
        h1, h2 {
          text-align: center;
          font-weight: bold;
        }
        p {
          text-align: justify;
          font-size: 18px;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "pendahuluan",
        h1("Analisis Kluster Karakteristik Hero dalam Mobile Legends"),
        h2("Latar Belakang"),
        p('Perkembangan teknologi di era digital membawa dampak signifikan pada industri hiburan, khususnya dalam dunia permainan. Salah satu fenomena yang mencolok adalah popularitas permainan mobile online, dengan Mobile Legends sebagai salah satu permainan yang mendominasi pasar. Mobile Legends, sebuah permainan MOBA (Multiplayer Online Battle Arena), telah menarik perhatian jutaan pemain di seluruh dunia.'),
        p('Tentunya, popularitas Mobile Legends tidak terjadi tanpa alasan yang kuat. Faktor pertama adalah aksesibilitasnya yang tinggi, memungkinkan pemain untuk menikmati permainan di perangkat seluler kapan saja dan di mana saja tanpa terbatas oleh perangkat keras khusus. Selain itu, grafis yang memukau dengan desain karakter yang menarik, efek khusus yang canggih, dan peta permainan yang dirancang dengan baik menciptakan pengalaman visual yang menghibur. Keunikan Mobile Legends sebagai permainan MOBA turut berkontribusi pada popularitasnya. Pemain dapat berkolaborasi dalam tim untuk menyelesaikan misi, menghadapi tantangan bersama, dan bersaing secara real-time dengan pemain lain'),
        p('Salah satu aspek lain yang krusial dari pengalaman bermain adalah pemilihan karakter atau yang dikenal sebagai "hero." Setiap hero memiliki karakteristik unik, kemampuan khusus, dan peran yang berbeda dalam permainan. Seiring dengan pertumbuhan pesat pemain Mobile Legends, pemahaman mendalam mengenai karakteristik hero menjadi esensial.'),
        p('Penelitian ini bertujuan untuk melakukan analisis kluster terhadap karakteristik hero dalam Mobile Legends. Dengan mendekati pemahaman yang sistematis terhadap karakter-karakter ini, diharapkan dapat memberikan wawasan mendalam tentang dinamika permainan dan preferensi pemain.'),
        h2('Rumusan Masalah'),
        p('1. Bagaimana tahap pre-processing data termasuk penanganan outlier, nilai yang hilang, dan transformasi data dapat mempengaruhi kualitas dan interpretasi data hero dalam Mobile Legends?'),
        p('2. Bagaimana visualisasi data dapat digunakan untuk menginterpretasikan karakteristik dan distribusi hero dalam Mobile Legends?'),
        p('3. Bagaimana teknik clustering dapat digunakan untuk mengelompokkan hero berdasarkan atribut serupa, dan apa implikasinya terhadap strategi permainan?'),
        h2("Tujuan Penelitian"),
        p('1. Mengembangkan visualisasi data yang efektif untuk membantu dalam interpretasi dan pemahaman tentang distribusi dan karakteristik hero dalam Mobile Legends.'),
        p('2. Mengembangkan model prediktif yang dapat membantu pemain dalam memilih hero yang paling cocok untuk gaya permainan mereka.'),
        h3('disusun oleh:'),
        p('1. Nisrina Khairunisa (164221097)'),
        p('2. Fadli Muhammad (164221081)'),
        p('3. Fabyan Riza Kiram (164221068)'),
        p('4. Edric Boby Tri Raharjo (16221032)')
      ),

      tabItem(tabName = 'pengenalan',
        fluidRow(
          box(
            title = "Penjelasan Data",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            textOutput("penjelasandata")
          )
        ),
        fluidRow(
          box(
            title = "Tabel Data",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DT::dataTableOutput("dataTable")
          )
        )
      ),

      tabItem(tabName = "visualisasi",
        titlePanel("Mobile Legends Hero Visualization"),
        fluidRow(
          valueBoxOutput("totalHeroBox"),
          valueBoxOutput("buffBox"),
          valueBoxOutput("blinkBox")
        ),
        fluidRow(
          box(
            title = "Skill Frequency",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            selectInput("skillSelect", "Select a skill:", choices = skill_columns),
            uiOutput("skillFrequencyBox")
          ),
          box(
            title = "Heroes with Selected Skill",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            DTOutput("skillTable")
          ),
          box(
            plotlyOutput("skillFreqPlot"), width = 12
          ),
          box(
            title = "Select Two Skills",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            selectInput("skill1Select", "Select Trait 1:", choices = skill_columns),
            selectInput("skill2Select", "Select Trait 2:", choices = skill_columns)
          ),
          box(
            title = "Heroes with Selected Skills",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            DTOutput("skillComparisonTable")
          ),
          box(
            title = "Skill Comparison Matrix",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("skillMatrixPlot")
          )
        )
      ),
      tabItem(tabName = "analisis",
        titlePanel("Clustering KMeans - PCA visualization"),
        fluidRow(
          plotlyOutput("pcaPlot", width = "100%", height = "600px")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Box penjelasan data
  output$penjelasandata <- renderText({
    "Data terdiri dari 123 Hero dan terdapat 123 variabel skill yang tersedia untuk masing-masing hero. Variabel tersebut terdiri dari berbagai aspek seperti recharging time, defense, skill characteristics, mobility, attack effect, dan lainnya. Variabel-variabel tersebut menunjukkan berbagai fitur yang dapat digunakan untuk analisis hero dalam permainan mobile legends. Tipe data untuk setiap variabel adalah numerik dengan nilai yang berupa angka biner (1 dan 0). Nilai tersebut mencerminkan keberadaan atau ketidakberadaan suatu karakteristik atau kemampuan pada setiap hero. Kami menggunakan metode aggregation dengan merangkum data mentah yang kita ambil dari web menjadi bentuk yang lebih ringkas dan sederhana. Hal ini bertujuan untuk menyajikan data dalam format yang lebih mudah diinterpretasi, mendukung pengambilan keputusan, dan mempermudah proses analisis selanjutnya."
  })
  
  # Data table output
  output$dataTable <- DT::renderDataTable({
    DT::datatable(
      dataset,
      options = list(
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = '_all')),
        pageLength = 10,
        searchHighlight = TRUE
      ),
      class = 'display nowrap'
    )
  })

  # Value box untuk total number of heroes
  output$totalHeroBox <- renderValueBox({
    valueBox(
      formatC(total_hero, format="d", big.mark=','),
      "Number of Heroes",
      icon = icon("gamepad"),
      color = "purple"
    )
  })

  # Value box untuk heroes with 'buff'
  output$buffBox <- renderValueBox({
    valueBox(
      formatC(buff_count, format="d", big.mark=','),
      "Heroes with Buff",
      icon = icon("shield"),
      color = "green"
    )
  })

  # Value box untuk heroes with 'blink' ability
  output$blinkBox <- renderValueBox({
    valueBox(
      formatC(blink_count, format="d", big.mark=','),
      "Heroes with Blink Ability",
      icon = icon("eye"),
      color = "blue"
    )
  })

  # Memilih skill 
  output$skillFrequencyBox <- renderUI({
    # frequency
    skill_freq <- sum(!is.na(dataset[[input$skillSelect]]))
    
    # valueBox
    valueBox(
      value = skill_freq,
      subtitle = paste('Heroes with the', input$skillSelect, 'skill'),
      icon = icon("users"),
      color = "red"
    )
  })

  # Tabel Hero dengan skill tertentu
  output$skillTable <- DT::renderDataTable({
    skill_terpilih <- input$skillSelect
    skill_data <- hilang_na[hilang_na[[skill_terpilih]] == 1, "Hero", drop = FALSE]
    
    
    
    datatable(skill_data, options = list(pageLength = 10, autoWidth = TRUE),colnames = c("Heroes")) 
  })

  # Skill Frequency Plot
  output$skillFreqPlot <- renderPlotly({
    skill_freq_data <- colSums(!is.na(dataset[, -1]))
    skill_freq_df <- data.frame(Skill = names(skill_freq_data), Frequency = skill_freq_data)

    # colorful plot
    p <- ggplot(skill_freq_df, aes(x = Skill, y = Frequency, fill = Skill)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      labs(
        x = "Skill",
        y = "Frequency",
        title = "Skill Frequency Distribution",
        caption = "Source: Mobile Legends Data"
      )

    ggplotly(p)
  })

  # Table for Heroes with Selected Skills
  output$skillComparisonTable <- DT::renderDataTable({
    skill1 <- input$skill1Select
    skill2 <- input$skill2Select
    skill_data <- hilang_na[hilang_na[[skill1]] == 1 & hilang_na[[skill2]] == 1, "Hero", drop = FALSE]
 


    DT::datatable(skill_data, options = list(pageLength = 10, autoWidth = TRUE), colnames = c("Heroes"))
  })


  # Skill Comparison Matrix Plot
  output$skillMatrixPlot <- renderPlotly({
    skill1 <- input$skill1Select
    skill2 <- input$skill2Select

    comparison_matrix <- matrix(nrow = 2, ncol = 2)
    colnames(comparison_matrix) <- c(paste(skill1, "Yes"), paste(skill1, "No"))
    rownames(comparison_matrix) <- c(paste(skill2, "Yes"), paste(skill2, "No"))

    comparison_matrix[1, 1] <- sum(!is.na(dataset[[skill1]]) & !is.na(dataset[[skill2]]))
    comparison_matrix[1, 2] <- sum(!is.na(dataset[[skill1]]) & is.na(dataset[[skill2]]))
    comparison_matrix[2, 1] <- sum(is.na(dataset[[skill1]]) & !is.na(dataset[[skill2]]))
    comparison_matrix[2, 2] <- sum(is.na(dataset[[skill1]]) & is.na(dataset[[skill2]]))

    fig <- plot_ly(z = comparison_matrix, x = colnames(comparison_matrix), y = rownames(comparison_matrix), type = "heatmap", colors = colorRamp(c("purple", "blue")))
    fig <- fig %>% add_annotations(
      text = sprintf("%d", comparison_matrix),
      x = rep(colnames(comparison_matrix), each = nrow(comparison_matrix)),
      y = rep(rownames(comparison_matrix), ncol(comparison_matrix)),
      font = list(color = "white"),
      showarrow = FALSE
    )
    fig
  })


  # Tab Analisis
  datafillnad <- dataset 
  datafillnad[is.na(datafillnad)] <- 0
  
  pca_plot_data <- reactive({
    new_data <- datafillnad[, !names(datafillnad) %in% c("Hero")]
    pca_res <- prcomp(datafillnad[,!names(datafillnad) %in% c("Hero")])$x[, 1:3]
    
    
    predict.kmeans <- function(object, newdata){
      centers <- object$centers
      n_centers <- nrow(centers)
      dist_mat <- as.matrix(dist(rbind(centers, newdata)))
      dist_mat <- dist_mat[-seq(n_centers), seq(n_centers)]
      max.col(-dist_mat)
    }
    
    set.seed(47)
    mod_kmeans <- kmeans(new_data, 4)
    test_preds <- predict(mod_kmeans, new_data)
    
    print (test_preds)
    color_dict <- c("orange", "pink", "blue", "black")
    # ini tidak mengubah warna
    colors <- color_dict[test_preds + 1]
    print (colors)
    
    df <- data.frame(
      PC1 = pca_res[, 1],
      PC2 = pca_res[, 2],
      PC3 = pca_res[, 3],
      Cluster = test_preds,
      Color = colors,
      size = 1,
      hero = datafillnad[,"Hero"]
    )
    df
  })
  
  # Render Plotnya
  output$pcaPlot <- renderPlotly({
    df <- pca_plot_data()
    
    fig <- plot_ly(df, x = ~PC1, y = ~PC2, z = ~PC3, color = ~factor(Cluster),
                   colors = c("purple", "red", "yellow", "green", "black"),
                   # ini yang mengubah warna
                   opacity = 1, size = ~size,
                   text = ~hero, mode = "markers",
                   type = "scatter3d") %>%
      layout(
        title = 'Interactive 3D Scatter Plot',
        scene = list(
          xaxis = list(title = 'Principal Component 1'),
          yaxis = list(title = 'Principal Component 2'),
          zaxis = list(title = 'Principal Component 3')
        )
      )
    
    fig
  })

}

shinyApp(ui, server)

