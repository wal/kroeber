library(shiny)
library(shinythemes)
library(ggthemes)
library(scales)
library(DT)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
  # Application title
  titlePanel("GPS Data : K-Means/PCA Cluster Analysis"),
   
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;",
      uiOutput("sessionTypeSelection"),
      uiOutput("metricSelection"),
      sliderInput("k", h4("# Clusters (k)"), min = 1, max = 5, value = 3)
    ),
  
    mainPanel(
      tabsetPanel(
        tabPanel("Cluster Membership", 
                 br(),
                 p("K-Means clustering groups \"similar\" athletes together in terms of the chosen metrics and session types."),
                 hr(),
                 h2("Cluster Membership by Principal Component"),
                 plotOutput("componentsClusterPlot"), 
                 p("The first 2 principal components of the data describe most of the variance explained in the chosen data and provide a rough guide to visualise the relative distances of athletes from each other. The PCA tab has more information about the PCA Analysis and explained variance."),
                 hr(), 
                 h2("Cluster Membership (Scaled Metric Data)"),
                 p("The scaled mean metric values per athlete, for the chosen metrics and session types"),
                 dataTableOutput("clusterTable"),
                 hr(),
                 h2("Metric contribution to cluster definition"),
                 p("The contributions that low/high values of each metric contributed to the definition of the clusters. This should provide a clue as to what sorts of athletes were clustered together (e.g high distance athletes in a particular cluster)"),
                 plotOutput("centersPlot")),
        tabPanel("Cluster Size", 
                 br(),
                 p("The Elbow Plot describes the sum of squared error for various cluster sizes. An \"Elbow\" in the line may represent an ideal number of clusters for this data."),
                 plotOutput("elbowPlot")),
        tabPanel("Raw Data", dataTableOutput("rawDataTable")),
        tabPanel("Information", includeHTML("html/references.html"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data <- read_csv('data/statsports.csv')
  
  filtered_data <- reactive({
    
    if(input$session_type == 'All') {
      data 
    } else {
      data %>% filter(Type == input$session_type)  
    }
  })
  
  scaled_per_athlete_data <- reactive({
    
    filtered_data() %>% 
      select(c(`Player Display Name`, input$metric_names)) %>%
      group_by(`Player Display Name`) %>% 
      summarise_all(mean) %>%
      mutate_if(is.numeric, scale)
    })
  
  metrics <- reactive({
    select_if(data, is.numeric) %>% 
      names() %>% 
      sort()
  })
  
  session_types <- reactive({
    unique(data$Type) %>% sort()
  })
  
  k_means_model <- reactive({
    model_data <- scaled_per_athlete_data() %>% select(-c(`Player Display Name`)) 
    kmeans(model_data, centers = input$k)
  })
  
  ## UI for the metrics selection (returns only numeric columns)
  output$metricSelection <- renderUI({
    valid_metrics <- metrics()
    checkboxGroupInput("metric_names", h4("Metrics to Include"), choices = valid_metrics, selected = valid_metrics[1:3])
  })
  
  ## UI for the session type selection
  output$sessionTypeSelection <- renderUI({
    selectInput("session_type", h4("Session Types"), choices = c("All", session_types()), selected = "All")
  })
  
  output$elbowPlot <- renderPlot({
    
    data <- scaled_per_athlete_data() %>% 
      select(-c(`Player Display Name`))
    
    tot_withinss <- map_dbl(1:10, function(k) {
      model <- kmeans(data, centers = k)
      model$tot.withinss
    })
    
    data.frame(k = 1:10, tot_withinss = tot_withinss) %>% 
      ggplot(aes(k, tot_withinss)) + 
      geom_line() +
      scale_x_continuous(breaks = 1:10) +
      ylab("Total Sum of Squared Error ") + 
      xlab("# Clusters (k)") +
      theme_minimal()
   })
  
  output$componentsClusterPlot <- renderPlot({
    
    data <- scaled_per_athlete_data() %>% select(-c(`Player Display Name`))
    
    pca <- prcomp(data, center = FALSE, scale = FALSE)
    model <- k_means_model()
    
    cbind(scaled_per_athlete_data(), pca$x, model$cluster) %>%
      ggplot(aes(PC1, PC2, label = `Player Display Name`, color = factor(model$cluster))) + 
        geom_point(size=4, alpha = 0.80) + 
        geom_text(nudge_y = -0.1) +
        scale_color_few(name = "Cluster") +
        theme_minimal() 
  })
  
  output$clusterTable <- renderDataTable({
    data <- scaled_per_athlete_data()
    
    data$cluster <- k_means_model()$cluster

    data %>% mutate_if(is.numeric, round, 2) %>% 
      arrange(cluster) %>%
      select(`Player Display Name`, cluster, everything()) %>%
      datatable(options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
  })
  
  output$centersPlot <- renderPlot({
    centers_df <- as.data.frame(k_means_model()$centers) %>% rownames_to_column("cluster")
    gather(centers_df, k = metric, value = value, -cluster) %>%
      ggplot(aes(metric, cluster, fill = value)) + 
      geom_tile() +
      scale_fill_continuous(low = "#FF0000", high = "#00FF00") +
      ylab("Cluster") + 
      xlab("Metric") +
      theme_minimal()
  })
  
  output$rawDataTable <- renderDataTable({
    filtered_data() %>% datatable(options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

