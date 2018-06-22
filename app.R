library(tidyverse)
library(shiny)
library(shinythemes)
library(ggthemes)
library(scales)
library(DT)
library(readr)


set.seed(10)

################
# Application UI
################
ui <- fluidPage(theme = shinytheme("cosmo"),
  
  titlePanel("GPS Data : K-Means/PCA Cluster Analysis"),
   
  sidebarLayout(
    sidebarPanel(
      uiOutput("sessionTypeSelection"),
      uiOutput("metricSelection"),
      sliderInput("k", h4("# Clusters (k)"), min = 1, max = 5, value = 3)
    ),
  
    mainPanel(
      tabsetPanel(
        tabPanel("Cluster Membership", 
                 br(),
                 p("The clustering groups \"similar\" athletes together in terms of the chosen metrics and session types. The filters in the sidebar can be manipulated to cause a new clustering."),
                 hr(),
                 h2("Cluster Membership"),
                 plotOutput("componentsClusterPlot"), 
                 p("The above plot uses the first 2 principal components of the data (from PCA Analysis) to roughly show the relative distances of athletes from each other 
                   and how the clustering grouped athletes."),
                 hr(), 
                 h2("Scaled Metric Data"),
                 p("The scaled mean metric values per athlete for the chosen metrics and session types are used as input to the K-Means clustering algorithn"),
                 dataTableOutput("clusterTable"),
                 hr(),
                 h2("Metric contribution to cluster definition"),
                 p("The contributions that low/high values of each metric contributed to the definition of the clusters. This should provide 
                   a clue as to what sorts of athletes were clustered together (e.g high distance athletes in a particular cluster)"),
                 plotOutput("centersPlot")),
        tabPanel("Ideal Cluster Count", 
                 br(),
                 p("The Elbow Plot describes the sum of squared error for various cluster sizes. An \"Elbow\" in the line may represent an 
                   ideal number of clusters for this data. The currently chosen k (from the filter in the sidebar) is shown in red."),
                 plotOutput("elbowPlot"),
                 hr(),
                 p("More on this technique can be read here https://datascienceplus.com/finding-optimal-number-of-clusters/")),
        tabPanel("Raw Data", dataTableOutput("rawDataTable")),
        tabPanel("Information", includeHTML("html/references.html"))
      )
    )
  )
)

######################
# Application Server
######################
server <- function(input, output) {
  
  data <- read_csv('data/statsports.csv')
  
  # Data filtered by selected session type
  filtered_data <- reactive({
    
    if(input$session_type == 'All') {
      data 
    } else {
      data %>% filter(Type == input$session_type)  
    }
  })
  
  # Per athlete data, aggregated (mean) and scaled
  scaled_per_athlete_data <- reactive({
    filtered_data() %>% 
      select(c(`Player Display Name`, input$metric_names)) %>%
      group_by(`Player Display Name`) %>% 
      summarise_all(mean) %>%
      mutate_if(is.numeric, scale)
    })
  
  # Available metrics from the dataset
  metrics <- reactive({
    select_if(data, is.numeric) %>% 
      names() %>% 
      sort()
  })
  
  # Available GPS session types from the dataset (uses Type column)
  session_types <- reactive({
    unique(data$Type) %>% sort()
  })
  
  # The K-Means model using the selected number value for k and the scaled per-athlete data
  k_means_model <- reactive({
    model_data <- scaled_per_athlete_data() %>% 
      select(-c(`Player Display Name`))  # Remove the athlete name as k-means can only be done on numeric data
    
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
  
  # Data for the elbow plot
  elbowData <- reactive({
    data <- scaled_per_athlete_data() %>% 
      select(-c(`Player Display Name`))
    
    # Calculate the k-means model and error for a range of values of k
    tot_withinss <- map_dbl(1:10, function(k) {
      model <- kmeans(data, centers = k, nstart = 10)
      model$tot.withinss
    })
    
    # re-assemble a dataframe and plot the elbow plot
    data.frame(k = 1:10, tot_withinss = tot_withinss)
  })
  
  # A PCA Elbow plot of number of clusters v total squared error. Used to determine an 'ideal' number of clusters for this data
  output$elbowPlot <- renderPlot({
      ggplot(elbowData(), aes(k, tot_withinss)) + 
      geom_line() +
      geom_vline(xintercept = input$k, color = "red") +
      scale_x_continuous(breaks = 1:10) +
      ylab("Total Sum of Squared Error ") + 
      xlab("# Clusters (k)") +
      theme_minimal()
   })
  
  # A plot to show relative distances between athletes using the first two components from PCA
  output$componentsClusterPlot <- renderPlot({
    
    # Ignore this. It is used to suppress an error on startup of the app
    if(is.null(input$session_type)) {
      return(NULL)
    }
    
    data <- scaled_per_athlete_data() %>% select(-c(`Player Display Name`))
    
    # PCA Analysis
    pca <- prcomp(data, center = FALSE, scale = FALSE)
    
    model <- k_means_model()
    
    # Bind together the k-means derrived clusters, the PCA values per component and the original scaled athlete data to plot the scatter plot
    cbind(scaled_per_athlete_data(), pca$x, model$cluster) %>%
      ggplot(aes(PC1, PC2, label = `Player Display Name`, color = factor(model$cluster))) + 
        geom_point(size=4, alpha = 0.80) + 
        geom_text(nudge_y = -0.1) +
        scale_color_few(name = "Cluster") +
        theme_minimal() 
  })
  
  # Table of the k-means derrived cluster membership
  output$clusterTable <- renderDataTable({
    
    # Ignore this. It is used to suppress an error on startup of the app
    if(is.null(input$session_type)) {
      return(NULL)
    }
    
    data <- scaled_per_athlete_data()
    
    data$cluster <- k_means_model()$cluster

    data %>% mutate_if(is.numeric, round, 2) %>% 
      arrange(cluster) %>%
      select(`Player Display Name`, cluster, everything()) %>%
      datatable(options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
  })
  
  # Plot to define the influence particular GPS metrics have on the cluster definitions
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
  
  # Raw data table
  output$rawDataTable <- renderDataTable({
    filtered_data() %>% datatable(options = list(paging = FALSE, searching = FALSE), rownames= FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

