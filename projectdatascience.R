library(readr)
library(shiny)
library(arules)
library(dplyr)

dataclean <- function(data) {
  data$items <- gsub(" ", "", data$items) 
  data$items <- strsplit(data$items, "[,]") 
  data$items <- lapply(data$items, function(x) trimws(x, "both")) 
  data$items <- sapply(data$items, function(x) paste(x, collapse = ", "))  
  return(data)
}

loading_data <- function(dataset_path) {
  data <- read_csv(dataset_path, col_types = cols(
    items = col_character(),
    count = col_double(),
    total = col_double(),
    rnd = col_double(),
    customer = col_character(),
    age = col_double(),
    city = col_character(),
    paymentType = col_character()
  ), quote = "")
  data <- dataclean(data)
  
  data <- distinct(data)
  
  char_cols <- sapply(data, is.character)
  for (col in names(data)[char_cols]) {
    data[[col]] <- ifelse(grepl("^\\d+\\.?\\d*$", data[[col]]), as.numeric(data[[col]]), NA)
  }
  data <- na.omit(data)
  
  return(data)
}

visualize_data <- function(data) {
  par(mfrow = c(2, 2)) 
  
  paymentType_totals <- data %>%
    group_by(paymentType) %>%
    summarize(total_spending = sum(total))
  
  pie(paymentType_totals$total_spending, 
      labels = paste0(paymentType_totals$paymentType, " ", round(prop.table(paymentType_totals$total_spending) * 100), "%"),
      main = "Total Spending by Payment Type",
      col = c("black", "grey"),
      radius = 1) 
  
  age_totals <- data %>%
    group_by(age) %>%
    summarize(total_spending = sum(total))
  
  plot(age_totals$age, age_totals$total_spending,
       main = "Comparison of Age and Total Spending",
       xlab = "Age", ylab = "Total Spending")
  
  city_totals <- data %>%
    group_by(city) %>%
    summarize(total_spending = sum(total))
  
  
  city_totals <- city_totals %>%
    arrange(desc(total_spending))
  
  barplot(city_totals$total_spending,
          names.arg = city_totals$city,
          main = "Total Spending by City",
          xlab = "City", ylab = "Total Spending")
  
  hist(data$total, main = "Distribution of Total Spending",
       xlab = "Total Spending", ylab = "Frequency", col = "blue", border = "black")
}

customercluster <- function(data, num_clusters) {
  customer <- data %>%
    group_by(customer, age) %>%
    summarize(total_spending = sum(total)) %>%
    ungroup() %>%
    mutate(avg_age = mean(age))
  
  total_spending_per_customer <- customer %>%
    group_by(customer) %>%
    summarize(total_spending = sum(total_spending))
  
  unique_customers <- unique(total_spending_per_customer$customer)
  dataforcustomer <- customer[customer$customer %in% unique_customers, ]
  
  cluster_data <- dataforcustomer[, c("total_spending")]
  
  clusters <- kmeans(cluster_data, centers = num_clusters)
  
  dataforcustomer$Cluster <- as.factor(clusters$cluster)
  
  return(dataforcustomer[, -which(names(dataforcustomer) == "avg_age")]) # Remove the column "avg_age"
}


associa <- function(data, min_support, min_confidence,min_lin) {
  item_lists <- data$items
  transactions <- lapply(item_lists, function(x) strsplit(x, ",")[[1]])
  transactions <- as(transactions, "transactions")
  # inspect(transactions)
  rules <- apriori(transactions, parameter = list(support = min_support, confidence = min_confidence,minlen=min_lin))
  if (length(rules) > 0) {
    print("Association rules found:")
    inspect(rules)
        
  } else {
    print("No association rules found.")
  }
  return(rules)
}

ui <- fluidPage(
  titlePanel("Grocery Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV file"),
      numericInput("num_clusters", "Number of Clusters", value = 3, min = 2, max = 4),
      numericInput("min_support", "Minimum Support", value = 0.1, min = 0.001, max = 1),
      numericInput("min_confidence", "Minimum Confidence", value = 0.1, min = 0.001, max = 1),
      numericInput("min_lin", "Minimum Lengh", value = 1, min = 0, max = 100),
      actionButton("analyze_button", "Analyze Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization", plotOutput("plot")),
        tabPanel("Clustered Data", dataTableOutput("cluster_table")),
        tabPanel("Association Rule", verbatimTextOutput("rules_output"))
      )
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    dataclean(df)
  })
  
  observeEvent(input$analyze_button, {
    output$plot <- renderPlot({
      visualize_data(data())
    })
    
    output$cluster_table <- renderDataTable({
      customercluster(data(), input$num_clusters)
    })
    
    output$rules_output <- renderPrint({
      associa(data(), input$min_support, input$min_confidence, input$min_lin)
    })
  })
}

shinyApp(ui = ui, server = server)