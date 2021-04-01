## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(highcharter)
library(DT)
library(shinyWidgets)

ui <- dashboardPage( skin ="black",
  dashboardHeader(
    title = "Customer Segmentation"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("RFM & Segmentation", tabName = "rfm", icon = icon("th")),
      menuItem("Behavior Cust", tabName = "behavior"),
      menuItem("Predict", tabName = "predict")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        valueBoxOutput("vbox_sales", width = 6),
        valueBoxOutput("vbox_trans", width = 6),
        valueBoxOutput("vbox_customers", width = 6),
        valueBoxOutput("vbox_new_customers", width = 6),
      ),
      tabItem(
        tabName = "rfm",
        fluidRow(
          box(width=6,
          plotOutput("recency_segment", width = "100%")),
          box(width=6,
          plotOutput("segment_value", width = "100%"))
        ),
        fluidRow(
            fluidRow(
              valueBox("2 Cust", "Special Value (0.04%)", width = 3),
              valueBox("8 Cust", "High Value (0.18%)", width = 3),
              valueBox("329 Cust", "Medium Value (7,58%)", width = 3),
              valueBox("3999 Cust", "Low Value (92.18%)", width = 3),
            ),
            fluidRow(
            box(
             plotlyOutput("monitery_segment_plot")
            ),
            box(
             plotlyOutput("monitery_recency_plot")
            )
            )
        )
      ),
      tabItem(
        tabName = "behavior",
        tabBox(width = 12,
          title = "Behavior Customers",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "tabset1", height = "250px",
          tabPanel("Summary", 
                   fluidRow(
                     box(
                       pickerInput(
                         inputId = "recency_segment_input",
                         label = "Recency Segment", 
                         choices = c("Active", "Warm", "Cold", "Inactive"),
                         multiple = TRUE,
                         selected = "Warm"
                       ),
                       pickerInput(
                         inputId = "value_segment_input",
                         label = "Value Segment", 
                         choices = c("Low Value", "Medium Value", "High Value", "Special Value"),
                         multiple = TRUE,
                         selected = "Low Value"
                       ),
                       fluidRow(
                         column(width = 6,
                           valueBoxOutput("total_customer_percent", width = 12),
                           box(width = 12,
                           plotOutput("summary_date_behavior_plot", height = "200px"))
                         ),
                         column(width = 6,
                           valueBoxOutput("total_monetery_percent", width = 12),
                           box(width = 12,
                           plotOutput("summary_month_behavior_plot", height = "200px"))
                         )
                       )
                     ),
                     box(
                       DTOutput("summary_table")
                     )
                   )),
          tabPanel("Individu", 
                   fluidRow(
                     box(width = 5,
                       pickerInput(
                         inputId = "recency_segment_input_ind",
                         label = "Recency Segment", 
                         choices = c("Active", "Warm", "Cold", "Inactive"),
                         multiple = TRUE,
                         selected = "Active"
                       ),
                       pickerInput(
                         inputId = "value_segment_input_ind",
                         label = "Value Segment", 
                         choices = c("Low Value", "Medium Value", "High Value", "Special Value"),
                         multiple = TRUE,
                         selected = "Low Value"
                       ),
                       uiOutput("customer_id"),
                       fluidRow(
                         column(width = 6,
                                infoBoxOutput("first_engagement", width = 12),
                                box(width = 12,
                                    plotOutput("ind_date_behavior_plot", height = "200px"))
                         ),
                         column(width = 6,
                                infoBoxOutput("ind_total_monetery_percent", width = 12),
                                box(width = 12,
                                    plotOutput("ind_month_behavior_plot", height = "200px"))
                         )
                       )
                     ),
                     box(width = 7,
                       fluidRow(
                         h4("Predict"),
                         DTOutput("ind_table")
                       ),
                       fluidRow(
                         box(
                           h4("Recomended Product"),
                           DTOutput("table_predict")
                         ),
                         box(
                           h4("Purchase History"),
                           DTOutput("table_purchase")
                         )
                       )
                     )
                   )
                  )
        ),
      ),
      tabItem(
        tabName = "predict",
        sidebarPanel(
          textInput('CustomerID', 'Costumer ID'),
          numericInput('recency', 'Recency', 40, min = 0, max = 369),
          numericInput('frequency', 'Frequency', 40, min = 0, max = 20000),
          numericInput('monitery', 'Monetery', 35, min = 0, max = 1000000),
          actionButton("goButton", "Clustering!!!")
        ),
        mainPanel(
          DTOutput('nText')
        )
      )
    )
  )
)

server <- function(input, output) { 
  
  df_RFM <- read.csv("data/df_RFM.csv")
  final_data <- read.csv("data/final_data.csv")
  df_data <- read.csv("data/data.csv")
  sales_data <- readRDS("data/data.sales_data.RDS")
  
  df_data <- df_data %>% 
    mutate(Quantity = replace(Quantity, Quantity<=0, NA),
           UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))
  
  df_data <- df_data %>%
    drop_na()
  
  df_data <- df_data %>% 
    mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
           InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
           Country=as.factor(Country))
  
  df_data <- df_data %>% 
    mutate(total_dolar = Quantity*UnitPrice)
  
  df_data$days <- weekdays(df_data$InvoiceDate)
  df_data$mday <- mday(as.POSIXlt(df_data$InvoiceDate))
  
  df_data$days <- as.factor(df_data$days)
  
    valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                            icon = NULL, color = "aqua", width = 4, href = NULL){
    
    shinydashboard:::validateColor(color)
    
    if (!is.null(icon))
      shinydashboard:::tagAssert(icon, type = "i")
    
    info_icon <- tags$small(
      tags$i(
        class = "fa fa-info-circle fa-lg",
        title = info,
        `data-toggle` = "tooltip",
        style = "color: rgba(255, 255, 255, 0.75);"
      ),
      # bs3 pull-right 
      # bs4 float-right
      class = "pull-right float-right"
    )
    
    boxContent <- div(
      class = paste0("small-box bg-", color),
      div(
        class = "inner",
        tags$small(title),
        if (!is.null(sparkobj)) info_icon,
        h3(value),
        if (!is.null(sparkobj)) sparkobj,
        p(subtitle)
      ),
      # bs3 icon-large
      # bs4 icon
      if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
    )
    
    if (!is.null(href)) 
      boxContent <- a(href = href, boxContent)
    
    div(
      class = if (!is.null(width)) paste0("col-sm-", width), 
      boxContent
    )
  }
  
    hc_theme_sparkline_vb <- function(...) {
      
      theme <- list(
        chart = list(
          backgroundColor = NULL,
          margins = c(0, 0, 0, 0),
          spacingTop = 0,
          spacingRight = 0,
          spacingBottom = 0,
          spacingLeft = 0,
          plotBorderWidth = 0,
          borderWidth = 0,
          style = list(overflow = "visible")
        ),
        xAxis = list(
          visible = FALSE, 
          endOnTick = FALSE, 
          startOnTick = FALSE
        ),
        yAxis = list(
          visible = FALSE,
          endOnTick = FALSE, 
          startOnTick = FALSE
        ),
        tooltip = list(
          outside = FALSE,
          shadow = FALSE,
          borderColor = "transparent",
          botderWidth = 0,
          backgroundColor = "transparent",
          style = list(textOutline = "5px white")
        ),
        plotOptions = list(
          series = list(
            marker = list(enabled = FALSE),
            lineWidth = 2,
            shadow = FALSE,
            fillOpacity = 0.25,
            color = "#FFFFFFBF",
            fillColor = list(
              linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
              stops = list(
                list(0.00, "#FFFFFF00"),
                list(0.50, "#FFFFFF7F"),
                list(1.00, "#FFFFFFFF")
              )
            )
          )
        ),
        credits = list(
          enabled = FALSE,
          text = ""
        )
      )
      
      theme <- structure(theme, class = "hc_theme")
      
      if (length(list(...)) > 0) {
        theme <- hc_theme_merge(
          theme,
          hc_theme(...)
        )
      }
      
      theme
    }
    
    
  output$vbox_customers <- renderValueBox({
    customer <- df_data %>%
      group_by(InvoiceDate) %>%
      summarise(total_customer = n_distinct(CustomerID))
    
    customer$month <- floor_date(customer$InvoiceDate, "month")
    customer = customer %>%
          group_by(month) %>%
          summarize(total_customer = sum(total_customer)) %>%
          filter(month != "2011-12-01") 
    
    sum(customer$total_customer)
    hc <- customer %>% hchart(
      'area', hcaes(x = month, y = total_customer),
      color = "steelblue"
    ) %>% hc_credits(enabled = FALSE) %>%
      
      hc_size(height = 200) 
    
    total_sales <- paste("16057")
    
    valueBoxSpark(
      value = total_sales,
      title = toupper("Total Customers by Month"),
      sparkobj = hc,
      subtitle = "",
      info = "Total customers increased by 98% from 2010-12-01 to 2011-11-01 ",
      icon = icon("code"),
      width = 6,
      color = "teal",
      href = NULL
    )
  })
  
  output$vbox_trans <- renderValueBox({
    transaction <- df_data %>%
      group_by(InvoiceDate) %>%
      summarise(total_transaction = n_distinct(InvoiceNo))
    
    #round dates down to week
    transaction$month <- floor_date(transaction$InvoiceDate, "month")
    
    transaction <- transaction %>%
      group_by(month) %>%
      summarize(total_transaction = sum(total_transaction)) %>%
      filter(month != "2011-12-01")
    
    
    hc_trans <- transaction %>% hchart(
      'area', hcaes(x = month, y = total_transaction),
      color = "steelblue"
    ) %>% hc_credits(enabled = FALSE) %>%
      hc_add_theme(hc_theme_sparkline_vb()) %>%
      hc_size(height = 200) 
    
    valueBoxSpark(
      value = "18352 Transaction",
      title = toupper("Total Transaction by Month"),
      sparkobj = hc_trans,
      subtitle = "",
      info = "Total sales increased by 89% from 2010-12-01 to 2011-11-01 ",
      icon = icon("code"),
      width = 6,
      color = "teal",
      href = NULL
    )
  })

  output$vbox_sales <- renderValueBox({
    sales <- df_data %>%
      mutate(total_dolar = Quantity*UnitPrice) %>%
      group_by(InvoiceDate) %>%
      summarise(total_sales = sum(total_dolar))
    
    sales$month <- floor_date(sales$InvoiceDate, "month")
    
    sales <- sales %>%
      group_by(month) %>%
      summarize(total_sales = sum(total_sales)) %>%
      filter(month != "2011-12-01")
    
    
    hc_sales <- sales %>% hchart(
      'area', hcaes(x = month, y = total_sales),
      color = "steelblue"
    ) %>% hc_credits(enabled = FALSE) %>%
      hc_add_theme(hc_theme_sparkline_vb()) %>%
      hc_size(height = 200) 
    sum(sales$total_sales)
    valueBoxSpark(
      value = "$ 8.393.215",
      title = toupper("Total Sales by Month"),
      sparkobj = hc_sales,
      subtitle = "",
      info = "Total sales increased by 102% from 2010-12-01 to 2011-11-01 ",
      icon = icon("code"),
      width = 6,
      color = "teal",
      href = NULL
    )
  })
  
  output$vbox_new_customers <- renderValueBox({
    sales_data <- df_data %>%
      group_by(CustomerID)%>%
      mutate(date_of_first_engagement=min(InvoiceDate))%>%
      ungroup()
    
    sales_data <- sales_data%>%
      mutate(Customer_Status = case_when(InvoiceDate>date_of_first_engagement ~ "Returning",
                                         InvoiceDate == date_of_first_engagement ~ "New",
                                         TRUE ~ "Other"))
    
    New_and_Returning_Customers <-  sales_data%>%
      group_by(floor_date(InvoiceDate,unit = 'month'))%>%
      summarise(New_Customers = n_distinct(CustomerID[Customer_Status=="New"]),
                Returning_Customers= n_distinct(CustomerID[Customer_Status=="Returning"]))
    
    colnames(New_and_Returning_Customers) <- c("Date", "New_Cus", "Return_Cus")
    #New_and_Returning_Customers
    
    
    hc_new_customers <- New_and_Returning_Customers %>% hchart(
      'area', hcaes(x = Date, y = New_Cus),
      color = "steelblue"
    ) %>% hc_credits(enabled = FALSE) %>%
     
      hc_size(height = 200) 
    
    valueBoxSpark(
      value = "Average 333",
      title = toupper("Total New Customer by Month"),
      sparkobj = hc_new_customers,
      subtitle = "",
      info = "Total New Customers decreased  by 95% from 2010-12-01 to 2011-11-01 ",
      icon = icon("code"),
      width = 6,
      color = "teal",
      href = NULL
    )
  })
  
  output$recency_segment <- renderPlot({
    percen_segment <- plyr::count(df_RFM$recency_segment) %>%
      mutate(percent = round(freq/sum(freq) *100),2)
    
    
    df = data.frame(start = c(0, 31, 91, 181),
                    end = c(30, 90, 180, 400),
                    segment= c("Active", "Warm", "Cold", "Inactive"))%>%
      mutate(median_x = start + floor((end-start)/2))
    df = inner_join(percen_segment, df, by=c("x" = "segment"))
    
    
      ggplot(df_RFM, aes(x=recency)) +
        geom_histogram(bins = 396) +
        geom_rect(data=df, aes(NULL,NULL,xmin=start,xmax=end,fill=x),
                  ymin=0,ymax=396, colour="white", size=0.5, alpha=0.5) +
        scale_fill_manual(values = c("#99eebb", "#159897", "#007788", "#21ada8")) + 
        geom_text(data=df,aes(x=median_x, y=125,label=paste(freq)), size=3) +
        geom_text(data=df,aes(x=median_x, y=120,label=x), size=4) +
        geom_text(data=df,aes(x=median_x, y=115,label=paste(percent,"%")), size=3) +
        ggtitle("Recency Segmentation",
                subtitle = "Active (0-30), Warm (31-90), Cold (91-180), Inactive (>180)") + 
        theme_classic() +
        theme(
          legend.position = "none"
        )
   
  })
  
  output$segment_value <- renderPlot({
    ggplot(df_RFM, aes(frequency, monitery, color=factor(segmentation))) +
      geom_point() +
      labs(title = "Customer Value Segmentation",
           subtitle = "Segmentation using K-Means",
           color = "Segmentation")
  })
  
  output$monitery_segment_plot <- renderPlotly({
    monitery_segment <- df_RFM %>%
      group_by(segmentation) %>%
      summarise(monitery = sum(monitery)) %>%
      mutate(percentage = round(monitery/sum(monitery) * 100, 2))
    
    ggplotly(
    ggplot(monitery_segment, aes(segmentation, percentage, fill="")) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(percentage, "%")), vjust = -0.2) +
      scale_fill_manual(values = c("#99eebb")) +
      labs(title = "Total Monetary per Value Segment",
           subtitle = "Segmentation using K-Means") +
      xlab("Value Segment") + ylab("Percentage") +
      theme_classic() +
      theme(
        legend.position = "none"
      )
    ) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$monitery_recency_plot <- renderPlotly({
    monitery_recency <- df_RFM %>%
      group_by(recency_segment) %>%
      summarise(monitery = sum(monitery)) %>%
      mutate(percentage = round(monitery/sum(monitery) * 100, 2))
    
    ggplotly(
    ggplot(monitery_recency, aes(recency_segment, percentage, fill="")) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(percentage, "%")), vjust = -0.2) +
      scale_fill_manual(values = c("#159897", "#007788", "#21ada8")) +
      labs(title = "Total Monetary per Recency Segment") +
      xlab("Recency Segment") + ylab("Percentage") +
      theme_classic() +
      theme(
        legend.position = "none"
      )
    ) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$total_customer_percent <- renderValueBox({
    
    input_recency <- c(input$recency_segment_input)
    input_value <- c(input$value_segment_input)
    
    summary_customer <- final_data %>%
      filter(recency_segment %in% input_recency,
             segmentation %in% input_value)
    
    total_customer <- length(unique(final_data$CustomerID))
    summary_total_customer <- length(unique(summary_customer$CustomerID))
    percen_customer <- round(summary_total_customer/total_customer*100, 2)
    
    valueBox(
      paste0(percen_customer, "%"), "Total Customers", icon = icon("users"),
      color = "purple"
    )
  })
  
  output$total_monetery_percent <- renderValueBox({
    
    input_recency <- c(input$recency_segment_input)
    input_value <- c(input$value_segment_input)
    
    summary_customer <- final_data %>%
      filter(recency_segment %in% input_recency,
             segmentation %in% input_value)
    
    total_monetary <- sum(final_data$total_dolar)
    summary_total_monetary <- sum(summary_customer$total_dolar)
    percen_monetary <- round(summary_total_monetary/total_monetary * 100, 2)
    
    valueBox(
      paste0(percen_monetary, "%"), "Total Monetary", icon = icon("dollar-sign"),
      color = "purple"
    )
  })
  
  output$summary_date_behavior_plot <- renderPlot({
    
    input_recency <- c(input$recency_segment_input)
    input_value <- c(input$value_segment_input)
    
    summary_customer <- final_data %>%
      filter(recency_segment %in% input_recency,
             segmentation %in% input_value)
    
    date_day <-
      plyr::count(summary_customer$mday)
    colnames(date_day) <- c("date", "freq")
    date_day
    date_cus <- data.frame(
      date = c(1:31))
    date_cus
    
    date_order_individu <- left_join(date_cus, date_day, by="date")
    ggplot(date_order_individu, aes(factor(date), freq, fill=freq)) +
      geom_bar(stat = "identity") +
      coord_polar() +
      theme_minimal() +
      labs(title = "Date Order Habbit") +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  output$summary_month_behavior_plot <- renderPlot({
    
    input_recency <- c(input$recency_segment_input)
    input_value <- c(input$value_segment_input)
    
    summary_customer <- final_data %>%
      filter(recency_segment %in% input_recency,
             segmentation %in% input_value)
    
    date_month <- plyr::count(summary_customer$month)
    colnames(date_month) <- c("month", "freq")
    
    month_year <- data.frame(
      month = factor(c(unique(final_data$month)),
                     levels = c("January", "February", "March", "April",    
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))
    )
    
    
    monthly_order_individu <- left_join(month_year, date_month, by="month")
    monthly_order_individu$month <- factor(monthly_order_individu$month,
                                           levels = c("January", "February", "March", "April",    
                                                      "May", "June", "July", "August", "September",
                                                      "October", "November", "December"))
    ggplot(monthly_order_individu, aes(month, freq, fill=freq)) +
      geom_bar(stat = "identity") +
      coord_polar() +
      theme_minimal() +
      labs(title = "Monthly Order Habbit") +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  output$summary_table <- renderDataTable({
    
    input_recency <- c(input$recency_segment_input)
    input_value <- c(input$value_segment_input)
    
    summary_customer <- df_RFM %>%
      filter(recency_segment %in% input_recency,
             segmentation %in% input_value)
    
    data_summary_table <- df_RFM %>%
      filter(CustomerID %in% unique(summary_customer$CustomerID))
    
    datatable(data_summary_table,
              options = list(
                pageLength=10, scrollX=T))
    
  })
  
  output$customer_id <- renderUI({
    
    input_recency <- c(input$recency_segment_input_ind)
    input_value <- c(input$value_segment_input_ind)
    
    summary_customer <- df_RFM %>%
      filter(recency_segment %in% input_recency,
             segmentation %in% input_value)
    
    pickerInput(
      inputId = "id",
      label = "Customer ID", 
      choices = c(unique(as.character(summary_customer$CustomerID)))
    )
  })
  
  output$first_engagement <- renderInfoBox({
    
    filter_id <- input$id
    
    individu_customer <- sales_data %>%
      filter(CustomerID == filter_id)
    
    infoBox(
      paste0("First Engagement"), individu_customer[1,'date_of_first_engagement'], icon = icon("users"),
      color = "purple"
    )
  })
  
  output$ind_total_monetery_percent <- renderInfoBox({
    
    filter_id <- input$id
    
    individu_customer <- final_data %>%
      filter(CustomerID == filter_id)
    
    
    ind_total_monetary <- sum(individu_customer$total_dolar)
    
    infoBox(
      "Total Monetary", paste0("$", ind_total_monetary), icon = icon("dollar-sign"),
      color = "purple"
    )
  })
  
  output$ind_date_behavior_plot <- renderPlot({
    
    filter_id <- input$id
    
    individu_customer <- final_data %>%
      filter(CustomerID == filter_id)
    
    
    date_day <-
      plyr::count(individu_customer$mday)
    colnames(date_day) <- c("date", "freq")
    date_day
    date_cus <- data.frame(
      date = c(1:31))
    date_cus
    
    date_order_individu <- left_join(date_cus, date_day, by="date")
    ggplot(date_order_individu, aes(factor(date), freq, fill=freq)) +
      geom_bar(stat = "identity") +
      coord_polar() +
      theme_minimal() +
      labs(title = "Date Order Habbit") +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  output$ind_month_behavior_plot <- renderPlot({
    
    filter_id <- input$id
    
    individu_customer <- final_data %>%
      filter(CustomerID == filter_id)
    
    date_month <- plyr::count(individu_customer$month)
    colnames(date_month) <- c("month", "freq")
    
    month_year <- data.frame(
      month = factor(c(unique(final_data$month)),
                     levels = c("January", "February", "March", "April",    
                                "May", "June", "July", "August", "September",
                                "October", "November", "December"))
    )
    
    
    monthly_order_individu <- left_join(month_year, date_month, by="month")
    monthly_order_individu$month <- factor(monthly_order_individu$month,
                                           levels = c("January", "February", "March", "April",    
                                                      "May", "June", "July", "August", "September",
                                                      "October", "November", "December"))
    ggplot(monthly_order_individu, aes(month, freq, fill=freq)) +
      geom_bar(stat = "identity") +
      coord_polar() +
      theme_minimal() +
      labs(title = "Monthly Order Habbit") +
      theme(
        legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  output$ind_table <- renderDataTable({
    
    filter_id <- input$id
    
    individu_customer <- final_data %>%
      filter(CustomerID == filter_id)
    
    data_summary_table <- df_RFM %>%
      filter(CustomerID == filter_id)
    
    datatable(data_summary_table[c(2,3,4,5,8,9)],
              options = list(
                pageLength=10, scrollX=T))
  })
  
  output$table_predict <- renderDataTable({
    predict_all <- readRDS("data/predict_all.RDS")
    CustomerId <- readRDS("data/CustomerId.RDS")
    itemCode <- readRDS("data/itemCode.RDS")
    model_details <- readRDS("data/model_details.RDS")
    df_data <- readRDS("data/df_data.RDS")
    
    filter_id <- input$id
    index_user <- match(as.integer(filter_id), CustomerId$CustomerID)
    user_1 <- CustomerId[as.integer(names(predict_all@items[index_user]))]
    
    vvv <- predict_all@items[[index_user]]
    vvv <- rownames(model_details$sim)[vvv]
    datatable(itemCode[vvv],
              options = list(
                pageLength=5, scrollX=T))
    
  })
  
  output$table_purchase <- renderDataTable({
    predict_all <- readRDS("data/predict_all.RDS")
    CustomerId <- readRDS("data/CustomerId.RDS")
    itemCode <- readRDS("data/itemCode.RDS")
    model_details <- readRDS("data/model_details.RDS")
    df_data <- readRDS("data/df_data.RDS")
    
    
    filter_id <- input$id
    index_user <- match(as.integer(filter_id), CustomerId$CustomerID)
    user_1 <- CustomerId[as.integer(names(predict_all@items[index_user]))]
    
    vvv <- predict_all@items[[index_user]]
    vvv <- rownames(model_details$sim)[vvv]
   
    
    user_1_buy <- df_data[CustomerID==as.integer(user_1), sum(Quantity), by=StockCode]
    datatable(merge(itemCode,user_1_buy, by='StockCode'),
              options = list(
                pageLength=5, scrollX=T))
  })
  
  ntext <- eventReactive(input$goButton, {
    
    databaru <- data.frame(CostumerID=input$CustomerID,
                           recency=input$recency, frequency=input$frequency,
                           monitery=input$monitery)
    mean_freq <- 4.272015
    sd_freq <- 7.697998
    
    mean_monitery <- 419.1663
    sd_monitery <- 1796.538
    
    databaru$frequency_standard <- (databaru$frequency - mean_freq)/sd_freq
    
    databaru$monitery_standard <- (databaru$monitery - mean_monitery)/sd_monitery
    
    Identitas.Cluster <- readRDS(file="data/cluster_rfm.rds")
    
  
      
    #Masukkan perintah untuk penggabungan data
    #databaru <- merge(databaru)
    
    #menentukan data baru di cluster mana
    #which.min(sapply( 1:4, function( x ) sum( ( databaru[Identitas.Cluster$column] - Identitas.Cluster$Segmentasi$centers[x,])^2 ) ))
    #Identitas.Cluster$Segmen.Pelanggan[which.min(sapply( 1:4, function( x ) sum( ( databaru[Identitas.Cluster$column] - Identitas.Cluster$Segmentasi$centers[x,])^2 ) )),]
  
    #column <- c("recency", "monitery")
    data_hasil <- data.frame(
      cbind(databaru,
            Identitas.Cluster$Segmen.Pelanggan[which.min(sapply( 1:4, function( x )
              sum( ( databaru[Identitas.Cluster$column] - Identitas.Cluster$Segmentasi$centers[x,])^2 ) )),]))
     datatable(data_hasil,
               options = list(
                 pageLength=10, scrollX=T))
  })
  
  output$nText <- renderDataTable({
    ntext()
  })
  
}

shinyApp(ui, server)

#https://www.youtube.com/watch?v=w25jdm-E8pE