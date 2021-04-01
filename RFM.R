#RFM
library(tidyverse)
library(lubridate)
library(highcharter)
library(plotly)

df_data <- read.csv("data/data.csv")

# membuang anomali data
df_data <- df_data %>% 
  mutate(Quantity = replace(Quantity, Quantity<=0, NA),
         UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))

df_data <- df_data %>%
  drop_na()

# membuat data RFM
df_data <- df_data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

df_data <- df_data %>% 
  mutate(total_dolar = Quantity*UnitPrice)

df_data$days <- weekdays(df_data$InvoiceDate)
df_data$mday <- mday(as.POSIXlt(df_data$InvoiceDate))

df_data$days <- as.factor(df_data$days)
glimpse(df_data)

# total customer
customer <- df_data %>%
  group_by(InvoiceDate) %>%
  summarise(total_customer = n_distinct(CustomerID))

customer$month <- floor_date(customer$InvoiceDate, "month")

customer <- customer %>%
  group_by(month) %>%
  summarize(total_customer = sum(total_customer)) %>%
  filter(month != "2011-12-01") 

ggplot(customer, aes(month, total_customer)) +
  geom_line()

plot_ly(customer, x = ~month, y = ~total_customer, mode = 'lines',
        color = I('white')) %>%
  layout(xaxis=list(title = ""),
         yaxis=list(title = ""),
         paper_bgcolor='teal',
         plot_bgcolor='teal') %>%
  config(displayModeBar = FALSE)

hc <- customer %>% hchart(
  'line', hcaes(x = month, y = total_customer),
  color = "steelblue"
) %>% hc_credits(enabled = FALSE) %>%
  hc_add_theme(hc_theme_sparkline_vb())

hc

# total transaksi
transaction <- df_data %>%
  group_by(InvoiceDate) %>%
  summarise(total_transaction = n_distinct(InvoiceNo))

#round dates down to week
transaction$month <- floor_date(transaction$InvoiceDate, "month")

transaction %>%
  group_by(month) %>%
  summarize(total_transaction = sum(total_transaction)) %>%
  filter(month != "2011-12-01") %>%
  ggplot(aes(month, total_transaction)) +
  geom_line()

#penjualan
sales <- df_data %>%
  mutate(total_dolar = Quantity*UnitPrice) %>%
  group_by(InvoiceDate) %>%
  summarise(total_sales = sum(total_dolar))

sales$month <- floor_date(sales$InvoiceDate, "month")

sales %>%
  group_by(month) %>%
  summarize(total_sales = sum(total_sales)) %>%
  filter(month != "2011-12-01") %>%
  ggplot(aes(month, total_sales)) +
  geom_line()

#new customer
sales_data <- df_data
str(sales_data)

sales_data <- sales_data %>%
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
New_and_Returning_Customers

#find mean sales by month
New_and_Returning_Customers %>%
  filter(Date != "2011-12-01") %>%
  ggplot(aes(Date, New_Cus)) +
  geom_line()

New_and_Returning_Customers %>%
  filter(Date != "2011-12-01") %>%
  ggplot(aes(Date, Return_Cus)) +
  geom_line()


## RFM
df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), 
            monitery= sum(total_dolar)/n_distinct(InvoiceNo)) 

summary(df_RFM)

## recency segmentation

ggplot(df_RFM, aes(x=recency)) +
  geom_histogram(bins = 396)

#data_recency <- final_data[c("recency", "recency_segment")]

## kmeans
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


#standarisasi data
df_RFM$frequency_standard <- scale(df_RFM$frequency)
df_RFM$monitery_standard <- scale(df_RFM$monitery)

summary(scale(df_RFM$frequency))
summary(scale(df_RFM$monitery))

set.seed(1)
fviz_nbclust(df_RFM[c("frequency_standard", "monitery_standard")], kmeans, method = "wss")
fviz_nbclust(df_RFM[c("frequency_standard", "monitery_standard")], kmeans, method = "silhouette")

segmentasi <- kmeans(x=df_RFM[c("frequency_standard", "monitery_standard")], 
                     centers=4, nstart=25)
segmentasi$centers
summary(segmentasi$centers)

df_RFM$cluster <- segmentasi$cluster

df_RFM %>%
  group_by(cluster) %>%
  summarise(mean_freq = mean(frequency),
            mean_monitery = mean(monitery))

# # A tibble: 4 x 3
# cluster mean_freq mean_monitery
# *   <int>     <dbl>         <dbl>
# 1       1      1.5         80710. special
# 2       2      2.85          369. low
# 3       3     18.7           529. medium
# 4       4    122.            823. high

df_RFM <- df_RFM%>%
  mutate(segmentation = case_when(cluster == 1 ~ "Special Value",
                                  cluster == 2 ~ "Low Value",
                                  cluster == 3 ~ "Medium Value",
                                  cluster == 4 ~ "High Value",
                                  TRUE ~ "Other"))

ggplot(df_RFM, aes(frequency, monitery, color=factor(segmentation))) +
  geom_point() +
  labs(title = "Customer Value Segmentation",
       color = "Segmentation") 

## predict
Segmen.Pelanggan <- data.frame(cluster=c(1,2,3,4), 
                               Value.Segment=c("Special Value", "Low Value",
                                             "Medium Value", "High Value"))

#Menggabungkan seluruh aset ke dalam variable Identitas.Cluster
Identitas.Cluster <- list(Segmentasi=segmentasi, 
                          Segmen.Pelanggan=Segmen.Pelanggan, 
                          column=c("frequency_standard", "monitery_standard"))
saveRDS(Identitas.Cluster,"data/cluster_rfm.rds")

databaru <- data.frame(CustomerID="CUST-100", recency=12,
                       frequency=209,monitery=161.3384)

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
which.min(sapply( 1:4, function( x )
  sum( ( databaru[Identitas.Cluster$column] - 
           Identitas.Cluster$Segmentasi$centers[x,])^2 ) ))
Identitas.Cluster$Segmen.Pelanggan[which.min(sapply( 1:4, function( x ) sum( ( databaru[Identitas.Cluster$column] - Identitas.Cluster$Segmentasi$centers[x,])^2 ) )),]


# Total customer per segment
customer_segment <- plyr::count(df_RFM$segmentation)
customer_segment <- customer_segment %>%
  group_by(x) %>%
  mutate(percen = freq/sum(customer_segment$freq))

# total monatory per recency
df_RFM <- df_RFM%>%
  mutate(recency_segment = case_when(recency <= 30 ~ "Active",
                                  recency <= 90 ~ "Warm",
                                  recency <= 180 ~ "Cold",
                                  recency >= 180 ~ "Inactive",
                                  TRUE ~ "Other"))
df_RFM %>%
  group_by(recency_segment) %>%
  summarise(monatory_segment = sum(monitery)) %>%
  mutate(percen = monatory_segment/sum(monatory_segment) *100) %>%
  ggplot(aes(recency_segment, percen)) +
  geom_bar(stat = "identity")

# total monatory per segment
df_RFM %>%
  group_by(segmentation) %>%
  summarise(monatory_segment = sum(monitery)) %>%
  mutate(percen = monatory_segment/sum(monatory_segment) *100) %>%
  ggplot(aes(segmentation, percen)) +
  geom_bar(stat = "identity")

table(df_RFM$recency_segment, df_RFM$segmentation)

final_data <- inner_join(df_data, df_RFM, by="CustomerID")

sum_customer <- length(unique(final_data$CustomerID))
sum_monetery <- sum(final_data$total_dolar)

#mendapatkan bulan
final_data$month <- months(final_data$InvoiceDate)

## percen
cus_17850 <- final_data %>%
  filter(segmentation == "High Value",
         recency_segment == "Active") 

length(unique(cus_17850$CustomerID))/sum_customer * 100
sum(cus_17850$total_dolar)/sum_monetery * 100
## radar

days <-
  plyr::count(cus_17850$days)
colnames(days) <- c("days", "freq")
days
days_cus <- data.frame(
  days = factor(c("Monday", "Tuesday", "Wednesday",
                  "Thursday", "Friday", "Saturday", "Sunday"),
                levels = c("Monday", "Tuesday", "Wednesday",
                           "Thursday", "Friday", "Saturday", "Sunday")))


data_df <- left_join(days_cus, days, by="days")
ggplot(data_df, aes(days, freq, fill=days)) +
  geom_bar(stat = "identity") +
  coord_polar()

## behavoir
#individu
#mdays

customer_individu <- final_data %>%
  filter(CustomerID == 13047)

date_day <-
  plyr::count(customer_individu$mday)
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
  labs(title = "Monthly Order Habbit") +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank()
  )

#month
date_month <- plyr::count(customer_individu$month)
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

##summary

input_recency <- c("Active", "Warm", "Cold")
input_value <- c("Low Value", "Medium Value")

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
  labs(title = "Monthly Order Habbit") +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank()
  )

#month
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

total_customer <- length(unique(final_data$CustomerID))
summary_total_customer <- length(unique(summary_customer$CustomerID))
percen_customer <- round(summary_total_customer/total_customer*100, 2)

total_monetary <- sum(final_data$total_dolar)
summary_total_monetary <- sum(summary_customer$total_dolar)
percen_monetary <- round(summary_total_monetary/total_monetary * 100, 2)

