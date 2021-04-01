predict_all <-predict(object = recc_model, newdata=df_train, n = n_recommended, type="topNList")

saveRDS(predict_all, "data/predict_all.RDS")
saveRDS(CustomerId, "data/CustomerId.RDS")
saveRDS(itemCode, "data/itemCode.RDS")
saveRDS(model_details, "data/model_details.RDS")
saveRDS(df_data, "data/df_data.RDS")


predict_all <- readRDS("data/predict_all.RDS")
CustomerId <- readRDS("data/CustomerId.RDS")
itemCode <- readRDS("data/itemCode.RDS")
model_details <- readRDS("data/model_details.RDS")
df_data <- readRDS("data/df_data.RDS")


index_user <- match(14911, CustomerId$CustomerID)
user_1 <- CustomerId[as.integer(names(predict_all@items[index_user]))]

vvv <- predict_all@items[[index_user]]
vvv <- rownames(model_details$sim)[vvv]
itemCode[vvv]

user_1_buy <- df_data[CustomerID==as.integer(user_1), sum(Quantity), by=StockCode]
merge(itemCode,user_1_buy, by='StockCode')


