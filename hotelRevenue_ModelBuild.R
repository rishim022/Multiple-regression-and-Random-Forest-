library("readxl")
library(lubridate)
library(dplyr)
my_data <- read_excel("D:/Chrome Downloads/xldatafor R.xlsx")
my_data
View(my_data)
summary(my_data$HotelrevenueinEuro)

set.seed(100) 
trainingRowIndex <- sample(1:nrow(my_data), 0.8*nrow(my_data)) 
trainingData <- my_data[trainingRowIndex, ]  # model training data
testData  <- my_data[-trainingRowIndex, ] 
lmMod <- lm( HotelrevenueinEuro ~ HotelADRinEuro+HotelRevPARinEuro+`Hoteloccupancyin%`+`Hoteldemand(rooms)`, data= trainingData) 
distPred <- predict(lmMod, testData) 
summary (lmMod)
AIC(lmMod)
cor(my_data$HotelrevenueinEuro,my_data$HotelADRinEuro)
cor(my_data$HotelrevenueinEuro,my_data$`Hoteloccupancyin%`)
cor(my_data$HotelrevenueinEuro,my_data$HotelRevPARinEuro)
cor(my_data$HotelrevenueinEuro,my_data$`Hoteldemand(rooms)`)
actuals_preds <- data.frame(cbind(actuals=testData$HotelrevenueinEuro, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds)  
correlation_accuracy
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
mape
RMSE= mean((actuals_preds$actuals - actuals_preds$predicteds)^2)%>% sqrt() 
RMSE
errors<-actuals_preds$predicteds - actuals_preds$actuals
errors
library(caret)
lm1<-train(HotelrevenueinEuro~., data = my_data, method = "lm")
rf1 <- train(HotelrevenueinEuro~., data = my_data, method = "rf")
summary(lm1)
summary(lm1$finalModel)$r.squared
summary(rf1)
rf1$results
pred <- predict( lm1, newdata = validdata, type = 'response')
#Cross validation
tc <- trainControl(method = "cv", number = 10)
lm1_cv <- train(HotelrevenueinEuro~., data = my_data, method = "lm",
                trControl = tc)
lm1_cv

#Comparison of the two models linear regression and random forest
model_list <- list(lm = lm1, rf = rf1)
res <- resamples(model_list)
summary(res)
