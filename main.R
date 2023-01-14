library(tidyverse)
library(MASS)
library(glmnet)
library(randomForest)
library(caret)
library(xgboost)


# Reading in data
df_2021 <- read.csv("data/df_2021.csv")
colnames(df_2021) <- c('station', 'yr', 'mon', 'day', 'total_rainfall', '30min', 
                       '60min', '120min', 'mean_temp', 'max_temp', 'min_temp',
                       'mean_windspeed', 'max_windspeed')

df_2020 <- read.csv("data/df_2020.csv")
colnames(df_2020) <- c('station', 'yr', 'mon', 'day', 'total_rainfall', '30min', 
                       '60min', '120min', 'mean_temp', 'max_temp', 'min_temp',
                       'mean_windspeed', 'max_windspeed')
df_2020$total_rainfall <- as.numeric(df_2020$total_rainfall)
df_2020$mean_temp <- as.numeric(df_2020$mean_temp)
df_2020$max_temp <- as.numeric(df_2020$max_temp)
df_2020$min_temp <- as.numeric(df_2020$min_temp)
df_2020$mean_windspeed <- as.numeric(df_2020$mean_windspeed)
df_2020$max_windspeed <- as.numeric(df_2020$max_windspeed)

df_2019 <- read.csv("data/df_2019.csv")
colnames(df_2019) <- c('station', 'yr', 'mon', 'day', 'total_rainfall', '30min', 
                       '60min', '120min', 'mean_temp', 'max_temp', 'min_temp',
                       'mean_windspeed', 'max_windspeed')
df_2019$mean_temp <- as.numeric(df_2019$mean_temp)
df_2019$max_temp <- as.numeric(df_2019$max_temp)
df_2019$min_temp <- as.numeric(df_2019$min_temp)
df_2019$mean_windspeed <- as.numeric(df_2019$mean_windspeed)
df_2019$max_windspeed <- as.numeric(df_2019$max_windspeed)

df_2018 <- read.csv("data/df_2018.csv")
colnames(df_2018) <- c('station', 'yr', 'mon', 'day', 'total_rainfall', '30min', 
                       '60min', '120min', 'mean_temp', 'max_temp', 'min_temp',
                       'mean_windspeed', 'max_windspeed')

stn_code_rgn <- read.csv("data/station_code_region.csv")



# Data pre-processing
# Missing data were omitted as there were some stations across Singapore which did not have any data collected.
df_2019 <- left_join(df_2019, stn_code_rgn, by = 'station')
df_2020 <- left_join(df_2020, stn_code_rgn, by = 'station')
df_2021 <- left_join(df_2021, stn_code_rgn, by = 'station')
df_2018 <- left_join(df_2018, stn_code_rgn, by = 'station')

df_2018 <- df_2018[, -c(14, 15)]
df_2019 <- df_2019[, -c(14, 15)]
df_2020 <- df_2020[, -c(14, 15)]
df_2021 <- df_2021[, -c(14, 15)]

df_2018 <- drop_na(df_2018, region)
df_2019 <- drop_na(df_2019, region)
df_2020 <- drop_na(df_2020, region)
df_2021 <- drop_na(df_2021, region)

# Summarise data by each region and day for each year
df_2018_new <- df_2018 %>% group_by(mon, day, region) %>%
  summarise(
    total_rain = sum(total_rainfall, na.rm = TRUE),
    mean_rain = mean(total_rainfall, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    max_temp = max(max_temp, na.rm = TRUE),
    min_temp = min(min_temp, na.rm = TRUE),
    mean_windspeed = mean(mean_windspeed, na.rm = TRUE),
    max_windspeed = max(max_windspeed, na.rm = TRUE)
  )
df_2018_new$year = 2018
df_2018_new$Y <- ifelse(df_2018_new$total_rain > 0, 1, 0)

df_2019_new <- df_2019 %>% group_by(mon, day, region) %>%
  summarise(
    total_rain = sum(total_rainfall, na.rm = TRUE),
    mean_rain = mean(total_rainfall, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    max_temp = max(max_temp, na.rm = TRUE),
    min_temp = min(min_temp, na.rm = TRUE),
    mean_windspeed = mean(mean_windspeed, na.rm = TRUE),
    max_windspeed = max(max_windspeed, na.rm = TRUE)
  )
df_2019_new$year = 2019
df_2019_new$Y <- ifelse(df_2019_new$total_rain > 0, 1, 0)

df_2020_new <- df_2020 %>% group_by(mon, day, region) %>%
  summarise(
    total_rain = sum(total_rainfall, na.rm = TRUE),
    mean_rain = mean(total_rainfall, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    max_temp = max(max_temp, na.rm = TRUE),
    min_temp = min(min_temp, na.rm = TRUE),
    mean_windspeed = mean(mean_windspeed, na.rm = TRUE),
    max_windspeed = max(max_windspeed, na.rm = TRUE)
  )
df_2020_new$year = 2020
df_2020_new$Y <- ifelse(df_2020_new$total_rain > 0, 1, 0)

df_2021_new <- df_2021 %>% group_by(mon, day, region) %>%
  summarise(
    total_rain = sum(total_rainfall, na.rm = TRUE),
    mean_rain = mean(total_rainfall, na.rm = TRUE),
    mean_temp = mean(mean_temp, na.rm = TRUE),
    max_temp = max(max_temp, na.rm = TRUE),
    min_temp = min(min_temp, na.rm = TRUE),
    mean_windspeed = mean(mean_windspeed, na.rm = TRUE),
    max_windspeed = max(max_windspeed, na.rm = TRUE)
  )
df_2021_new$year = 2021
df_2021_new$Y <- ifelse(df_2021_new$total_rain > 0, 1, 0)

rm(df_2018)
rm(df_2019)
rm(df_2020)
rm(df_2021)



# Split train (2018, 2019, 2020), test (2021) data
df_train <- do.call(rbind, list(df_2018_new, df_2019_new, df_2020_new))

df_train <- drop_na(df_train, mon)
df_train$mean_temp[is.nan(df_train$mean_temp)] <- NA
df_train$max_temp[sapply(df_train$max_temp, is.infinite)] <- NA
df_train$min_temp[sapply(df_train$min_temp, is.infinite)] <- NA

df_2021_new <- drop_na(df_2021_new, mon)
df_2021_new$mean_temp[is.nan(df_2021_new$mean_temp)] <- NA
df_2021_new$max_temp[sapply(df_2021_new$max_temp, is.infinite)] <- NA
df_2021_new$min_temp[sapply(df_2021_new$min_temp, is.infinite)] <- NA



# Exploratory data analysis
df_monthly <- df_train %>% group_by(year, mon) %>%
  summarise(
    mean_rainfall = mean(total_rain, na.rm = TRUE)
  )

plot(df_monthly$mean_rainfall[df_monthly$year == 2018], type = 'l', col = 'blue',
     ylab = "Mean monthly rainfall",xlab = 'Months', ylim = c(0, 160))
lines(df_monthly$mean_rainfall[df_monthly$year == 2019], col = 'coral1')
lines(df_monthly$mean_rainfall[df_monthly$year == 2020], col= 'magenta1')
legend(10, 50, lty = 1, legend = c(2018, 2019, 2020), 
       col = c('blue', 'coral1', 'magenta1'), cex = 0.8)


par(mfrow = c(1,2))
barplot(table(df_train$Y), xlab = 'Rainfall', ylab = 'Counts', main = 'Proportion of Rainy vs Non-rainy days in training',
        cex.main = 0.8)
barplot(table(df_2021_new$Y), xlab = 'Rainfall', ylab = 'Counts', main = 'Proportion of Rainy vs Non-rainy days in testing',
        cex.main = 0.8)


# Check for outliers
par(mfrow = c(2,1))
hist(df_train$mean_temp, xlab = "Mean temperature", main = "Histogram of mean temperature", breaks = 30)
hist(df_train$mean_windspeed, xlab = "Mean windspeed", main = "Histogram of mean windspeed", breaks = 30)

hist(df_train$total_rain, xlab = "Total Rainfall", main = "Histogram of total rainfall", breaks = 150)


# Correlation
cor_data <- cor(df_train[-c(3,11)])
cor_data[lower.tri(cor_data,diag=TRUE)] <- NA
cor_data <- as.data.frame(as.table(cor_data))
cor_data <- na.omit(cor_data)
cor_data <- cor_data[order(-abs(cor_data$Freq)),]


# VIF
df_train2 <- df_train[-c(4, 11)]
df_train2$Y <- as.factor(df_train2$Y)
df_train2$region <- as.factor(df_train2$region)
df_train2[c(6:nrow(df_train2)),c(4:9)] <- df_train2[1:(nrow(df_train2)-5),c(4:9)]
df_train2 <- df_train2[-c(1:5),]
df_train2 <- na.omit(df_train2)

df_test <- df_2021_new[-c(4, 11)]
df_test$Y <- as.factor(df_test$Y)
df_test$region <- as.factor(df_test$region)
df_test[c(6:nrow(df_test)),c(4:9)] <- df_test[1:(nrow(df_test)-5),c(4:9)]
df_test <- df_test[-c(1:5),]
df_test <- na.omit(df_test)

data.frame('columns' = colnames(df_train2[,c(5,6,7)],),
           'VIF' = diag(ginv(cor(df_train2[,c(5,6,7)]))))


# Split train to train and validation
# 2018 and 2019 used for train, 2020 used for validation
df_subtrain <- df_train[df_train$year != 2020,]
df_subtrain <- df_subtrain[-c(4, 11)]
df_subtrain$Y <- as.factor(df_subtrain$Y)
df_subtrain$region <- as.factor(df_subtrain$region)
df_subtrain[c(6:nrow(df_subtrain)),c(4:9)] <- df_subtrain[1:(nrow(df_subtrain)-5),c(4:9)]
df_subtrain <- df_subtrain[-c(1:5),]
df_subtrain <- na.omit(df_subtrain)

df_val <- df_train[df_train$year == 2020,]
df_val <- df_val[-c(4, 11)]
df_val$Y <- as.factor(df_val$Y)
df_val$region <- as.factor(df_val$region)
df_val[c(6:nrow(df_val)),c(4:9)] <- df_val[1:(nrow(df_val)-5),c(4:9)]
df_val <- df_val[-c(1:5),]
df_val <- na.omit(df_val)


barplot(table(df_train2$Y), xlab = 'Rainfall', ylab = 'Counts')
barplot(table(df_test$Y), xlab = 'Rainfall', ylab = 'Counts')



# Baseline model
table(df_test$Y)
1314/(493+1314)


# Logistic Regression
lm1 <- glm(Y~., data = df_train2, family = 'binomial')
summary(lm1)
prob <- predict(lm1, newdata = df_test, type = 'response')
pred <- ifelse(prob > 0.5, 1, 0)
table(predict = pred, truth = df_test$Y)
# accuracy
(44+1293)/nrow(df_test)

## removing the insignificant variables:
lm2 <- glm(Y~. -region, data = df_train2, family = 'binomial')
summary(lm2)
prob <- predict(lm2, newdata = df_test, type = 'response')
pred <- ifelse(prob > 0.5, 1, 0)
table(predict = pred, truth = df_test$Y)
# accuracy
(48 + 1291)/nrow(df_test)


# Random Forest
set.seed(1)
# tuning
accuracy <- c()
# for loop to find the optimal mtry
for (i in 1:8){
  set.seed(1)
  rf.fit <- randomForest(Y ~., data = df_subtrain, mtry = i, importance = TRUE)
  rf.pred <- predict(rf.fit, newdata = df_val, type = 'class')
  confusion_mat <- table(predict = rf.pred, truth = df_val$Y)
  accuracy[i] <- (confusion_mat[1,1]+confusion_mat[2,2])/nrow(df_val)
}
plot(c(1:8), accuracy, type = 'b', xlab = 'mtry')
points(1, accuracy[1], pch = 19, col = 'red')

rf.fit <- randomForest(Y ~., data = df_train2, mtry = 1, importance = TRUE)
rf.pred <- predict(rf.fit, newdata = df_test, type = 'class')
table(predict = rf.pred, truth = df_test$Y)
# accuracy 
(108 + 1248)/ nrow(df_test)

varImpPlot(rf.fit)


# XGBoost
# cv on eta, gamma and max_depth
# eta: 0.2, 0.25, 0.3, 0.35
# gamma: 0, 0.01, 0.1, 0.2
# max_depth: 4, 5, 6, 7

combi <- c()
eta <- c(0.2, 0.25, 0.3, 0.35)
gam <- c(0, 0.01, 0.05, 0.1)
max_dep <- c(4,5,6,7)
idx <- 1
for (i in eta) {
  for (j in gam) {
    for (k in max_dep) {
      combi[[idx]] <- c(i,j,k)
      idx <- idx + 1
    }
  }
}

# one-hot-encode for XGB
dummies <- dummyVars(~ .-Y, data = df_train2)
df_xgb_train2 <- data.frame(predict(dummies, newdata = df_train2))
df_xgb_train2$Y <- df_train2$Y

dummies <- dummyVars(~ .-Y, data = df_test)
df_xgb_test <- data.frame(predict(dummies, newdata = df_test))
df_xgb_test$Y <- df_test$Y

dummies <- dummyVars(~ .-Y, data = df_subtrain)
df_xgb_subtrain <- data.frame(predict(dummies, newdata = df_subtrain))
df_xgb_subtrain$Y <- df_subtrain$Y

dummies <- dummyVars(~ .-Y, data = df_val)
df_xgb_val <- data.frame(predict(dummies, newdata = df_val))
df_xgb_val$Y <- df_val$Y

# cross-validation
accuracy <- c()
for (i in combi) {
  set.seed(1)
  xgb <- xgboost(data = as.matrix(df_xgb_subtrain[,-14]), label = as.numeric(df_xgb_subtrain$Y)-1, objective = 'binary:logistic',
                 nrounds = 10, eta = i[1], gamma = i[2], max_depth = i[3])
  pred <- predict(xgb, as.matrix(df_xgb_val[,-14]))
  pred2 <- ifelse(pred > 0.5, 1, 0)
  confusion_mat <- table(predict = pred2, truth = df_xgb_val$Y)
  accuracy <- c(accuracy, (confusion_mat[1,1]+confusion_mat[2,2])/nrow(df_xgb_val))
}
which(accuracy == max(accuracy))
combi[which(accuracy == max(accuracy))]

accuracy <- c()
for (i in 2:20) {
  set.seed(1)
  xgb <- xgboost(data = as.matrix(df_xgb_subtrain[,-14]), label = as.numeric(df_xgb_subtrain$Y)-1, objective = 'binary:logistic',
                 nrounds = i, eta = 0.2, gamma = 0.01, max_depth = 5)
  pred <- predict(xgb, as.matrix(df_xgb_val[,-14]))
  pred2 <- ifelse(pred > 0.5, 1, 0)
  confusion_mat <- table(predict = pred2, truth = df_xgb_val$Y)
  accuracy <- c(accuracy, (confusion_mat[1,1]+confusion_mat[2,2])/nrow(df_xgb_val))
}
plot(c(2:20), accuracy, type = 'b', xlab = 'nrounds')
points(8, accuracy[7], pch = 19, col = 'red')

# result
set.seed(1)
xgb <- xgboost(data = as.matrix(df_xgb_train2[,-14]), label = as.numeric(df_xgb_train2$Y)-1,
               objective = 'binary:logistic',
               nrounds = 8, eta = 0.2, gamma = 0.01, max_depth = 5)
pred <- predict(xgb, as.matrix(df_xgb_test[,-14]))
pred2 <- ifelse(pred > 0.5, 1, 0)
table(predict = pred2, truth = df_xgb_test$Y)
# accuracy 
(157+1222)/nrow(df_test)


