library(astsa)
library(tseries)
library(forecast)
library(ggplot2)

train <- read.csv('train.csv')
test <- read.csv('test.csv')
br.train <- ts(train$Bankruptcy_Rate)
s_train <- train
s_train[,c(2,3,5)] <- scale(s_train[,c(2,3,5)])


plot(br.train) # variances grow with time - transforming with log
lbr <- log(br.train)
plot(lbr) # looks homoscedastic, but clearly not stationary
lbr1 <- diff(lbr)
plot(lbr1) # now looks stationary, let's perform dickey-fuller test
adf.test(lbr1) # it is stationary indeed

par(mfrow=c(2,1))
acf(lbr1, lag.max = 100)
pacf(lbr1, lag.max = 100)
# seasonality is clear by looking at plots

lbr1.12 <- diff(lbr1, lag = 12)
acf(lbr1.12, lag.max = 100)
pacf(lbr1.12, lag.max = 100)

# whatever, just attemtping ARIMA fit:
auto.arima(lbr1, allowdrift = F)
auto.arima(lbr1.12, allowdrift = F)

# checking other stuff
ndiffs(br.train)
nsdiffs(lbr1, m=12)

# fitting model
m.ml <- arima(log(train$Bankruptcy_Rate), order = c(2,1,1), seasonal = list(order = c(1,1,0), period = 12), method = "ML")

#residual plot - testing normality
plot(m.ml$residuals)
qqnorm(m.ml$residuals)
qqline(m.ml$residuals)
shapiro.test(m.ml$residuals) # normal enough

# diagnostics
tsdiag(m.ml) # model sucks

# checking which variables might be relevant to predict bankruptcy rate
summary(lm(Bankruptcy_Rate ~ Population, data = s_train)) # pvalue = 0
summary(lm(Bankruptcy_Rate ~ House_Price_Index, data = s_train)) # pvalue = 0
summary(lm(Bankruptcy_Rate ~ Unemployment_Rate, data = s_train)) # pvalue = 0
summary(lm(Bankruptcy_Rate ~ ., data = s_train)) # pvalue = 0
# all regressors seem valuable for prediction

# using other variables and creating model
m.ml <- arima(train2.tr$Bankruptcy_Rate, xreg = train2.tr[,c(3,5)],
              order = c(2,1,3), seasonal = list(order = c(1,0,1), period = 12),
              method = "ML")
m.ml
plot(m.ml$residuals)
adf.test(m.ml$residuals) # stationarity confirmed
qqnorm(m.ml$residuals)
qqline(m.ml$residuals)
shapiro.test(m.ml$residuals) # residuals are normal
tsdiag(m.ml)
Acf(m.ml$residuals)
Pacf(m.ml$residuals)
# best model so far

m.ml2 <- arima(train2.tr$Bankruptcy_Rate, xreg = train2.tr[,c(5)],
              order = c(2,1,1), seasonal = list(order = c(1,0,1), period = 12),
              method = "ML")
m.ml2
plot(m.ml2$residuals)
adf.test(m.ml2$residuals) # stationarity confirmed
qqnorm(m.ml2$residuals)
qqline(m.ml2$residuals)
shapiro.test(m.ml2$residuals) # residuals are normal
tsdiag(m.ml2)
Acf(m.ml2$residuals)
Pacf(m.ml2$residuals)

# initially chosen models
m1 <- arima(train2.tr$Bankruptcy_Rate, xreg = train2.tr[,c(3,5)],
            order = c(2,1,3), seasonal = list(order = c(1,0,1), period = 12),
            method = "ML")
m2 <- arima(train2.tr$Bankruptcy_Rate, xreg = train2.tr[,c(2,3,5)],
            order = c(2,1,3), seasonal = list(order = c(1,0,1), period = 12),
            method = "ML")
m3 <- arima(train2.tr$Bankruptcy_Rate, xreg = train2.tr[,c(3, 5)],
            order = c(3,1,3), seasonal = list(order = c(2,0,3), period = 12),
            method = "ML")
m4 <- arima(train2.tr$Bankruptcy_Rate, xreg = train2.tr[,c(5)],
            order = c(2,1,1), seasonal = list(order = c(1,0,1), period = 12),
            method = "ML")
m5 <- arima(train2.tr$Bankruptcy_Rate, xreg = train2.tr[,c(3,5)],
            order = c(2,1,3), seasonal = list(order = c(0,0,0), period = 12),
            method = "ML")


# predictions for the training set (to calculate MSE)
mfit1 <- fitted(m1)
mfit2 <- fitted(m2)
mfit3 <- fitted(m3)
mfit4 <- fitted(m4)
mfit5 <- fitted(m5)

# predictions for the test set (to calculate MSPE)
mpred1 <- predict(m1, n.ahead=58, newxreg = train2.te[, c(3, 5)])
mpred2 <- predict(m2, n.ahead=58, newxreg = train2.te[, c(2,3,5)])
mpred3 <- predict(m3, n.ahead=58, newxreg = train2.te[, c(3, 5)])
mpred4 <- predict(m4, n.ahead=58, newxreg = train2.te[, c(5)])
mpred5 <- predict(m5, n.ahead=58, newxreg = train2.te[, c(3, 5)])

# making lists of models and predictions to loop through
models <- list(m1, m2, m3, m4, m5)
modfit <- list(mfit1, mfit2, mfit3, mfit4, mfit5)
modpred <- list(mpred1, mpred2, mpred3, mpred4, mpred5)

# matrix that shows AIC, MSE and MSPE for each model
sum_matrix <- matrix(nrow = length(models), ncol=3)
colnames(sum_matrix) <- c("AIC", "MSE", "MSPE")

# loop to get key indicators for each model
for (i in 1:length(models)) {
  fitted_values <- as.vector(modfit[i][[1]])
  sq_err_train <- (train2.tr$Bankruptcy_Rate - fitted_values)^2
  pred_values <- as.vector(modpred[i][[1]]$pred)
  sq_err_test <- (train2.te$Bankruptcy_Rate - pred_values)^2
  sum_matrix[i, 1] <- models[i][[1]]$aic
  sum_matrix[i, 2] <- sum(sq_err_train)
  sum_matrix[i, 3] <- sum(sq_err_test)
}

# first and second models are the best, but the plots did not convince me..
sum_matrix
best <- m1

# running diagnostics for the best model
par(mfrow=c(1,1))
plot(best$residuals)
# looks stationary. adf test:
adf.test(best$residuals) # stationarity confirmed
# ACF and PACF plots
par(mfrow=c(2,1))
Acf(best$residuals)
Pacf(best$residuals)
# Checking normality
par(mfrow=c(1,1))
qqnorm(best$residuals)
qqline(best$residuals)
shapiro.test(best$residuals) # residuals are normal
# Testing for autocorrelation of residuals
tsdiag(best) # good up to lag 5

# Plot comparing fitted values and actual values in train2.tr
par(mfrow=c(1,1))
plot(ts(train2.tr$Bankruptcy_Rate), main = "Fitted values for Bankruptcy Rate")
points(mfit1,type='l',col='red')

# Plot for the predicted vs. actual values in the train2.te set
t <- time(train2.te$Bankruptcy_Rate)
par(mfrow=c(1,1))
plot(ts(train2.te$Bankruptcy_Rate), ylim = c(-4,-3),
     main = "Predicted values for Bankruptcy Rate")
points(t, mpred1$pred, type='l',col='red')


# Running model again to capture the whole training set
m <- arima(train2$Bankruptcy_Rate, xreg = train2[,c(3,5)],
           order = c(2,1,3), seasonal = list(order = c(1,0,1), period = 12),
           method = "ML")

# Scaling test set variables accordingly
test.pred <- test
test.pred$Unemployment_Rate <- (test.pred$Unemployment_Rate - attributes(train2$Unemployment_Rate)$`scaled:center`)/attributes(train2$Unemployment_Rate)$`scaled:scale`
test.pred$Population <- (test.pred$Population - attributes(train2$Population)$`scaled:center`)/attributes(train2$Population)$`scaled:scale`
test.pred$House_Price_Index <- (test.pred$House_Price_Index - attributes(train2$House_Price_Index)$`scaled:center`)/attributes(train2$House_Price_Index)$`scaled:scale`

# predicting values and confidence intervals
fit <- predict(m, n.ahead=12, newxreg = test.pred[, c(3, 4)])
upper <- fit$pred + 1.96 * fit$se
lower <- fit$pred - 1.96 * fit$se

# backtransforming from log
fit$pred <- exp(fit$pred)
upper <- exp(upper)
lower <- exp(lower)

# Plot for predicted values (2011 data)
t <- time(fit$pred)
par(mfrow=c(1,1))
plot(ts(train$Bankruptcy_Rate), xlim=c(1,300), main = "2011 Forecast")
abline(v = 289, col='blue', lty=2)
points(t, fit$pred, type='l', col='red')
points(t, upper, type='l', col='green')
points(t, lower, type='l', col='green')










