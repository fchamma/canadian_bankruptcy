
# ============================== data frame for Felipe's prediction =======================
pred.m <- data.frame("fitted" = fit$pred, "lower" = lower, "upper" = upper)
pred.m


# ============================== Rolling window prediction ================================

# create a new train data frame, train2.rw,  for rolling window prediction
head(train2$Bankruptcy_Rate)
head(train2) 

train2.rw <- train2[,c(3,4,5)]
head(train2.rw)

# make sure that we are using the scaled test data frame, test.pred
head(test)
head(test.pred)  # test.pred is CREATED in FELIPE's CODE

# make sure that we are using the best model chosen previously
best <- m

# select the covariates chosen in the best model
xreg.rw <- train2.rw[,c(1,3)]  # 1 for Population, 3 for House_Price_Index
lower.rw <- c() 
upper.rw <- c() 

# predict for the following 12 months
n <- 12
for (i in 1:n){
    # test.pred[i, c(3, 4)], 3 for Population, 4 for House_Price_Index
    fit.rw <- predict(best, n.ahead = 1, trace = F, newxreg = test.pred[i, c(3, 4)])
    
    # create a new row to add to train2.rw
    # test.pred[i,3] for Population, fit.rw$pred for Bankruptcy_Rate, 
    # test.pred[i,4] for House_Price_Index
    new.row <- c(test.pred[i,3], fit.rw$pred, test.pred[i,4])
    train2.rw <- rbind(train2.rw, new.row)
    
    lower.rw <- c(lower.rw, fit.rw$pred - 1.96*fit.rw$se)
    upper.rw <- c(upper.rw, fit.rw$pred + 1.96*fit.rw$se)
    
    # run the best model again to capture the new train2.rw
    # train2.rw[, c(1, 3)], 1 for Population, 3 for House_Price_Index
    best <- arima(train2.rw$Bankruptcy_Rate, xreg = train2.rw[,c(1,3)],
                  order = c(2,1,3), seasonal = list(order = c(1,0,1), period = 12),
                  method = "ML")
}

pred.rw <- data.frame("fitted"= tail(train2.rw$Bankruptcy_Rate,n),"lower" = lower.rw, "upper" = upper.rw)
pred.rw$fitted <- exp(pred.rw$fitted)
pred.rw$lower <- exp(pred.rw$lower)
pred.rw$upper <- exp(pred.rw$upper)

print(pred.rw)

# Plot for 1-step-ahead rolling window predicted values (2011 data)
par(mfrow=c(1,1))
plot(ts(train$Bankruptcy_Rate), xlim=c(1,300), main = "2011 Rolling-Window Forecast")
abline(v = 289, col='blue', lty=2)
points(289:300, pred.rw$fitted, type='l', col='red')
points(289:300, pred.rw$lower, type='l', col='green')
points(289:300, pred.rw$upper, type='l', col='green')


# A close-up look at 1-step-ahead rolling window predicted values (2011 data)
before <- 12
whole <- before + 12
par(mfrow=c(1,1))
plot(ts(train$Bankruptcy_Rate[1:before]), type='o', xlim=c(1, whole), ylim=c(0,0.1), main = "2011 Rolling-Window Forecast (close-up look)")
abline(v = before, col='blue', lty=2)
points((before+1):whole, pred.rw$fitted, type='o', col='red')
points((before+1):whole, pred.rw$lower, type='o', col='green')
points((before+1):whole, pred.rw$upper, type='o', col='green')