install.packages('lmtest') 
library(lmtest)
install.packages("nlme")
library(nlme)
install.packages("ggplot2")
library(ggplot2)
install.packages("tseries")
library(tseries)
install.packages("car")
library(car)
install.packages("reshape2")
library(reshape2)
data <- read.csv("Desktop/stat 4205/BTC_3.csv")
#change the unit and remove one outlier
data$BTC.Volume <- data$BTC.Volume / 1000000
max_volume<- max(data$BTC.Volume)
data<-data[data$BTC.Volume !=max_volume, ]

#simple linear regression OLS
model_slr <- lm(BTC ~ BTC.Volume, data=data)
summary(model_slr)
residuals_slr<- residuals(model_slr)
fitted_values_slr<- predict(model_slr)

coef<-summary(model_slr)$coefficients
slope <- coef[2]
intercept <- coef[1]
new_x <- seq(min(data$BTC.Volume), max(data$BTC.Volume), length.out = 100)
new_y <- slope * new_x + intercept
plot(data$BTC.Volume, data$BTC, pch = 16, xlab = "BTC.Volume", ylab = "BTC", main = "OLS Fit")
lines(new_x, new_y, col = "red")


#check linearity between residuals and fitted values
ggplot(data = data.frame(fitted_values_slr, residuals_slr), aes(x = fitted_values_slr, y = residuals_slr)) +
geom_point(alpha = 0.75) + # alpha for transparency
labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
theme(plot.title = element_text(size = 18), axis.text = element_text(size = 15), axis.title = element_text(size = 15))

# check normality of residuals
qqnorm(residuals_slr)
qqline(residuals_slr)
# probability density function of residual
ggplot(data.frame(residuals_slr), aes(x = residuals_slr)) +
  geom_density(fill = "blue", alpha = 0.2) +
  labs(title = "Probability density function of residual", x = "Residuals") +
  theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 12))
# Jarque-Bera test for normality
jbtest <- jarque.bera.test(residuals_slr)
print(jbtest)

# check autocorrelation
dwtest <- durbinWatsonTest(residuals_slr)
print(dwtest)

# Run the Breusch-Pagan test to check homoscedasticity 
bptest <- bptest(model_slr)
print(bptest[1:2])

#multiple linear regression
model_mlr<-lm(BTC ~ BTC.Volume+Bitcoin.My.Wallet.Number.of.Transaction.Per.Day+NVDA+Ethereum+DJI, data = data)
summary(model_mlr)
residuals_mlr<- residuals(model_mlr)
fitted_values_mlr<- predict(model_mlr)

#check linearity between residuals and fitted values
ggplot(data = data.frame(fitted_values_mlr, residuals_mlr), aes(x = fitted_values_mlr, y = residuals_mlr)) +
  geom_point(alpha = 0.75) + # alpha for transparency
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme(plot.title = element_text(size = 18), axis.text = element_text(size = 15), axis.title = element_text(size = 15))

# check normality of residuals
qqnorm(residuals_mlr)
qqline(residuals_mlr)
# probability density function of residual
ggplot(data.frame(residuals_mlr), aes(x = residuals_mlr)) +
  geom_density(fill = "blue", alpha = 0.2) +
  labs(title = "Probability density function of residual", x = "Residuals") +
  theme(plot.title = element_text(size = 15), axis.text = element_text(size = 12), axis.title = element_text(size = 12))
# Jarque-Bera test for normality
jbtest <- jarque.bera.test(residuals_mlr)
print(jbtest)

# check autocorrelation
dwtest <- durbinWatsonTest(residuals_mlr)
print(dwtest)

# Run the Breusch-Pagan test to check homoscedasticity 
bptest <- bptest(model_slr)
print(bptest[1:2])

# check multicollinearity
X <- data[ , c('BTC.Volume','Bitcoin.My.Wallet.Number.of.Transaction.Per.Day','NVDA','Ethereum','DJI')]
# compute the correlation matrix
corr_matrix <- cor(X)
# create a heatmap of the correlation matrix
ggplot(melt(corr_matrix), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", high="red", midpoint=0, 
                       limit=c(min(corr_matrix), max(corr_matrix)), 
                       name="Correlation") +
  ggtitle("Correlation Matrix") +
  theme(plot.title = element_text(size = 18), axis.text = element_text(size = 15), axis.title = element_text(size = 15),
        legend.text = element_text(size = 12), legend.title = element_text(size = 12))
# check VIF for multicollinearity
vif_values <- vif(lm(BTC~. , data = data[, c("BTC", colnames(X))])) # replace "BTC.Volume" with your dependent variable name
print(vif_values)

#multiple linear regression GLS
data$Date <- as.Date(data$Date, format = "%m/%d/%y")
model_gls <- gls(BTC ~ BTC.Volume+Bitcoin.My.Wallet.Number.of.Transaction.Per.Day+NVDA+Ethereum+DJI, data = data, correlation = corAR1(form = ~Date))
summary(model_gls)


#backtest
back_df <- read.csv('Desktop/stat 4205/BTC_backtest.csv')
back_df <- back_df[ ,c("Date","BTC","BTC.Volume.mln","Bitcoin.My.Wallet.Number.of.Transaction.Per.Day","NVDA","Ethereum","DJI")]
colnames(back_df)[3] <- "BTC.Volume"
back_df <-na.omit(back_df ) 
XX= back_df[ ,c("BTC.Volume","Bitcoin.My.Wallet.Number.of.Transaction.Per.Day","NVDA","Ethereum","DJI")]
response<-"BTC"
y_test <- back_df[[response]]
# simple linear regression
predictor_slr <- 'BTC.Volume'
BTC.Volume <- back_df[[predictor_slr]]
XX_slr_ac <- cbind(1, BTC.Volume)
y_pred <- predict(model_slr, newdata = data.frame(XX_slr_ac))
mse <- mean((y_pred - y_test)^2)
r2 <- summary(model_slr)$r.squared
print(paste0("MSE: ", mse))
print(paste0("R-squared: ", r2))

#OLS backtesting
y_pred <- predict(model_mlr, newdata = XX)
mse <- mean((y_pred - y_test)^2)
SSres <- sum((y_test - y_pred)^2)
SStot <- sum((y_test - mean(y_test))^2)
r2 <- 1 - SSres/SStot
print(paste0("MSE: ", mse))
print(paste0("R-squared: ", r2))
#GLS backtesting
y_pred <- predict(model_gls, newdata = XX)
mse <- mean((y_pred - y_test)^2)
SSres <- sum((y_test - y_pred)^2)
SStot <- sum((y_test - mean(y_test))^2)
r2 <- 1 - SSres/SStot
print(paste0("MSE: ", mse))
print(paste0("R-squared: ", r2))








