#Modeling 

library(tidyverse)
library(nlme)
df<-read_csv("full.csv")
train_idx <- sample(1:nrow(df), 0.8 * nrow(df))

train_df <- df[train_idx, ]
test_df <- df[-train_idx, ]



model <- lm(rodent_rate ~ date+tmax+hot_day+extreme_heat+heat_wave_start, data = train_df)
ljung_box_lm <- Box.test(residuals(model), lag = 12, type = "Ljung-Box")

residuals_ols <- residuals(model)
ljung_box_lm
#significant, seems to have autocorrelation!

# Boxplot by month
#there appears to be no seasonality
boxplot(df$rodent_rate~factor(df$month), ylim=c(0.0,0.5))
par(mfrow = c(1, 2))
acf(residuals_ols,  lag.max = 20, main = "ACF")
pacf(residuals_ols, lag.max = 20, main = "PACF")
fit_base <- gls(rodent_rate ~ date + tmax + hot_day + extreme_heat + heat_wave_start,
                data = train_df, method = "ML")



fit_ar1  <- gls(rodent_rate ~ date + tmax + hot_day + extreme_heat + heat_wave_start,
                data = train_df, method = "ML",
                correlation = corAR1(form = ~ date))
AIC(fit_base, fit_ar1)
#autocorrelation in residuals is negligible (diff in AIC is tiny)
fit_final <- lm(rodent_rate ~ date + tmax + heat_wave_start,
                data = train_df)
summary(fit_final)


#testing
test_df$pred_linear <- predict(fit_final, newdata = test_df)
test_df$pred_linear <- pmax(0, pmin(1, test_df$pred_linear))

library(Kendall)
MannKendall(df$rodent_rate)
#tau = 0.0951, 2-sided pvalue =< 2.22e-16
#statistically significant upward trend, but it's very weak (tau close to 0)
par(mfrow = c(2, 2))
plot(fit_final)          
summary(fit_final)      
confint(fit_final)       
library(betareg)
#this dont look so good for 0 rodent rates
#divded the problem into 2
train_df$any_rodent <- as.integer(train_df$rodent_rate > 0)
fit_part1 <- glm(any_rodent ~ date + tmax + heat_wave_start,
                 data = train_df,
                 family = binomial(link = "logit"))

summary(fit_part1)
exp(coef(fit_part1))

# Model 1: continuous heat only
fit1 <- glm(any_rodent ~ date +heat_wave_start, 
            data = train_df, family = binomial(link = "logit"))
# Model 2: threshold effect
fit2 <- glm(any_rodent ~ date + tmax +heat_wave_start , 
            data = train_df, family = binomial(link = "logit"))

# Model 3: extreme events only
fit3 <- glm(any_rodent ~ date + tmax + extreme_heat + heat_wave_start, 
            data = train_df,family = binomial(link = "logit"))

AIC(fit1, fit2, fit3)
#fit 2 appears to be the best

summary(fit2)
exp(coef(fit2))


#violation occurs regardless of heat??
library(betareg)

#days with at least some violation
train_nonzero <- train_df[train_df$rodent_rate > 0, ]
train_nonzero$date_scaled <- scale(as.numeric(train_nonzero$date))

fit_part2 <- betareg(rodent_rate ~ date_scaled + tmax + heat_wave_start,
                     data = train_nonzero)
fit_part2_none_heat <- betareg(rodent_rate ~ date_scaled,
                     data = train_nonzero)
dim(train_nonzero)
summary(fit_part2)
library(lmtest)
lrtest(fit_part2_none_heat, fit_part2)
##TESTING
test_df$pred_logistic <- predict(fit2, newdata = test_df, type = "response")

rmse_logistic <- sqrt(mean((test_df$rodent_rate - test_df$pred_logistic)^2, na.rm = TRUE))
mae_logistic <- mean(abs(test_df$rodent_rate - test_df$pred_logistic), na.rm = TRUE)

test_nonzero <- test_df[test_df$rodent_rate > 0, ]
test_nonzero$date_scaled <- scale(as.numeric(test_nonzero$date))

test_nonzero$pred_beta <- predict(fit_part2, newdata = test_nonzero, type = "response")

rmse_beta <- sqrt(mean((test_nonzero$rodent_rate - test_nonzero$pred_beta)^2))
mae_beta <- mean(abs(test_nonzero$rodent_rate - test_nonzero$pred_beta))
test_df$pred_beta <- NA
test_df$pred_beta[test_df$rodent_rate > 0] <- test_nonzero$pred_beta

#cmbinedlogistic probability * beta magnitude
test_df$pred_combined <- test_df$pred_logistic * test_df$pred_beta

rmse_combined <- sqrt(mean((test_df$rodent_rate - test_df$pred_combined)^2, na.rm = TRUE))
mae_combined <- mean(abs(test_df$rodent_rate - test_df$pred_combined), na.rm = TRUE)
