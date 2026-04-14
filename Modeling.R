#Modeling 

library(tidyverse)
library(nlme)
df<-read_csv("full.csv")
model <- lm(rodent_rate ~ date+tmax+hot_day+extreme_heat+heat_wave_start, data = df)
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
df$time_index <- 1:nrow(df)
fit_base <- gls(rodent_rate ~ time_index + tmax + hot_day + extreme_heat + heat_wave_start,
                data = df, method = "ML")

fit_ar1  <- gls(rodent_rate ~ time_index + tmax + hot_day + extreme_heat + heat_wave_start,
                data = df, method = "ML",
                correlation = corAR1(form = ~ time_index))
AIC(fit_base, fit_ar1)
#autocorrelation in residuals is negligible (diff in AIC is tiny)
fit_final <- lm(rodent_rate ~ time_index + tmax + hot_day + extreme_heat + heat_wave_start,
                data = df)
summary(fit_final)
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
df$any_rodent <- as.integer(df$rodent_rate > 0)
fit_part1 <- glm(any_rodent ~ date + tmax + heat_wave_start,
                 data = df,
                 family = binomial(link = "logit"))

summary(fit_part1)
exp(coef(fit_part1))

# Model 1: continuous heat only
fit1 <- glm(any_rodent ~ date +heat_wave_start, 
            data = df, family = binomial(link = "logit"))
# Model 2: threshold effect
fit2 <- glm(any_rodent ~ date + tmax +heat_wave_start , 
            data = df, family = binomial(link = "logit"))

# Model 3: extreme events only
fit3 <- glm(any_rodent ~ date + tmax + extreme_heat + heat_wave_start, 
            data = df,family = binomial(link = "logit"))

AIC(fit1, fit2, fit3)
#fit 2 appears to be the best

summary(fit2)
exp(coef(fit2))



#violation occurs regardless of heat??
library(betareg)

#days with at least some violation
df_nonzero <- df[df$rodent_rate > 0, ]
df_nonzero$date_scaled <- scale(as.numeric(df_nonzero$date))

fit_part2 <- betareg(rodent_rate ~ date_scaled + tmax + heat_wave_start,
                     data = df_nonzero)
summary(fit_part2)




