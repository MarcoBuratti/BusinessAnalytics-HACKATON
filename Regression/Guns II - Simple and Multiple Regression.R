#Import the dataset
data("Guns")
#Clean the data and transfrom certain features to log values to avoid skewness towards large values
Guns$violent <- log(Guns$violent) 
#Guns$income <- log(Guns$income)
Guns$murder <- NULL
Guns$robbery <-NULL
#Compute the simple linear regression for interesting variables onto the response violence (ethnicity, gender,income, living factors)
response <- Guns$violent ##Alway take into consideration that by using the log values, even small coefficients (0.5) have a significant impact
#Ethnicities (Caucasian & Afroamerican)
rg_cauc <- Guns$cauc
rg_afam <- Guns$afam
model_cauc <- lm(response~rg_cauc)
summary(model_cauc)
model_afam <- lm(response~rg_afam)
summary(model_afam)
###Result: Both models are significant to explain both show a small p-value (<2.2e-16) and good/high F-Statistics (334-356)
###The R^2 is relatively small with approx. 22% for each model -> solely ethnicty do not explain a lot of the variance of the dependent variable
### While the estimate coefficient of afam has a low positive impact (0.06) on a prediction of an estimation of violence, the estimated coefficient of
###cauc is slightly negative (0.03). However, both estimates are quite low 

#Gender (only checking for male, opposite effect can be expected for female)
rg_male <- Guns$male
model_male <- lm(response~rg_male)
summary(model_male)
###Result: F-Statistics is really low (24.71) so probably not a good model. R^2 is equally low (2%). P-Value is, however, quite low (7.66e-07)
###The estimated coefficient of the gender male is 
### slighlty negative and significant. However, this model doesn't provide us with a good explanation. 

#Income (log values) <- Use the normal and not the log values
rg_inc <- Guns$income
model_inc <- lm(response~rg_inc)
summary(model_inc)
# Income (normal values, as seen by the data source provider)
rg_inc2 <- Guns$income
model_inc2 <- lm(response~rg_inc2)
summary(model_inc2)
###Result1 (log values): F-Statistic (173) and R^2 are again not high. Providing us with the assumption that this is not a good model fit.
###P-Value is again low <2.2e-16
###Curiosity: the intercept changes to negative value while staying in the previously observed range (5-8). 
###The estimated coefficient, however, is signifcant and highly positive (1.27).
###Result2 (normal values): F-Statistics, R^2 and P-Value remain the same. 
###The intercept becomes positive again while falling a bit shorter than the usual range (4.7 instead of 5-8).
###The estimated coefficient is still positive but non-existent (0.00009173)

#Living factors (population and density) 
rg_pop <- Guns$population
rg_den <- Guns$density
model_pop <- lm(response~rg_pop)
summary(model_pop)
model_den <- lm(response~rg_den)
summary(model_den)
###Result:(quite interesting compared to ethnicities, maybe because of multicollinearity!)
###F-Statistics are quite good (194-250), R^2 is again, like for all models, quite low (14-17%) and P-Value is small for both (<2.2e-16)
###Both estimates of coefficients are positive and especially density seems to contribute "a lot" (0.17). Population is quite small (0.05).
###The intercept falls both times in the usual range (5-8)

###
#Result from the simple regression analysis: some models provided us with a relatively good explanation, e.g. F-Statistics of ethnicities and living factors were quite good.
#Additionally, these 2(/4) factors provided the best R^2 and the most interesting coefficients.

#Plotting afam and density (most interesting but correlated factors)
plot(rg_afam, response, asp = 1)
abline(coefficients(model_afam), col='red')
points(rg_afam, fitted(model_afam), col='red', pch=16)
grid()

plot(rg_den, response, asp = 1)
abline(coefficients(model_den), col='red')
points(rg_den, fitted(model_den), col='red', pch=16)
grid()

#Additional computations for Afam
  # Create a qq-plot of the residuals
  qqnorm(residuals(model_afam))
  
  # Perform a Normality test of the residuals
  shapiro.test(residuals(model_afam))
  
  # Print the estimates of the regression coefficients
  coefficients(model_afam)
  
  # Compute CI of the regression coefficients
  confint(model_afam, level= 0.95)
  
  # Print the estimate of the error variance
  s2 <- sum(residuals(model_afam)^2)/model_afam$df
  sqrt(s2)
  
  # Plot the residuals vs regressor
  plot(rg_afam, residuals(model_afam))
  abline(h=0)
  
#Additional computations for density
  # Create a qq-plot of the residuals
  qqnorm(residuals(model_den))
  
  # Perform a Normality test of the residuals
  shapiro.test(residuals(model_den))
  
  # Print the estimates of the regression coefficients
  coefficients(model_den)
  
  # Compute CI of the regression coefficients
  confint(model_den, level= 0.95)
  
  # Print the estimate of the error variance
  s2 <- sum(residuals(model_den)^2)/model_den$df
  sqrt(s2)
  
  # Plot the residuals vs regressor
  plot(rg_den, residuals(model_den))
  abline(h=0)
  
#########################

#Compute the multiple linear regression
  
#Using mixed selection (starting with the most relevant factors: afam and density)
  
model_multi1 <- lm(response~rg_afam+rg_den)
summary(model_multi1)
###Result: adj. R^2 (now 25%) didn't change that muhch, probably because of correlation between afam and density (see corr_table).
###P-Value of the model is still significant (<2.2e-16). F-Statistics are alright too. 
###Coefficiens still positive and significant but density lost impact (0.17 ->  0.07) while afam remained equal

#Adding population as a regressor
model_multi2 <- lm(response~rg_afam+rg_den+rg_pop)
summary(model_multi2)
###Result: adj. R^2 got a lot better (25% -> 42%). P-Value and F-Statistics good (285 & <2.2e-16)
###Coefficients all remained significant. Population estimated coeff. remained equal, density got bigger again and afam lost a bit. 

#Try some models (reduce pop, reduce afam, add den, reduce den,...)
model_multi3 <- lm(response~rg_afam+rg_pop)
summary(model_multi3)
###Result: reduce density (might due to correlation) as population and afam provide the better F-Statistics (369) and R^2 (38%)

#Add income to afam and population (not log)
model_multi4 <- lm(response~rg_afam+rg_pop+rg_inc2)
summary(model_multi4)
###Result: Reduced F-Statistics of Model 3 to 276 and improved R^2 by only 3%

#Add prisoners to afam and pouplation 
rg_pris <- Guns$prisoners
model_multi5 <- lm(response~rg_afam+rg_pop+rg_pris)
summary(model_multi5)
###Result: F-Statistics improved to 393 and R^2 increased to 50% 

#Add law to afam, population and prisoner
rg_law <- Guns$law
model_multi5 <-lm(response~rg_afam+rg_pop+rg_pris+rg_law)
summary(model_multi5)
###Result:R^2 only increased a bit

#Add all variables, except for state and year & reduce some
model_multi6 <- lm(response~rg_afam+rg_pop+rg_pris+ rg_law + rg_inc)
summary(model_multi6)












