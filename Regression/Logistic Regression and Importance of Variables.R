plot(violent,as.numeric(rg_law)-1, asp=1)
grid()

plot()

#Add all variables, except for state and year & reduce some
model_multi6 <- lm(response~rg_afam+rg_pop+rg_pris+ rg_law + rg_inc)
summary(model_multi6)

#Create a new subset of data to avoid mixing up variables

sub_importance <- subset(Guns, select = c(violent,afam,population,prisoners,income,law))
#Scale everything excluding law 
center_scale <- function(x) {
  scale(x, scale = FALSE)
}
sub_importance$violent <- center_scale(sub_importance$violent)
sub_importance$afam <- center_scale(sub_importance$afam)
sub_importance$population <- center_scale(sub_importance$population)
sub_importance$prisoners <- center_scale(sub_importance$prisoners)
sub_importance$income <- center_scale(sub_importance$income)
sub_importance$law <- center_scale(as.numeric(sub_importance$law))

#Logistic Regression Model
log_mod <- glm(rg_law~response, family = "binomial")
summary(log_mod)

#Plot the logistic model
plot(response, as.numeric(rg_law)-1, xlim=c(0, 10), xlab = "Log(Violence)", ylab = "Odds of Gun-Law in place", main = "Logistic Regression Plot")
lines(seq(0, 10, by=0.1), predict(log_reg, data.frame(response=seq(0, 10, by=0.1)), type='response'), col='red')
points(response, predict(log_reg, type='response'), pch=16, col='red')
abline(h=0.5, lty=3)


#Run best fit model with scaled variables
model_multi7 <- lm(sub_importance$violent~sub_importance$afam+sub_importance$population+sub_importance$prisoners+ sub_importance$law + sub_importance$income)
summary(model_multi7)




