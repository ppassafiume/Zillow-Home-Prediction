library(car)
library(psych)
library(caret)
library(C50)
library(mfp)
library(MASS)
library(olsrr)
library(Hmisc)
library(corrplot)
library(purrr)
library(DAAG)

options(scipen = 999)

#use comp_finder to read in data
df <- comp_finder("address, zip")

df2 <- df

#find correlations between predictor variables
corrs <- rcorr(as.matrix(df[,c(7:12)], type = "pearson"))
corrplot(corrs$r)

#check for normality
map2(df[,7:12], colnames(df[,7:12]), function(x, y) hist(x, main = y))
map2(df[,7:12], colnames(df[, 7:12]), function(x, y) qqnorm(x, main = y))

#transform data for models built
df2 <- df
df2$last_sold_price <- log(df2$last_sold_price)
df2$lot_size <- log(df2$lot_size)
df2$finished_sqft <- log(df2$finished_sqft)

#check normality
map2(df2[,c(7,9,10)], colnames(df2[,c(7,9,10)]), function(x, y) hist(x, main = y))
map2(df2[,c(7,9,10)], colnames(df2[, c(7,9,10)]), function(x, y) qqnorm(x, main = y))

#try model with all variables
model <- lm(zestimate ~ zip + year_built + last_sold_price + tax_assessment + lot_size + finished_sqft + bathrooms + bedrooms, data = df)
summary(model)

residualPlot(model)
hist(residuals(model))

#try new model without zip
model2 <- lm(zestimate ~ year_built + last_sold_price + tax_assessment + lot_size + finished_sqft + bedrooms + bathrooms, data = df)
summary(model2) #zip doesn't add much to r squared

residualPlot(model2)
hist(residuals(model2))

#year built doesn't seem to add much to r squared
model3 <- lm(zestimate ~ year_built + last_sold_price + lot_size + finished_sqft + bedrooms + bathrooms, data = df)
summary(model3)

residualPlot(model3)
hist(residuals(model3))

model4 <- lm(zestimate ~ last_sold_price + lot_size + finished_sqft + bedrooms + bathrooms, data = df)
summary(model4)

residualPlot(model4)
hist(residuals(model4))

#try using aic method for variable selection
forward <- ols_stepaic_forward(model4, details = T)
backward <- ols_step_backward(model4, details = T)
step <- ols_stepaic_both(model4, details = T)

model5 <- lm(zestimate ~ last_sold_price + lot_size + finished_sqft + bedrooms + bathrooms,data = df)
summary(model5)

residualPlot(model5)
hist(residuals(model5))

#bedrooms is not sigificant, so remove it
model6 <- lm(zestimate ~ last_sold_price + finished_sqft,data = df)
summary(model6)

#check residuals
residualPlot(model6)
hist(residuals(model6))
crPlots(model6)

#model with transformed data
trans_model <- lm(zestimate ~ last_sold_price + lot_size + finished_sqft,data = df2[,])
summary(model7)

#check residuals
residualPlot(trans_model)
hist(residuals(trans_model))
crPlots(trans_model)

#the transformed data seems to be a worse fitting model

# Divide data into 2 partitioned sets to build training and test sets
set.seed(12345)

rand_ids <- order(runif(219))

train_set <- df[rand_ids[1:164],]
test_set <- df[rand_ids[165:219],]

train_mod <- lm(zestimate ~ last_sold_price + finished_sqft, data = train_set)

predictions <- predict(train_mod, test_set)

actuals_preds <- data.frame(cbind(actuals=test_set$zestimate, predicteds=predictions))

cor(actuals_preds)




