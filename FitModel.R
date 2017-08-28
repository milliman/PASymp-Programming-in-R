## We fit a logistic regression model with the binary "Success" variable as the response
mod <- glm(Success ~ budget.capped + facebook.capped + Audience,
           data = filter(ModelData, Sample == "Training"),
           family = "binomial")

## You can pass a model object to the summary function to find model coefficients, errors, p-values, etc.
summary(mod)

## We'll add the model's predictions to the data as its own column, and bucket our continuous predictor variables 
ResultData <- ModelData %>%
  mutate(Prediction = predict(mod, newdata = ModelData, type = "response"))