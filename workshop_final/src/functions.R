logistic_model <- function(x1, x2) {
  
  e <- exp(model_fit$finalModel$coefficients[1] +
             model_fit$finalModel$coefficients[2] * x1 + 
             model_fit$finalModel$coefficients[3] * x2)
  return(e / (1 + e))
}