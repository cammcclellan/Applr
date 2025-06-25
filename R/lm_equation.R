
library(mosaic)

model <- lm(width ~ length + sex, KidsFeet)

lm_equation <- function(model){

model_vars <- names(model$model)[-1]
model_y <- names(model$model)[1]

model_coef <- model$coefficients[-1]
model_intercept <- model$coefficients[1]

model_terms <- paste('(',model_coef, '*', model_vars,')', collapse = ' + ')

model_equation <- paste(model_y, '=', model_intercept, ' + ', model_terms)

model_equation

}

lm_equation(model)

