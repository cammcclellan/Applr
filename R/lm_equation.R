#' Write out the equation of a linear model
#'
#' @param model A linear model
#'
#' @returns
#' The equation of an indicated linear model with coefficients and variable names
#'
#' @examples
#' model <- lm(width ~ length + I(length^2) + sex + sex:length + sex:I(length^2), KidsFeet)
#' lm_equation(model)
#'
#' @export
lm_equation <- function(model){

model_vars <- attr(terms(model), "term.labels")
model_y <- names(model$model)[1]

model_coef <- signif(model$coefficients,3)[-1]
model_intercept <- signif(model$coefficients,3)[1]

model_terms <- paste0(model_coef, '*', model_vars, collapse = ' + ')

model_equation <- paste0(model_y, ' = ', model_intercept, ' + ', model_terms)
model_equation <- gsub("\\+ -", "- ", model_equation)

model_equation

}

#' Write out the equation of a linear model in LaTeX format
#'
#' @param model A linear model
#'
#' @returns
#' The equation of an indicated linear model with coefficients and variable names in LaTeX form
#'
#' @examples
#' model <- lm(width ~ length + I(length^2) + sex + sex:length + sex:I(length^2), KidsFeet)
#' lm_latex(model)
#'
#' @export
lm_latex <- function(model){

  model_vars <- attr(terms(model), "term.labels")
  model_y <- names(model$model)[1]

  model_coef <- signif(model$coefficients,3)[-1]
  model_intercept <- signif(model$coefficients,3)[1]

  x_nums <- paste0('X_{',seq_along(model_vars),'i}')
  x_under <- paste0("\\underbrace{", x_nums, "}_{\\text{", model_vars, "}}")
  x_terms <- paste0(model_coef,x_under, collapse = ' + ')
  x_terms <- gsub("\\+ -", "- ", x_terms)

  respon <- paste0('\\underbrace{\\hat{Y_i}}_{\\text{Pred. ',model_y,'}}')

  lat_equat <- paste0('$$',respon, ' = ',model_intercept, ' + ', x_terms,'$$')

  cat(lat_equat)
}

library(mosaic)

model <- lm(width ~ length + sex + domhand, KidsFeet)
b <- coef(model)
b
lm_equation(model)

lm_latex(model)
