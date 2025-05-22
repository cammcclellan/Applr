model <- lm(width ~ length + birthmonth + sex, KidsFeet)

slice_2d <- function(model,xaxis=NA,n=100, ...){

  if (!inherits(model, 'lm'))
    stop('Model must be in lm format')

  if (is.na(xaxis)){
    xaxis <- names(model$model)[2]
    warning('X-axis not specified; used first x variable as x-axis: Use xaxis=\"var_name\"')
  }

  y <- model$model[1][[1]]
  x1 <- model$model[[xaxis]]

  y_name <- names(model$model)[1]

  if (!(xaxis %in% names(model$model))) {
    stop(paste("xaxis variable", xaxis, "not found in the model."))
  }

  other_vars <- setdiff(names(model$model), c(y_name, xaxis))

  dots <- list(...)

  means_list <- lapply(other_vars, function(var) {
    if (!is.null(dots[[var]])) {
      message('Slice variable not specified:\n Use \"var_name\" = value to specify')
      dots[[var]]
    } else {
      col <- model$model[[var]]
      if (is.numeric(col)) {
        message('Slice value not specified - Used mean of ', var)
        mean(col, na.rm = TRUE)
      } else if (is.factor(col)) {
        message('Slice value not specified - Used first level of ', var)
        levels(col)[1]
      } else {
        stop(paste("Unsupported variable type for:", var))
      }
    }
  })

  x_names <- as.data.frame(as.list(setNames(means_list,other_vars)))

  vals <- seq(min(x1), max(x1), length.out = n)

  new_data <- setNames(data.frame(vals),xaxis)

  new_data <- cbind(new_data, x_names)

  new_data$preds <- predict(model, newdata = new_data)

  plot(y ~ x1,
       xlab = xaxis,
       ylab = paste("Predicted", y_name))

  lines(new_data[[xaxis]],new_data$preds)

}

slice_2d(model)

add_slice_2d <- function(model,xaxis=NA,n=100, ...){

  if (!inherits(model, 'lm'))
    stop('Model must be in lm format')

  if (is.na(xaxis)){
    xaxis <- names(model$model)[2]
    message('X-axis not specified; used first x variable as x-axis: Use xaxis=\"var_name\"')
  }

  y <- model$model[1][[1]]
  x1 <- model$model[[xaxis]]

  y_name <- names(model$model)[1]

  if (!(xaxis %in% names(model$model))) {
    stop(paste("xaxis variable", xaxis, "not found in the model."))
  }

  other_vars <- setdiff(names(model$model), c(y_name, xaxis))

  dots <- list(...)

  means_list <- lapply(other_vars, function(var) {
    if (!is.null(dots[[var]])) {
      message('Slice variable not specified:\n Use \"var_name\" = value to specify')
      dots[[var]]
    } else {
      col <- model$model[[var]]
      if (is.numeric(col)) {
        message('Slice value not specified - Used mean of ', var)
        mean(col, na.rm = TRUE)
      } else if (is.factor(col)) {
        message('Slice value not specified - Used first level of ', var)
        levels(col)[1]
      } else {
        stop(paste("Unsupported variable type for:", var))
      }
    }
  })

  x_names <- as.data.frame(as.list(setNames(means_list,other_vars)))

  vals <- seq(min(x1), max(x1), length.out = n)

  new_data <- setNames(data.frame(vals),xaxis)

  new_data <- cbind(new_data, x_names)

  new_data$preds <- predict(model, newdata = new_data)

  lines(new_data[[xaxis]],new_data$preds)

}


kids <- lm(width ~ length + birthmonth, data = KidsFeet)
plot(width ~ length, data = KidsFeet)
add_slice_2d(kids)
