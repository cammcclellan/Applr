model <- lm(width ~ length, KidsFeet)

geom_slice <- function(model,xaxis=NA,n=100, ...){

  if (!inherits(model, 'lm'))
    stop(message = 'Model must be in lm format')

  if (is.na(xaxis)){
    xaxis <- names(model$model)[2]
    warning('X-axis not specified; used first x variable as x-axis')
  }

  y <- model$model[1][[1]]
  x1 <- model$model[[xaxis]]

  y_name <- names(model$model)[1]

  other_vars <- setdiff(names(model$model), c(y_name, xaxis))

  dots <- list(...)

  means_list <- lapply(other_vars, function(var) {
    if (!is.null(dots[[var]])) {
      dots[[var]]
    } else {
      col <- model$model[[var]]
      if (is.numeric(col)) {
        mean(col, na.rm = TRUE)
      } else if (is.factor(col)) {
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

  geom_line(
    data=new_data,
    mapping = aes(x=xaxis,y=preds),
    color=color,
    linewidth=1,
    inherit.aes = FALSE
  )

}

ggplot(KidsFeet, aes(x=length,
                     y=width))+
  geom_point()+
  geom_slice(model, xaxis='length')
