
library(mosaic)
model <- lm(totalbill ~ temp + ccf + temp:ccf + month, Utilities)


geom_slice <- function(model,xaxis=NA,n=100, color = 'skyblue',linewidth=2, linetype = 'solid', alpha = 1,...){

  if (!inherits(model, 'lm'))
    stop('Model must be in lm format')

  if (is.na(xaxis)){
    xaxis <- names(model$model)[2]
    warning('X-axis not specified; used first x variable as x-axis')
  }

  y <- model$model[1][[1]]
  x1 <- model$model[[xaxis]]

  y_name <- names(model$model)[1]

  other_vars <- setdiff(names(model$model), c(y_name, xaxis))

  if(length(other_vars) > 0) {

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

  }else{
    message('Model is a simple linear regression')

    vals <- seq(min(x1), max(x1), length.out = n)

    new_data <- setNames(data.frame(vals),xaxis)
  }

  new_data$preds <- predict(model, newdata = new_data)

  eval(parse(text=paste0('geom_line(',
                         'data=new_data,',
                         'mapping = aes(x=',xaxis,',y=preds),',
                         'color = color,',
                         'linewidth= linewidth,',
                         'alpha = alpha,',
                         'linetype = linetype,',
                         'inherit.aes = FALSE)'
  )))

}

StatSlice <- ggproto("StatSlice", Stat,
                     compute_group = function(data, scales, model, xaxis = NULL, n = 100, ...) {

                       if (!inherits(model, "lm")) stop("Model must be an lm object")

                       model_vars <- names(model$model)
                       y_name <- model_vars[1]
                       xaxis <- xaxis %||% model_vars[2]

                       x1 <- model$model[[xaxis]]
                       other_vars <- setdiff(model_vars, c(y_name, xaxis))

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

                       x_names <- as.data.frame(as.list(setNames(means_list, other_vars)))
                       vals <- seq(min(x1, na.rm = TRUE), max(x1, na.rm = TRUE), length.out = n)
                       new_data <- data.frame(setNames(list(vals), xaxis))

                       if (length(other_vars) > 0) {
                         new_data <- cbind(new_data, x_names[rep(1, n), , drop = FALSE])
                       }

                       new_data$y <- predict(model, newdata = new_data)
                       new_data$x <- new_data[[xaxis]]
                       new_data
                     }
)

GeomSlice <- ggproto('GeomSlice',GeomLine,
                     default_aes = aes(color = "skyblue", linewidth = 1, linetype = "solid", alpha = 1)
                     )

# Wrapper function for the geom
geom_slice <- function(model, xaxis = NULL, n = 100,inherit.aes = TRUE, ...){
  layer(
    stat = StatSlice,
    geom = GeomSlice,
    position = "identity",
    inherit.aes = inherit.aes,
    mapping = aes(x = after_stat(x), y = after_stat(y)),
    params = list(model = model, xaxis = xaxis, n = n, ...)
  )
}

b <- coef(model)

ggplot(Utilities, aes(x=temp,
                      y=totalbill))+
  geom_point()+
  facet_wrap(~month)+
  geom_slice(model, xaxis='temp')+
  stat_function(fun = function(x) b[1]+b[2]*x+b[3]*6)



