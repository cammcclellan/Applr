#' Plot a 3D-grapheable linear model
#'
#' @param model A saved linear model
#' Ex. df <- lm(y ~ x + x2, data)
#' @param n Number of evaluations
#' @param colors Colors used for the gradient scale
#' colors = c(color1, color2, etc.)
#'
#' @returns A 3D graph in the viewer section
#' @export
#'
#' @examples
#' model <- lm(width ~ length + sex, data = KidsFeet)
#' scatter_3d(model, colors=c('blue','yellow')
scatter_3d <- function(model, n=100, colors = c('blue', 'yellow')){

  if (!inherits(model, 'lm'))
    stop(message = 'Model must be in lm format')

  vars <- all.vars(formula(model))

  if(length(vars) > 3)
    stop(message='Not 3D Graphable, Reduce lm to 2 x variables')


  y <- model$model[1][[1]]
  x1 <- model$model[2][[1]]
  x2 <- model$model[3][[1]]

  y_name <- names(model$model)[1]
  x1_name <- names(model$model)[2]
  x2_name <- names(model$model)[3]

  df <- setNames(data.frame(y,x1,x2), c(y_name, x1_name, x2_name))






  if (!is.numeric(x1))
    if (!is.factor(x1))
      x1 <- as.factor(x1)
    x1 <- as.numeric(x1)

  if (!is.numeric(x2))
      if (!is.factor(x2))
        x2 <- as.factor(x2)
    x2 <- as.numeric(x2)


  axisx <- seq(min(x1), max(x1), length.out=n)
  axisy <- seq(min(x2), max(x2), length.out=n)


  surface <- expand.grid(setNames(list(axisx, axisy), c(x1_name,x2_name)), KEEP.OUT.ATTRS = F)

  if (is.factor(x1))
    surface[[x1_name]] <- factor(surface[[x1_name]], levels = levels(x1))

  if (is.factor(x2))
    surface[[x2_name]] <- factor(surface[[x2_name]], levels = levels(x2))

  surface$Z <- predict.lm(model, newdata = surface)

  formula = paste(x2_name, "~", x1_name)

  surface <- acast(surface, formula, value.var = 'Z')


  eval(parse(text=paste0("plot_ly() %>%",
    "add_markers(data=df,",
         "x=~",x1_name,",",
         "y=~",x2_name,",",
         "z=~",y_name,",",
         "type='scatter3d',",
         "mode='markers',",
         "colors = colors) %>%",
    "add_trace(z=surface,",
              "x=axisx,",
              "y=axisy,",
              "type='surface')")))


}
