
model <- lm(width ~ length + sex, KidsFeet)
graph_reso <- 0.5

scatter_3d <- function(model, n=100){

  vars <- all.vars(formula(model))

  if(length(vars) > 3)
    stop(message='Not 3D Graphable, Reduce lm to 2 x variables')

  x1 <- model$model[2][[1]]
  x2 <- model$model[3][[1]]

  if (class(x1) != 'numeric')
    if (class(x1) != 'factor')
      x1 <- as.factor(x1)
    x1 <- as.numeric(x1)

  if (class(x2) != 'numeric')
      if (class(x2) != 'factor')
        x1 <- as.factor(x2)
    x2 <- as.numeric(x2)


  axisx <- seq(min(x1), max(x1), length.out=n)
  axisy <- seq(min(x2), max(x2), length.out=n)


}
