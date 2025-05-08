
drawit <- function(model, )

drawit <- function(x4=0, x5=0, x6=0 ,x10 = 0, i=1){
    curve(b[1] + b[2]*x2 + b[3]*x4 + b[4]*x5 + b[5]*x10 + b[6]*x2*x4 + b[7]*x4*x2^2 + b[8]*x4*x2^3 + b[9]*x4*x6 + b[10]*x5*x10 + b[11]*x4*x5 + b[12]*x2*x10, add=TRUE, xname="x2", col=palette()[i])
}

drawit <- function(model, xvar = "x2", fixed_vals = list(), i = 1, from = -3, to = 3, n = 100) {
  # Create a sequence of x values for the variable we're plotting against
  x_seq <- seq(from, to, length.out = n)

  # Create a data.frame to hold all variables needed for prediction
  vars <- all.vars(formula(model))[-1]  # exclude response variable
  data <- as.data.frame(matrix(0, nrow = n, ncol = length(vars)))
  names(data) <- vars

  # Fill in the fixed values
  for (v in vars) {
    if (v == xvar) {
      data[[v]] <- x_seq
    } else if (v %in% names(fixed_vals)) {
      data[[v]] <- fixed_vals[[v]]
    } else {
      data[[v]] <- 0  # default to 0 if not provided
    }
  }

  # Predict using the model
  y_vals <- predict(model, newdata = data)

  # Plot the curve
  lines(x_seq, y_vals, col = palette()[i])
}

model <- lm(width ~ length + sex + length:sex, KidsFeet)
formula(model)
