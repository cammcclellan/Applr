drawit <- function(x4=0, x5=0, x6=0 ,x10 = 0, i=1){
  curve(b[1] + b[2]*x2 + b[3]*x4 + b[4]*x5 + b[5]*x10 + b[6]*x2*x4 + b[7]*x4*x2^2 + b[8]*x4*x2^3 + b[9]*x4*x6 + b[10]*x5*x10 + b[11]*x4*x5 + b[12]*x2*x10, add=TRUE)
}
