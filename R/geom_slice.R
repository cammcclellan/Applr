
library(mosaic)
model <- lm(totalbill ~ temp + month, Utilities)

StatSlice <- ggproto("StatSlice", Stat,
                     compute_group = function(data, scales, model,facet = NULL, n = 100, ...) {

                       if (!inherits(model, "lm"))
                         stop("Model must be an lm object")

                       model_vars <- names(model$model)[-1]
                       xaxis <- all.vars(formula(model))[2]

                       xvals <- model$model[[xaxis]]
                       x1 <- seq(min(xvals, na.rm=TRUE),max(xvals,na.rm=TRUE),length.out=n)
                       x_data <- data.frame(setNames(list(x1),xaxis))

                       other_vars <- setdiff(model_vars, c(xaxis, facet))

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

                       x_names <- as.data.frame(as.list(setNames(means_list, other_vars)))

                       if (length(other_vars) > 0) {
                         new_data <- cbind(x_data, x_names[rep(1, n), , drop = FALSE])
                       } else {
                         new_data <- x_data
                       }

                       new_data$y <- predict(model, newdata = new_data)
                       names(new_data)[names(new_data) == xaxis] <- "x"
                       new_data <- new_data[, c("x", "y")]
                       new_data
                     }

)


GeomSlice <- ggproto('GeomSlice',GeomLine,
                     default_aes = aes(color = "skyblue", linewidth = 1, linetype = "solid", alpha = 1)
                     )


geom_slice <- function(model, facet=NULL, n = 100,inherit.aes = TRUE, ...){
  layer(
    stat = StatSlice,
    geom = GeomSlice,
    position = "identity",
    inherit.aes = inherit.aes,
    params = list(model = model, n = n, ...)
  )
}

b <- coef(model)

ggplot(Utilities, aes(x=temp,
                      y=totalbill))+
  geom_point()+
  facet_wrap(~month)+
  geom_slice(model=model,facet='month')+
  stat_function(fun = function(x) b[1]+b[2]*x+b[3]*12)+
  geom_smooth(method='lm',formula=y~x,se=F)




