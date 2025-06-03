
library(mosaic)
model <- lm(totalbill ~ temp + month, Utilities)

StatSlice <- ggproto("StatSlice", Stat,
                     compute_group = function(data, scales, model, xaxis = NULL, n = 100, ...) {

                       if (!inherits(model, "lm")) stop("Model must be an lm object")

                       model_vars <- names(model$model)
                       y_name <- model_vars[1]
                       xaxis <- xaxis %||% model_vars[2]

                       x1 <- model$model[[xaxis]]
                       other_vars <- setdiff(model_vars, c(y_name, xaxis))

                       dots <- list(...)

                       # means_list <- lapply(other_vars, function(var) {
                       #   if (var %in% names(data)) {
                       #     unique_val <- unique(data[[var]])
                       #     if (length(unique_val) > 1){
                       #       warning(paste("Multiple values for", var, "in facet group"))}
                       #     unique_val[1]
                       #   } else {
                       #     col <- model$model[[var]]
                       #   }
                       # })

                       means_list <-

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
  stat_function(fun = function(x) b[1]+b[2]*x+b[3]*12)+
  geom_smooth(method='lm',formula=y~x,se=F)




