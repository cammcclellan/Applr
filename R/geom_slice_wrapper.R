library(mosaic)

gg_slice <- function(data, x, y, model) {
  pred_data <- data |>
    mutate(
      {{ y }} := predict(model, newdata = data),
      predicted = TRUE
    )

  data |>
    mutate(predicted = FALSE) |>
    rbind(pred_data) |>
    ggplot(aes(x=!!sym(x), y=!!sym(y), color=predicted)) +
    geom_point()
}

model <- lm(totalbill ~ temp + month, Utilities)

gg_slice(Utilities, "temp", "totalbill", model) +
  facet_wrap(~month)

