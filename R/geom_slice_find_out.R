
library(mosaic)

StatSlice <- ggproto(
  "StatSlice",
  StatIdentity,
  required_aes = c('x','y'),
  setup_data = function(self, data, params){
    data
  },
  get_default_value = function(model, varname) {
    fdata <- model$model[[varname]]

    if (is.numeric(fdata)) {
      mean(fdata, na.rm = TRUE)
    } else if (is.factor(fdata) || is.character(fdata)) {
      freq <- table(fdata)

      val <- names(freq)[which.max(freq)]
      if (is.factor(fdata))
        val <- factor(val, levels = levels(fdata))
      val
    } else {
      stop("Unsupported column type: ", class(data))
    }
  },
  compute_layer = function(self, data, params, layout){
    params$facet_layout <- layout$layout
    mapping <- params$mapping
    ggproto_parent(Stat,self)$compute_layer(data,params,layout)
  },
  compute_panel = function(data, scales, params, n = 100,facet_layout,predict_vars, model, mapping) {

    facet_vars <- setdiff(names(facet_layout),
                          c("ROW",'COL', "SCALE_X", "SCALE_Y", "COORD"))

    data <- facet_layout |>
      select(!!!facet_vars) |>
      left_join(x = data, y = _, by = "PANEL")

    xvar <- rlang::as_name(rlang::quo_get_expr(mapping$x))
    yvar <- rlang::as_name(rlang::quo_get_expr(mapping$y))

    vars <- names(model$model) #Identify col names from model
    oldnames <- names(data) #Identify & save names from ggplot data

    names(data)[1:2] <- c(xvar,yvar)
    othervars <- setdiff(vars,c(xvar,yvar,facet_vars))

    for (var in othervars) {
        if (!is.null(predict_vars[[var]])) {
          data[[var]] <- predict_vars[[var]]
        } else {
          data[[var]] <- StatSlice$get_default_value(model, var)
        }
      }

    for (facet_var in facet_vars) {
      model_class <- class(model$model[[facet_var]])
      if ("factor" %in% model_class) {
        data[[facet_var]] <- factor(data[[facet_var]], levels = levels(model$model[[facet_var]]))
      } else if ("character" %in% model_class) {
        data[[facet_var]] <- as.character(data[[facet_var]])
      } else if ("numeric" %in% model_class) {
        data[[facet_var]] <- as.numeric(data[[facet_var]])
      } else if ("integer" %in% model_class) {
        data[[facet_var]] <- as.integer(data[[facet_var]])
      }
    }


    tmp <- predict(model, newdata = data) #Predict new y col for displaying the model

    names(data)<- c(oldnames,othervars) #Revert col names to old names

    data$y<- tmp
    data
  }
  )

GeomSlice <- ggproto(
  'GeomSlice',
  GeomLine,
  default_aes = aes(
    color = "skyblue",
    linewidth = 1,
    linetype = "solid",
    alpha = 1)
)

geom_slice <- function(
    model,
    n = 100,
    inherit.aes = TRUE,
    predict_vars = list(),
    ...
    )
  {
  my_layer <- layer(
    stat = StatSlice,
    geom = GeomSlice,
    position = 'identity',
    inherit.aes = inherit.aes,
    params = list(model = model,predict_vars = predict_vars, n = n,...)
  )

  ggproto(
    NULL, my_layer,
    compute_statistic = function(self, data, layout){
      params <- self$stat$setup_params(data, self$stat_params)
      self$computed_stat_params <- params[['mapping']] <- self$computed_mapping

      data <- self$stat$setup_data(data,params)

      self$stat$compute_layer(data,params,layout)

    }
  )
}

model <- lm(totalbill ~ temp + month + temp:month + kwh + billingDays, Utilities)

b <- coef(model)

ggplot(Utilities, aes(x=temp,
                      y=totalbill))+
  geom_point()+
  facet_wrap(~month)+
  geom_slice(model=model)
