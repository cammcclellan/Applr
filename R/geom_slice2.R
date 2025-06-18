
library(mosaic)
model <- lm(totalbill ~ temp + month + temp:month, Utilities)

StatSlice <- ggproto(
  "StatSlice",
  Stat,
  required_aes = c('x','y'),
  compute_group = function(data, scales, model,xaxis,facet, n = 100, ...) {


                       vars <- names(model$model)
                       oldnames <- names(data)

                       names(data)[1:3] <- c(xaxis,vars[1],facet)
                       data$month <- as.numeric(data$month)

                       tmp <- predict(model, newdata = data)
                       names(data)<- oldnames
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
    xaxis = NULL,
    facet=NULL,
    n = 100,
    inherit.aes = TRUE, ...){
  layer(
    stat = StatSlice,
    geom = GeomSlice,
    position = 'identity',
    inherit.aes = inherit.aes,
    params = list(model = model, xaxis = xaxis, facet=facet,n = n, ...)
  )
}

b <- coef(model)

ggplot(Utilities, aes(x=temp,
                      y=totalbill))+
  geom_point()+
  facet_wrap(~month)+
  geom_slice(model=model,xaxis = 'temp', facet='month')
