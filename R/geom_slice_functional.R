
library(mosaic)

model <- lm(totalbill ~ temp + month + temp:month, Utilities)


compute_group_slice = function(data, scales, model,xaxis,facet, n = 100, ...) {

  vars <- names(model$model) #Identify col names from model
  oldnames <- names(data) #Identify & save names from ggplot data

  names(data)[1:3] <- c(xaxis,vars[1],facet) #HARDCODED: Rename ggplot data to match model names
  data$month <- as.numeric(data$month) #HARDCODED: Adjust class of facet variable as needed

  tmp <- predict(model, newdata = data) #Predict new y col for displaying the model
  names(data)<- oldnames #Revert col names to old names
  data$y<- tmp

  data
}

StatSlice <- ggproto(
  "StatSlice",
  Stat,
  required_aes = c('x','y'),
  compute_group = compute_group_slice)


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

p <- ggplot(Utilities, aes(x=temp,
                      y=totalbill))+
  geom_point()+
  facet_wrap(~month)+
  geom_slice(model=model,xaxis = 'temp', facet='month')
