
drawit <- function(model,
                   xaxis,
                   ...){
  setvars <- list(...)
  fullmodel <- model$model
  varsnames <- names(model$model)[-1]
  clean_varsnames <- setdiff(varsnames, xaxis)

  b <- coef(model)
  numb <- length(b)
  nameb <- names(b)

  curveequat <- paste0('b[',1:numb,']*',nameb, collapse = ' + ')
  clean_curveequat <- gsub('\\*\\(Intercept\\)', '', curveequat)

  curvecode <- paste0('curve(',clean_curveequat,', add = TRUE, xname =\"',xaxis,'\")')

  flag = FALSE

  for (name in clean_varsnames){
    if (is.numeric(model$model[[name]])){
      namerange <- range(model$model[[name]])
      if (!name %in% setvars){
        warning(paste0(name,' value not specified; enter value between:',namerange[1],'-',namerange[2]))
        flag = TRUE
      }
    }
    else if (is.factor(model$model[[name]])| is.character(model$model[[name]])){
      namelevels <- levels(model$model[[name]])
      levelslen <- length(namelevels)
      if(!name %in% setvars){
        warning(paste0(name,' value not specified; enter value as one of the following: ',paste0('\"',namelevels,'\"', collapse = ',')))
        flag = TRUE
      }
    }
  }
  if (!flag){
    coef_names <- names(b)
    fact_vars <- sapply(model$model,is.character)
    fact_vars <- sapply(model$model,is.factor)



    eval(parse(text=curvecode))
  }
}

model <- lm(width ~ length + sex + birthyear + birthmonth, KidsFeet)
xaxis = 'length'
plot(width ~ length, KidsFeet)
drawit(model = model, xaxis= 'length')
