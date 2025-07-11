
drawit <- function(model,
                   xaxis,
                   ...,
                   col='black',
                   lty=1){
  setvars <- list(...)
  fullmodel <- model$model
  varsnames <- names(model$model)[-1]
  clean_varsnames <- setdiff(varsnames, xaxis)

  b <- coef(model)
  numb <- length(b)
  nameb <- names(b)

  curveequat <- paste0('b[',1:numb,']*',nameb, collapse = ' + ')
  clean_curveequat <- gsub('\\*\\(Intercept\\)', '', curveequat)

  curvecode <- paste0('curve(',clean_curveequat,', add = TRUE, xname =\"',xaxis,'\", col = \"', col,'\", lty =',lty,')')

  flag = FALSE

  for (name in clean_varsnames){

    if (is.numeric(fullmodel[[name]])){
      namerange <- range(model$model[[name]])
      if (!name %in% names(setvars)){
        warning(paste0(name,' value not specified; enter value between:',namerange[1],'-',namerange[2]))
        flag = TRUE
      }
      else{
        assign(name, setvars[[name]])
      }
    }
    else if (is.factor(model$model[[name]])| is.character(model$model[[name]])){
      namelevels <- levels(model$model[[name]])
      levelslen <- length(namelevels)
      if(!name %in% names(setvars)){
        warning(paste0(name,' value not specified; enter value as one of the following: ',paste0('\"',namelevels,'\"', collapse = ',')))
        flag = TRUE
      }
      else{
        selected_level <- as.character((setvars[[name]]))
        dummyvars <- grep(name,names(b),value=TRUE)

        for (dummy in dummyvars){
          dummylevel <- sub(name,'',dummy)
          assign(dummy, as.numeric(dummylevel == selected_level))
        }
      }
    }
  }
  if (!flag){
    eval(parse(text=curvecode))
  }
}
