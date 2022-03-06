

# FIXME, something about the toString is causing problems, says there's a closure being passed around somewhere?
# weird wrapper I made to add to facet labels in ggplot using the preexisting label as marker
label_apply <- function(string, func, ...) {
  return(paste(sep = " ", string, toString(func(get(string), ...))) )
}

# I really tried to use a deparse(substitute()) to make a naming scheme that could just look at the variable name of the list,
# as it turns out, it really acts weird on lists/inside a function. # https://stackoverflow.com/questions/47904321/deparsesubstitute-returns-function-name-normally-but-function-code-when-cal
# So instead it just enforces a really tight naming scheme.
add_model_predictions = function(data, ...) { 
  models = list(...)
  i <- 0
  for(model in models) {
    data <- add_predictions(data=data,model=model,var=paste(sep="", "mod", toString(i+1)) )
    i <- i+1 
  }
  data <- pivot_longer(data=data, cols = starts_with(vars=colnames(data), match="mod"), names_to = "model", values_to = "pred")
  return(data)
}


####################
# Not Generalized  #
####################

det_prediction_diff <- function(model, det, target = 12.827) {
  return(target - predict(object=model,det)) # get diff from target prediction, just used for labels
}

diff_labeller = function(string) { # Thrown together function used to stick that diff on the end of the facet label
  i <- 0
  for(s in string) {
    var <- get(s) # "mod1"
    diff <- det_prediction_diff(var,det)
    s <- paste(sep = " | off by ", s, toString( round(diff,2) ))
    
    string[i+1] <- s
    i <- i+1
  }
  return(string)
}





# add_model_predictions = function(data, models) { 
#   # Loop through and add predictions
#   i <- 0L
#   for(model in models) {
#     data <- add_predictions(data=data,model=model,var=names(models)[i+1])
#     i <- i+1
#   }
#   
#   # Pivot columns
#   data <- pivot_longer(
#     data=data, cols = starts_with(vars=colnames(data), match=names(model)), names_to = "model", values_to = "pred")
#   
#   return(data)
# }

