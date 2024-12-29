## Check Normality
##### Function ###

Check_normality = function(data,var,
                           Method=NULL){
  if(!is.null(Method) ){
    if( Method %in% c("s", "sh")) Method ="shapiro"
    if( Method == "k") Method ="kolmog"
    if(Method %in% c("shapiro", "kolmog"))
      stop("the Method must be shapiro or kolmog")
  }
  # if (!is.character(substitute(var))) {
  #  stop("Error: 'var' is not a string, add cotation.")}
  
  sample_size = dim(data)[1]
  if (is.null(Method)){
    if (sample_size<2000){
      result = shapiro.test(data[,var])
    } else if(sample_size>2000) {
      result = ks.test(data[,var], "pnorm")
    }}
  else  {
    if (Method=="shapiro"){
      result = shapiro.test(data[,var])
    } else if(Method=="kolmog") {
      result = ks.test(data[,var], "pnorm")
    }
  }
  return(result)
}
