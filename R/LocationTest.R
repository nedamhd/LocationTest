Location.Test = function (data, var, group=NULL, Test = NULL, paired=FALSE, paired.var=NULL, draw_plot = TRUE, save_plot = TRUE, 
                          y_adjust = 1.2, filename = "plot.123", ...) 
{
  dataTemp = data
  require(dplyr)
  require(ggplot2)
  if(!paired){           
      data$.group123 = data[[group]]
      uniq.level = length(unique(data$.group123))
  } else {
    uniq.level = 2
    
  }
      
    if (uniq.level == 2) {
      for(i in 1:length(var)){
         data = dataTemp
        
      if(paired){ 
        if(length(var) != length(paired.var)) stop("The length of var and paired.var must be the same.")
        var.value = data[,var[i]]
        paired.var.value = data[,paired.var[i]]
        data = data.frame(group = as.factor(c(rep(var[i], length(var.value)),
                                              rep(paired.var[i], length(paired.var.value))
        )), var = c(var.value, paired.var.value))
        names(data)[2]= var[i]
        group = "group"
        
      }
       
        if(i == 1) {
          Test0 = Draw.table(data, var[i], group, Test, paired=paired)
          tab0= Test0$tab 
        }
        if(i != 1) {
          T0 = Draw.table(data, var[i], group, Test, paired=paired)
          tab1= T0$tab 
          tab0 =rbind(tab0,tab1) 
          Test0=rbind(Test0,T0) 
      }
      
      } 
      if(paired){
      tab0 = cbind(lable=paste0(var," - ",paired.var), tab0 ) } else {
      tab0 = cbind(lable=paste0(var), tab0 )
      }
      
      Test0= list(tab = tab0, Test0)
      }
    if (uniq.level > 2) {
     Test0 = Anova.Table(data, group, var, Test, draw_plot = draw_plot, save_plot = save_plot, 
                                            y_adjust = y_adjust, filename = filename, ...)   
                        
                        
                      }
                      Test0
  }
  
  
