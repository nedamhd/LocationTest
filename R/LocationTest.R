Location.Test = function (data, data, group=NULL, Test = NULL, paired=FALSE, paired.var=NULL, draw_plot = TRUE, save_plot = TRUE, 
          y_adjust = 1.2, filename = "plot.123", ...) 
{
          
  require(dplyr)
  require(ggplot2)
  if(paired){        
  var.value = data[,var]
  paired.var.value = data[,paired.var]
  data = data.frame(group = 
            
  data$.group123 = data[[group]]
  uniq.level = unique(data$.group123)
  if (length(uniq.level) == 2) {
            
    Test0 = Draw.table(data, var, group, Test, paired=paired, paired.var=paired.var)
  }
  if (length(uniq.level) > 2) {
    Test0 = Anova.Table(data, group, var, Test, draw_plot = draw_plot, save_plot = save_plot, 
    y_adjust = y_adjust, filename = filename, ...)   
    
    
  }
  Test0
}

