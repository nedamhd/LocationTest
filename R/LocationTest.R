source("E://SOFTware//R Leaning//LocationTest3//R//Anova table AND posthoc.R")
source("E://SOFTware//R Leaning//LocationTest3//R//ANOVA Test.R")
source("E://SOFTware//R Leaning//LocationTest3//R//AnovaNormality.R")
source("E://SOFTware//R Leaning//LocationTest3//R//compare Mean in two group.R")
source("E://SOFTware//R Leaning//LocationTest3//R//Normality Test.R")
source("E://SOFTware//R Leaning//LocationTest3//R//plot.R")
source("E://SOFTware//R Leaning//LocationTest3//R//Table_compare Mean in two group.R")

Location.Test = function (data, var, group=NULL,split=NULL, Test = NULL, paired=FALSE, paired.var=NULL, draw_plot = TRUE, save_plot = TRUE,
                          y_adjust = 1.2, filename = "plot.123", ...)
{
  dataTemp = data
  if(any(is.na(data[[group]]))) stop("group contains NA. REMOVE them.")
  require(dplyr)
  require(ggplot2)
  if(!paired){
      data$.group123 = data[[group]]
      uniq.level = length(unique(data$.group123))
  } else {
    uniq.level = 2

  }
  if (!is.null(split)){
    # uniq.split = unique(data[[split]])
    uniq.split<- sort(unique(data[[split]]))
  }else {
    uniq.split=1
  }

    if (uniq.level == 2) {
      for(i in 1:length(var)){
        for(j in 1:length(uniq.split)) {
        data = dataTemp

        if (!is.null(split)){
        data=  data[data[[split]]==uniq.split[j],]
        }


      if(paired){
        if(length(var) != length(paired.var)) stop("The length of var and paired.var must be the same.")
        var.value = data[[var[i]]]
        paired.var.value = data[[paired.var[i]]]
        data = data.frame(group = as.factor(c(rep(var[i], length(var.value)),
                                              rep(as.character(paired.var[i]), length(paired.var.value))
        )), var = c(var.value, paired.var.value))
        names(data)[2]= var[i]
        group = "group"

      }

        if(i+j == 2) {
          Test0 = Draw.table(data, var[i], group, Test, paired=paired)
          if(!is.null(split)) tab0=cbind(split=uniq.split[j], Test0$tab ) else {
            tab0= Test0$tab}

            if(paired){
              tab0 = cbind(lable=paste0(var[i]," - ",paired.var[i]), tab0 ) } else {
                tab0 = cbind(lable=paste0(var[i]), tab0 )

          }
        }
        if(i+j != 2) {
          T0 = Draw.table(data, var[i], group, Test, paired=paired)
          if(!is.null(split)) {tab1=cbind(split=uniq.split[j], T0$tab )} else {
            tab1= T0$tab}
            if(paired){
              tab1 = cbind(lable=paste0(var[i]," - ",paired.var[i]), tab1 )
            } else {
                tab1 = cbind(lable=paste0(var[i]), tab1 )

          }

          tab0 =rbind(tab0,tab1)
          Test0=rbind(Test0,T0)
      }

       }}


      Test0= list(tab = tab0, Test0)
    }


    if (uniq.level > 2) {
     Test0 = Anova.Table(data, group, var, Test, draw_plot = draw_plot, save_plot = save_plot,
                                            y_adjust = y_adjust, filename = filename, ...)

      # Test0 <-  ANOVA_table$new(data = dataTemp, group = group,
                      #         deps.quantitative = var, shapiro.t = FALSE, kruskal_wallis = FALSE  )
    }




g = list(Test0 = Test0, data= dataTemp, group = group, var = var)

##BarPlot
# TODO: Add plot for more than 1 var.
if(uniq.level == 2)
          { p = barTGroup(data = NULL,
                     group = NULL,
                     var = NULL,
                     p_value = NULL,
                     LocationInput = g)}

  if(uniq.level > 2){
    p=barAGroup(data = NULL,
                group = NULL,
                var = NULL,
                p_value = NULL,
                LocationInput = g)}

list(Test0 = Test0 , plot= p)
  }


