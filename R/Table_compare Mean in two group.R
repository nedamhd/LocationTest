# 3- Draw table for t.Test

###### Main Func
Draw.table = function(data, var, group, Test, paired=FALSE, draw_plot=TRUE, save_plot=TRUE, y_adjust,filename= "plot.123",...) {
  #functions

  "%r%" = function(a, b)  round(a, digits = b)
  "%+%" = function(a, b)  paste0(a, b)

  Quant = function(x) {
    quantile(x, probs = c(0.25, 0.5, .75), na.rm = TRUE)
  }
  ############
  if (!is.factor(data[,group])) stop("The group variable most be factor.")
  # if (Test=="NULL")Test=NULL
  ress =Test_Mean(data = data,
                   var = var,
                   group = group,Test=Test, paired=paired )

  if (is.null(Test)){
    if (ress$Test == "t.test") {
      group_mean = data %>%
      group_by(data[group]) %>%
      summarise_at(vars(var), # Specify column
                   list(mean = mean, sd = sd))
    group_mean$group = c( group_mean[[group]])
    group_mean$lower =  group_mean$mean- group_mean$sd
    group_mean$upper =  group_mean$mean+ group_mean$sd
    group_mean$centre = group_mean$mean
    a = (group_mean$mean %r% 2) %+% "\u00B1" %+% (group_mean$sd %r% 2)
    table1 = list(table=cbind.data.frame(group1 = a[1], group2 = a[2], ress),
                 summary=group_mean[,c("group","lower","centre", "upper")])
    summ=cbind.data.frame(table1$summary)


    }
  else if (ress$Test == "wilcox") {
    Quant1 = function(x) {
      quantile(x, probs = 0.25, na.rm = TRUE)
    }
    Quant2 = function(x) {
      quantile(x, probs = 0.5, na.rm = TRUE)
    }
    Quant3 = function(x) {
      quantile(x, probs = 0.75, na.rm = TRUE)
    }
    temp=data
    temp$group=data[[group]]
    group_median = temp %>%
      group_by(group) %>%
      summarise_at(vars(var),
                   list (q1 = Quant1,q2 = Quant2,q3 = Quant3))
    a = group_median$q2 %+% " (" %+% (group_median$q1) %+% "," %+% (group_median$q3) %+% ")"
    a3 =Test_Mean(data = data,
                   var = var,
                   group = group,Test=Test)
    def=d[,c("group","q1","q2", "q3")]
    names(def)= c("group","lower","centre","upper")
    table1 = list(table=cbind.data.frame(group1 = a[1] , group2 = a[2], a3),
                 summary=def)
    summ=cbind.data.frame(table1$summary)

  }}

    else if(!is.null(Test)){
        if (Test == "t.test") {
        group_mean = data %>%
          group_by(data[group]) %>%
          summarise_at(vars(var), # Specify column
                       list(mean = mean, sd = sd))
         group_mean$group = c( group_mean[[group]])
        group_mean$lower =  group_mean$mean- group_mean$sd
        group_mean$upper =  group_mean$mean+ group_mean$sd
        group_mean$centre = group_mean$mean
        a = (group_mean$mean %r% 2) %+% "\u00B1" %+% (group_mean$sd %r% 2)
        table1 = list(table=cbind.data.frame(group1 = a[1], group2 = a[2], ress),
                      summary=group_mean[,c("group","lower","centre", "upper")])
        summ=cbind.data.frame(table1$summary)


      }

      else if (Test == "wilcox") {
        Quant1 = function(x) {
          quantile(x, probs = 0.25, na.rm = TRUE)
        }
        Quant2 = function(x) {
          quantile(x, probs = 0.5, na.rm = TRUE)
        }
        Quant3 = function(x) {
          quantile(x, probs = 0.75, na.rm = TRUE)
        }
        temp=data
        temp$group=data[[group]]
        group_median = temp %>%
          group_by(group) %>%
          summarise_at(vars(var),
                       list (q1 = Quant1,q2 = Quant2,q3 = Quant3))
        a = group_median$q2 %+% " (" %+% (group_median$q1) %+% "," %+% (group_median$q3) %+% ")"

        a3 =Test_Mean(data = data,
                       var = var,
                       group = group,Test=Test)
        def=d[,c("group","q1","q2", "q3")]
        names(def)= c("group","lower","centre","upper")
        table1 = list(table=cbind.data.frame(group1 = a[1] , group2 = a[2], a3),
                      summary=def)
        summ=cbind.data.frame(table1$summary)

      } }

  names(summ)=c("group", "lower", "centre","upper")
  if (draw_plot)  ggplot = barplotMean (summ,y_adjust=y_adjust)
  if (save_plot)  ggsave(filename=paste0(filename,".jpeg"), plot=ggplot,...)
  list(tab=table1$table, summary=summ, ggplot= ggplot)
}
