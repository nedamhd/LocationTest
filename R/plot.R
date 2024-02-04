

barplotMean = function(summary, y_adjust=1.2){
  # "%r%" =function(a,b) round(a,digits=b)
  # "%+%" =function(a,b) paste0(a,b)
 tab=as.data.frame(summary)
  g=ggplot(tab, aes(x=group, y=centre, fill=group))+
        stat_summary(geom = "bar", fun = mean, show.legend = FALSE)  +
        geom_errorbar(aes(ymin=  lower ,
                      ymax=upper , y= centre),
                      width=.2, position = position_dodge(0.9))+theme_bw()
  if("letter" %in% names(tab))
g= g+geom_text(aes(x=group, y=upper+y_adjust,label=letter))

g
}


