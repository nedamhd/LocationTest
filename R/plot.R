
# barplotMean = function(summary, y_adjust=1.2){
#   # "%r%" =function(a,b) round(a,digits=b)
#   # "%+%" =function(a,b) paste0(a,b)
#   tab=as.data.frame(summary)
#   g=ggplot(tab, aes(x=group, y=centre, fill=group))+
#     stat_summary(geom = "bar", fun = mean, show.legend = FALSE)  +
#     geom_errorbar(aes(ymin=  lower ,
#                       ymax=upper , y= centre),
#                   width=.2, position = position_dodge(0.9))+theme_bw()
#   if("letter" %in% names(tab))
#     g= g+geom_text(aes(x=group, y=upper+y_adjust,label=letter))
#
#   g
# }


barTGroup = function(data = NULL,
                     group = NULL,
                     var = NULL,
                     p_value = NULL,
                     LocationInput = NULL){
if(!is.null(LocationInput)){
  data = LocationInput$data
  group = LocationInput$group
  var = LocationInput$var
  p_value = round(LocationInput$Test0$tab$p.value,3)}


Lmsd = function(x) mean(x, na.rm=TRUE) - sd(x, na.rm=TRUE)
  M    = function(x) mean(x, na.rm=TRUE)
  Umsd = function(x) mean(x, na.rm=TRUE) + sd(x, na.rm=TRUE)
  Q1   = function(x) quantile(x,0.25, na.rm=TRUE)
  Q2   = function(x) quantile(x,0.5, na.rm=TRUE)
  Q3   = function(x) mean(x,0.75, na.rm=TRUE)

 p= ggplot(data=data,aes_string(x=group, y=var, fill = group ), position = position_dodge(0.9))+
    stat_summary(geom= "bar", fun = M)+
    stat_summary(geom= "errorbar", fun.min = Lmsd, fun.max=Umsd , width= 0.3)

  maxy = max(ggplot_build(p)$data[[2]]$ymax)
 p = p+geom_text(mapping = aes(x=1.5,y=maxy+maxy*0.07, label= glue::glue("P-Value = {p_value}")))

p+geom_segment(aes(x=1,xend =2, y=maxy+maxy*0.04, yend=maxy+maxy*0.04) )

  }





barAGroup = function(data = NULL,
                     group = NULL,
                     var = NULL,
                     p_value = NULL,
                     LocationInput = NULL){
  if(!is.null(LocationInput)){
    data = LocationInput$data
    group = LocationInput$group
    var = LocationInput$var
    label =  LocationInput$Test0$summary$letter
    groupValue =  LocationInput$Test0$summary$group
  }

  Lmsd = function(x) mean(x, na.rm=TRUE) - sd(x, na.rm=TRUE)
  M    = function(x) mean(x, na.rm=TRUE)
  Umsd = function(x) mean(x, na.rm=TRUE) + sd(x, na.rm=TRUE)
  Q1   = function(x) quantile(x,0.25, na.rm=TRUE)
  Q2   = function(x) quantile(x,0.5, na.rm=TRUE)
  Q3   = function(x) mean(x,0.75, na.rm=TRUE)

  p= ggplot(data=data,aes_string(x=group, y=var, fill = group ), position = position_dodge(0.9))+
    stat_summary(geom= "bar", fun = M)+
    stat_summary(geom= "errorbar", fun.min = Lmsd, fun.max=Umsd , width= 0.3)

  maxy = max(ggplot_build(p)$data[[2]]$ymax)

  dd=data.frame(group=groupValue,var=rep(maxy+maxy*0.07,3), label= label)
  names(dd) = c(group,var,"label")
  p = p+geom_text(mapping = aes_string(x=group,y=var, label= "label"),data=dd)
  p

}
