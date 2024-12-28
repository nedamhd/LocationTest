Test_Mean = function(data, var, group = NULL,Test=NULL, paired=FALSE){
if(isFALSE(paired)){
    formula=as.formula(paste0(var,"~",group))
  R=Check_normality(data=data,var=var)
} else {
  formula=as.formula(paste0("Pair(",paste0(var, collapse = ","),")~",1))
  newdata = data.frame(diffvar = data[,var[1]] - data[,var[2]] )
  R=Check_normality(data=newdata,var="diffvar")
  
}

  if (is.null(Test)){
    if (R$p.value<0.05) {
      p.value = wilcox.test(formula, data=data)$p.value
      Test = "wilcox"
    }
    else{
      p.value = t.test(formula, data=data)$p.value
      Test = "t.test"
    }}
  
  if (!is.null(Test)){
    if (Test=="wilcox") {
      p.value = wilcox.test(formula, data=data)$p.value
      Test = "wilcox"
    }
    else if(Test=="t.test"){
      p.value = t.test(formula, data=data)$p.value
      Test = "t.test"
    }}

  
  
  
  data.frame(p.value, Test)
  
}
