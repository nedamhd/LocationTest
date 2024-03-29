

##########################
Test_Mean = function(data, var, group,Test=NULL){
 formula=as.formula(paste0(var,"~",group))
  R=Check_normality(data=data,var=var)
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
