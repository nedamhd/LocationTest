
### Function Anova.Test for one variable
Anova.Test= function(data, var, group, Test=NULL){
  data$.Group123456 = data[, group]
  uniq.level = unique(data$.Group123456)
  sample.size=length(data[,var])
  formula=as.formula(paste0(var, "~",group))
  if (is.null(Test)){
   if (sample.size<40) {
        NormTest=normality.Anova(data=data ,var=var ,group=group , Method=NULL )
        AA=any(NormTest$p.value<0.05)
        if(AA){
          Test=kruskal.test(formula, data = data)$p.value
          Test=data.frame(p.value=Test,Test="Kruskalwalis")

        } else{
          Test=anova(aov(formula, data = data))$`Pr(>F)`[1]
          Test=data.frame(p.value=Test,Test="Anova")
        }
      # any(NormTest$p.value[length(uniq.level)])
        for (i in 1:length(uniq.level)){
          a=NormTest$p.value[i]
      }
   }else{
      Test = anova(aov(formula, data = data))$`Pr(>F)`[1]
      Test = data.frame(p.value = Test, Test = "Anova")
   }
  }else if (!is.null(Test)){
if(Test %in% c("Anova","ANOVA","aov","A")) Test="Anova"
if(Test %in% c("Kruskalwalis","Kruskal","Krus","K")) Test="Kruskalwalis"
if(!(Test %in% c("Anova", "Kruskalwalis")))
  stop("the Test must be Anova or Kruskalwalis")

      if(Test=="Anova"){
        Test=anova(aov(formula, data = data))$`Pr(>F)`[1]
       Test=data.frame(p.value=Test,Test="Anova")
      }else if(Test=="Kruskalwalis"){
        Test=kruskal.test(formula, data = data)$p.value
        Test=data.frame(p.value=Test,Test="Kruskalwalis")
      }
    }


  Test
}
