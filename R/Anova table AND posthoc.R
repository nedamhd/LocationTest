
### Anova Table for one variable:
Anova.Table = function(data, group, var, Test = NULL,draw_plot=TRUE, save_plot=TRUE, y_adjust=1.2,filename= "plot.123",...) {
  # source("E://SOFTware//R Leaning//Two//normality.Anova.R")
  # source("E://SOFTware//R Leaning//Two//Anova Test.R")
  library("multcomp")
  library("multcompView")
  library("emmeans")
  library(dplyr)
  library(ggplot2)

 #### functions
   meansd=function(m){
    group=m[,".group123"]
    lower=m[,"mean"]-m[,"sd"]
    upper=m[,"mean"]+m[,"sd"]
    centre=m[,"mean"]
    letter=m[,"letter.1"]

    cbind.data.frame(group, lower, centre,upper,letter)
   }

   #######

  formula = as.formula(paste0(var, "~", group))
  data$.group123 = data[[group]]
  data$.var123 = data[[var]]
  res0 = Anova.Test(data , var, group,  Test)
  model.lm <- lm(formula, data )
  formula1 = as.formula(paste0("pairwise", "~", group))
  smodel = summary(glht(model.lm, emm(formula1)))
  p.val = smodel$test$pvalues
  a = names(smodel$test$tstat)
  b = strsplit(a, " - ")
  f = function(a)    paste0(a[1], "-", a[2])
  name = sapply(b, f)
  names(p.val) = name
  letter.1 = multcompLetters(p.val)$Letters



  if (is.null(Test)) {
    if (res0$Test == "Anova") {
      res1 =  data %>% group_by(.group123) %>% summarise_at(vars(var), list(mean =
                                                                              mean , sd = sd)) %>%
        as.data.frame() %>% mutate_if(is.numeric, round, digits = 3)
      summ=meansd(cbind.data.frame(res1,letter.1))
      res2 = rbind(paste0(res1$mean, "\u00B1", res1$sd, letter.1))
      res3 = Anova.Test(data, var, group,  Test = "Anova")
      tab = data.frame(
        variable = var,
        group = res2,
        p.value = res3[, "p.value"],
        Test = res3[, "Test"]
      )
    }
    if (res0$Test == "Kruskalwalis") {
      Quant1 = function(x) {
        quantile(x, probs = 0.25, na.rm = TRUE)
      }
      Quant2 = function(x) {
        quantile(x, probs = 0.5, na.rm = TRUE)
      }
      Quant3 = function(x) {
        quantile(x, probs = 0.75, na.rm = TRUE)
      }
      temp = data
      res1 = temp %>% group_by(.group123) %>% summarise_at(vars(var), list(q1 =
                                                                             Quant1, q2 = Quant2, q3 = Quant3))
      summ=cbind.data.frame(res1,letter.1)
      res2 = rbind(paste0(res1$q2, "(", res1$q1, ",", res1$q3, ")", letter.1))
      res3 = Anova.Test(data, var, group,  Test = "Kruskalwalis")
      tab = data.frame(
        variable = var,
        group = res2,
        p.value = res3[, "p.value"],
        Test = res3[, "Test"]
      )
    }
  }
  if (!is.null(Test)) {
    if (Test %in% c("Anova", "ANOVA", "aov", "A"))
      Test = "Anova"
    if (Test %in% c("Kruskalwalis", "Kruskal", "Krus", "K"))
      Test = "Kruskalwalis"
    if (!(Test %in% c("Anova", "Kruskalwalis")))
      stop("the Test must be Anova or Kruskalwalis")

    if (Test == "Anova") {
      res1 =  data %>% group_by(.group123) %>% summarise_at(vars(var), list(mean =
                                                                              mean , sd = sd)) %>%
        as.data.frame() %>% mutate_if(is.numeric, round, digits = 3)
      summ=meansd(cbind.data.frame(res1,letter.1))

      res2 = rbind(paste0(res1$mean, "\u00B1", res1$sd, letter.1))
      res3 = Anova.Test(data, var, group, Test = "Anova")

      tab = data.frame(
        variable = var,
        group = res2,
        p.value = res3[, "p.value"],
        Test = res3[, "Test"]
      )
    }

    if (Test == "Kruskalwalis") {
      Quant1 = function(x) {
        quantile(x, probs = 0.25, na.rm = TRUE)
      }
      Quant2 = function(x) {
        quantile(x, probs = 0.5, na.rm = TRUE)
      }
      Quant3 = function(x) {
        quantile(x, probs = 0.75, na.rm = TRUE)
      }
      temp = data
      res1 = temp %>% group_by(.group123) %>% summarise_at(vars(var), list(q1 =
                                                                             Quant1, q2 = Quant2, q3 = Quant3))
      res2 = rbind(paste0(res1$q2, "(", res1$q1, ",", res1$q3, ")", letter.1))
      summ=cbind.data.frame(res1,letter.1)
      res3 = Anova.Test(data, var, group,  Test = "Kruskalwalis")
      tab = data.frame(
        variable = var,
        group = res2,
        p.value = res3[, "p.value"],
        Test = res3[, "Test"]
      )

    }
  }
names(summ)=c("group", "lower", "centre","upper","letter")
if (draw_plot)  ggplot = barplotMean (summ,y_adjust=y_adjust)
if (save_plot)  ggsave(filename=paste0(filename,".jpeg"), plot=ggplot,...)
list(tab=tab, summary=summ, ggplot= ggplot)
}

