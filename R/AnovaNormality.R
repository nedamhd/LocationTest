
normality.Anova = function(data, var, group, Method = NULL) {
  data$.Group123456 = data[, group]
  uniq.level = unique(data$.Group123456)
  # statistic=list()
  p.value = c()
  # vars = list()
  # method=list()
# i=2
    for (i in 1:length(uniq.level)) {
      check_norm = Check_normality(data[data$.Group123456 == uniq.level[i], ], var = var)
      # statistic[[i]]=chech_norm$statistic
      p.value[i] = check_norm$p.value
      # method[[i]]=chech_norm$method
      # names(p.value) = paste0(group, " ", uniq.level[[i]])

    }
    names(p.value) = paste0(group, " ", uniq.level)
    # vars = p.value
    # return(p.value)
    return(as.data.frame(p.value))
}



