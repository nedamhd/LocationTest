
ANOVA_table <-   R6::R6Class(
    "ANOVA_table",
  public =  list(
    data                 = NULL,
    group                = NULL, 
    model                = list(),
    deps.quantitative    = NULL,
    kruskal_wallis       = FALSE,
    results              = data.frame(),
    comparisons          = list(),
    initialize          = function(data, group, 
                                   deps.quantitative=NULL, 
                                   kruskal_wallis = FALSE, 
                                   posthoc= "Tukey", 
                                   p.adj= "holm", 
                                   shapiro.t = TRUE,
                                   digits = 2,
                                   digits.pvalue = 3,
                                   scientific.notation = FALSE) {
      self$data = data 
      self$group = group
      self$deps.quantitative = deps.quantitative
      self$kruskal_wallis = kruskal_wallis
      self$add.quantitative(posthoc = posthoc, p.adj = p.adj, 
                            shapiro.t = shapiro.t, 
                            digits = digits,
                            digits.pvalue = digits.pvalue,
                            scientific.notation = scientific.notation
      )
    },
    
    add.quantitative = function(data = NULL, 
                                group = NULL, 
                                deps.quantitative = NULL, 
                                kruskal_wallis = NULL, 
                                posthoc= "Tukey", 
                                p.adj="holm", shapiro.t=TRUE,
                                digits = 2,
                                digits.pvalue = 3,
                                scientific.notation = FALSE) {
      
      if(is.null(data))               data               = self$data
      if(is.null(group))              group              = self$group
      if(is.null(deps.quantitative))  deps.quantitative  = self$deps.quantitative
      if(is.null(kruskal_wallis))     kruskal_wallis     = self$kruskal_wallis
      
      private$ANOVA(data = data, group = group, 
                    deps = deps.quantitative, 
                    kruskal_wallis = kruskal_wallis,
                    posthoc = posthoc, p.adj = p.adj,
                    shapiro.t = shapiro.t,
                    digits = digits,
                    digits.pvalue = digits.pvalue,
                    scientific.notation = scientific.notation
      )
      names(self$comparisons) = deps.quantitative
      self$comparisons= do.call(rbind,self$comparisons)
      
    },
    
    
    wd.Table = function(x= NULL,..., filename=NULL, path = ""){
      if(is.null(x))
        x <- as.data.frame(self$results)
      if("RDCOMClient" %in% rownames(installed.packages()) == FALSE)  { 
        # Sys.setenv("TAR" = "internal") # if you need it.
        # devtools::install_github("omegahat/RDCOMClient")
        install.packages('RDCOMClient', repos = 'http://www.omegahat.org/R') }
      R2wd::wdGet(filename,path , method="RDCOMClient")
      R2wd::wdBody("\n\n")
      R2wd::wdTable(as.data.frame(x), ...)
      cat("Done!\n")
    }
  ),
  private  = list(
    result.for.plot = data.frame(),
    ANOVA = function(data, group, deps, 
                     kruskal_wallis = FALSE,   posthoc= "Tukey", 
                     p.adj= "holm", shapiro.t =TRUE,  digits = 2,
                     digits.pvalue = 3, scientific.notation = FALSE) {
      if(!(length(deps)== length(kruskal_wallis) | length(kruskal_wallis) == 1 ))
        stop("length of kruskal_wallis must be the same as deps or one!")
      if( any(unique(data[,group]) == 0)) stop("group must start from 1.")
      
      i = 0
      G.shapiro = c()
      leng= c()
      counts =c()
      for (jj in  1:length(deps)) {
        rm.data = na.omit(data[,c(deps[jj],group)])
        rm.group.level<- sort(unique(rm.data[[group]]))
        rm.group.counts =   table(rm.data[[group]])
        cat( "Count of ",group , " based on ",deps[jj],"\n" ,rm.group.counts,"\n")
        counts [jj] <- any(rm.group.counts<2)
        leng [jj] = length(table(rm.group.level))
      }
      deps1 = deps[which(counts==FALSE)  ] 
      deps2 = deps1[ which(leng[which(counts==FALSE)] == max(leng[which(counts==FALSE)])) ] 
      rm.deps2 = deps[which(!(deps %in% deps2))]
      if(length(deps2)!=length(deps)) 
        cat("Some deps.quantitatives removed due to empty cells.
        \nRemoved: \n", rm.deps2,  
            "\n\nNew deps.quantitative:\n", deps2)
      deps = deps2
      if(length(kruskal_wallis )> 1) {
        kruskal_wallis1= kruskal_wallis[which(counts==FALSE)]
        kruskal_wallis2= kruskal_wallis1[which(leng[which(counts==FALSE)] == max(leng[which(counts==FALSE)]))]
        kruskal_wallis = kruskal_wallis2
      }
      
      k=0
      for (dep in  deps) {
        
        if(length(deps)== length(kruskal_wallis)) {i = i +1} else {i =1}
        rm.data = na.omit(data[,c(dep,group)])
        group.value <- c(rm.data[[group]])
        group.level<- sort(unique(group.value))
        d= rm.data[[dep]]
        
        
        l = data.frame()
        for (j in 1:length(group.level)) {
          l =rbind(l, data.frame(dep, group.level[j],
                                 private$descriptive(d[group.value==group.level[j]])))
          
          # if(count [j] > 3)
          # {
          if(isTRUE(shapiro.t))
            G.shapiro[j] <-  shapiro.test(d[which(group.value==group.level[j])])$p.value  
          # } else{
          # G.shapiro[j] <- 1#ks.test(d[which(group.value==group.level[j])], "pnorm", 0, 1, exact = TRUE)$p.value 
          # }
        }
        
        
        if(isTRUE(shapiro.t)) 
        {shapiro    <- all(G.shapiro > 0.05) }else{
          shapiro    <- FALSE
        }
        
        formula <- paste0(dep, "~as.factor(",  group, ")")
        formula <- as.formula(formula)
        model <- aov(formula, data =  rm.data)
        k= k+1
        self$model[[k]] = model
        test.r <- "ANOVA" 
        if(isTRUE(shapiro.t))
          if(!isTRUE(shapiro))  
          {results <- kruskal.test(formula, data =  rm.data); test.r <- "KruskalWallis" }  
        
        if(length(kruskal_wallis) > 1){
          if(isTRUE(kruskal_wallis[i])) {results <- kruskal.test(formula, data =  rm.data); test.r <- "KruskalWallis" } 
        }
        
        if(length(kruskal_wallis)==1){
          if(isTRUE(kruskal_wallis)) {results <- kruskal.test(formula, data =  rm.data); test.r <- "KruskalWallis" } 
          
        }
        if(test.r=="KruskalWallis")
          p.value = sprintf("%.3f", round(results$p.value, digits.pvalue)) 
        if(test.r=="ANOVA")
          p.value = sprintf("%.3f", round(summary(model)[[1]][["Pr(>F)"]][1], digits.pvalue)) 
        
        if(p.value == "0.000") p.value = "<0.001"
        
        self$comparisons[[k]] <- private$regenerate_label_summary(model = model, posthoc= posthoc, p.adj= p.adj)$Result$comparison
        label = private$regenerate_label_summary(model = model, posthoc= posthoc, p.adj= p.adj)$labels.df$labels
        
        # label = multcompView::multcompLetters(
        #   TukeyHSD(model)[[1]][,"p adj"]  )$Letters
        l = cbind(l, label=label)
        private$result.for.plot <- rbind(private$result.for.plot, l)
       
         if(isFALSE(scientific.notation))
          values2 = data.frame( t(c(
          Factor= dep, 
          paste0(sprintf("%.2f", round(l[,9], digits)),
                 ' (', 
                 sprintf("%.2f", round(l[,10], digits)), ', ',
                 sprintf("%.2f", round(l[,11], digits)), ')',
                 label), 
          p.value,test = test.r)))
        
        
        
        if(isTRUE(scientific.notation))
          values2 = data.frame( t(c(
            Factor= dep, 
            paste0(sprintf("%.3e", l[,9]),
                   ' (', 
                   sprintf("%.3e", l[,10]), ', ',
                   sprintf("%.3e", l[,11]), ')',
                   label), 
            p.value,test = test.r)))
        
        
        if(isFALSE(scientific.notation))
          values1 = data.frame( t(c(
          Factor= dep, 
          paste0(sprintf("%.2f", round(l[,4], digits)),
                 ' \u00B1 ', 
                 sprintf("%.2f", round(l[,5], digits)), label), 
          p.value,test = test.r)))
       
        if(isTRUE(scientific.notation))
          values1 = data.frame( t(c(
            Factor= dep, 
            paste0(sprintf("%.3e", l[,4]),
                   ' \u00B1 ', 
                   sprintf("%.3e", l[,5]), label), 
            p.value,test = test.r))) 
          
          
          
        values = values1
        if(test.r == "KruskalWallis")
          values = values2
        
        names(values)<-   c("Factor", paste0("Level ", l[,2]) ,"P value", "Test")
        self$results <- rbind(self$results,values)
        rm(values)       
        # rm(l)
        
        
      }
      
    },
    
    descriptive = function(x, alpha = 0.05) {
      
      n = sum(!is.na(x))
      m = mean(x, na.rm = TRUE)
      s = sd(x, na.rm = TRUE)
      se = s/sqrt(n)
      l.ci = m - se*qnorm(1-(alpha/2))
      u.ci = m + se*qnorm(1-(alpha/2))
      med = quantile(x, p= 0.5)
      q1 = quantile(x, p= 0.25)
      q3 = quantile(x, p= 0.75)
      
      data.frame(n, m, s, se, l.ci, u.ci, med, q1, q3, alpha)
    },
    
    
    regenerate_label_summary=
      function(d = NULL,y = NULL, flev = NULL, model,posthoc=c("Tukey", "LSD","duncan","SNK" ,"REGW" ,"scheffe" ,"adjustment" ,"waerden" ,"median")
               ,p.adj=c("none","holm","hommel", 
                        "hochberg", "bonferroni", "BH", "BY", "fdr")
      ){
        # d is data
        # flev is factor  variable in
        if(is.null(d))       d = model$model
        if(is.null(y))       y = names(model$model)[1]
        if(is.null(flev)) flev = names( model$model)[2]
        require(agricolae)
        se<- function(x) sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))
        summary.me <- function(d=d,y=y,flev=flev){ 
          summa    <- function(x) c(length=sum(!is.na(x)), mean=mean(x,na.rm=TRUE),sd= sd(x,na.rm=TRUE),se=se(x),
                                    ci.l=mean(x,na.rm=TRUE) -1.96*se(x),
                                    ci.u=mean(x,na.rm=TRUE) +1.96*se(x) ,
                                    med = quantile(x, p= 0.5),
                                    q1 = quantile(x, p= 0.25),
                                    q3 = quantile(x, p= 0.75)
          )
          
          summary_data <-  aggregate(d[[y]],   by=list(  d[[flev]]),FUN=  summa)
          colnames(summary_data$x)<-      c("Length","Mean","SD","SE","CI.Lower","CI.Upper", "Median", "Q1", "Q3")
          cbind(level=summary_data$Group.1,summary_data$x)
        }
        summary_data<-data.frame(  summary.me(d=d,y=y,flev=flev))
        x<-flev
        # Extract labels and factor levels from Tukey post-hoc 
        if(posthoc== "LSD") out <-LSD.test(model, x, group=F, p.adj= p.adj,console=TRUE)
        
        if(posthoc== "duncan") out <-duncan.test(model, x, group=F,console=TRUE)
        
        if(posthoc== "SNK") out <-SNK.test(model, x, group=F,console=TRUE)
        
        if(posthoc== "REGW") out <-REGW.test(model, x, group=F,console=TRUE)
        
        if(posthoc== "Tukey") out <-HSD.test(model, x, group=F,console=TRUE)
        
        if(posthoc== "scheffe")out <- scheffe.test(model, x, group=FALSE,console=FALSE)
        
        if(posthoc== "adjustment") out<-with(d,kruskal(d[,y],d[,x],group=FALSE, p.adj=p.adj))
        
        if(posthoc== "waerden") out<-with(d,waerden.test(d[,y],d[,x],group=FALSE,console=TRUE))
        
        # if(posthoc== "median") out<-with(d,Median.test(d[,y],d[,x],console=TRUE))
        
        
        
        Tukey.levels<-out$comparison[,"pvalue"]
        sp<-strsplit(  rownames(out$comparison),"[ - ]")
        sp2<-c() 
        for(i in 1:length(sp))    sp2[i]<-   paste0( sp[[i]][1],sp[[i]][2],sp[[i]][3] )
        names( Tukey.levels)<- sp2
        Tukey.labels<-multcompView::multcompLetters(Tukey.levels)['Letters']
        plot.labels <- names(Tukey.labels[['Letters']])
        
        
        
        plot.levels <- data.frame(plot.labels, 
                                  labels = Tukey.labels[['Letters']],
                                  stringsAsFactors = FALSE)
        
        # Merge it with the labels
        labels.df <- merge(plot.levels, summary_data, 
                           by.x = 'plot.labels', 
                           by.y = "level", 
                           sort = FALSE)
        # if (all(labels.df$labels=="a")) {
        #   labels.df$labels<- rep(NA,length(labels.df$labels))
        # }
        list(labels.df=labels.df,Result=out)
      }
    
    
  )
  
)



# Data  = data.frame(
#   R = 1:1000,
#   Age = abs(rnorm(1000,32,10)),
#   Group = factor(rbinom(1000,3,0.5)+1),
#   Sex = factor(rbinom(1000,1,0.5)),
#   y1 =  (rnorm(1000)),
#   y2 =  (rnorm(1000)) ,
#   y3 = abs(rnorm(1000))
# )
# D<-  ANOVA_table$new(data = Data, group =  "Group",
#                      deps.quantitative = c("y1", "y2")  )

# D$deps.quantitative()
# D$add.qualitative()
# D$add.qualitative(deps.qualitative = "rs115.Cat")
# D$combine()
# D$wd.Table()
# D$results
