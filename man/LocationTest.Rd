\name{LocationTest}
\alias{LocationTest}
\title{LocationTest(parametric and non-parametric)}
\description{conducting statistical analysis using t-tests, ANOVA, and nonparametric tests.}
\usage{Location.Test(data, var,group, Test=NULL, draw_plot=TRUE, save_plot=TRUE, y_adjust=1.2,filename= "plot.123",...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{Your file name}
  \item{var}{The response variable}
  \item{group}{The group variable}
  \item{Test}{You can leave the name of the test you want as NULL (or you can choose one of :t.test or wilcox or       Anova Kruskalwalis) }
  \item{draw_plot}{TRUE(if you want to draw an error bar plot) or FALSE}
  \item{filename}{This is the name of the file you want to save as}

}


\author{
%%  ~~NedaMHD~~
}

\examples{

Data= women
Data = rbind(Data, Data)
Data$edu = rbinom(dim(Data)[1], 2, .4)
Data$sex=rbinom(dim(Data)[1],1,0.6)
Data$edu = factor(Data$edu,levels = c(0,1,2))
Data$sex = factor(Data$sex,levels = c(0,1))

Location.Test(data=Data, var="height",group="edu", Test=NULL, draw_plot=TRUE, save_plot=TRUE, y_adjust=1.8,filename= "plot.123")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
