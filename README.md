
> The LocationTest
> 
**Authors:** [Neda Mohammadi](https://nedamohammadi.ir/)
-----

### Description :
This repository hosts a collection of R code for conducting statistical analysis using t-tests, ANOVA, and nonparametric tests.
The code provided checks all necessary assumptions(Normality and equality of variance) and generates informative result tables.

**Key Features:**
T-test Analysis: The repository includes code for conducting t-tests, allowing you to compare means between two groups.
The code supports independent samples t-tests.
ANOVA Analysis: In addition to t-tests, the repository covers ANOVA (Analysis of Variance), which is used to compare means across multiple groups.
Nonparametric Tests: Recognizing that data may not always meet the assumptions of parametric tests, this repository also includes code for nonparametric tests.
The nonparametric tests covered are the "Mann-Whitney U test" and "Wilcoxon rank-sum test".
Result Summary: The code generates clear and concise error bars that summarize the results of the statistical tests.
These error bars provide essential information such as test statistics, p-values, facilitating the interpretation and presentation of your findings.

**Usage:**
Clone or download the code from this repository to your local machine. Open the R script using your preferred R environment.
Customize the code in "Location.Test" file to fit your specific data and research question.You can specify the type of analysis (t-test, ANOVA, or nonparametric test) or leave them blank.
Then specify the appropriate variables." Run the code to perform the statistical tests and obtain the result.

 
## Installation
``` r
remotes::install_github("nedamhd/LocationTest")
``` 
## Example
``` r
Data= women
Data = rbind(Data, Data)
Data$edu = rbinom(dim(Data)[1], 2, .4)
Data$sex=rbinom(dim(Data)[1],1,0.6)
Data$edu = factor(Data$edu,levels = c(0,1,2))
Data$sex = factor(Data$sex,levels = c(0,1))

Location.Test(data=Data, var="height",group="edu", Test=NULL, draw_plot=TRUE, save_plot=TRUE, y_adjust=1.8,filename= "plot.123")

``` 
-----
