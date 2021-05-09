Project2
================

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

## Alay Shah aas4449

**Introduction**

*The title of my project is Modeling Relationship between Wealth and
Public Education in the United States.*

*The two datasets I’ve chosen are statesinfo, which has information
about public high school education for each state and us\_rent\_income
dataset, which has data about average income and rent of a household in
each state.*

*The statesinfo dataset contains 8 variables: X:(state), region, pop:
population of high school students who took SAT, SATV: S.A.T. verbal
score, SATM: S.A.T math score, percent(percentage of graduating high
school students who take S.A.T), dollars(state spending on public
education in $1000 per student), pay:(average teachers salary). This
dataset was acquired through CSV file of online data.The
us\_rent\_income dataset contains 5 variables: GEOID, Name:(State),
variable (rent or income), estimated average of that variable, and
MOE(margin of error). This data was acquired from the all package
function built in r from 1995.*

*The statesinfo dataset has 51 observations, while us\_rent\_income
dataset has 104 observations. The us\_rent\_income dataset was tidyed
since there were multiple observ.(rent and income) of variable (as shown
by us\_rent\_income1 which had 52 obs.). Furthermore, Puerto Rico was
dropped from us\_rent\_income, since upon (join on state) the other
dataset had no info on Puerto Rico, and its not considered a US state.
Columns were appropriately renamed, and observations were now both 51 in
each for merging. *

*These two datasets are interesting to me because Im curious to see how
high school education effectiveness in a state is related to its overall
wealth. I expect that a higher SAT\_avg score would correlate to a
higher income/rent estimate for a particular state. Im also excited to
see potential associations between the two datasets, for example if
you’re raised in a particular state with a better education system, is
it possible to predict your overall wealth(rent&income) in life, given
that you did not move.*

``` r
#data(package= .packages(all.available = TRUE))

#install.packages("tidyr")
library(tidyr)
library(dplyr)
data("us_rent_income")

#imported read csv file into data frame statesinfo
statesinfo <- read.csv("C:\\Users\\amana\\OneDrive\\Documents\\SDS348\\Projects\\statesinfo.csv", header = TRUE)

#TIDY

#statesinfo dataset was already tidy
#used pivot wider on us_rent_income only since there were multiple observ.(rent and income) of variable and they could be organized into their own column
us_rent_income1 <- us_rent_income%>%pivot_wider(names_from = variable, names_glue = "{variable}_{.value}", values_from = c(estimate,moe))
us_rent_income1
```

    ## # A tibble: 52 x 6
    ##    GEOID NAME                 income_estimate rent_estimate income_moe rent_moe
    ##    <chr> <chr>                          <dbl>         <dbl>      <dbl>    <dbl>
    ##  1 01    Alabama                        24476           747        136        3
    ##  2 02    Alaska                         32940          1200        508       13
    ##  3 04    Arizona                        27517           972        148        4
    ##  4 05    Arkansas                       23789           709        165        5
    ##  5 06    California                     29454          1358        109        3
    ##  6 08    Colorado                       32401          1125        109        5
    ##  7 09    Connecticut                    35326          1123        195        5
    ##  8 10    Delaware                       31560          1076        247       10
    ##  9 11    District of Columbia           43198          1424        681       17
    ## 10 12    Florida                        25952          1077         70        3
    ## # ... with 42 more rows

``` r
#JOIN/MERGE
library(tidyverse)
# renamed columns appropriately to state to allow for left join. Used left join to match on the fifty states +district of Columbia and dropped Puerto Rico, which was in us_rent_income dataset; it isnt considered a US state, had missing values for income, and its info was not provided in statesinfo dataset
#dropped GEOID from us_rent_income upon join since it didnt have meaningful info. 
us_rent_income1 <- us_rent_income1 %>% rename(state= "NAME")
statesdata <- statesinfo %>% rename(state= "X") %>% left_join(us_rent_income1[,c(-1)], "state")
#used mutate to make a new column for average SAT Score
statesdata <- statesdata %>% mutate(SAT_avg = (SATV+SATM)/2)
```

## EDA

``` r
#Next I began calculating summary statistics for my numeric variables overall and after grouping with my categorical variable region.

#Summary Statistics of population
statesdata %>% 
  summarize(mean_pop = mean(pop), 
            sd_pop = sd(pop),
            var= var(pop),
            IQR = IQR(pop), 
            n_rows = n(),
            quants = quantile(pop, probs=c(.1, .25, .5, .75, .90)),
            min = min(pop),
            max= max(pop),
            n_pop = n_distinct(pop)) 
```

    ##   mean_pop   sd_pop      var  IQR n_rows quants min   max n_pop
    ## 1 4876.647 5439.203 29584926 4565     51    666 454 29760    51
    ## 2 4876.647 5439.203 29584926 4565     51   1215 454 29760    51
    ## 3 4876.647 5439.203 29584926 4565     51   3294 454 29760    51
    ## 4 4876.647 5439.203 29584926 4565     51   5780 454 29760    51
    ## 5 4876.647 5439.203 29584926 4565     51  11431 454 29760    51

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD population per region
pop_data_grouped<- statesdata %>% 
  group_by(region)%>%
  summarize(mean_pop = mean(pop), 
            sd_pop = sd(pop),
            var_pop= var(pop),
            IQR = IQR(pop), 
            n_rows = n(),
            quant_50_pop = quantile(pop, probs=c(0.5)),
            min = min(pop),
            max= max(pop),
            n_pop = n_distinct(pop))

#Summary Statistics of SAT Verbal score
statesdata %>% 
  summarize(mean_SATV = mean(SATV), 
            sd_SATV = sd(SATV),
            var= var(SATV),
            IQR = IQR(SATV), 
            n_rows = n(),
            quants = quantile(SATV, probs=c(.1, .25, .5, .75, .90)),
            min = min(SATV),
            max= max(SATV),
            n_SATV = n_distinct(SATV)) 
```

    ##   mean_SATV  sd_SATV      var IQR n_rows quants min max n_SATV
    ## 1  448.1569 30.82101 949.9349  52     51  409.0 397 511     42
    ## 2  448.1569 30.82101 949.9349  52     51  422.5 397 511     42
    ## 3  448.1569 30.82101 949.9349  52     51  443.0 397 511     42
    ## 4  448.1569 30.82101 949.9349  52     51  474.5 397 511     42
    ## 5  448.1569 30.82101 949.9349  52     51  484.0 397 511     42

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD SAT Verbal score per region
SATV_data_grouped<- statesdata %>% 
  group_by(region)%>%
  summarize(mean_SATV = mean(SATV), 
            sd_SATV = sd(SATV),
            var_SATV= var(SATV),
            IQR = IQR(SATV), 
            n_rows = n(),
            quant_50_SATV = quantile(SATV, probs=c(0.5)),
            min = min(SATV),
            max= max(SATV),
            n_SATV = n_distinct(SATV))
#Summary Statistics of SAT Math score
statesdata %>% 
  summarize(mean_SATM = mean(SATM), 
            sd_SATM = sd(SATM),
            var= var(SATM),
            IQR = IQR(SATM), 
            n_rows = n(),
            quants = quantile(SATM, probs=c(.1, .25, .5, .75, .90)),
            min = min(SATM),
            max= max(SATM),
            n_SATV = n_distinct(SATM)) 
```

    ##   mean_SATM  sd_SATM      var  IQR n_rows quants min max n_SATV
    ## 1  497.3922 34.56882 1195.003 52.5     51  461.0 437 577     40
    ## 2  497.3922 34.56882 1195.003 52.5     51  470.0 437 577     40
    ## 3  497.3922 34.56882 1195.003 52.5     51  490.0 437 577     40
    ## 4  497.3922 34.56882 1195.003 52.5     51  522.5 437 577     40
    ## 5  497.3922 34.56882 1195.003 52.5     51  543.0 437 577     40

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD SAT Math score per region
SATM_data_grouped<- statesdata %>% 
  group_by(region)%>%
  summarize(mean_SATM = mean(SATM), 
            sd_SATM = sd(SATM),
            var_SATM= var(SATM),
            IQR = IQR(SATM), 
            n_rows = n(),
            quant_50_SATM = quantile(SATM, probs=c(0.5)),
            min = min(SATM),
            max= max(SATM),
            n_SATM = n_distinct(SATM))

#Summary Statistics of percent(percentage of graduating high school students who take S.A.T)
statesdata %>% 
  summarize(mean_percent = mean(percent), 
            sd_pop = sd(percent),
            var= var(percent),
            IQR = IQR(percent), 
            n_rows = n(),
            quants = quantile(percent, probs=c(.1, .25, .5, .75, .90)),
            min = min(percent),
            max= max(percent),
            n_pop = n_distinct(percent)) 
```

    ##   mean_percent   sd_pop      var IQR n_rows quants min max n_pop
    ## 1      33.7451 24.07392 579.5537  46     51    6.0   4  74    37
    ## 2      33.7451 24.07392 579.5537  46     51   11.5   4  74    37
    ## 3      33.7451 24.07392 579.5537  46     51   25.0   4  74    37
    ## 4      33.7451 24.07392 579.5537  46     51   57.5   4  74    37
    ## 5      33.7451 24.07392 579.5537  46     51   67.0   4  74    37

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD percent per region
percent_data_grouped<- statesdata %>% 
  group_by(region)%>%
  summarize(mean_percent = mean(percent), 
            sd_percent = sd(percent),
            var_percent= var(percent),
            IQR = IQR(percent), 
            n_rows = n(),
            quants_50_percent = quantile(percent, probs=c(0.5)),
            min = min(percent),
            max= max(percent),
            n_percent = n_distinct(percent))
#Summary Statistics of  dollars(state spending on public education in $1000 per student)
statesdata %>% 
  summarize(mean_dollars = mean(dollars), 
            sd_dollars = sd(dollars),
            var= var(dollars),
            IQR = IQR(dollars), 
            n_rows = n(),
            quants = quantile(dollars, probs=c(.1, .25, .5, .75, .90)),
            min = min(dollars),
            max= max(dollars),
            n_dollars = n_distinct(dollars)) 
```

    ##   mean_dollars sd_dollars      var    IQR n_rows quants   min   max n_dollars
    ## 1      5.17549   1.376166 1.893833 1.3355     51 3.6850 2.993 9.159        51
    ## 2      5.17549   1.376166 1.893833 1.3355     51 4.3540 2.993 9.159        51
    ## 3      5.17549   1.376166 1.893833 1.3355     51 5.0450 2.993 9.159        51
    ## 4      5.17549   1.376166 1.893833 1.3355     51 5.6895 2.993 9.159        51
    ## 5      5.17549   1.376166 1.893833 1.3355     51 6.9890 2.993 9.159        51

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD dollars per region
dollars_data_grouped <- statesdata %>% 
  group_by(region)%>%
  summarize(mean_dollars = mean(dollars), 
            sd_dollars = sd(dollars),
            var_dollars= var(dollars),
            IQR = IQR(dollars), 
            n_rows = n(),
            quants_50_dollars = quantile(dollars, probs=c(0.5)),
            min = min(dollars),
            max= max(dollars),
            n_dollars = n_distinct(dollars))
#Summary Statistics of pay:(average teachers salary)
statesdata %>% 
  summarize(mean_pay = mean(pay), 
            sd_pay = sd(pay),
            var= var(pay),
            IQR = IQR(pay), 
            n_rows = n(),
            quants = quantile(pay, probs=c(.1, .25, .5, .75, .90)),
            min = min(pay),
            max= max(pay),
            n_pay = n_distinct(pay)) 
```

    ##   mean_pay   sd_pay      var IQR n_rows quants min max n_pay
    ## 1 30.94118 5.308151 28.17647   6     51   25.0  22  43    20
    ## 2 30.94118 5.308151 28.17647   6     51   27.5  22  43    20
    ## 3 30.94118 5.308151 28.17647   6     51   30.0  22  43    20
    ## 4 30.94118 5.308151 28.17647   6     51   33.5  22  43    20
    ## 5 30.94118 5.308151 28.17647   6     51   38.0  22  43    20

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD pay region
pay_data_grouped <- statesdata %>% 
  group_by(region)%>%
  summarize(mean_pay = mean(pay), 
            sd_pay = sd(pay),
            var_pay= var(pay),
            IQR = IQR(pay), 
            n_rows = n(),
            quants_50_pay = quantile(pay, probs=c(0.5)),
            min = min(pay),
            max= max(pay),
            n_pay = n_distinct(pay))

#Summary Statistics of income estimate
statesdata %>% 
  summarize(mean_income_estimate = mean(income_estimate), 
            sd_income_estimate = sd(income_estimate),
            var= var(income_estimate),
            IQR = IQR(income_estimate), 
            n_rows = n(),
            quants = quantile(income_estimate, probs=c(.1, .25, .5, .75, .90)),
            min = min(income_estimate),
            max= max(income_estimate),
            n_income_estimate = n_distinct(income_estimate)) 
```

    ##   mean_income_estimate sd_income_estimate      var    IQR n_rows  quants   min
    ## 1             29188.24           3918.746 15356570 5573.5     51 24702.0 22766
    ## 2             29188.24           3918.746 15356570 5573.5     51 26365.5 22766
    ## 3             29188.24           3918.746 15356570 5573.5     51 28923.0 22766
    ## 4             29188.24           3918.746 15356570 5573.5     51 31939.0 22766
    ## 5             29188.24           3918.746 15356570 5573.5     51 33172.0 22766
    ##     max n_income_estimate
    ## 1 43198                51
    ## 2 43198                51
    ## 3 43198                51
    ## 4 43198                51
    ## 5 43198                51

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD income estimate region
income_estimate_data_grouped <- statesdata %>% 
  group_by(region)%>%
  summarize(mean_income_estimate = mean(income_estimate), 
            sd_income_estimate = sd(income_estimate),
            var_income_estimate= var(income_estimate),
            IQR = IQR(income_estimate), 
            n_rows = n(),
            quants_50_income_estimate = quantile(income_estimate, probs=c(0.5)),
            min = min(income_estimate),
            max= max(income_estimate),
            n_income_estimate = n_distinct(income_estimate))
#Summary Statistics of rent estimate
statesdata %>% 
  summarize(mean_rent_estimate = mean(rent_estimate), 
            sd_rent_estimate = sd(rent_estimate),
            var= var(rent_estimate),
            IQR = IQR(rent_estimate), 
            n_rows = n(),
            quants = quantile(rent_estimate, probs=c(.1, .25, .5, .75, .90)),
            min = min(rent_estimate),
            max= max(rent_estimate),
            n_rent_estimate = n_distinct(rent_estimate)) 
```

    ##   mean_rent_estimate sd_rent_estimate      var   IQR n_rows quants min  max
    ## 1           941.4314         204.1532 41678.53 293.5     51  740.0 681 1507
    ## 2           941.4314         204.1532 41678.53 293.5     51  783.0 681 1507
    ## 3           941.4314         204.1532 41678.53 293.5     51  885.0 681 1507
    ## 4           941.4314         204.1532 41678.53 293.5     51 1076.5 681 1507
    ## 5           941.4314         204.1532 41678.53 293.5     51 1200.0 681 1507
    ##   n_rent_estimate
    ## 1              48
    ## 2              48
    ## 3              48
    ## 4              48
    ## 5              48

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD rent estimate region
rent_estimate_data_grouped <- statesdata %>% 
  group_by(region)%>%
  summarize(mean_rent_estimate = mean(rent_estimate), 
            sd_rent_estimate = sd(rent_estimate),
            var_rent_estimate= var(rent_estimate),
            IQR = IQR(rent_estimate), 
            n_rows = n(),
            quants_50_rent_estimate = quantile(rent_estimate, probs=c(0.5)),
            min = min(rent_estimate),
            max= max(rent_estimate),
            n_rent_estimate = n_distinct(rent_estimate))

#Summary Statistics of income moe (Margin of Error)
statesdata %>% 
  summarize(mean_income_moe = mean(income_moe), 
            sd_income_moe = sd(income_moe),
            var= var(income_moe),
            IQR = IQR(income_moe), 
            n_rows = n(),
            quants = quantile(income_moe, probs=c(.1, .25, .5, .75, .90)),
            min = min(income_moe),
            max= max(income_moe),
            n_income_moe = n_distinct(income_moe)) 
```

    ##   mean_income_moe sd_income_moe      var  IQR n_rows quants min max
    ## 1        187.1569      110.7603 12267.85 97.5     51  101.0  69 681
    ## 2        187.1569      110.7603 12267.85 97.5     51  113.0  69 681
    ## 3        187.1569      110.7603 12267.85 97.5     51  155.0  69 681
    ## 4        187.1569      110.7603 12267.85 97.5     51  210.5  69 681
    ## 5        187.1569      110.7603 12267.85 97.5     51  276.0  69 681
    ##   n_income_moe
    ## 1           46
    ## 2           46
    ## 3           46
    ## 4           46
    ## 5           46

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD income moe (Margin of Error) estimate region
income_moe_data_grouped <- statesdata %>% 
  group_by(region)%>%
  summarize(mean_income_moe = mean(income_moe), 
            sd_income_moe = sd(income_moe),
            var_income_moe= var(income_moe),
            IQR = IQR(income_moe), 
            n_rows = n(),
            quants_50_income_moe = quantile(income_moe, probs=c(0.5)),
            min = min(income_moe),
            max= max(income_moe),
            n_income_moe = n_distinct(income_moe))

#Summary Statistics of rent moe (Margin of Error)
statesdata %>% 
  summarize(mean_rent_moe = mean(rent_moe), 
            sd_rent_moe = sd(rent_moe),
            var= var(rent_moe),
            IQR = IQR(rent_moe), 
            n_rows = n(),
            quants = quantile(rent_moe, probs=c(.1, .25, .5, .75, .90)),
            min = min(rent_moe),
            max= max(rent_moe),
            n_rent_moe = n_distinct(rent_moe)) 
```

    ##   mean_rent_moe sd_rent_moe      var IQR n_rows quants min max n_rent_moe
    ## 1      5.607843    3.458777 11.96314   3     51      3   2  18         12
    ## 2      5.607843    3.458777 11.96314   3     51      3   2  18         12
    ## 3      5.607843    3.458777 11.96314   3     51      4   2  18         12
    ## 4      5.607843    3.458777 11.96314   3     51      6   2  18         12
    ## 5      5.607843    3.458777 11.96314   3     51     10   2  18         12

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD rent moe (Margin of Error) estimate region
rent_moe_data_grouped <- statesdata %>% 
  group_by(region)%>%
  summarize(mean_rent_moe = mean(rent_moe), 
            sd_rent_moe = sd(rent_moe),
            var_rent_moe= var(rent_moe),
            IQR = IQR(rent_moe), 
            n_rows = n(),
            quants_50_rent_moe = quantile(rent_moe, probs=c(0.5)),
            min = min(rent_moe),
            max= max(rent_moe),
            n_rent_moe = n_distinct(rent_moe))
#Summary Statistics of SAT Avg score
statesdata %>% 
  summarize(mean_SAT_avg = mean(SAT_avg), 
            sd_SAT_avg = sd(SAT_avg),
            var= var(SAT_avg),
            IQR = IQR(SAT_avg), 
            n_rows = n(),
            quants = quantile(SAT_avg, probs=c(.1, .25, .5, .75, .90)),
            min = min(SAT_avg),
            max= max(SAT_avg),
            n_SAT_avg = n_distinct(SAT_avg)) 
```

    ##   mean_SAT_avg sd_SAT_avg      var   IQR n_rows quants min max n_SAT_avg
    ## 1     472.7745   32.38415 1048.733 50.75     51 437.00 417 544        45
    ## 2     472.7745   32.38415 1048.733 50.75     51 446.50 417 544        45
    ## 3     472.7745   32.38415 1048.733 50.75     51 466.50 417 544        45
    ## 4     472.7745   32.38415 1048.733 50.75     51 497.25 417 544        45
    ## 5     472.7745   32.38415 1048.733 50.75     51 515.00 417 544        45

``` r
#Found Summary Statistics by subgroup of categorical variable region
#used group by to find the groups of data, and calculated mean and SD SAT_avg score per region
SAT_avg_data_grouped <- statesdata %>% 
  group_by(region)%>%
  summarize(mean_SAT_avg = mean(SAT_avg), 
            sd_SAT_avg = sd(SAT_avg),
            var_SAT_avg= var(SAT_avg),
            IQR = IQR(SAT_avg), 
            n_rows = n(),
            quants_50_SAT_avg = quantile(SAT_avg, probs=c(0.5)),
            min = min(SAT_avg),
            max= max(SAT_avg),
            n_SAT_avg = n_distinct(SAT_avg))


#correlation coefficients matrix heatmap only
#cor function used for pairwise complete observation
# created correlation matrix for all numeric variables
states_num <- statesdata %>%
  select_if(is.numeric) 
correlation_matrix_states<- cor(states_num, use = "complete.obs")


# Make it pretty using a heatmap with geom_tile!
cor(states_num, use = "pairwise.complete.obs") %>%
  # Save as a data frame
  as.data.frame %>%
  # Convert row names to an explicit variable
  rownames_to_column %>%
  # Pivot so that all correlations appear in the same column
  pivot_longer(-1, names_to = "other_var", values_to = "correlation") %>%
  ggplot(aes(rowname, other_var, fill=correlation)) +
  # Heatmap with geom_tile
  geom_tile() +
  # Change the scale to make the middle appear neutral
  scale_fill_gradient2(low="red",mid="white",high="blue") +
  # Overlay values
  geom_text(aes(label = round(correlation,1.5)), color = "black", size = 4) +
  # Give title and labels
  labs(title = "Correlation matrix for the dataset statesdata", x = "variable 1", y = "variable 2")
```

![](Project2_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#correlation matrix with univariate/bivariate graphs and correlation coefficients all in 1 plot
#1) distribution of each variable is shown on diagonal, 2) On the bottom of the diagonal : the bivariate scatter plots with a fitted line are displayed 3)On the top of the diagonal : the value of the correlation plus the significance level as stars (Each significance level is associated to a symbol : p-values(0, 0.001, 0.01, 0.05, 0.1, 1))

#install.packages("PerformanceAnalytics", dependencies = TRUE)
library("PerformanceAnalytics")
#used Chart Correlation function and installed Performance Analytics
chart.Correlation(statesdata[,3:13], histogram=TRUE, pch="+")
```

![](Project2_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
library(ggpubr)
# conduct t -test between SAT_avg and income_estimate
t.test(states_num$pop, states_num$SAT_avg, var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  states_num$pop and states_num$SAT_avg
    ## t = 5.782, df = 50.004, p-value = 4.765e-07
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  2874.048 5933.697
    ## sample estimates:
    ## mean of x mean of y 
    ## 4876.6471  472.7745

``` r
#made scatterplot of bivariate visualization of SAT_avg and income_estimate with regression line
ggscatter(states_num, x = "SAT_avg", y = "income_estimate",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "SAT_avg score", ylab ="Income estimate (in $1000 dollars)")
```

![](Project2_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
# conduct t -test between pay(average teachers salary in dollars) and dollars(state spending on public education in $1000 per student)
t.test(states_num$pay, states_num$dollars, var.equal = FALSE)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  states_num$pay and states_num$dollars
    ## t = 33.555, df = 56.691, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  24.22789 27.30349
    ## sample estimates:
    ## mean of x mean of y 
    ##  30.94118   5.17549

``` r
#made scatterplot of bivariate visualization of pay(average teachers salary in dollars) and dollars(state spending on public education in $1000 per student) with regression line
ggscatter(states_num, x = "pay", y = "dollars",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pay(average teachers salary in dollars)", ylab ="Dollars (state spending on public education in $1000 per student)")
```

![](Project2_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

*Findings: The Visualizations correlation matrix disproved my hypothesis
there Wasn’t a strong correlation between SAT\_avg and rent/income as
predicted(-0.55). However, a higher SAT Score had a strong negative
correlation with percent of students graduating from high school who
took SAT(-0.87). The second visualization of the correlation matrix with
univariate/bivariate graphs shows key results such as the diagonal shows
distribution of univariate variable foe example, (dollars)- (state
spending on public education in $1000 per student) & SAT\_avg vary a
lot.Furthermore, bivariate scatterplot of rent\_estimate and
income\_estimate and correlation is significant(0.74). *

*T -test was conducted (The null hypothesis is there no difference
between SAT\_avg and income\_estimate, while the alternate hypothesis is
there is a difference between SAT\_avg and income\_estimate.However, as
shown by t-test, the p value is 4.765e-07 which is less than 0.05, and
the null hypothesis is rejected, and there is a difference between
SAT\_avg and income\_estimate (in $1000 dollars) Seperate Scatterplot
with regression line confirmed findings in second visualization.*

*T -test was conducted (The null hypothesis is there no difference
between pay and dollars, while the alternate hypothesis is there is a
difference between pay and dollars.However, as shown by t-test, the p
value is less than 2.2e-16 which is less than 0.05, and the null
hypothesis is rejected, and there is a difference between pay(average
teachers salary in dollars) and dollars(state spending on public
education in $1000 per student). Seperate Scatterplot with regression
line confirmed findings in second visualization.*

## MANOVA

``` r
#H0: the means of the numeric variables are same across the different regions
#Ha: at least one of the means of the numeric variables is different across the different sites

# Perform MANOVA with numeric response variables listed in cbind() across categorical variable Region in US
manova_states <- manova(cbind(pop,SATV, SATM, percent, dollars, pay,income_estimate, rent_estimate, income_moe, rent_moe, SAT_avg) ~ region, data = statesdata)

# OUtput of MANOVA
summary(manova_states, tol = 0)
```

    ##           Df Pillai approx F num Df den Df    Pr(>F)    
    ## region     8 3.6227   2.9342     88    312 2.915e-12 ***
    ## Residuals 42                                            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# MANOVA is significant so we can perform one-way ANOVA for each variable
summary.aov(manova_states)
```

    ##  Response pop :
    ##             Df     Sum Sq  Mean Sq F value Pr(>F)  
    ## region       8  461256327 57657041  2.3788 0.0327 *
    ## Residuals   42 1017989969 24237856                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response SATV :
    ##             Df Sum Sq Mean Sq F value    Pr(>F)    
    ## region       8  34669  4333.6  14.189 9.536e-10 ***
    ## Residuals   42  12828   305.4                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response SATM :
    ##             Df Sum Sq Mean Sq F value    Pr(>F)    
    ## region       8  45685  5710.6  17.052 6.065e-11 ***
    ## Residuals   42  14065   334.9                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response percent :
    ##             Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## region       8 24194.1 3024.27  26.553 4.512e-14 ***
    ## Residuals   42  4783.6  113.89                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response dollars :
    ##             Df Sum Sq Mean Sq F value    Pr(>F)    
    ## region       8 60.719  7.5899  9.3834 2.581e-07 ***
    ## Residuals   42 33.972  0.8089                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response pay :
    ##             Df Sum Sq Mean Sq F value    Pr(>F)    
    ## region       8 782.92  97.865   6.567 1.547e-05 ***
    ## Residuals   42 625.91  14.903                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response income_estimate :
    ##             Df    Sum Sq  Mean Sq F value Pr(>F)  
    ## region       8 238792353 29849044  2.3697 0.0333 *
    ## Residuals   42 529036156 12596099                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response rent_estimate :
    ##             Df  Sum Sq Mean Sq F value    Pr(>F)    
    ## region       8 1089249  136156  5.7492 5.897e-05 ***
    ## Residuals   42  994678   23683                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ##  Response income_moe :
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## region       8 121210   15151  1.2929 0.2734
    ## Residuals   42 492183   11719               
    ## 
    ##  Response rent_moe :
    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## region       8 151.17  18.897  1.7756 0.1094
    ## Residuals   42 446.98  10.643               
    ## 
    ##  Response SAT_avg :
    ##             Df Sum Sq Mean Sq F value    Pr(>F)    
    ## region       8  39615  4951.9  16.221 1.302e-10 ***
    ## Residuals   42  12822   305.3                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# ANOVA is significant (except for income_moe and rent_moe) then we can perform post-hoc analysis
# post hoc tests conducted for numeric variables that were significant

pairwise.t.test(statesdata$pop,statesdata$region, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  statesdata$pop and statesdata$region 
    ## 
    ##     ENC    ESC    MA     MTN    NE     PAC    SA     WNC   
    ## ESC 0.1703 -      -      -      -      -      -      -     
    ## MA  0.2569 0.0250 -      -      -      -      -      -     
    ## MTN 0.0217 0.4927 0.0023 -      -      -      -      -     
    ## NE  0.0437 0.6188 0.0049 0.8536 -      -      -      -     
    ## PAC 0.8540 0.2290 0.1974 0.0349 0.0661 -      -      -     
    ## SA  0.2018 0.7253 0.0239 0.1974 0.3148 0.2833 -      -     
    ## WNC 0.0477 0.6825 0.0052 0.7505 0.9070 0.0729 0.3555 -     
    ## WSC 0.6040 0.4124 0.1268 0.1068 0.1664 0.7296 0.5384 0.1856
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(statesdata$SATV,statesdata$region, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  statesdata$SATV and statesdata$region 
    ## 
    ##     ENC     ESC     MA      MTN     NE      PAC     SA      WNC    
    ## ESC 0.03923 -       -       -       -       -       -       -      
    ## MA  0.01062 6.7e-05 -       -       -       -       -       -      
    ## MTN 0.27263 0.20189 0.00043 -       -       -       -       -      
    ## NE  0.04723 0.00017 0.31756 0.00123 -       -       -       -      
    ## PAC 0.04022 0.00017 0.40512 0.00125 0.86822 -       -       -      
    ## SA  0.00140 1.7e-06 0.94709 5.0e-06 0.21014 0.31296 -       -      
    ## WNC 0.00020 0.13212 1.5e-07 0.00151 7.1e-08 1.2e-07 1.0e-10 -      
    ## WSC 0.47504 0.18900 0.00269 0.80743 0.01083 0.00953 0.00027 0.00404
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(statesdata$SATM,statesdata$region, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  statesdata$SATM and statesdata$region 
    ## 
    ##     ENC     ESC     MA      MTN     NE      PAC     SA      WNC    
    ## ESC 0.36891 -       -       -       -       -       -       -      
    ## MA  0.00467 0.00071 -       -       -       -       -       -      
    ## MTN 0.64953 0.57247 0.00081 -       -       -       -       -      
    ## NE  0.00117 0.00013 0.91842 7.5e-05 -       -       -       -      
    ## PAC 0.02768 0.00386 0.31703 0.00467 0.27718 -       -       -      
    ## SA  1.9e-05 2.2e-06 0.45391 3.2e-07 0.28001 0.03119 -       -      
    ## WNC 0.00032 0.01028 7.9e-08 0.00031 7.4e-10 1.1e-07 1.6e-12 -      
    ## WSC 0.65061 0.20259 0.01825 0.35983 0.00782 0.09760 0.00028 0.00016
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(statesdata$percent,statesdata$region, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  statesdata$percent and statesdata$region 
    ## 
    ##     ENC     ESC     MA      MTN     NE      PAC     SA      WNC    
    ## ESC 0.04921 -       -       -       -       -       -       -      
    ## MA  9.7e-07 6.2e-09 -       -       -       -       -       -      
    ## MTN 0.41582 0.15348 2.2e-08 -       -       -       -       -      
    ## NE  4.2e-08 1.7e-10 0.84340 1.8e-10 -       -       -       -      
    ## PAC 0.00123 4.1e-06 0.00925 3.1e-05 0.00386 -       -       -      
    ## SA  1.6e-05 3.0e-08 0.03321 6.3e-08 0.01568 0.35221 -       -      
    ## WNC 0.02885 0.95767 5.9e-10 0.10532 3.2e-12 3.9e-07 5.2e-10 -      
    ## WSC 0.36909 0.29515 1.6e-07 0.81958 7.3e-09 0.00015 1.8e-06 0.25968
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(statesdata$dollars,statesdata$region, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  statesdata$dollars and statesdata$region 
    ## 
    ##     ENC     ESC     MA      MTN     NE      PAC     SA      WNC    
    ## ESC 0.01015 -       -       -       -       -       -       -      
    ## MA  0.00020 1.7e-07 -       -       -       -       -       -      
    ## MTN 0.04572 0.30787 2.6e-07 -       -       -       -       -      
    ## NE  0.07131 4.8e-05 0.01221 0.00012 -       -       -       -      
    ## PAC 0.70036 0.00387 0.00056 0.01686 0.15571 -       -       -      
    ## SA  0.75134 0.00197 0.00014 0.00806 0.08094 0.90474 -       -      
    ## WNC 0.08900 0.21646 8.1e-07 0.76687 0.00040 0.03656 0.02214 -      
    ## WSC 0.01331 0.91939 2.3e-07 0.36559 6.8e-05 0.00517 0.00275 0.26074
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(statesdata$pay,statesdata$region, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  statesdata$pay and statesdata$region 
    ## 
    ##     ENC     ESC     MA      MTN     NE      PAC     SA      WNC    
    ## ESC 0.01201 -       -       -       -       -       -       -      
    ## MA  0.09165 0.00029 -       -       -       -       -       -      
    ## MTN 0.01172 0.67444 0.00020 -       -       -       -       -      
    ## NE  0.82063 0.00528 0.11991 0.00409 -       -       -       -      
    ## PAC 0.41732 0.00150 0.31506 0.00098 0.53377 -       -       -      
    ## SA  0.35302 0.04567 0.01056 0.05045 0.21604 0.06875 -       -      
    ## WNC 0.00443 1.00000 7.8e-05 0.61932 0.00143 0.00035 0.01827 -      
    ## WSC 0.00197 0.52494 4.5e-05 0.25128 0.00073 0.00020 0.00742 0.47353
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(statesdata$income_estimate,statesdata$region, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  statesdata$income_estimate and statesdata$region 
    ## 
    ##     ENC    ESC    MA     MTN    NE     PAC    SA     WNC   
    ## ESC 0.0928 -      -      -      -      -      -      -     
    ## MA  0.2181 0.0098 -      -      -      -      -      -     
    ## MTN 0.8141 0.1036 0.1291 -      -      -      -      -     
    ## NE  0.1537 0.0030 0.9625 0.0672 -      -      -      -     
    ## PAC 0.2781 0.0086 0.7666 0.1529 0.7619 -      -      -     
    ## SA  0.3435 0.0075 0.5730 0.1756 0.5160 0.7749 -      -     
    ## WNC 0.4567 0.0148 0.4966 0.2730 0.4337 0.6653 0.8521 -     
    ## WSC 0.2706 0.5700 0.0352 0.3218 0.0155 0.0372 0.0386 0.0648
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(statesdata$rent_estimate,statesdata$region, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  statesdata$rent_estimate and statesdata$region 
    ## 
    ##     ENC     ESC     MA      MTN     NE      PAC     SA      WNC    
    ## ESC 0.47156 -       -       -       -       -       -       -      
    ## MA  0.01593 0.00406 -       -       -       -       -       -      
    ## MTN 0.37752 0.11139 0.05679 -       -       -       -       -      
    ## NE  0.05662 0.01301 0.36495 0.21593 -       -       -       -      
    ## PAC 0.00014 3.0e-05 0.27136 0.00053 0.02023 -       -       -      
    ## SA  0.01817 0.00352 0.49070 0.08310 0.72859 0.02709 -       -      
    ## WNC 0.62122 0.75621 0.00363 0.12969 0.01109 9.9e-06 0.00198 -      
    ## WSC 0.89277 0.57807 0.01558 0.33324 0.05430 0.00019 0.01931 0.75063
    ## 
    ## P value adjustment method: none

``` r
pairwise.t.test(statesdata$SAT_avg,statesdata$region, p.adj="none")
```

    ## 
    ##  Pairwise comparisons using t tests with pooled SD 
    ## 
    ## data:  statesdata$SAT_avg and statesdata$region 
    ## 
    ##     ENC     ESC     MA      MTN     NE      PAC     SA      WNC    
    ## ESC 0.13106 -       -       -       -       -       -       -      
    ## MA  0.00588 0.00017 -       -       -       -       -       -      
    ## MTN 0.43072 0.34941 0.00046 -       -       -       -       -      
    ## NE  0.00681 0.00011 0.57856 0.00023 -       -       -       -      
    ## PAC 0.02952 0.00069 0.34710 0.00200 0.62453 -       -       -      
    ## SA  0.00012 1.3e-06 0.71881 7.9e-07 0.23319 0.10072 -       -      
    ## WNC 0.00019 0.03529 6.5e-08 0.00053 3.8e-09 6.9e-08 6.1e-12 -      
    ## WSC 0.90381 0.18564 0.00620 0.54679 0.00776 0.03002 0.00021 0.00063
    ## 
    ## P value adjustment method: none

``` r
# To interpret the p-values, what is the value for the Bonferonni alpha?

#336 hypothesis tests conducted (1 Manova +11 ANOVA for each variable,  (36 x9) t tests, 9 variables significant)
#x=336
#Pr(at least 1 Type 1 Error) = 1 - Pr(no Type 1 Error) = 1- 0.95^(x) = 99.999996725 %
#new significance level  (0.05/336)=  0.00014880952

#After correcting for new significance level, one-way ANOVA for each variable shows that population(pop) & income_estimate are above new significance level= 0.00014880952 and the null hypothesis is accepted for this. 


#Checking for Assumptions

#Random sample and independent observ.
#Multivariate normality of the numeric response variables
#Homogeneity of within-groups covariance matrices
#Linear relationships among response variables but no multicollinearity
#No extreme univariate or multivariate outliers

# Inspect multivariate plots of response variable for each Region
ggplot(statesdata, aes(x = SAT_avg, y = income_estimate)) +
  geom_point()+
  geom_density2d()+
  facet_wrap(~region)
```

![](Project2_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# Inspect homogeneity of (co)variances
#only 3 numeric columns shown due to limited space
covmats <- statesdata %>%
  group_by(region) %>%
  do(covs=cov(.[3:4]))
# Covariance matrices per species
for(i in 2:4){print(as.character(covmats$region[i])); print(covmats$covs[i])}
```

    ## [1] "ESC"
    ## [[1]]
    ##           pop       SATV
    ## pop  912206.7 1735.00000
    ## SATV   1735.0   31.58333
    ## 
    ## [1] "MA"
    ## [[1]]
    ##           pop         SATV
    ## pop  26635728 -17020.00000
    ## SATV   -17020     17.33333
    ## 
    ## [1] "MTN"
    ## [[1]]
    ##              pop       SATV
    ## pop  1361278.554 -4463.9464
    ## SATV   -4463.946   338.4107

*THe Manova test with p value 2.915e-12 (a&lt;0.05) indicates the null
hypothesis is rejected and the alternate hypothesis is correct for at
least 1 response variable at least one of these groups mean differs. 336
hypothesis tests were conducted, and the probability that you have made
at least one type I error is there fore 99.999996725 %, (1- 0.95^(336)).
To keep overall type I error rate at .05, significance level should be
0.05/336= 0.00014880952. After correcting for new significance level,
one-way ANOVA for each variable shows that population(pop) &
income\_estimate are above new significance level= 0.00014880952 and the
null hypothesis is accepted for this. This causes population(pop)
&income estimate to be above p value, and the means of the numeric
variables are same across the different regions. However, SATV, SATM,
percent(percentage of graduating high school students who take S.A.T),
dollars(state spending on public education in $1000 per student),
pay:(average teachers salary), rent\_estimate,&SAT\_avg are still
significant and alternate hypothesis is true at least one of the means
of these numeric variables is different across the different sites. *

*Checking for Assumptions: Random sample and independent observ.- MET,
Multivariate normality of the numeric response variables- Not Met due to
not seeing normal distribution of the numeric variables. Homogeneity of
within-groups covariance matrices- MET, Linear relationships among
response variables but no multicollinearity- MET, No extreme univariate
or multivariate outliers- MET. Since one of the assumptions was
violated, the PERMOVA test could be used with a higher p value than
MANOVA, as the test loses power and dropped assumptions of Manova and
harder to reject null hypothesis now. *

## Randomization Test

``` r
#Randomization test was conducted to see whether there was a difference in mean SAT_Avg between South Atlantic(SA) region (n = 9) and Mountain(MTN) region (n = 8). Assumptions for the independent t test were violated.
#check hypothesis
#H0: There is no difference in means SAT_Avg between South Atlantic & Mountain region
#Ha: There is a difference in means SAT_Avg between South Atlantic & Mountain region

statesdata %>% filter(region =="SA") %>% select(SAT_avg)
```

    ##   SAT_avg
    ## 1   451.5
    ## 2   425.0
    ## 3   442.0
    ## 4   422.0
    ## 5   454.0
    ## 6   420.5
    ## 7   417.0
    ## 8   447.5
    ## 9   466.5

``` r
statesdata %>% filter(region =="MTN") %>% select(SAT_avg)
```

    ##   SAT_avg
    ## 1   471.0
    ## 2   484.5
    ## 3   484.0
    ## 4   493.5
    ## 5   460.5
    ## 6   503.5
    ## 7   515.5
    ## 8   488.5

``` r
rm(score_SA, score_MTN)

score_SA <- c(451.5, 425.0, 442.0, 422.0, 454.0, 420.5, 417.0, 447.5, 466.5)
score_MTN <- c(471.0, 484.5, 484.0, 493.5, 460.5, 503.5, 515.5, 488.5)
#create dataframe
region_comp <- data.frame(region = c(rep("SA", 9), rep("MTN",8)), SAT_Avg =c(score_SA,score_MTN))
# Calculate the mean difference between the two regions
obs_diff<- region_comp  %>% group_by(region)  %>% summarise(mean_SAT_Avg = mean(SAT_Avg)) %>% summarise(obs_diff = diff(mean_SAT_Avg))
obs_diff
```

    ## # A tibble: 1 x 1
    ##   obs_diff
    ##      <dbl>
    ## 1    -49.2

``` r
## Repeat randomization many times 
# Create an empty vector to store the mean differences
set.seed(348)
mean_diff <- vector()
# Create many randomizations with a for loop 
for(i in 1:5000){ 
  temp <- data.frame(region = region_comp$region, SAT_Avg = sample(region_comp$SAT_Avg)) 
  
  mean_diff[i] <- temp  %>% group_by(region)  %>% summarise(mean_SAT_Avg = mean(SAT_Avg)) %>% summarise(mean_diff = diff(mean_SAT_Avg))  %>%pull
  
}

# Represent the distribution of the mean differences with a vertical line showing the true difference
{hist(mean_diff, main="Distribution of the mean differences"); abline(v = 49.181, col="red")}
```

![](Project2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# Calculate the corresponding two-sided p-value
mean(mean_diff > -obs_diff | mean_diff < obs_diff)
```

    ## [1] 0

``` r
# Compare to a Welch's t-test
t.test(data = region_comp, SAT_Avg ~ region)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  SAT_Avg by region
    ## t = 5.7724, df = 14.852, p-value = 3.829e-05
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  31.00500 67.35611
    ## sample estimates:
    ## mean in group MTN  mean in group SA 
    ##          487.6250          438.4444

*Results: The p value is very close to zero (3.829e-05), which is less
than 0.05, and this means alternate hypothesis is accepted there is a
true difference in means SAT\_Avg between South Atlantic & Mountain
region. Histogram confirms this by representing the distribution of the
mean differences with a vertical line showing the true difference(49.181
in SAT\_avg score). The Mountain Region SAT\_avg score is signifcantly
higher. *

## Linear Regression Model

``` r
library(ggplot2)
#fit regression model with categorical variable region and continuous variable pay, include interaction term with *
fit <- lm(dollars ~ pay + region *pay, data = statesdata)
summary(fit)
```

    ## 
    ## Call:
    ## lm(formula = dollars ~ pay + region * pay, data = statesdata)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.39973 -0.27572 -0.02886  0.19577  1.26621 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)    6.62715    4.29478   1.543   0.1323  
    ## pay           -0.03657    0.12679  -0.288   0.7748  
    ## regionESC     -7.72233    6.26932  -1.232   0.2267  
    ## regionMA      -8.51258    7.10704  -1.198   0.2395  
    ## regionMTN     -6.57240    4.90677  -1.339   0.1896  
    ## regionNE      -5.58860    4.65846  -1.200   0.2388  
    ## regionPAC     -7.88856    4.86791  -1.621   0.1146  
    ## regionSA      -8.06419    4.57080  -1.764   0.0869 .
    ## regionWNC     -6.35119    4.68636  -1.355   0.1845  
    ## regionWSC     -7.04878    5.98142  -1.178   0.2470  
    ## pay:regionESC  0.21664    0.21107   1.026   0.3122  
    ## pay:regionMA   0.29389    0.19348   1.519   0.1383  
    ## pay:regionMTN  0.18945    0.15230   1.244   0.2223  
    ## pay:regionNE   0.19269    0.13705   1.406   0.1691  
    ## pay:regionPAC  0.22855    0.14181   1.612   0.1166  
    ## pay:regionSA   0.25648    0.13585   1.888   0.0679 .
    ## pay:regionWNC  0.19206    0.14430   1.331   0.1923  
    ## pay:regionWSC  0.20501    0.20762   0.987   0.3306  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6314 on 33 degrees of freedom
    ## Multiple R-squared:  0.8611, Adjusted R-squared:  0.7895 
    ## F-statistic: 12.03 on 17 and 33 DF,  p-value: 1.297e-09

``` r
# Center the data around the means for pay 
statesdata$pay_c <- statesdata$pay - mean(statesdata$pay, na.rm = TRUE)

#regression model centered around pay &include an interaction term in the regression model with centered predictor of pay
fit_c <- lm(dollars ~ region + statesdata$pay_c * region, data = statesdata)
summary(fit_c)
```

    ## 
    ## Call:
    ## lm(formula = dollars ~ region + statesdata$pay_c * region, data = statesdata)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.39973 -0.27572 -0.02886  0.19577  1.26621 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 5.49555    0.45948  11.960 1.53e-13 ***
    ## regionESC                  -1.01911    0.86782  -1.174   0.2487    
    ## regionMA                    0.58084    1.27228   0.457   0.6510    
    ## regionMTN                  -0.71067    0.56792  -1.251   0.2196    
    ## regionNE                    0.37353    0.55561   0.672   0.5061    
    ## regionPAC                  -0.81694    0.62138  -1.315   0.1977    
    ## regionSA                   -0.12853    0.50703  -0.253   0.8015    
    ## regionWNC                  -0.40861    0.58463  -0.699   0.4895    
    ## regionWSC                  -0.70543    1.08914  -0.648   0.5217    
    ## statesdata$pay_c           -0.03657    0.12679  -0.288   0.7748    
    ## regionESC:statesdata$pay_c  0.21664    0.21107   1.026   0.3122    
    ## regionMA:statesdata$pay_c   0.29389    0.19348   1.519   0.1383    
    ## regionMTN:statesdata$pay_c  0.18945    0.15230   1.244   0.2223    
    ## regionNE:statesdata$pay_c   0.19269    0.13705   1.406   0.1691    
    ## regionPAC:statesdata$pay_c  0.22855    0.14181   1.612   0.1166    
    ## regionSA:statesdata$pay_c   0.25648    0.13585   1.888   0.0679 .  
    ## regionWNC:statesdata$pay_c  0.19206    0.14430   1.331   0.1923    
    ## regionWSC:statesdata$pay_c  0.20501    0.20762   0.987   0.3306    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6314 on 33 degrees of freedom
    ## Multiple R-squared:  0.8611, Adjusted R-squared:  0.7895 
    ## F-statistic: 12.03 on 17 and 33 DF,  p-value: 1.297e-09

``` r
# Visualize the relationships between the 2 variables on the response
#used ggplot to put x axis pay, y axis dollars, and color by region
ggplot(statesdata, aes(x = pay_c, y = dollars, color = region)) +
  geom_smooth(method=lm) +
  ggtitle("Pay(average teachers salary in dollars) vs. Dollars (state spending on public education in $1000 per student) for Regions in US")
```

![](Project2_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#Interpreted coefficient estimates in context &proportion of the variation in the response below

#Check Assumptions


# Residuals vs Fitted values plot
# Residuals against fitted values plot to check for any problematic patterns (nonlinear, equal variance)
plot(fit_c, which = 1)
```

![](Project2_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
# Q-Q plot for the residuals
plot(fit_c, which = 2)
```

![](Project2_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
library(sandwich)
library(lmtest)
#original Standard error bars
# Breusch-Pagan test for homoscedasticity
bptest(fit_c)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  fit_c
    ## BP = 29.252, df = 17, p-value = 0.03227

``` r
# Uncorrected Standard Errors for the model
summary(fit_c)$coef
```

    ##                               Estimate Std. Error    t value     Pr(>|t|)
    ## (Intercept)                 5.49555455  0.4594765 11.9604683 1.527632e-13
    ## regionESC                  -1.01911128  0.8678210 -1.1743335 2.486666e-01
    ## regionMA                    0.58084461  1.2722808  0.4565381 6.509939e-01
    ## regionMTN                  -0.71067220  0.5679240 -1.2513508 2.196050e-01
    ## regionNE                    0.37352889  0.5556086  0.6722878 5.060807e-01
    ## regionPAC                  -0.81694048  0.6213828 -1.3147137 1.976702e-01
    ## regionSA                   -0.12852584  0.5070317 -0.2534868 8.014647e-01
    ## regionWNC                  -0.40860567  0.5846308 -0.6989123 4.895037e-01
    ## regionWSC                  -0.70542893  1.0891447 -0.6476907 5.216659e-01
    ## statesdata$pay_c           -0.03657258  0.1267896 -0.2884510 7.748056e-01
    ## regionESC:statesdata$pay_c  0.21664401  0.2110743  1.0263875 3.121740e-01
    ## regionMA:statesdata$pay_c   0.29389401  0.1934765  1.5190164 1.382821e-01
    ## regionMTN:statesdata$pay_c  0.18944758  0.1522983  1.2439242 2.222913e-01
    ## regionNE:statesdata$pay_c   0.19269249  0.1370457  1.4060453 1.690573e-01
    ## regionPAC:statesdata$pay_c  0.22855031  0.1418124  1.6116383 1.165653e-01
    ## regionSA:statesdata$pay_c   0.25647576  0.1358490  1.8879474 6.785222e-02
    ## regionWNC:statesdata$pay_c  0.19206068  0.1442973  1.3310065 1.923102e-01
    ## regionWSC:statesdata$pay_c  0.20501326  0.2076160  0.9874639 3.306006e-01

``` r
#WS15 -robust standard errors for the model
coeftest(fit_c, vcov = vcovHC(fit_c))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 5.495555   0.348921 15.7501  < 2e-16 ***
    ## regionESC                  -1.019111   0.807152 -1.2626  0.21558    
    ## regionMA                    0.580845   7.602948  0.0764  0.93956    
    ## regionMTN                  -0.710672   0.460060 -1.5447  0.13195    
    ## regionNE                    0.373529   0.462623  0.8074  0.42521    
    ## regionPAC                  -0.816940   0.665915 -1.2268  0.22858    
    ## regionSA                   -0.128526   0.423697 -0.3033  0.76353    
    ## regionWNC                  -0.408606   0.389843 -1.0481  0.30220    
    ## regionWSC                  -0.705429   0.505918 -1.3944  0.17253    
    ## statesdata$pay_c           -0.036573   0.064946 -0.5631  0.57716    
    ## regionESC:statesdata$pay_c  0.216644   0.218776  0.9903  0.32925    
    ## regionMA:statesdata$pay_c   0.293894   1.140488  0.2577  0.79825    
    ## regionMTN:statesdata$pay_c  0.189448   0.130006  1.4572  0.15451    
    ## regionNE:statesdata$pay_c   0.192692   0.081489  2.3647  0.02408 *  
    ## regionPAC:statesdata$pay_c  0.228550   0.254978  0.8964  0.37656    
    ## regionSA:statesdata$pay_c   0.256476   0.118134  2.1711  0.03722 *  
    ## regionWNC:statesdata$pay_c  0.192061   0.070964  2.7065  0.01068 *  
    ## regionWSC:statesdata$pay_c  0.205013   0.091908  2.2306  0.03262 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
library(dplyr)

#compute bootstrapped standard errors
# estimating coefficients of SEs
# Use the function replicate to repeat the process 
set.seed(348)
samp_SEs <- replicate(5000, {
# Bootstrap your data (resample observations)
boot_data <- sample_frac(statesdata, replace = TRUE)
# Fit regression model
fitboot <- lm(dollars ~ region + statesdata$pay_c * region, data = boot_data)
predict(fitboot,interval = "confidence")[1]
})
head(samp_SEs)
```

    ## [1] 5.520971 7.412371 5.841265 3.791409 4.076426 4.099565

``` r
#used unlist to make numerical and then calculated summarize all to get standard error of 1.402239 dollars (state spending on public education in $1000 per student)
standard_errors<- head(unlist(samp_SEs))
standard_errors %>% as.data.frame %>% summarize_all(sd)
```

    ##          .
    ## 1 1.402239

``` r
#quantiles for the 2.5% and 97.5% percentiles
quantile(samp_SEs,.025)
```

    ##     2.5% 
    ## 3.546725

``` r
quantile(samp_SEs,.975)
```

    ## 97.5% 
    ##   8.5

*In the interaction model, There was no significant effect of a
particular variable(pay\_c or regions), while controlling for the
regions or the pay centered; this means the slope (B1) is not
significant (reject alternate hypothesis(B1 not equal to 0) is false)
and the null hypothesis is accepted(B1=0). there is no significant
interaction between pay\_c(average teachers salary in dollars) and a
particular region. Regression model is also not significant relationship
between those who are in particular region or not and pay\_c(average
teachers salary in dollars) on Dollars (state spending on public
education in $1000 per student).*

*The model accounts for 86.11% of the variance in Y explained by X
(R^2=0.8611).As shown the residuals vs fitted values plot, the linear
assumption is met since there is no pattern and with 1 or 2 possible
outliers, however there is unequal variance, as seen by the funnel shape
of the residuals, there assumption of equal variance is not met. As
shown by the QQ plot, the normality assumption is not met as the
standardized residuals and theoretical quantiles is not linear.*

*After computing original standard error bars, robust standard error
bars are calculated, and p-values for most predictors are now lesser.
Futhermore, The interaction between
regionNE:statesdata*p**a**y*<sub>*c*</sub>, *r**e**g**i**o**n**S**A* : *s**t**a**t**e**s**d**a**t**a*pay\_c,
regionWNC:statesdata*p**a**y*<sub>*c*</sub>, *r**e**g**i**o**n**W**S**C* : *s**t**a**t**e**s**d**a**t**a*pay\_c
are now significant with slope (B1),(0.192692, 0.256476, 0.192061,
0.205013 ) , respectively, while controlling for other regions and
pay\_c,and the alternate hypothesis is true for only these: there is a
significant relationship between those specific regions and
pay\_c(average teachers salary in dollars). Bootstrapped standard error
bars((standard deviation of the sampling distribution) calculated also
show 1.402239 dollars (state spending on public education in $1000 per
student). The bootstrap 95% confidence interval for the mean dollars is
between 3.55 and 8.5; this is wider confidence interval of SEs since
there are more samples taken, and higher p value than robust SE. *

## Logistic Regression

``` r
#didnt have binary categorical variable so split the 9 initial regions into a variable (North or SOuth US) called Continental_Div
# North signifies 1: regions: ENC, MA, NE, PAC, WNC
# South signifies 0: ESC, MTN, SA, WSC
statesdata2 <- statesdata %>%
  mutate(Continental_Div = ifelse((region == "ENC" | region == "MA" | region == "NE" | region == "PAC" | region == "WNC"), 1, 0))

# Let's look at the definion of odds = p/(1-p)
odds <- function(p)p/(1-p)

# Simulate probability values (varying between 0 and 1 by 0.1)
p <-seq(0, 1, by = .1)

# Create a dataframe with these probabilities and corresponding odds
cbind(p, odds = odds(p)) %>%
  round(4) %>%
  as.data.frame %>%
  # Represent the relationship between probabilities and odds
  ggplot() +
  stat_function(aes(p), fun = odds, geom="line") + 
  ylab("odds(p)") + xlab("p")
```

![](Project2_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# We also need to define the logit link function (logarithm of odds)
logit <- function(p) log(odds(p))
cbind(p, odds = odds(p), logit = logit(p)) %>% 
  round(4) %>%
  as.data.frame %>%
  # Represent the relationship between probabilities and odds
  ggplot() +
  stat_function(aes(p), fun = logit, geom="line") + 
  ylab("logit(p)") + xlab("p")
```

![](Project2_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
# Logistic model
logistic <- function(x){exp(x) / (1 + exp(x))}
x <- seq(-5,5, by = .1)
cbind(x, model = logistic(x)) %>%
  as.data.frame %>%
  ggplot() + 
  stat_function(aes(x), fun = logistic, geom="line") +
  xlab("x") + ylab("Pr(y=1)")
```

![](Project2_files/figure-gfm/unnamed-chunk-5-3.png)<!-- -->

``` r
# Fit a new regression model by predicting Continental_Div from 2 explanatory variables (income_estimate + SAT_avg)
fit1 <- glm(Continental_Div ~ income_estimate + SAT_avg, data = statesdata2, family = binomial(link="logit"))
summary(fit1)
```

    ## 
    ## Call:
    ## glm(formula = Continental_Div ~ income_estimate + SAT_avg, family = binomial(link = "logit"), 
    ##     data = statesdata2)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3146  -0.9483   0.6691   0.9514   1.5141  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)     -1.115e+01  5.854e+00  -1.904   0.0568 .
    ## income_estimate  2.169e-04  9.125e-05   2.376   0.0175 *
    ## SAT_avg          1.033e-02  9.881e-03   1.045   0.2961  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 70.681  on 50  degrees of freedom
    ## Residual deviance: 63.574  on 48  degrees of freedom
    ## AIC: 69.574
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# Interpret the coefficients by considering the odds (inverse of log(odds))
exp(coef(fit1))
```

    ##     (Intercept) income_estimate         SAT_avg 
    ##    1.439716e-05    1.000217e+00    1.010379e+00

``` r
# Add predicted probabilities to the dataset
statesdata2$prob <- predict(fit1, type = "response")

# Predicted outcome is based on the probability of being from the North
# if the probability is greater than 0.5, its found in the North
statesdata2$predicted <- ifelse(statesdata2$prob > .5, "1", "0") 

# Confusion matrix
#NOTE: Truth (1 is North- being from the North part of the US(Continental Divide), 0 is South- being from the South part of the US(Continental Divide))
table(truth = statesdata2$Continental_Div, prediction = statesdata2$predicted)
```

    ##      prediction
    ## truth  0  1
    ##     0 18  7
    ##     1 10 16

``` r
# Accuracy (correctly classified cases)
(16 + 18)/51 
```

    ## [1] 0.6666667

``` r
# Sensitivity (True Positive Rate, TPR)
16/26
```

    ## [1] 0.6153846

``` r
# Specificity (True Negative Rate, TNR)
18/25 
```

    ## [1] 0.72

``` r
# Precision (Positive Predictive Value, PPV)
16/23
```

    ## [1] 0.6956522

``` r
#graph to plot density of log-odds (logit) by your binary Continental_Div variable
# Save the predicted log-odds in the dataset
statesdata2$logit <- predict(fit1)

# Compare to the outcome in the dataset with a density plot
ggplot(statesdata2, aes(logit, fill = as.factor(Continental_Div))) +
  geom_density(alpha = .3) +
  geom_vline(xintercept = 0, lty = 2) +
  labs(fill = "Continental_Div:(1 is North, 0 is South)")+
  ggtitle("Plot density of log-odds (logit) by your binary Continental_Div variable")
```

![](Project2_files/figure-gfm/unnamed-chunk-5-4.png)<!-- -->

``` r
library(plotROC) 

# Plot ROC depending on values of y and its probabilities displaying some cutoff values
ROCplot <- ggplot(statesdata2) + 
  geom_roc(aes(d = Continental_Div , m = prob), cutoffs.at = list(0.1, 0.5, 0.9))
ROCplot
```

![](Project2_files/figure-gfm/unnamed-chunk-5-5.png)<!-- -->

``` r
# Calculate the area under the curve still using the library plotROC with function calc_auc
calc_auc(ROCplot)
```

    ##   PANEL group       AUC
    ## 1     1    -1 0.7630769

``` r
#interpretation below
```

*The effect of income\_estimate on Continental\_Div is significant(p
value=0.0175 ); Through interpreting the coefficient of odds, Every
one-unit increase in income\_estimate multiplies the odds of being from
the North part of the US(Continental Divide) by 1.000217 (i.e., the odds
of being from the North part of the US(Continental Divide) increase by
100% for every additional unit of income\_estimate). The effect of
SAT\_avg on Continental\_Div is not significant(p value=0.2961). *

*Accuracy( proportion of correctly classified cases) is 66.67%,
Sensitivity (proportion of true positive) is 61.54%,
Specificity(proportion of true negative) is 72%, Precision(proportion of
true positive prediction) is 69.57%. Ultimately these measures aren’t
very high, which shows the model is satisfactory, but can fail in around
1/3 of cases.*

*THE AUC indicates the model is only fair at predicting
Continental\_Div(whether someone is from the North or South part of the
US) from income\_estimate and SAT\_avg. A randomly selected observation
from the positive group has a test value larger than for a randomly
chosen observation from the negative group 76.3% percent of the time.*
