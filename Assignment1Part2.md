Statisic Inference - Assignment 1 part II
================
Simon Baumgart
May 26, 2019

Part II - Analysis of the tooth growth dataset
============================================

Introduction
------------

The data set "ToothGrowth" is analyzed within this document to explore the effect of vitamine C on the growth of the odontoblasts of guinea pigs. Briefly, guinea pigs getting different doses (0.5, 1, 2 mg/day) of either orange juice (OJ) or vitamine C (VC) over the same timeframe. After the treatment the length of the odontoblasts is compared between each group. 

To get the dataset follow this link:
[ToothGrowth dataset](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html)



Structure of the ToothGrowth dataset
--------------------------
``` r
library("dplyr")
library("ggplot2")
```

``` r
data(ToothGrowth)
head(ToothGrowth)
```

    ##    len supp dose
    ## 1  4.2   VC  0.5
    ## 2 11.5   VC  0.5
    ## 3  7.3   VC  0.5
    ## 4  5.8   VC  0.5
    ## 5  6.4   VC  0.5
    ## 6 10.0   VC  0.5

``` r
str(ToothGrowth)
```

    ## 'data.frame':    60 obs. of  3 variables:
    ##  $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
    ##  $ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...

``` r
summary(ToothGrowth)
```

    ##       len        supp         dose      
    ##  Min.   : 4.20   OJ:30   Min.   :0.500  
    ##  1st Qu.:13.07   VC:30   1st Qu.:0.500  
    ##  Median :19.25           Median :1.000  
    ##  Mean   :18.81           Mean   :1.167  
    ##  3rd Qu.:25.27           3rd Qu.:2.000  
    ##  Max.   :33.90           Max.   :2.000

``` r
table(ToothGrowth$supp, ToothGrowth$dose)
```

    ##     
    ##      0.5  1  2
    ##   OJ  10 10 10
    ##   VC  10 10 10

The dataset has __60 observations of 3 variables__ with two variables being numeric and one being a factor variable


Data Plotting 
--------------------

``` r
ggplot(data = ToothGrowth , aes(x = dose,y = len, group = dose, col = dose)) + 
    geom_boxplot() + facet_grid(.~supp)  + ggtitle("Tooth Growth by length, supplement and dose") +  xlab("Dose (mg/day)") + ylab("Length")
```

![](Assignment1Part2_files/figure-markdown_github/unnamed-chunk-3-1.png)

This plot shows a dose response for vitamine C and orange juice on the length of odontoblasts. Next, these effects are tested if their distribution is statistically different.

Statistic comparison between supplements and doses
--------------------------------------------------

First, I compare the significance between the doses within group which was supplemented with orange juice

``` r
OJ_05 <- ToothGrowth %>% filter(supp == "OJ") %>% filter(dose == 0.5) %>% select(len)
OJ_1 <- ToothGrowth %>% filter(supp == "OJ") %>% filter(dose == 1) %>% select(len)
OJ_2 <- ToothGrowth %>% filter(supp == "OJ") %>% filter(dose == 2) %>% select(len)
```

``` r
t.test(OJ_05, OJ_1, paired = F, var.equal = F)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  OJ_05 and OJ_1
    ## t = -5.0486, df = 17.698, p-value = 8.785e-05
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -13.415634  -5.524366
    ## sample estimates:
    ## mean of x mean of y 
    ##     13.23     22.70

``` r
t.test(OJ_1, OJ_2, paired = F, var.equal = F)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  OJ_1 and OJ_2
    ## t = -2.2478, df = 15.842, p-value = 0.0392
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -6.5314425 -0.1885575
    ## sample estimates:
    ## mean of x mean of y 
    ##     22.70     26.06

The comparisons show that there is a difference on the length of the odontoblasts with higher doses of orange juice with a p-value < 0.05. If this level of confidence is enough we can reject the NULL-Hypothesis that the distribution of the treatment values are same and that there is no difference. 

In the following the same analysis is performed for the vitamine C group:
``` r
VC_05 <- ToothGrowth %>% filter(supp == "VC") %>% filter(dose == 0.5) %>% select(len)
VC_1 <- ToothGrowth %>% filter(supp == "VC") %>% filter(dose == 1) %>% select(len)
VC_2 <- ToothGrowth %>% filter(supp == "VC") %>% filter(dose == 2) %>% select(len)
```

``` r
t.test(VC_05, VC_1, paired = F, var.equal = F)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  VC_05 and VC_1
    ## t = -7.4634, df = 17.862, p-value = 6.811e-07
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -11.265712  -6.314288
    ## sample estimates:
    ## mean of x mean of y 
    ##      7.98     16.77

``` r
t.test(VC_1, VC_2, paired = F, var.equal = F)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  VC_1 and VC_2
    ## t = -5.4698, df = 13.6, p-value = 9.156e-05
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -13.054267  -5.685733
    ## sample estimates:
    ## mean of x mean of y 
    ##     16.77     26.14

We can conclude that with the higher dosing of vitamine C there is a significant increase of the tooth length, similarly to the orange juice supplemented group.

Finally, the treatments will be compared if orange juice has a similar effect as the vitamine c supplement or if there is a difference between the treatments

``` r
t.test(VC_05, OJ_05, paired = F, var.equal = F)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  VC_05 and OJ_05
    ## t = -3.1697, df = 14.969, p-value = 0.006359
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -8.780943 -1.719057
    ## sample estimates:
    ## mean of x mean of y 
    ##      7.98     13.23

``` r
t.test(VC_1, OJ_1, paired = F, var.equal = F)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  VC_1 and OJ_1
    ## t = -4.0328, df = 15.358, p-value = 0.001038
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -9.057852 -2.802148
    ## sample estimates:
    ## mean of x mean of y 
    ##     16.77     22.70

``` r
t.test(VC_2, OJ_2, paired = F, var.equal = F)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  VC_2 and OJ_2
    ## t = 0.046136, df = 14.04, p-value = 0.9639
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -3.63807  3.79807
    ## sample estimates:
    ## mean of x mean of y 
    ##     26.14     26.06

We observe differences on the length of odontoblast when supplementing with vitamine C or orange juice. The differences were for doses of 0.5 and 1 mg/day with vitamine C having a greater effect. However, for the lower and the highest dosis there was no different effect observed. This data suggest that dosing of 2mg/ day with either orange juice or vitamine D has the best effect on the growth of odontoblast on guinea pigs. 
It would be interesting to compare those groups to non-treated group to see how they compare to untreated control and how the base level is. Further, it might be interesting to see if those treatments can be compared to the growth of human teeth.
