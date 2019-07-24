---
    title: "Statistical Inference - Course Project - Assignment 1"
output: html_notebook
---
    # Part I - compare exponential distribution and the Central Limit Theorem 
    
    ## Introduction
    In this first part of the assignment within the "Statistic Inference" Coursera course a dataset will be simulated and compared to its theoretical mean and variance. The simulated distribution will be compared graphically and by t.test. Closer instructions and guide lines can be found here:
    [assignment 1 - statistical inference - coursera](https://www.coursera.org/learn/statistical-inference/peer/3k8j5/statistical-inference-course-project)

## simulation of the data
```{r}
set.seed(1)
mns_1000 <- c()
for(i in 1:1000){
    mns <- mean(rexp(n = 40,rate = 0.2))
    mns_1000 <- c(mns_1000, mns)
}

simMn <- mean(mns_1000)
varMn <- var(mns_1000)
```

## theoretical mean
```{r}
theoMn <- 1/0.2
theoVar <- (1/0.2)^2 /40
```

## comparison of simulated and theoretical 

```{r}
library(ggplot2)
gplot <- data.frame(mns_1000)
m <- ggplot(gplot, aes(x = mns_1000)) + xlab("simulated mean")
m <- m + geom_histogram(aes(y=..density..), colour = "black", fill = "blue")
m + geom_density(colour = "red", size = 1) + geom_vline(xintercept = theoMn, color = "red", size =1) + geom_vline(xintercept = simMn, color = "black", size =1)
```

## statistical comparison between the theoretical and the simulated values

### 95% conf interv l
```{r}
simConfIntervl <- round( simMn + c(-1,+1) * 1.96 * sd(mns_1000)/sqrt(40),3)
theoretiConfIntervl <- round(theoMn + c(-1,+1) * 1.96 * theoVar/sqrt(40),3)

```

### norml distribution

```{r}
qqline(mns_1000)
qqnorm(mns_1000)
```


# part II - analysing the tooth growth dataset 

## introduction
Within this part of the assignment the data set "The Effect of Vitamine C on Tooth Growth in Guinea Pigs" is analyzed. More explanatory material can be found here:  
    [r data set - follow this link](https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/ToothGrowth.html)
In short guinea pigs get different doses (0.5, 1, 2 mg/day) of either orange juice (OJ) or vitamine C (VC) and the length of their odontoblast is compared. 

### loading of the data
```{r}
data(ToothGrowth)
```

## exploration of the dataset
```{r}
head(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)
table(ToothGrowth$supp, ToothGrowth$dose)
``` 

## plotting of the data
```{r}
library("dplyr")
library("ggplot2")
data("ToothGrowth")


df_means_OJ <- ToothGrowth %>% filter(supp == "OJ") %>% group_by(dose) %>% summarise(value=mean(len)) %>% mutate(supp = "OJ")
df_means_VC <- ToothGrowth %>% filter(supp == "VC") %>% group_by(dose) %>% summarise(value=mean(len)) %>% mutate(supp ="VC")

df_merged <- rbind(df_means_OJ, df_means_VC)

ggplot(data = ToothGrowth , aes(x = dose,y = len, group = dose, col = dose)) + 
    geom_boxplot() + facet_grid(.~supp)
```
This plot already shows that there is a dose response for the length of odontoblasts in both groups. If those are statistically relevant needs to be validated.

## statistic comparison between supplements and doses

The boxplot indicate that there are differences between the median of the different doses within the different treatment groups. However, it is not clear if 
- the differecens which we see are significant 
- the differences between the different treatments are significant

Therefore, the next lines will analyse exactly those differences:
    
    compare the significance between the doses within the orange juice group
```{r}
OJ_05 <- ToothGrowth %>% filter(supp == "OJ") %>% filter(dose == 0.5) %>% select(len)
OJ_1 <- ToothGrowth %>% filter(supp == "OJ") %>% filter(dose == 1) %>% select(len)
OJ_2 <- ToothGrowth %>% filter(supp == "OJ") %>% filter(dose == 2) %>% select(len)
```

```{r t-test}
t.test(OJ_05, OJ_1, paired = F, var.equal = F)
t.test(OJ_1, OJ_2, paired = F, var.equal = F)
```
Both comparisons show that the comparison to the next higher dose is significant and that we can reject the NULL-Hypothesis that with the treatment there is no difference. We can further say that it is likely that with the higher dosing of orange juice in this given experiment the length of the odontoblasts are increased.

Next we do the same analysis with for the vitamine C group:
    ```{r}
VC_05 <- ToothGrowth %>% filter(supp == "VC") %>% filter(dose == 0.5) %>% select(len)
VC_1 <- ToothGrowth %>% filter(supp == "VC") %>% filter(dose == 1) %>% select(len)
VC_2 <- ToothGrowth %>% filter(supp == "VC") %>% filter(dose == 2) %>% select(len)
```

```{r t-test}
t.test(VC_05, VC_1, paired = F, var.equal = F)
t.test(VC_1, VC_2, paired = F, var.equal = F)
```
We can conclude that with the higher dosing of vitamine C there is a significant increase of the tooth length, similarly to the orange juice supplemented group.

Finally, the treatments will be compared if orange juice has a similar effect like vitamine c supplement or if there is a difference

```{r}
t.test(VC_05, OJ_05, paired = F, var.equal = F)
t.test(VC_1, OJ_1, paired = F, var.equal = F)
t.test(VC_2, OJ_2, paired = F, var.equal = F)

``` 

With these comparison we can conclude that there are differences between the treatments of either vitamine C or orange juice. The differences are between the dosing of 0.5 and 1. However, there are no differences when dosing both with 2mg/ day on the growth of the odontoblasts of guinea pigs.
It would be however interesting to compare those groups to non treated groups to see how the base level is. Further, it might be interesting to see if those treatments can be compared to human teeth grows in some way.



