---
title: "Homework_ANOVA"
author: "Shakir Suleimanov"
date: "2022-12-05"
output:
  html_document:
    keep_md: TRUE
---




```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
## ✔ readr   2.1.3      ✔ forcats 0.5.2 
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(multcomp)
```

```
## Loading required package: mvtnorm
## Loading required package: survival
## Loading required package: TH.data
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## 
## Attaching package: 'TH.data'
## 
## The following object is masked from 'package:MASS':
## 
##     geyser
```


```r
soccer_general <- read.csv("soccer.csv", sep=";")[, 2:6] %>% 
    mutate(Position = as.factor(Position), 
    Nationality = as.factor(Nationality), 
    Age = as.numeric(Age), 
    Height = as.numeric(Height)
) %>% 
filter(Nationality %in% c("Spanish", "Italian", "German", "English", "Argentinian")) 

set.seed(1) 
```


```r
soccer_wrk <- soccer_general[sample(1:nrow(soccer_general), 150), ] %>% 
    mutate(Nationality = factor(Nationality))
```


ANOVA

Проводится для того, чтобы понять, есть ли разница между какими-либо исследуемыми группами. По результатам теста мы видим, что разница между группами есть, но мы не знаем, между какими конкретно.

```r
aov_position <- aov(Height ~ Position, data = soccer_wrk)
summary(aov_position)
```

```
##              Df Sum Sq Mean Sq F value   Pr(>F)    
## Position      3   1345   448.3   12.88 1.64e-07 ***
## Residuals   146   5082    34.8                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

Post-hoc.

Для того, чтобы понять, между какими конкретно группами есть разница в росте, мы проводит post-hoc анализ. Для начала мы проведем попарные сравнения между всеми группами.

```r
post_hoc_position <- aov_position %>%
  glht(linfct = mcp(Position = "Tukey")) 
summary(post_hoc_position)
```

```
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: aov(formula = Height ~ Position, data = soccer_wrk)
## 
## Linear Hypotheses:
##                              Estimate Std. Error t value Pr(>|t|)    
## Forward - Defender == 0        -1.718      1.339  -1.283  0.57138    
## Goalkeeper - Defender == 0      5.258      1.513   3.474  0.00366 ** 
## Midfielder - Defender == 0     -4.218      1.193  -3.535  0.00297 ** 
## Goalkeeper - Forward == 0       6.976      1.679   4.156  < 0.001 ***
## Midfielder - Forward == 0      -2.500      1.397  -1.790  0.27918    
## Midfielder - Goalkeeper == 0   -9.476      1.565  -6.056  < 0.001 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## (Adjusted p values reported -- single-step method)
```

```r
par(mar = c(5, 10, 4, 2)+0.1)
plot(post_hoc_position)
```

![](Homework_ANOVA_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Мы получили статистически значимую разницу в четырех попарных сравнениях.

Построим доверительные интервалы для разницы между группами. Так мы сможем чуть лучше понять силу различий между группами, тогда как при проведении попарных сравнений с p-value мы этого получить не смогли. 


```r
post_hoc_position %>% confint()
```

```
## 
## 	 Simultaneous Confidence Intervals
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: aov(formula = Height ~ Position, data = soccer_wrk)
## 
## Quantile = 2.5901
## 95% family-wise confidence level
##  
## 
## Linear Hypotheses:
##                              Estimate lwr      upr     
## Forward - Defender == 0       -1.7182  -5.1866   1.7503
## Goalkeeper - Defender == 0     5.2580   1.3380   9.1780
## Midfielder - Defender == 0    -4.2182  -7.3090  -1.1273
## Goalkeeper - Forward == 0      6.9762   2.6283  11.3241
## Midfielder - Forward == 0     -2.5000  -6.1182   1.1182
## Midfielder - Goalkeeper == 0  -9.4762 -13.5293  -5.4231
```
Мы получили схожие результаты, что логично:)


Теперь проанализируем взаимосвязь роста и страны, за которую играет футболист.

Сначала проведем ANOVA.

```r
aov_nationality <- aov(Height ~ Nationality, data = soccer_wrk)
summary(aov_nationality)
```

```
##              Df Sum Sq Mean Sq F value Pr(>F)
## Nationality   4    164   41.03    0.95  0.437
## Residuals   145   6263   43.19
```
Уже на этом этапе мы видим, что разницы между группами вообще нет. 

В общем виде дальше нет смысла делать Post-hoc анализ. Но для закрепления материала мы его сделаем.

```r
post_hoc_nationality <- aov_nationality %>%
  glht(linfct = mcp(Nationality = "Tukey")) 

summary(post_hoc_nationality)
```

```
## 
## 	 Simultaneous Tests for General Linear Hypotheses
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: aov(formula = Height ~ Nationality, data = soccer_wrk)
## 
## Linear Hypotheses:
##                            Estimate Std. Error t value Pr(>|t|)
## English - Argentinian == 0  0.71111    2.11643   0.336    0.997
## German - Argentinian == 0  -1.14667    2.14645  -0.534    0.983
## Italian - Argentinian == 0  0.08485    2.04656   0.041    1.000
## Spanish - Argentinian == 0 -1.94667    1.93478  -1.006    0.849
## German - English == 0      -1.85778    1.82413  -1.018    0.844
## Italian - English == 0     -0.62626    1.70546  -0.367    0.996
## Spanish - English == 0     -2.65778    1.56958  -1.693    0.436
## Italian - German == 0       1.23152    1.74258   0.707    0.954
## Spanish - German == 0      -0.80000    1.60984  -0.497    0.987
## Spanish - Italian == 0     -2.03152    1.47402  -1.378    0.638
## (Adjusted p values reported -- single-step method)
```

```r
post_hoc_nationality %>% confint()
```

```
## 
## 	 Simultaneous Confidence Intervals
## 
## Multiple Comparisons of Means: Tukey Contrasts
## 
## 
## Fit: aov(formula = Height ~ Nationality, data = soccer_wrk)
## 
## Quantile = 2.7526
## 95% family-wise confidence level
##  
## 
## Linear Hypotheses:
##                            Estimate lwr      upr     
## English - Argentinian == 0  0.71111 -5.11466  6.53688
## German - Argentinian == 0  -1.14667 -7.05508  4.76174
## Italian - Argentinian == 0  0.08485 -5.54860  5.71830
## Spanish - Argentinian == 0 -1.94667 -7.27244  3.37910
## German - English == 0      -1.85778 -6.87896  3.16341
## Italian - English == 0     -0.62626 -5.32080  4.06828
## Spanish - English == 0     -2.65778 -6.97829  1.66273
## Italian - German == 0       1.23152 -3.56519  6.02822
## Spanish - German == 0      -0.80000 -5.23131  3.63131
## Spanish - Italian == 0     -2.03152 -6.08897  2.02594
```
В принципе, такого результата мы и ожидали. 

