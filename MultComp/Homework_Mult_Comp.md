---
title: "Homework_Mult_Comp"
author: "Shakir Suleimanov"
date: "2022-11-30"
output:
  html_document:
    keep_md: TRUE
---



Установим библиотеки


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
library(ggpubr)
```

Загрузим данные


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

Создадим выборку


```r
soccer_wrk <- soccer_general[sample(1:nrow(soccer_general), 150), ] %>% 
    mutate(Nationality = factor(Nationality))
```

Посмотрим на распределение изучаемого признака в нашей выборке и "генеральной" совокупности


```r
ggplot_wrk <- ggplot(data = soccer_wrk, aes(x = Position, y = Height))+
  geom_boxplot()+
  geom_jitter(alpha = 0.5, size = 3, color = "tomato")+
  theme_bw()

ggplot_general <- ggplot(data = soccer_general, aes(x = Position, y = Height))+
  geom_boxplot()+
  geom_jitter(alpha = 0.4, size = 1, color = "tomato")+
  theme_bw()

ggarrange(ggplot_general, ggplot_wrk, col = 2)
```

```
## Warning in as_grob.default(plot): Cannot convert object of class numeric into a
## grob.
```

![](Homework_Mult_Comp_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Посчитаем p-values


```
## The following object is masked from package:ggplot2:
## 
##     Position
```

```
## 
## 	Pairwise comparisons using t tests with non-pooled SD 
## 
## data:  Height and Position 
## 
##            Defender Forward Goalkeeper
## Forward    0.29106  -       -         
## Goalkeeper 0.00026  0.00039 -         
## Midfielder 0.00020  0.14559 2.5e-08   
## 
## P value adjustment method: none
```

Посчитаем adjusted p-values методом Холма


```r
pairwise_ttest_work_holm <- pairwise.t.test(Height, Position, p.adjust.method = "holm", pool.sd = FALSE) 

pairwise_ttest_work_holm
```

```
## 
## 	Pairwise comparisons using t tests with non-pooled SD 
## 
## data:  Height and Position 
## 
##            Defender Forward Goalkeeper
## Forward    0.29118  -       -         
## Goalkeeper 0.00103  0.00118 -         
## Midfielder 0.00099  0.29118 1.5e-07   
## 
## P value adjustment method: holm
```

Посчитаем adjusted p-values методом Бенжамини-Хохберга


```r
pairwise_ttest_work_BH <- pairwise.t.test(Height, Position, p.adjust.method = "BH", pool.sd = FALSE)
pairwise_ttest_work_BH
```

```
## 
## 	Pairwise comparisons using t tests with non-pooled SD 
## 
## data:  Height and Position 
## 
##            Defender Forward Goalkeeper
## Forward    0.29106  -       -         
## Goalkeeper 0.00051  0.00059 -         
## Midfielder 0.00051  0.17471 1.5e-07   
## 
## P value adjustment method: BH
```

По итогу проведения попарных тестов с разными методами поправки мы не получили каких-либо различий между ними. Из 6 сравниваемых групп в 4 наблюдаются статистически значимые различия, однако добавление поправки не изменило результат. 


Попарные доверительные интервалы без поправки

```r
Defender_height <- soccer_wrk %>% filter(Position == "Defender") %>% pull(Height) 
Goalkeeper_height <- soccer_wrk %>% filter(Position == "Goalkeeper") %>% pull(Height) 
Midfielder_height <- soccer_wrk %>% filter(Position == "Midfielder") %>% pull(Height)
Forward_height <- soccer_wrk %>% filter(Position == "Forward") %>% pull(Height) 

t.test(Defender_height, Goalkeeper_height) %>% with(conf.int)
```

```
## [1] -7.885903 -2.630114
## attr(,"conf.level")
## [1] 0.95
```

```r
t.test(Defender_height, Midfielder_height) %>% with(conf.int)
```

```
## [1] 2.061467 6.374897
## attr(,"conf.level")
## [1] 0.95
```

```r
t.test(Defender_height, Forward_height) %>% with(conf.int)
```

```
## [1] -1.525215  4.961579
## attr(,"conf.level")
## [1] 0.95
```

```r
t.test(Goalkeeper_height, Midfielder_height) %>% with(conf.int)
```

```
## [1]  6.658979 12.293402
## attr(,"conf.level")
## [1] 0.95
```

```r
t.test(Goalkeeper_height, Forward_height) %>% with(conf.int)
```

```
## [1]  3.293399 10.658982
## attr(,"conf.level")
## [1] 0.95
```

```r
t.test(Midfielder_height, Forward_height) %>% with(conf.int)
```

```
## [1] -5.8976773  0.8976773
## attr(,"conf.level")
## [1] 0.95
```

Мы видим, что по результатам построения попарных доверительных интервалов, Форварды значимо не отличаются от Защитников и Полузащитников.

Попарные доверительные интервалы с поправкой Бонферрони

```r
bonferroni_CWER <- 1 - (0.05/6)

t.test(Defender_height, Goalkeeper_height, conf.level = bonferroni_CWER) %>% with(conf.int)
```

```
## [1] -8.876583 -1.639434
## attr(,"conf.level")
## [1] 0.9916667
```

```r
t.test(Defender_height, Midfielder_height, conf.level = bonferroni_CWER) %>% with(conf.int)
```

```
## [1] 1.288373 7.147990
## attr(,"conf.level")
## [1] 0.9916667
```

```r
t.test(Defender_height, Forward_height, conf.level = bonferroni_CWER) %>% with(conf.int)
```

```
## [1] -2.732988  6.169352
## attr(,"conf.level")
## [1] 0.9916667
```

```r
t.test(Goalkeeper_height, Midfielder_height, conf.level = bonferroni_CWER) %>% with(conf.int)
```

```
## [1]  5.613456 13.338925
## attr(,"conf.level")
## [1] 0.9916667
```

```r
t.test(Goalkeeper_height, Forward_height, conf.level = bonferroni_CWER) %>% with(conf.int)
```

```
## [1]  1.936553 12.015828
## attr(,"conf.level")
## [1] 0.9916667
```

```r
t.test(Midfielder_height, Forward_height, conf.level = bonferroni_CWER) %>% with(conf.int)
```

```
## [1] -7.149936  2.149936
## attr(,"conf.level")
## [1] 0.9916667
```
Мы видим, что по результатам применения поправки Бонферрони доверительные интервалы стали шире, но также Форварды значимо не отличаются от Защитников и Полузащитников.

Но конечно проще было бы сразу провести ANOVA и Post-hoc анализ и посчитать доверительные интервалы.


```r
aov_result <- aov(Height ~ Position, data = soccer_wrk)
tukey <- TukeyHSD(aov_result)

par(mar = c(5, 10, 4, 2)+0.1)
tukey %>% plot(las = 1)
```

![](Homework_Mult_Comp_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

На самом деле мы видим, что Форварды также значимо не отличаются от Защитников и Полузащитников, как и в предыдущих группах доверительных интервалов. 
