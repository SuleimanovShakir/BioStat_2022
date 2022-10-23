---
title: "Data Visualization HW"
author: "Shakir Suleimanov"
date: '2022-10-16'
output:
  html_document:
    keep_md: TRUE
---

# Подгружаем библиотеки



# Задание 1. Загружаем данные


```r
insurance <- read.csv("insurance_cost.csv")
head(insurance)
```

```
##   age    sex    bmi children smoker    region   charges
## 1  19 female 27.900        0    yes southwest 16884.924
## 2  18   male 33.770        1     no southeast  1725.552
## 3  28   male 33.000        3     no southeast  4449.462
## 4  33   male 22.705        0     no northwest 21984.471
## 5  32   male 28.880        0     no northwest  3866.855
## 6  31 female 25.740        0     no southeast  3756.622
```

# Смотрим на данные


```r
str(insurance)
```

```
## 'data.frame':	1338 obs. of  7 variables:
##  $ age     : int  19 18 28 33 32 31 46 37 37 60 ...
##  $ sex     : chr  "female" "male" "male" "male" ...
##  $ bmi     : num  27.9 33.8 33 22.7 28.9 ...
##  $ children: int  0 1 3 0 0 0 1 3 2 0 ...
##  $ smoker  : chr  "yes" "no" "no" "no" ...
##  $ region  : chr  "southwest" "southeast" "southeast" "northwest" ...
##  $ charges : num  16885 1726 4449 21984 3867 ...
```


```r
summary(insurance)
```

```
##       age            sex                 bmi           children    
##  Min.   :18.00   Length:1338        Min.   :15.96   Min.   :0.000  
##  1st Qu.:27.00   Class :character   1st Qu.:26.30   1st Qu.:0.000  
##  Median :39.00   Mode  :character   Median :30.40   Median :1.000  
##  Mean   :39.21                      Mean   :30.66   Mean   :1.095  
##  3rd Qu.:51.00                      3rd Qu.:34.69   3rd Qu.:2.000  
##  Max.   :64.00                      Max.   :53.13   Max.   :5.000  
##     smoker             region             charges     
##  Length:1338        Length:1338        Min.   : 1122  
##  Class :character   Class :character   1st Qu.: 4740  
##  Mode  :character   Mode  :character   Median : 9382  
##                                        Mean   :13270  
##                                        3rd Qu.:16640  
##                                        Max.   :63770
```

# Задание 2


```r
ggplot()+
  geom_histogram(data = insurance, aes(x = bmi), alpha = 0.9, bins = 12)
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
ggplot()+
  geom_histogram(data = insurance, aes(x = charges), alpha = 0.9, bins = 15)
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
ggplot()+
  geom_histogram(data = insurance, aes(x = age), alpha = 0.9, binwidth = 3)
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
ggplot()+
  geom_histogram(data = insurance, aes(x = children), alpha = 0.9, binwidth = 1)
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

# Задание 3


```r
median_charges <- round(median(insurance$charges), 0)
mean_charges <- round(mean(insurance$charges), 0)

density_charges <- ggplot()+
  geom_density(data = insurance, aes(x = charges))+
  geom_vline(aes(xintercept = median_charges), color = "blue")+
  geom_vline(aes(xintercept = mean_charges), color = "red")+
  annotate("text", x= median_charges-5500, y =  0.00002, label=paste0("Median=", median_charges), color = "blue")+
  annotate("text", x= mean_charges+5000, y =  0.00004, label=paste0("Mean=", mean_charges), color = "red")+
  labs(x = "Сумма страховой выплаты", y = "Частота встречаемости")+
  theme_linedraw()

density_charges
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

# Задание 4


```r
boxplot_sex_charges <- ggplot()+
  geom_boxplot(data = insurance, aes(x = sex, y = charges))+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=8))+
  labs(x = "Пол", y = "Сумма страховой выплаты")
  

boxplot_smoker_charges <-  ggplot()+
  geom_boxplot(data = insurance, aes(x = smoker, y = charges))+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=8))+
  labs(x = "Пол", y = "Сумма страховой выплаты")

boxplot_region_charges <- ggplot()+
  geom_boxplot(data = insurance, aes(x = region, y = charges))+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=8))+
  labs(x = "Пол", y = "Сумма страховой выплаты")

combine_boxplot <- ggarrange(boxplot_sex_charges, boxplot_region_charges, boxplot_smoker_charges,
                          ncol = 3, nrow = 1, widths = c(1, 1.5, 1))

combine_boxplot
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

# Задание 5


```r
combine_plot <- ggarrange(density_charges, combine_boxplot, 
                          ncol = 1, nrow = 2)
  

combine_plot
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

# Задание 6


```r
ggplot()+
  geom_density(data = insurance, aes(x = charges))+
  facet_wrap(vars(region))+
  labs(x = "Размер страховой выплаты", y = "Частота встречаемости")+
  theme_linedraw()
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

# Задание 7-8


```r
ggplot(data = insurance, aes(x = age, y = charges))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14), plot.title = element_text(hjust = 0.5))+
  labs(title = "Возрост vs Сумма страховой выплаты", x = "Возраст", y = "Сумма страховой выплаты")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

# Задание 9


```r
ggplot(data = insurance, aes(x = age, y = charges, fill = smoker))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14), plot.title = element_text(hjust = 0.5))+
  labs(title = "Возрост vs Сумма страховой выплаты", x = "Возраст", y = "Сумма страховой выплаты")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

# Задание 10


```r
ggplot(data = insurance, aes(x = bmi, y = charges))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14), plot.title = element_text(hjust = 0.5))+
  labs(title = "ИМТ vs Сумма страховой выплаты", x = "ИМТ", y = "Сумма страховой выплаты")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
ggplot(data = insurance, aes(x = bmi, y = charges, fill = smoker))+
  geom_point()+
  geom_smooth(method = lm)+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14), plot.title = element_text(hjust = 0.5))+
  labs(title = "ИМТ vs Сумма страховой выплаты", x = "ИМТ", y = "Сумма страховой выплаты")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

# Задание 11

Вопрос: Влияет ли количество детей индивида на ту сумму, которую страховая компания потратила на индивида в год? Так как нам нужно посмотреть распределение признака в разных группах, и мы точно не знаем параметры выборки (нормальность, гомогенность дисперсий), для оценки отлично подойдет график box plot.


```r
boxplot_children_charges <- ggplot()+
  geom_boxplot(data = insurance, aes(x = children, y = charges, group = children))+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14), plot.title = element_text(hjust = 0.5))+
  labs(title = "Отношение страховой суммы индивида к количеству детей", x = "Количество детей", y = "Страховая сумма")

boxplot_children_charges
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

Ответ: Нет, так как при разбивке индивидов на группы по количеству детей, медиана каждой группы находится на одном уровне.

# Задание 12

Интересно было бы посмотреть, в какой группе больше курильщиков: в группе женщин или мужчин? Это может быть важно тарификации суммы страховки в зависимости от пола. Для первичной оценки количества элементов в группе подходит график bar chart


```r
ggplot()+
  geom_bar(data = insurance, aes(x = sex, fill = smoker), position = "dodge")+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14), plot.title = element_text(hjust = 0.5))+
  labs(title = "Распределение курильщиков в зависимости от пола", x = "Пол", y = "Количество курильщиков")
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

Ответ: Мы видим, что в группе мужчин чуть больше курильщиков, но нужно проводить дополнительные тесты для оценки значимости данных различий.

# Задание 13

Так как мы знаем, что медиана страховой выплаты у курильщиков и не курильщиков отличается, проверим статистическую значимость данной разницы. Данная задача является дополнением задачи №4, где мы строили box plot распределения суммы страховых выплат с разбивкой по разным группам. Поэтому здесь я тоже использую box plot.


```r
charges_smokers <- insurance[insurance$smoker == "yes", ]$charges
charges_non_smokers <- insurance[insurance$smoker == "no", ]$charges

mann_whitney_smokers <- wilcox.test(charges_smokers, charges_non_smokers)

paste0 ("P-value for Mann-Whitney test is: ", mann_whitney_smokers$p.value)
```

```
## [1] "P-value for Mann-Whitney test is: 5.27023344450394e-130"
```

Так как эти две группы статистически различаются, посмотрим, отличается ли сумма страховой выплаты курильщиков в разных регионах, чтобы проводить тарификацию суммы страховки в зависимости от региона:


```r
ggplot()+
  geom_boxplot(data = insurance, aes(x = region, y = charges, fill = smoker))+
  theme_pubclean()+
  theme(axis.text.x = element_text(size=14), plot.title = element_text(hjust = 0.5))+
  labs(title = "Распределение страховой выплаты курильщиков по регионам", x = "Регион", y = "Сумма страховой выплаты")
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

Ответ: На данном графике мы видим, что на курильщиков, проживаюших на юге города, уходит больше денег, чем на курильщиков севера, следовательно, мы можем увеличивать для них сумму страховки.

# Задание 14


```r
for (i in 1:length(insurance$age)){
  if (insurance$age[i] <= 34){
    insurance$age_group[i] = "age: 21-34"
  } else if (insurance$age[i] >= 50){
    insurance$age_group[i] = "age: 50+"
  } else {
    insurance$age_group[i] = "age: 35-49"
  }
}
```


```r
ggplot(data = insurance, aes(x = bmi, y = log(charges), fill = age_group))+
  geom_point(color = "#6600ff", alpha = 0.4)+
  facet_wrap(vars(age_group))+
  geom_smooth(method = lm)+
  theme_pubclean()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Отношение индекса массы тела к логарифму трат по возрастным группам", x = "ИМТ", y = "log(сумма страховых выплат")
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](Data_Visualization_HW_files/figure-html/unnamed-chunk-20-1.png)<!-- -->
