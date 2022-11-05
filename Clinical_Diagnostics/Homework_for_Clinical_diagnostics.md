---
title: "HW_for_Clinical_Diagnostics"
author: "Shakir Suleimanov"
date: '2022-11-05'
output: 
  html_document:
    keep_md: TRUE
    code_folding: show
    df_print: paged
    highlight: pygments
    smooth_scroll: no
    theme: united
    toc: yes
    toc_depth: 3
    toc_float: yes
    toc_position: right
---



# Library


```r
library(dplyr)
library(tidyr)
library(ggplot2)
library(pROC)
library(gtsummary)
library(readr)
```


```r
pima <- read.csv("diabetes.csv")
str(pima)
```

```
## 'data.frame':	768 obs. of  9 variables:
##  $ Pregnancies             : int  6 1 8 1 0 5 3 10 2 8 ...
##  $ Glucose                 : int  148 85 183 89 137 116 78 115 197 125 ...
##  $ BloodPressure           : int  72 66 64 66 40 74 50 0 70 96 ...
##  $ SkinThickness           : int  35 29 0 23 35 0 32 0 45 0 ...
##  $ Insulin                 : int  0 0 0 94 168 0 88 0 543 0 ...
##  $ BMI                     : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
##  $ DiabetesPedigreeFunction: num  0.627 0.351 0.672 0.167 2.288 ...
##  $ Age                     : int  50 31 32 21 33 30 26 29 53 54 ...
##  $ Outcome                 : int  1 0 1 0 1 0 1 0 1 1 ...
```

# Задание 1


```r
pima <- pima %>%
  filter(Glucose != 0) %>%
  mutate(Glucose_mg_dl = Glucose / 18)

glucose_intolerance <- sum(pima$Glucose_mg_dl >= 7.8)

glucose_tolerance <- sum(pima$Glucose_mg_dl < 7.8)

cat("Есть НТГ:", glucose_intolerance, ".", "Нет НТГ:", glucose_tolerance)
```

```
## Есть НТГ: 192 . Нет НТГ: 571
```

# Задание 2


```r
pima_filt <- pima %>%
  filter(BloodPressure != 0 & SkinThickness != 0 & Insulin != 0 & BMI != 0 & DiabetesPedigreeFunction != 0 & Age != 0)

pima_filt %>%
  tbl_summary(by = Outcome) %>%
  add_p()
```

```{=html}
<div id="krncagcywl" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#krncagcywl .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#krncagcywl .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#krncagcywl .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#krncagcywl .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#krncagcywl .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#krncagcywl .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#krncagcywl .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#krncagcywl .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#krncagcywl .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#krncagcywl .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#krncagcywl .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#krncagcywl .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#krncagcywl .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#krncagcywl .gt_from_md > :first-child {
  margin-top: 0;
}

#krncagcywl .gt_from_md > :last-child {
  margin-bottom: 0;
}

#krncagcywl .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#krncagcywl .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#krncagcywl .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#krncagcywl .gt_row_group_first td {
  border-top-width: 2px;
}

#krncagcywl .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#krncagcywl .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#krncagcywl .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#krncagcywl .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#krncagcywl .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#krncagcywl .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#krncagcywl .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#krncagcywl .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#krncagcywl .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#krncagcywl .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#krncagcywl .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#krncagcywl .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#krncagcywl .gt_left {
  text-align: left;
}

#krncagcywl .gt_center {
  text-align: center;
}

#krncagcywl .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#krncagcywl .gt_font_normal {
  font-weight: normal;
}

#krncagcywl .gt_font_bold {
  font-weight: bold;
}

#krncagcywl .gt_font_italic {
  font-style: italic;
}

#krncagcywl .gt_super {
  font-size: 65%;
}

#krncagcywl .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#krncagcywl .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#krncagcywl .gt_indent_1 {
  text-indent: 5px;
}

#krncagcywl .gt_indent_2 {
  text-indent: 10px;
}

#krncagcywl .gt_indent_3 {
  text-indent: 15px;
}

#krncagcywl .gt_indent_4 {
  text-indent: 20px;
}

#krncagcywl .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col"><strong>0</strong>, N = 262<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col"><strong>1</strong>, N = 130<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td class="gt_row gt_left">Pregnancies</td>
<td class="gt_row gt_center">2 (1, 4)</td>
<td class="gt_row gt_center">3 (1, 7)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">Glucose</td>
<td class="gt_row gt_center">108 (94, 126)</td>
<td class="gt_row gt_center">144 (124, 172)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">BloodPressure</td>
<td class="gt_row gt_center">70 (60, 76)</td>
<td class="gt_row gt_center">74 (66, 82)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">SkinThickness</td>
<td class="gt_row gt_center">27 (18, 34)</td>
<td class="gt_row gt_center">33 (26, 40)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">Insulin</td>
<td class="gt_row gt_center">105 (66, 164)</td>
<td class="gt_row gt_center">170 (128, 239)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">BMI</td>
<td class="gt_row gt_center">31 (26, 36)</td>
<td class="gt_row gt_center">35 (32, 38)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">DiabetesPedigreeFunction</td>
<td class="gt_row gt_center">0.41 (0.26, 0.62)</td>
<td class="gt_row gt_center">0.55 (0.33, 0.79)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">Age</td>
<td class="gt_row gt_center">25 (22, 30)</td>
<td class="gt_row gt_center">33 (27, 43)</td>
<td class="gt_row gt_center"><0.001</td></tr>
    <tr><td class="gt_row gt_left">Glucose_mg_dl</td>
<td class="gt_row gt_center">5.97 (5.22, 7.00)</td>
<td class="gt_row gt_center">8.03 (6.90, 9.54)</td>
<td class="gt_row gt_center"><0.001</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">1</sup> Median (IQR)</td>
    </tr>
    <tr>
      <td class="gt_footnote" colspan="4"><sup class="gt_footnote_marks">2</sup> Wilcoxon rank sum test</td>
    </tr>
  </tfoot>
</table>
</div>
```

&nbsp;

Анализ ROC-кривых

Проведём ROC-анализ для показателя гликемии


```r
roc_curve_gluc <- roc(Outcome ~ Glucose, data = pima_filt, ci = T)

roc_curve_gluc
```

```
## 
## Call:
## roc.formula(formula = Outcome ~ Glucose, data = pima_filt, ci = T)
## 
## Data: Glucose in 262 controls (Outcome 0) < 130 cases (Outcome 1).
## Area under the curve: 0.8058
## 95% CI: 0.7594-0.8521 (DeLong)
```


```r
ggroc(roc_curve_gluc) + 
    theme_grey()
```

![](Homework_for_Clinical_diagnostics_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

# Задание 3


```r
roc_curve_gluc$auc
```

```
## Area under the curve: 0.8058
```

# Задание 4


```r
roc_curve_gluc$ci
```

```
## 95% CI: 0.7594-0.8521 (DeLong)
```

# Задание 5

Проведём ROC-анализ для показателя инсулина


```r
roc_curve_ins <- roc(Outcome ~ Insulin, data = pima_filt, ci = T)

roc_curve_ins
```

```
## 
## Call:
## roc.formula(formula = Outcome ~ Insulin, data = pima_filt, ci = T)
## 
## Data: Insulin in 262 controls (Outcome 0) < 130 cases (Outcome 1).
## Area under the curve: 0.7299
## 95% CI: 0.6789-0.7809 (DeLong)
```

```r
roc_curve_ins %>% coords(x = "best", best.method = "closest.topleft")
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["threshold"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["specificity"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["sensitivity"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"126.5","2":"0.6374046","3":"0.7538462"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Показатель инсулина чуть хуже предсказывает сахарный диабет, чем показатель гликемии.

# Задание 6


```r
pima_filt %>%
  select(!Glucose_mg_dl) %>%
  pivot_longer(cols = !Outcome) %>%
  group_by(name) %>%
  summarise(AUC = roc(Outcome, value)$auc[1])
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["AUC"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"Age","2":"0.7431738"},{"1":"BloodPressure","2":"0.6213153"},{"1":"BMI","2":"0.6637551"},{"1":"DiabetesPedigreeFunction","2":"0.6215355"},{"1":"Glucose","2":"0.8057692"},{"1":"Insulin","2":"0.7299325"},{"1":"Pregnancies","2":"0.6213594"},{"1":"SkinThickness","2":"0.6594392"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Мы видим, что наибольшая площадь под ROC-кривой наблюдается для параметра глюкозы, а наименьшая - для давления крови, хотя и для DiabetesPedigreeFunction и для Pregnancies площадь под ROC-кривой практически так же мала.
