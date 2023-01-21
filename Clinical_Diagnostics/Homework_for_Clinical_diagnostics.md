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
#str(pima)
```

# Задание 1


```r
pima <- pima %>%
  mutate_at(vars(Glucose, BloodPressure, SkinThickness, Insulin, BMI),
            ~ ifelse(. == 0, NA, .)) %>%
  mutate(Glucose_mg_dl = Glucose / 18)

glucose_intolerance <- sum(pima$Glucose_mg_dl >= 7.8, na.rm = TRUE)

glucose_tolerance <- sum(pima$Glucose_mg_dl < 7.8, na.rm = TRUE)

cat("Есть НТГ:", glucose_intolerance, ".", "Нет НТГ:", glucose_tolerance)
```

```
## Есть НТГ: 192 . Нет НТГ: 571
```

# Задание 2


```r
pima%>%
  tbl_summary(by = Outcome, missing = "no") %>%
  add_p()
```

```{=html}
<div id="wboatvzwkq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#wboatvzwkq .gt_table {
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

#wboatvzwkq .gt_heading {
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

#wboatvzwkq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#wboatvzwkq .gt_title {
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

#wboatvzwkq .gt_subtitle {
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

#wboatvzwkq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wboatvzwkq .gt_col_headings {
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

#wboatvzwkq .gt_col_heading {
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

#wboatvzwkq .gt_column_spanner_outer {
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

#wboatvzwkq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wboatvzwkq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wboatvzwkq .gt_column_spanner {
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

#wboatvzwkq .gt_group_heading {
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
  text-align: left;
}

#wboatvzwkq .gt_empty_group_heading {
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

#wboatvzwkq .gt_from_md > :first-child {
  margin-top: 0;
}

#wboatvzwkq .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wboatvzwkq .gt_row {
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

#wboatvzwkq .gt_stub {
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

#wboatvzwkq .gt_stub_row_group {
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

#wboatvzwkq .gt_row_group_first td {
  border-top-width: 2px;
}

#wboatvzwkq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wboatvzwkq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#wboatvzwkq .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#wboatvzwkq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wboatvzwkq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wboatvzwkq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wboatvzwkq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wboatvzwkq .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wboatvzwkq .gt_footnotes {
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

#wboatvzwkq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wboatvzwkq .gt_sourcenotes {
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

#wboatvzwkq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wboatvzwkq .gt_left {
  text-align: left;
}

#wboatvzwkq .gt_center {
  text-align: center;
}

#wboatvzwkq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wboatvzwkq .gt_font_normal {
  font-weight: normal;
}

#wboatvzwkq .gt_font_bold {
  font-weight: bold;
}

#wboatvzwkq .gt_font_italic {
  font-style: italic;
}

#wboatvzwkq .gt_super {
  font-size: 65%;
}

#wboatvzwkq .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#wboatvzwkq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#wboatvzwkq .gt_indent_1 {
  text-indent: 5px;
}

#wboatvzwkq .gt_indent_2 {
  text-indent: 10px;
}

#wboatvzwkq .gt_indent_3 {
  text-indent: 15px;
}

#wboatvzwkq .gt_indent_4 {
  text-indent: 20px;
}

#wboatvzwkq .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;0&lt;/strong&gt;, N = 500&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>0</strong>, N = 500<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;1&lt;/strong&gt;, N = 268&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>1</strong>, N = 268<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;p-value&lt;/strong&gt;&lt;sup class=&quot;gt_footnote_marks&quot;&gt;2&lt;/sup&gt;"><strong>p-value</strong><sup class="gt_footnote_marks">2</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Pregnancies</td>
<td headers="stat_1" class="gt_row gt_center">2 (1, 5)</td>
<td headers="stat_2" class="gt_row gt_center">4 (2, 8)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Glucose</td>
<td headers="stat_1" class="gt_row gt_center">107 (93, 125)</td>
<td headers="stat_2" class="gt_row gt_center">140 (119, 167)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">BloodPressure</td>
<td headers="stat_1" class="gt_row gt_center">70 (62, 78)</td>
<td headers="stat_2" class="gt_row gt_center">74 (68, 84)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">SkinThickness</td>
<td headers="stat_1" class="gt_row gt_center">27 (19, 33)</td>
<td headers="stat_2" class="gt_row gt_center">32 (27, 39)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Insulin</td>
<td headers="stat_1" class="gt_row gt_center">102 (66, 161)</td>
<td headers="stat_2" class="gt_row gt_center">170 (128, 239)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">BMI</td>
<td headers="stat_1" class="gt_row gt_center">30 (26, 35)</td>
<td headers="stat_2" class="gt_row gt_center">34 (31, 39)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">DiabetesPedigreeFunction</td>
<td headers="stat_1" class="gt_row gt_center">0.34 (0.23, 0.56)</td>
<td headers="stat_2" class="gt_row gt_center">0.45 (0.26, 0.73)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age</td>
<td headers="stat_1" class="gt_row gt_center">27 (23, 37)</td>
<td headers="stat_2" class="gt_row gt_center">36 (28, 44)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Glucose_mg_dl</td>
<td headers="stat_1" class="gt_row gt_center">5.94 (5.17, 6.94)</td>
<td headers="stat_2" class="gt_row gt_center">7.78 (6.61, 9.28)</td>
<td headers="p.value" class="gt_row gt_center"><0.001</td></tr>
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
roc_curve_gluc <- roc(Outcome ~ Glucose, data = pima, ci = T)

roc_curve_gluc
```

```
## 
## Call:
## roc.formula(formula = Outcome ~ Glucose, data = pima, ci = T)
## 
## Data: Glucose in 497 controls (Outcome 0) < 266 cases (Outcome 1).
## Area under the curve: 0.7928
## 95% CI: 0.7599-0.8257 (DeLong)
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
## Area under the curve: 0.7928
```

# Задание 4


```r
roc_curve_gluc$ci
```

```
## 95% CI: 0.7599-0.8257 (DeLong)
```

# Задание 5

Проведём ROC-анализ для показателя инсулина


```r
roc_curve_ins <- roc(Outcome ~ Insulin, data = pima, ci = T)

roc_curve_ins
```

```
## 
## Call:
## roc.formula(formula = Outcome ~ Insulin, data = pima, ci = T)
## 
## Data: Insulin in 264 controls (Outcome 0) < 130 cases (Outcome 1).
## Area under the curve: 0.7316
## 95% CI: 0.6809-0.7824 (DeLong)
```

```r
roc_curve_ins %>% coords(x = "best", best.method = "closest.topleft")
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["threshold"],"name":[1],"type":["dbl"],"align":["right"]},{"label":["specificity"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["sensitivity"],"name":[3],"type":["dbl"],"align":["right"]}],"data":[{"1":"121","2":"0.6212121","3":"0.7846154"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Показатель инсулина чуть хуже предсказывает сахарный диабет, чем показатель гликемии.

# Задание 6


```r
pima %>%
  select(!Glucose_mg_dl) %>%
  pivot_longer(cols = !Outcome) %>%
  group_by(name) %>%
  summarise(AUC = roc(Outcome, value)$auc[1])
```

<div data-pagedtable="false">
  <script data-pagedtable-source type="application/json">
{"columns":[{"label":["name"],"name":[1],"type":["chr"],"align":["left"]},{"label":["AUC"],"name":[2],"type":["dbl"],"align":["right"]}],"data":[{"1":"Age","2":"0.6869403"},{"1":"BloodPressure","2":"0.6075760"},{"1":"BMI","2":"0.6870358"},{"1":"DiabetesPedigreeFunction","2":"0.6062015"},{"1":"Glucose","2":"0.7927906"},{"1":"Insulin","2":"0.7316288"},{"1":"Pregnancies","2":"0.6195149"},{"1":"SkinThickness","2":"0.6625115"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>
</div>

Мы видим, что наибольшая площадь под ROC-кривой наблюдается для параметра глюкозы, а наименьшая - для давления крови, хотя и для DiabetesPedigreeFunction и для Pregnancies площадь под ROC-кривой практически так же мала.
