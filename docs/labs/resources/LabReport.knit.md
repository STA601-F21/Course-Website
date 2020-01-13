---
title: 'STA 602L Homework and Lab Template'
author: "Student"
date: "13 January, 2020"
output:
  pdf_document: default
---



Visit [this site](https://rmarkdown.rstudio.com/lesson-1.html) for more information on R Markdown.

***

## Exercise 1

Brief statement of the problem (optional)

### Part (a)

Then, some math:

$$
\begin{aligned}
X \sim N(\mu,1) \implies p_X(x) = \frac{1}{\sqrt{2\pi}}e^{-\frac{1}{2}(x - \mu)^2}
\end{aligned}
$$

### Part (b)

Finally, some code:


```r
x <- rnorm(1000, 0, 1)
x %>% data.frame() %>% ggplot2::ggplot() + geom_histogram(aes(x = x))
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```



\begin{center}\includegraphics{LabReport_files/figure-latex/unnamed-chunk-1-1} \end{center}

## Exercise 2

Repeat...

### Part (a)

### Part (b)

***

