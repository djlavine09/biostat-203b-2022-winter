---
title: "Biostat 203B Homework 4"
subtitle: Due Mar 18 @ 11:59PM
output:
  html_document:
    theme: spacelab
    highlight: textmate
    code_folding: show
    toc: true
    toc_float: true
    number_sections: false
    fig_width: 15 
    fig_height: 9 
    keep_md: true
---





Display machine information:

```r
sessionInfo()
```

```
## R version 3.6.0 (2019-04-26)
## Platform: x86_64-redhat-linux-gnu (64-bit)
## Running under: CentOS Linux 7 (Core)
## 
## Matrix products: default
## BLAS/LAPACK: /usr/lib64/R/lib/libRblas.so
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] pROC_1.18.0         randomForest_4.6-14 glmnet_4.1-3       
##  [4] Matrix_1.2-17       splitTools_0.3.2    knitr_1.37         
##  [7] bigrquery_1.4.0     DBI_1.1.2           miceRanger_1.5.0   
## [10] lubridate_1.8.0     forcats_0.5.1       stringr_1.4.0      
## [13] dplyr_1.0.7         purrr_0.3.4         readr_2.1.1        
## [16] tidyr_1.2.0         tibble_3.1.6        ggplot2_3.3.5      
## [19] tidyverse_1.3.1    
## 
## loaded via a namespace (and not attached):
##  [1] fs_1.5.2          bit64_4.0.5       httr_1.4.2        tools_3.6.0      
##  [5] backports_1.4.1   bslib_0.3.1       utf8_1.2.2        R6_2.5.1         
##  [9] colorspace_2.0-2  withr_2.4.3       tidyselect_1.1.1  Exact_3.1        
## [13] bit_4.0.4         compiler_3.6.0    cli_3.1.0         rvest_1.0.2      
## [17] expm_0.999-6      xml2_1.3.3        sass_0.4.0        scales_1.1.1     
## [21] mvtnorm_1.1-3     proxy_0.4-26      digest_0.6.29     rmarkdown_2.11   
## [25] pkgconfig_2.0.3   htmltools_0.5.2   dbplyr_2.1.1      fastmap_1.1.0    
## [29] rlang_1.0.1       readxl_1.3.1      rstudioapi_0.13   FNN_1.1.3        
## [33] shape_1.4.6       jquerylib_0.1.4   generics_0.1.1    jsonlite_1.7.2   
## [37] car_3.0-12        magrittr_2.0.1    Rcpp_1.0.8        DescTools_0.99.44
## [41] munsell_0.5.0     fansi_0.5.0       abind_1.4-5       lifecycle_1.0.1  
## [45] stringi_1.7.6     yaml_2.2.1        carData_3.0-4     MASS_7.3-51.4    
## [49] rootSolve_1.8.2.3 plyr_1.8.6        grid_3.6.0        crayon_1.4.2     
## [53] lmom_2.8          lattice_0.20-38   splines_3.6.0     haven_2.4.3      
## [57] hms_1.1.1         pillar_1.6.4      ggpubr_0.4.0      ranger_0.13.1    
## [61] boot_1.3-22       gld_2.6.4         ggsignif_0.6.3    codetools_0.2-16 
## [65] reprex_2.0.1      glue_1.6.1        evaluate_0.14     data.table_1.14.2
## [69] modelr_0.1.8      vctrs_0.3.8       tzdb_0.2.0        foreach_1.5.1    
## [73] cellranger_1.1.0  gtable_0.3.0      assertthat_0.2.1  xfun_0.29        
## [77] broom_0.7.10      e1071_1.7-9       rstatix_0.7.0     survival_3.2-13  
## [81] class_7.3-15      gargle_1.2.0      iterators_1.0.13  corrplot_0.92    
## [85] ellipsis_0.3.2
```
Load database libraries and the tidyverse frontend:

```r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(miceRanger))
library(knitr)
```






## Q1. Missing data

Through the Shiny app developed in HW3, we observe abundant missing values in the MIMIC-IV ICU cohort we created. In this question, we use multiple imputation to obtain a data set without missing values.

0. Read following tutorials on the R package miceRanger for imputation: <https://github.com/farrellday/miceRanger>, <https://cran.r-project.org/web/packages/miceRanger/vignettes/miceAlgorithm.html>.

    A more thorough book treatment of the practical imputation strategies is the book [*_Flexible Imputation of Missing Data_*](https://stefvanbuuren.name/fimd/) by Stef van Buuren. 

### 1. Explain the jargon MCAR, MAR, and MNAR.

<span style="color: #0066CC;">Solution: MCAR, MAR, and MNAR are different methods of classifying missing data. MCAR stands for Missing Completely at random.  MCAR assumes that whether a datum is missing is independent of the data itself, and that the probability of a missing 
value is the same for all values. MAR stands for Missing at Random. NMAR stands for Not Missing at Random, and is the third classification used when 
missingness is neither MCAR nor MAR. Missingness may be occuring because of some
unmeasured variable.</span>



### 2. Explain in a couple of sentences how the Multiple Imputation by Chained Equations (MICE) work.

<span style="color: #0066CC;">The MICE algorithm works to impute missing data by computing many models to predict missing values. For each iteration, the missing value is filled in using
other values in the dataset.</span>


### 3. Perform a data quality check of the ICU stays data. Discard variables with substantial missingness, say >5000 `NA`s. Replace apparent data entry errors by `NA`s.


```r
icu <- read_rds("~/biostat-203b-2022-winter/hw3/mimiciv_shiny/icu_cohort.rds") 
```


```r
icu$thirty_day_mort[is.na(icu$thirty_day_mort)] <- 0
```

<span style="color: #0066CC;">Determine outliers that may be due to missing data/data entry errors. Outliers are calculated by the IQR method. That is, high outliers are set as values that are greater than Q3 + 1.5IQR, and low outliers are values that are smaller than Q1 - 1.5IQR, where Q1 and Q3 are the first and third quartiles, respectively. </span>


```r
var <- integer()
low <- numeric()
high <- numeric()
outlier <- as.data.frame(cbind(var, low, high))
n <- 1
x <- c(20:34)
for (i in x){
  q1 <- quantile(icu[,i], .25, na.rm = TRUE)
  q3 <- quantile(icu[,i], .75, na.rm = TRUE)
  outlier[n,] <- c(i,  q1 - 1.5 * (q3 - q1), q3 + 1.5 * (q3 - q1))
  n = n + 1
}
kable(outlier)
```



| var|    low|   high|
|---:|------:|------:|
|  20|  15.95|  52.35|
|  21|  13.50|  33.50|
|  22|  -0.20|   2.20|
|  23|  -2.65|  23.35|
|  24|  89.50| 117.50|
|  25|   1.10|   2.70|
|  26| 128.50| 148.50|
|  27|   6.50|  10.50|
|  28|   2.75|   5.55|
|  29|  20.00| 236.00|
|  30|   4.50|  32.50|
|  31|  36.50| 136.50|
|  32|  95.95| 100.35|
|  33|  35.50| 127.50|
|  34|  59.00| 187.00|

<span style="color: #0066CC;">Replace data that is above or below these values with NAs </span>


```r
n <- 1
for (i in x){
 r <- which(icu[,i] > outlier[n,3] | icu[,i] < outlier[n,2])
 icu[r,i] <- NA
 n <- n +1
}
```

<span style="color: #0066CC;">Now let's examine how many NAs there are </span>


```r
colSums(is.na(icu))
```

```
##      subject_id          intime         hadm_id         stay_id  first_careunit 
##               0               0               0               0               0 
##   last_careunit         outtime             los       admittime       dischtime 
##               0               0               0               0               0 
##       deathtime       insurance        language  marital_status       ethnicity 
##           47319               0               0            4507               0 
##          gender      anchor_age     anchor_year             dod      meas_51221 
##               0               0               0           44916             549 
##      meas_50882      meas_50912      meas_51301      meas_50902      meas_50960 
##            2278            4976            2446            1931            2273 
##      meas_50983      meas_50893      meas_50971      meas_50931     meas_220210 
##            2534            2515            2654            3755            1683 
##     meas_220045     meas_223761     meas_220181     meas_220179      admit_date 
##             986            4798            1656            1321               0 
##   time_to_death thirty_day_mort 
##           44916               0
```
<span style="color: #0066CC;">Remove columns with more than 5000 NAs: </span>


```r
icu <- icu %>% select( which(colSums(is.na(icu)) < 5000))
```

### 4. Impute missing values by `miceRanger` (request $m=3$ data sets). This step is computational intensive. Make sure to save the imputation results as a file. Hint: Setting `max.depth=10` in the `miceRanger` function may cut some computing time.


```r
mice <- miceRanger(icu, m = 3, returnModels = FALSE, max.depth = 10)
```

Write the rds file. 

```r
  write_rds(mice,"mice_results.rds", compress = "gz")
```

Read the rds file. 

```r
mice_res <- read_rds("mice_results.rds")
```
### 5. Make imputation diagnostic plots and explain what they mean. {.tabset}

#### Distribution of Imputed Values
<span style="color: #0066CC;">The following figure shows the distribution
of imputed values for each numeric variable in the icu data set that had missing values. The original density of each variable is shown in red, and the densities for each of the three imputed data sets are in black. We can see that for most of the numeric variables, the densities of the imputed data sets are somewhat similar to the original density. However, for some variables such as meas_50963, the densities are quite different. This might suggest that the data for meas_50963 is not missing completely at random. </span>


```r
plotDistributions(mice_res,vars='allNumeric')
```

![](hw4_sol_files/figure-html/unnamed-chunk-13-1.png)<!-- -->



#### Convergence of Correlation
<span style="color: #0066CC;">The following convergence of correlation plots show how the correlations between imputed values in each of the three data sets converged over the five iterations performed in the `miceRanger()` function. The convergence of correlation seems to vary across each variable, some of which don't show a clear pattern of convergence. Most of the numeric variables do not have particularly high Pearson's correlation coefficients, indicating that the linear relationship of the values in each data set did not have a strong linear relationship.  </span>

```r
plotCorrelations(mice_res,vars='allNumeric')
```

![](hw4_sol_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

#### Center & Dispersion Convergence

<span style="color: #0066CC;">Center and dispersion convergence plots show how the mean in each of the imputed data sets changes over each iteration. If the means of each data set appear to converge to the same value, then that indicates that the mean of the imputed data is converging to the true theoretical mean. Several of the numeric variables appear to converge to a particular value over the five iterations, but for variables such as meas_220179 and meas_51301, the convergence is not clear. This could indicate that more iterations of the `miceRanger()` algorithm is necessary. </span>

```r
plotVarConvergence(mice_res,vars='allNumeric')
```

![](hw4_sol_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

#### Model OOB Error

<span style="color: #0066CC;">Model Out of Bag (OOB) error refers to the prediction error from a random forest procedure with bagging- a type of bootstrap method. Model OOB error measures the predictive performance of the random forest method. In the figures below, the OOB-$R^2$ is shown over each iteration. We see that for many of the variables, the OOB-$R^2$ gets larger, and each data set has approximately the same $R^2$ values over each iteration. None of the $R^2$ values are very high, but it is promising that each iteration appears to improve the predictive performance of the random forest models. The graphs of the Model OOB error correspond to the graphs of center and dispersion convergence. Variables that appeared to converge towards a particular mean in the five iterations have Model OOB errors that also converge and get larger in each of the three data sets. </span>

```r
plotModelError(mice_res,vars='allNumeric')
```

![](hw4_sol_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

#### Variable Importance

<span style="color: #0066CC;">The following chart shows which variables were most important in imputing the missing values for each variable. Higher, darker values are those that had higher importance in each variable imputation. For example, meas_51221, meas_50882, and meas_50902 were the most important in finding the imputation for meas_50893.</span> 

```r
plotVarImportance(mice_res)
```

![](hw4_sol_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

#### Imputed Variance Between Datasets

<span style="color: #0066CC;">The last set of figures provides a visual for understanding how confident we can be in the imputations for each variable. On the left, we see the densities of the standard deviations for each numeric imputed variable. The shaded area shows the proportion of samples that had a standard deviation that was lower than the population standard deviation, with the value labeled in the title. For many of the variables, this was the case for over 70% of the samples. </span>

<span style="color: #0066CC;">The graph on the right hand side allows us to measure out confidence in the imputation of the categorical variable `marital status`. In pink, we see the number of distinct imputations, and in blue we see the theoretical value of these levels. </span>

<span style="color: #0066CC;">The figure shows that there were 2 distinct imputations in most of the imputed values, and that 2 random samples would be drawn. </span>


```r
plotImputationVariance(mice_res, ncol=2, widths=c(5,3))
```

![](hw4_sol_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


### 6. Choose one of the imputed data sets to be used in Q2. This is **not** a good idea to use just one imputed data set or to average multiple imputed data sets. Explain in a couple of sentences what the correct Multiple Imputation strategy is.

<span style="color: #0066CC;">The correct method of choosing a data set is described in Ch 5 of Stef van Buuren's book. The process involves creating a one pooled result from the three imputed datasets. This method helps to reduce variance in the estimates and derive more accurate conclusions. </span>

```r
mice_data <- completeData(mice_res)
mice_data <- mice_data$Dataset_1
```

Change categorical variables into factors:

```r
mice_data <- mice_data %>% mutate(marital_status_fact = 
                                    as.factor(marital_status), 
                                  gender_fact = as.factor(gender),
                                  ethnicity_fact = as.factor(ethnicity),
                                  thirty_fact = as.factor(thirty_day_mort)
                                  )
```


## Q2. Predicting 30-day mortality

Develop at least two analytic approaches for predicting the 30-day mortality of patients admitted to ICU using demographic information (gender, age, marital status, ethnicity), first lab measurements during ICU stay, and first vital measurements during ICU stay. For example, you can use (1) logistic regression (`glm()` function in base R or keras), (2) logistic regression with lasso penalty (glmnet or keras package), (3) random forest (randomForest package), or (4) neural network (keras package).

### 1. Partition data into 80% training set and 20% test set. Stratify partitioning according the 30-day mortality status.

<span style="color: #0066CC;">We will use the package `splitTools` to create a partition of our data into 80% training set and 20% test set. </span>

```r
install.packages("splitTools")
```


```r
library(splitTools)
set.seed(3602)

mice_split <- partition(mice_data$thirty_day_mort,
                        p = c(train = 0.8, test = 0.2))
test <- mice_data[mice_split$test, ]
train <- mice_data[mice_split$train, ]
```


### 2. Train the models using the training set. {.tabset}

#### Method 1: Logistic regression 

<span style="color: #0066CC;">The first method that we will use to predict the 30-day mortality of patients admitted to ICU is a logistic regression. To decide which variables to include in the logistic regression model, we will use the LASSO variable selection method. Selecting variables is an important consideration in classification, because simply including all of the possible predictors can lead to overfitting. </span>

<span style="color: #0066CC;">The LASSO method selects variables in such a way that increases the interpretability of the model, which is often lost through methods such as ridge regression, but also reduces the overfitting that can occur with stepwise variable selection methods. </span>

<span style="color: #0066CC;">To run the LASSO selection, we will follow the steps outlined by James et al.'s Introduction to Statistical Learning with Applications in R (Chapter 6.6 pgs. 251-255): </span>

```r
library(glmnet)
#setting alpha= 1 in glmnet performs lasso regression.
preds <- model.matrix(thirty_fact~., train[,c(13:16, 18:32,38)])[,-1]
y <- as.numeric(train$thirty_fact)
lass <- glmnet(preds, y, alpha = 1) #lasso on training set

cv <- cv.glmnet(preds, y, alpha = 1)

#Lasso model on full data with optimized choice of lambda from the training set. 
x <- model.matrix(thirty_fact~., mice_data[,c(13:16, 18:32,38)])[,-1]
y <- as.numeric(mice_data$thirty_fact)
out <- glmnet(x, y,alpha = 1)
coeff <- predict(out, type = "coefficients", s= cv$lambda.1se)

#Only show variables with non-zero coefficients. 
coeff[which(round(coeff,3) != 0),]
```

```
##               (Intercept) ethnicityUNABLE TO OBTAIN          ethnicityUNKNOWN 
##              1.9971608610              0.0212993371              0.0767656909 
##                   genderM                anchor_age                meas_50882 
##             -0.0019576609              0.0016722309             -0.0055624069 
##                meas_50912                meas_51301                meas_50902 
##              0.0740647315              0.0023086536             -0.0028002707 
##                meas_50960                meas_50893               meas_220210 
##              0.0027714464             -0.0143443570              0.0039325595 
##               meas_220045               meas_223761 
##              0.0008849147             -0.0067069159
```

<span style="color: #0066CC;">From the LASSO selection, it appears that an indicator variable for ethnicity unable to obtain, ethnicity unknown, gender, age, and all lab/vital measurements except for meas_50983, meas_50971, meas_50931, and meas_220181. </span>


<span style="color: #0066CC;">The following is the logistic model fit with the variables selected from the LASSO method. </span>


```r
mice_logistic <- glm(thirty_day_mort ~ gender + anchor_age  + 
                       as.numeric(ethnicity == "UNABLE TO OBTAIN") +
                       as.numeric(ethnicity == "UNKNOWN")+  meas_51221 +
                       meas_50882 + meas_50912 + meas_51301 + meas_50902 + 
                       meas_50960 + meas_220210 + meas_220045 + meas_223761 +  
                       meas_220179, data = train, family = binomial)
```


#### Method 2: Random Forests

<span style="color: #0066CC;">The second method we choose to predict 30-day mortality of patients admitted to the ICU is with a random forest. Random forests is a method of creating prediction models using decision trees and bagging, wuth a limited number of predictors allowed to be considered for each tree. Random forests allows for the variance between each tree created to be minimized, and produces a more steady set of trees used for prediction. </span>

<span style="color: #0066CC;"> The `randomForest` package is used to create random forests. </span>


```r
library(randomForest)

mice_forest <- randomForest(thirty_fact ~ gender_fact + anchor_age + marital_status_fact + 
                       ethnicity_fact +  meas_51221 + meas_50882 + meas_50912 + 
                       meas_51301 + meas_50902 + meas_50960 + meas_50983 + 
                       meas_50893 +  meas_50971 + meas_50931 + meas_220210 +
                       meas_220045 + meas_223761+ meas_220181 + 
                       meas_220179, data = train)
```




### 3. Compare model prediction performance on the test set. {.tabset}

<span style="color: #0066CC;">We will use the AUC of the ROC curve to evaluate predictive performance. The package `pROC` can calculate the AUC and plot the ROC curves. ROC curves show the relationship between sensitivity (the proportion of correctly identified "positives" out of all true positives) and the specificity (the proportion of "negative" results that were correctly identified). The AUC is the area under the ROC curve. Higher values of AUC correspond to better performance of classification models.  </span>


```r
install.packages("pROC")
```

```r
library(pROC)
```


#### Method 1: Logistic Regression


```r
#Predicted values in the test data using logistic regression model.
yhat_logistic <- predict(mice_logistic, type = "response", newdata = test)
```

<span style="color: #0066CC;">ROC and AUC: </span>

```r
roc_logistic <- roc(as.numeric(test$thirty_fact), yhat_logistic)

auc(roc_logistic)
```

```
## Area under the curve: 0.7663
```

```r
plot(roc_logistic)
```

<img src="hw4_sol_files/figure-html/unnamed-chunk-29-1.png" width="400px" />

#### Method 2: Random Forests

```r
yhat_forest <- predict(mice_forest, type = "response", newdata = test)
```


```r
roc_logistic <- roc(as.numeric(test$thirty_fact), as.numeric(yhat_forest))

auc(roc_logistic)
```

```
## Area under the curve: 0.5124
```

```r
plot(roc_logistic)
```

<img src="hw4_sol_files/figure-html/unnamed-chunk-31-1.png" width="400px" />

### Remarks
<span style="color: #0066CC;">We see that the AUC for the logistic regression is substantially larger than that of the random forest (.77 and .51, respectively). That is, the logistic model with variable selection using LASSO regression predicts 30-day mortality better than the random forest.
</span>
