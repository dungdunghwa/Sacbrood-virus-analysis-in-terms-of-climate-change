# Sacbrood-virus-analysis-in-terms-of-climate-change
Descriptive analysis of relationship between Sacbrood virus and Climate change

## Content
  1. flow chart
  2. crawling
  3. blockwise stochastic regression imputation
  4. dynamic time wrapping
  5. weighted mean dvector 
  6. penalized logistic regression

## 1. flow chart
![flow chart (en)](https://user-images.githubusercontent.com/108067353/199183520-ccee3593-e6d9-4286-a958-d7044f40ec85.png)

## 2. Crawling
I used Google Colab for crawling Virus infection history. Packages and initial settings are different from desktop.

### Packages

 from os import close
 import time
 from selenium import webdriver
 from selenium.webdriver.common.by import By
 from selenium.webdriver.common.keys import Keys
 from bs4 import BeautifulSoup
 from selenium.webdriver.common.alert import Alert
 from selenium.webdriver.support.ui import WebDriverWait
 from selenium.webdriver.support import expected_conditions as EC
 import numpy as np
 import pandas as pd

  
 ## 3. Blockwise stochastic regression imputation
 Since all predictor variables are time series data, it might not be appropriate to apply stochastic regression imputation directly. There would be too much loss of information. Therefore I create a blockwise stochastic regression imputation function via Python.
 ![imputation (en)](https://user-images.githubusercontent.com/108067353/199185234-976d1ee1-f67f-4211-8fd2-9aa7fcec8e3c.png)
 
 This is how it works:
 
![impute1](https://user-images.githubusercontent.com/108067353/199185499-ecf04d07-0aa8-4646-8863-acd952f032ee.png) ![impute2](https://user-images.githubusercontent.com/108067353/199185503-3110a8bb-b534-47aa-b16c-1718ed87270b.png)


## 4. dynamic time wrapping(dtw)
To classify regions by unique ecosystems(to track the flow of occurence of virus in each region and classify regions as having a same ecosystem if they have similiar occurence flow), dynamic time wrapping was deployed. 95% t-test was used in classification as well.
### packages

 import dtaidistance as dt
 
### t-test 
 def dtw_function(df, z=1.645):
        ...
        lowerCI=np.mean(loss)-z*np.std(loss)/len(loss)**(1/2)
        box=[]
        for idx in np.where(np.array(loss)<lowerCI)[0]:
            box.append(loc[idx])
        ...

 
### example
Here are wrapping loss plots that shows a part of dtw function. As you can see, regions that are close to each other(adjacent) have relatively small wrappig loss. This means they are truly under same ecosystem.

![dtw1](https://user-images.githubusercontent.com/108067353/199188097-c3403908-746e-4136-b929-e41117d71985.png)

This is final map of regional classification.

![regions(en)](https://user-images.githubusercontent.com/108067353/199189048-a57643e9-c604-45ae-a587-b77a08af8068.png)

## 5. weighted mean vector generation
Since bee's life span is 30-60 days, and it is shown that bees average death rate drastically increase between 12-14 days, I converted each predictor vector to 2-weeks weighted mean vector. 

![웨이트민](https://user-images.githubusercontent.com/108067353/199191229-dbce6fbd-48ab-41dd-9ab3-0572f5fefe4c.png)


## 6. penalized logisitic regression
there are 17 predictor variables in total. I deployed penalized logisitic regression(lasso and adaptive lasso) and also compared them with simple logistic regression.

### packages

  require(glmnet)
  require(pscl)


 
