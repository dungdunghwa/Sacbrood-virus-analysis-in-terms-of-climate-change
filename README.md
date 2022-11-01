# Sacbrood-virus-analysis-in-terms-of-climate-change
Descriptive analysis of relationship between Sacbrood virus and Climate change

## Content
1. flow chart
2. crawling
3. blockwise stochastic regression imputation
4. dynamic time wrapping
5. penalized logistic regression

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
  
  
 ## 2. Blockwise stochastic regression imputation
 Since all predictor variables are time series data, it might not be appropriate to apply stochastic regression imputation directly. There would be too much loss of information. Therefore I create a blockwise stochastic regression imputation function via Python.
 ![imputation (en)](https://user-images.githubusercontent.com/108067353/199185234-976d1ee1-f67f-4211-8fd2-9aa7fcec8e3c.png)
 
 This is how it works:
 
![impute1](https://user-images.githubusercontent.com/108067353/199185499-ecf04d07-0aa8-4646-8863-acd952f032ee.png) ![impute2](https://user-images.githubusercontent.com/108067353/199185503-3110a8bb-b534-47aa-b16c-1718ed87270b.png)


## 3. dynamic time wrapping
To classify regions by unique ecosystems(to track the flow of occurence of virus in each region and classify regions as having a same ecosystem if they have similiar occurence flow), dynamic time wrapping was used.
### packages

 import dtaidistance as dt
 
### example
![dtw1](https://user-images.githubusercontent.com/108067353/199188097-c3403908-746e-4136-b929-e41117d71985.png) ![dtw2](https://user-images.githubusercontent.com/108067353/199188102-b4da5e72-2696-48c6-a7e9-e7a9f74afbd7.png)
