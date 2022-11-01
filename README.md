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
 
 Since response variable(Sacbrrod virus occurence from 2017.09.22~2022.10.15) is tim
