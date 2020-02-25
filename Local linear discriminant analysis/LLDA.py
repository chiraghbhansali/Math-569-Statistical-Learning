#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis as LDA
from sklearn.neighbors.kde import KernelDensity


# In[2]:


d0 = pd.read_csv("digit0.txt", sep=',', header=None)
d0['y'] = 0
d1 = pd.read_csv("digit1.txt", sep=',', header=None)
d1['y'] = 1
d2 = pd.read_csv("digit2.txt", sep=',', header=None)
d2['y'] = 2


# In[3]:


combined = pd.concat([d0,d1,d2])


# In[4]:


def five_lambdas(lambdas):
    misclassifications = []
    for each_lambda in lambdas:
        ##kde = KernelDensity(kernel='epanechnikov', bandwidth=each_lambda)
        kdg = KernelDensity(kernel='gaussian', bandwidth=each_lambda)
        kdg.fit(combined.loc[:, combined.columns != 'y'])
        smooth = kdg.score_samples(combined.loc[:, combined.columns != 'y'])
        data = pd.DataFrame({'Dat': smooth, 'y': pd.concat([d0['y'],d1['y'],d2['y']])})
        data_x_train, data_x_test, data_y_train, data_y_test = train_test_split(data, pd.DataFrame(data['y']), test_size=0.3, stratify=data['y'])
        model = LDA()
        model.fit(data_x_train.loc[:, data_x_train.columns != 'y'], data_x_train['y'])
        misclassification = model.score(data_x_test.loc[:, data_x_test.columns != 'y'], data_y_test)
        misclassifications.append(misclassification)
    return misclassifications


# In[5]:


ans = five_lambdas([0.05,0.1,0.25,0.75,1])
ans


# In[ ]:




