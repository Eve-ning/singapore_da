# -*- coding: utf-8 -*-
"""
Created on Mon Jul 15 12:39:08 2019

@author: johnc
"""

import pandas as pd
import matplotlib.pyplot as plt
from pandas.plotting import register_matplotlib_converters

# Register converter for date time
register_matplotlib_converters()

class WBTSG:
  def __init__(self, file_path):
    self.file_path = file_path
    self.data = None
    
  def data_load(self):
    '''Purely loads the data into the correct format'''
    
    with open(self.file_path, 'r') as f:
      data = pd.read_csv(f)
    
    data['wbt_date'] = pd.to_datetime(data['wbt_date'])
    self.data = data
    
    self.data['wbt_year'] = pd.DatetimeIndex(data['wbt_date']).year
    self.data['wbt_month'] = pd.DatetimeIndex(data['wbt_date']).month
    self.data['wbt_day'] = pd.DatetimeIndex(data['wbt_date']).day
    
  def data_groupby(self,
                   groups:str=['wbt_year', 'wbt_month'], axis = 0):
    self.data = self.data.groupby([groups]).max().reset_index()
    

data = WBTSG("../../../../../Data/singapore/wbt/main.csv")
data.data_load()
data.data_groupby()
wbt = data.data

plt.plot(data.data['wbt_date'][1:100], data.data['wet_bulb_temperature'][1:100])