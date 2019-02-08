# -*- coding: utf-8 -*-
"""
Created on Fri Feb  8 14:25:53 2019

@author: kobyr
"""

import pandas as pd
import matplotlib.pyplot as plt
# Read data from file 'filename.csv' 
# (in the same directory that your python process is based)
# Control delimiters, rows, column names with read_csv (see later) 
data = pd.read_csv("errors.txt") 
# Preview the first 5 lines of the loaded data 
#data.head()

plt.plot(data, linewidth=1)
plt.xlabel("Iterations  (x100)")
plt.ylabel("Loss")
plt.margins(0)
plt.xlim(0,600)
plt.ylim(0,2.5)
plt.savefig("Loss_FP.png")
plt.show()