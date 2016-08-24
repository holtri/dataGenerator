# -*- coding: utf-8 -*-
"""
Created on Thu Aug 11 14:18:09 2016

@author: elekes
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

#number of dimensions
dim=20
#number of generated clusters
num_clusters=3
#number of points in each cluster
num_points_per_cluster=10
#should the cluster sizes be the same or not
cluster_size_randomized=True
#number of noise points
num_noise_points=3
#the vectors we will form the clusters around
cluster_middle=np.random.randn(num_clusters,dim)
#setting the relevant (correlated) dimensions
corr_dims=np.zeros(dim)
for i in range(3):   #set how many should be correlated
    corr_dims[np.random.randint(0,dim-1)]=1

#generating random vectors
vecs=[]

for i in range(num_clusters):
    size_multiplier=1
    if cluster_size_randomized:
        size_multiplier = np.random.rand()*10
    vecs.append(np.random.randn(int(num_points_per_cluster*size_multiplier), dim))
    for vector in vecs[i]:
        vector += cluster_middle[i]*np.random.randint(18,21)*corr_dims #YOU CAN SET THIS TO HAVE MORE DENSE (BIGGER NUMBERS) OR LESS DENSE CLUSTERS(SMALLER NUMBER)
        vector /= (corr_dims*10)+1
vecs=np.concatenate(vecs)

#adding noise
noise=np.random.randn(num_noise_points,dim)*np.max(vecs)/3
vecs=np.concatenate((vecs,noise),axis=0)

#NORMALIZING IF YOU WANT
vecs = (vecs.transpose() / np.linalg.norm(vecs, axis=1)).transpose()

#correlated dimensions
[x[0] for x in enumerate(corr_dims) if x[1] >0]

#plotting the relevant dimension projections
x=[x[8] for x in vecs]
y=[x[14] for x in vecs]
z=[x[15] for x in vecs]
#plt.scatter(x,y)
#plt.show()

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(x, y, z, c='r', marker='o')
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('z')
plt.show()

#little more mashup, if needed (randomly changeing some values from correlated dimensions to random numbers in some of the vectors)
#this ruins 1/100 of the values in vecs:
for cnt in range(int(len(vecs)*sum(corr_dims)/100)):
    i=np.random.randint(0,len(vecs))
    j=[i for i,x in enumerate(corr_dims) if x==1]
    vecs[i,j]=np.random.randn()
    
#writing out with fictive words( numbers will be our words), if you dont want words, set index=False
df=pd.DataFrame(vecs)
df.to_csv("generated_data.txt", sep=' ', index=False, header=None)

#you have to have a header usually for loading text models, i usually do this with bash:
#sed -i '1i$NUM_ROWS $DIMENSION' generated_data.txt
