# -*- coding: utf-8 -*-
"""
Created on Fri Jul 28 11:46:10 2017

@author: woods
"""
#simple kmeans cluster p322 
import tensorflow as tf
#import pandas as pd
#import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import make_blobs
X, Y = make_blobs(n_samples = 150, n_features=2, centers=3, cluster_std = 0.5,
                  shuffle = True, random_state=0)

plt.scatter(X[:,0],X[:,1], c='red', marker = 'x', s=20 )
plt.grid()
plt.show()

#sk kmeans cluster p324
from sklearn.cluster import KMeans
km = KMeans(n_clusters = 3, init='random', n_init=10, max_iter=300, tol=1e-04, random_state=0)
Y_km=km.fit_predict(X)

sess = tf.Session()
sess.run(tf.global_variables_initializer())
accuracy = tf.reduce_mean(tf.cast(tf.equal(Y_km, Y), dtype=tf.float32))
print("accuracy ", sess.run(accuracy))
    
#center와 함께 시각화 p326
plt.scatter(X[Y_km==0,0], X[Y_km==0,1], c='lightgreen',
            marker = 's', s=50, label='cluster 1' )

plt.scatter(X[Y_km==1,0], X[Y_km==1,1], c='orange',
            marker = 'o', s=50, label='cluster 2' )

plt.scatter(X[Y_km==2,0], X[Y_km==2,1], c='lightblue',
            marker = 'v', s=50, label='cluster 3' )

plt.scatter(km.cluster_centers_[:,0],
            km.cluster_centers_[:,1], c='red',
            marker = '*', s=250, label='centroids' )

plt.legend()
plt.grid()
plt.show()


