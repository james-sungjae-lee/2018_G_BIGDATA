# -*- coding: utf-8 -*-
"""
Created on Wed Jul 26 14:38:33 2017

@author: woods
"""

# Lab 5 Logistic Regression Classifier (wine)page=108~110
import tensorflow as tf
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
tf.set_random_seed(777)  # for reproducibility
df_wine = pd.read_csv('http://archive.ics.uci.edu/ml/' 
                 'machine-learning-databases/wine/wine.data', header=None)
df_wine.columns = ['Class label', 'Alcohol', 'Malic acid', 'Ash', 
                   'Alcalinity of ash', 'Magnesium', 'Total', 'Flavanoids', 
                   'Nonfla', 'Proan', 'Color', 'Hue', 'Dwine', 'Proline']

print('Class label', np.unique(df_wine['Class label']))
df_wine.head()

#split test/train
from sklearn.cross_validation import train_test_split
X1 = df_wine.iloc[:,1:].values
Y1 = df_wine.iloc[:,[0]].values
Y1 = np.where(Y1 == 2, 0,1)
X_train, X_test, Y_train, Y_test = train_test_split(X1, Y1, test_size=0.3, random_state=0)


#정규화(normalization)
from sklearn.preprocessing import MinMaxScaler
mms = MinMaxScaler()
X_train_norm = mms.fit_transform(X_train)
X_test_norm = mms.transform(X_test)

#표준화(standardization)
from sklearn.preprocessing import StandardScaler
stdsc = StandardScaler()
X_train_std = stdsc.fit_transform(X_train)
X_test_std = stdsc.transform(X_test)


#x_data = X_train_std
x_data = X_train_norm
y_data = Y_train

print(x_data.shape, y_data.shape)

# placeholders for a tensor that will be always fed.
X = tf.placeholder(tf.float32, shape=[None, 13])
Y = tf.placeholder(tf.float32, shape=[None, 1])

W = tf.Variable(tf.random_normal([13, 1]), name='weight')
b = tf.Variable(tf.random_normal([1]), name='bias')

# Hypothesis using sigmoid: tf.div(1., 1. + tf.exp(tf.matmul(X, W)))
hypothesis = tf.sigmoid(tf.matmul(X, W) + b)

# cost/loss function
cost = -tf.reduce_mean(Y * tf.log(hypothesis) + (1 - Y) *
                       tf.log(1 - hypothesis))

train = tf.train.GradientDescentOptimizer(learning_rate=0.9).minimize(cost)

# Accuracy computation
# True if hypothesis>0.5 else False
predicted = tf.cast(hypothesis > 0.5, dtype=tf.float32)
accuracy = tf.reduce_mean(tf.cast(tf.equal(predicted, Y), dtype=tf.float32))

# Launch graph
with tf.Session() as sess:
    # Initialize TensorFlow variables
    sess.run(tf.global_variables_initializer())

    for step in range(500):
        cost_val, _ = sess.run([cost, train], feed_dict={X: x_data, Y: y_data})
        if step % 10 == 0:
            print(step, cost_val)

    # Accuracy report
#   h, c, a = sess.run([hypothesis, predicted, accuracy],
#                       feed_dict={X: x_data, Y: y_data})
#    print("\nHypothesis: ", h, "\nCorrect (Y): ", c, "\nAccuracy: ", a)
#    ht, ct, at = sess.run([hypothesis, predicted, accuracy],
#                      feed_dict={X: X_test_std, Y: Y_test})
#    print("\nHypothesis: ", ht, "\nCorrect (Y): ", ct, "\nAccuracy: ", at)    
    
    
    
    