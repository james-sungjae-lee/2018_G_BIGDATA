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

df_wine.head()

print('Class label', np.unique(df_wine['Class label']))

#split test/train
from sklearn.cross_validation import train_test_split
X1 = df_wine.iloc[:,1:]
Y1 = df_wine.iloc[:,[0]]
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
x_data = X_train_std
y_data = Y_train

print(x_data.shape, y_data.shape)

# placeholders for a tensor that will be always fed.
X = tf.placeholder(tf.float32, shape=[None, 13])
Y = tf.placeholder(tf.int32, shape=[None, 1])

print(np.unique(y_data))
y_classes = 3

Y_one_hot = tf.one_hot(Y, y_classes)
Y_one_hot = tf.reshape(Y_one_hot, [-1, y_classes])

W = tf.Variable(tf.random_normal([13, 1]), name='weight')
b = tf.Variable(tf.random_normal([1]), name='bias')

logits = tf.matmul(X, W) + b
hypothesis = tf.nn.softmax(tf.matmul(X, W) + b)
cost_i = tf.nn.softmax_cross_entropy_with_logits(logits=logits, labels=Y_one_hot)


cost = tf.reduce_mean(cost_i)
optimizer = tf.train.GradientDescentOptimizer(learning_rate=0.001).minimize(cost)
prediction = tf.argmax(hypothesis, 1)
correct_prediction = tf.equal(prediction, tf.argmax(Y_one_hot, 1))
accuracy = tf.reduce_mean(tf.cast(correct_prediction, tf.float32))

x_data = x_data.iloc[:]
y_data

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
    sess.run(optimizer, feed_dict={X: X_train, Y: Y_one_hot})
    loss, acc = sess.run([cost, accuracy], feed_dict={X: x_data, Y: y_data})



    for step in range(2000):
        sess.run(optimizer, feed_dict={X: X_train, Y: Y_one_hot})
        if step % 100 == 0:
            loss, acc = sess.run([cost, accuracy], feed_dict={
                                 X: x_data, Y: y_data})
            print("Step: {:5}\tLoss: {:.3f}\tAcc: {:.2%}".format(
                step, loss, acc))

    # Let's see if we can predict
    pred = sess.run(prediction, feed_dict={X: X_test})
    # y_data: (N,1) = flatten => (N, ) matches pred.shape
    for p, y in zip(pred, Y_test.flatten()):
        print("[{}] Prediction: {} True Y: {}".format(p == int(y), p, int(y)))



# Hypothesis using sigmoid: tf.div(1., 1. + tf.exp(tf.matmul(X, W)))
hypothesis = tf.sigmoid(tf.matmul(X, W) + b)

# cost/loss function
cost = -tf.reduce_mean(Y * tf.log(hypothesis) + (1 - Y) *
                       tf.log(1 - hypothesis))

train = tf.train.GradientDescentOptimizer(learning_rate=0.0001).minimize(cost)

# Accuracy computation
# True if hypothesis>0.5 else False
predicted = tf.cast(hypothesis > 0.5, dtype=tf.float32)
accuracy = tf.reduce_mean(tf.cast(tf.equal(predicted, Y), dtype=tf.float32))

# Launch graph
with tf.Session() as sess:
    # Initialize TensorFlow variables
    sess.run(tf.global_variables_initializer())

    for step in range(10001):
        cost_val, _ = sess.run([cost, train], feed_dict={X: x_data, Y: y_data})
        if step % 100 == 0:
            print(step, cost_val)

    # Accuracy report
    h, c, a = sess.run([hypothesis, predicted, accuracy],
                       feed_dict={X: x_data, Y: y_data})
    print("\nHypothesis: ", h, "\nCorrect (Y): ", c, "\nAccuracy: ", a)
    ht, ct, at = sess.run([hypothesis, predicted, accuracy],
                      feed_dict={X: X_test_std, Y: Y_test})
    print("\nHypothesis: ", ht, "\nCorrect (Y): ", ct, "\nAccuracy: ", at)
    
    
    
    
    