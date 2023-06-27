# Python code


# Sklearn
# Tensorflow
# Tourche


ch#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct  7 16:12:01 2020

@author: bjoern
"""

#%matplotlib inline
import numpy as np
import os
os.getcwd()
os.chdir('/home/bjoern/Desktop/Studie/9. Semester/ML/')

class NN():
    
    def __init__(self, input_dim, hidden_size):
        output_size = 1
        self.W1 = np.random.rand(input_dim, hidden_size)
        self.b1 = np.random.rand(1, hidden_size)
        self.W2 = np.random.rand(hidden_size, output_size)
        self.b2 = np.random.rand(1, output_size)
        print('Neural net initialized with random values')
        
    def predict(self, X):    
        """ Evaluate the network on given data batch 
        
        np.maximum may come in handy
        
        Args:
        X: np.array shape (n, d)  Each row is a data point
        
        Output:
        pred: np.array shape (n, 1) output of network on each input point
        """
        # compute the following values
        pred = None # the output of neural net n x 1
        
        #X = np.random.rand(10, input_dim)
        #b1 = np.random.rand(1, hidden_size)
        tmp_test = np.dot(X,self.W1) + self.b1
        tmp_test = (tmp_test)*(tmp_test > 0)
        tmp_test = np.dot(tmp_test, self.W2) + self.b2
        pred = tmp_test
        
        ### YOUR CODE HERE
        ### END CODE
        return pred
    
    
    def score(self, X, y):
        """ Compute least squares loss (1/n sum (nn(x_i) - y_i)^2)
        
          X: np.array shape (n, d) - Data
          y: np.array shape (n, 1) - Targets

        """
        score = None
        score = 0
        #for i in range(len(y)):
            #print(i)
            #score = score + predict(X)
        n = len(y)
        score = sum((self.predict(X) - y)**2)/n
        ### YOUR CODE HERE
        ### END CODE
        return score
        
# random data test
def simple_test():
    input_dim = 3
    hidden_size = 8
    X = np.random.rand(10, input_dim)
    y = np.random.rand(10, 1)
    my_net = NN(input_dim=input_dim, hidden_size=hidden_size)

    nn_out = my_net.predict(X)
    print('shape of nn_out', nn_out.shape) # should be n x 1
    print('least squares error: ', my_net.score(X, y))
    
# actual data test
def housing_test():
    from sklearn.preprocessing import StandardScaler
    from  sklearn.datasets import fetch_california_housing
    rdata = fetch_california_housing()
    s = StandardScaler()
    Xr = rdata.data
    yr = rdata.target   
    print('data size:', len(yr), 'num features:', Xr.shape[1])
    s.fit(Xr)
    X_scaled = s.transform(Xr)
    house_net = NN(input_dim=Xr.shape[1], hidden_size=8)
    weights = np.load('good_weights.npz')
    house_net.W1 = weights['W1']
    house_net.W2 = weights['W2']
    house_net.b1 = weights['b1'].reshape(1, -1)
    house_net.b2 = weights['b2'].reshape(1, -1)
    print('hidden layer size:', house_net.W1.shape[1])
    lsq = house_net.score(X_scaled, yr.reshape(-1, 1))
    pred = house_net.predict(X_scaled)
    print('mean house price least squares error:', lsq)
    #print('5 house prediction:\nestimated price , true price')
    #print(np.c_[house_net.predict(X_scaled[0:5, :]), yr[0:5]])

simple_test()
housing_test()





import torch
z = torch.zeros(1, requires_grad=True)
sz = 1.0 / (1+ torch.exp(-z))
sz.backward() # compute gradient of sz relative to z in this case
print('Gradient of f(z)=1/(1 + e^{-z}) at z = 0', z.grad)




import numpy as np 
import torch # the torch tensor library
#import torchviz

## CREATE SOME DATA
n = 100
x1 = np.random.rand(n)
x2 = np.random.rand(n)
## Grahm schmidt process - ignore
x1 = x1/np.linalg.norm(x1)
x2 = x2/np.linalg.norm(x2)
x2 = x2 - np.dot(x1, x2) * x1 #
x2 = x2/np.linalg.norm(x2)

## CREATE THE DATA MATRIX
a = 4.0
D = np.c_[x1, a*x2]
# CREATE TARGET FUNCTION VECTOR
y = x1 + x2

# MAKE TORCH VARIABLES TO USE
X = torch.from_numpy(D).double()
ty = torch.from_numpy(y).double()
ni = torch.tensor(1./n, dtype=torch.double)

def torch_gd():
    w = torch.tensor([42.0, 2.0], dtype=torch.double)
    lr = torch.tensor(10.0/a, dtype=torch.double)
    epochs = 42
    cost_hist = []
    for i in range(epochs):
        w.requires_grad_() # say i want gradient relative to w in upcoming computation
        cost = None
        ### YOUR CODE HERE - Compute Ein for linear regression as function of w and store in cost 1 - 4 lines
        #cost = np.sum((np.dot(X,w) - y)**2)/n
        cost = 1/n * torch.sum((X @ w- ty)**2)
        #cost = 1/n * torch.norm(X@w - ty)**2
        ### END CODE
        cost_hist.append(cost)
        print('epoch {0}: Cost {1}'.format(i, cost))
        cost.backward() # compute gradient cost as function of w
        w = w - lr * w.grad # update w
        w.detach_() # removes w from current computation graph
    print('best w found', w)
torch_gd()



print('Do the python forward and backward pass here')
x1 = 3.0
x2 = 1.0 
y = 9.
w1 = 1.0
w2 = 2.0
w3 = 1.0
### YOUR CODE HERE
### END CODE
tmp_test = w3*w1*(w1*x1 + w2*x2)

#forward
a = x1*w1
b = x2*w2
c = a + b
d = c * (c > 0)
e = w3*d
e_end = (y - e)**2

#backward


print('Do the python forward and backward pass here')
x1 = 3.0
x2 = 1.0 
y = 9.
w1 = 1.0
w2 = 2.0
w3 = 1.0
### YOUR CODE HERE
# Long Forward Pass
z1 = w1 * x1
z2 = w2 * x2
hin = z1 + z2
hout = max(hin, 0)
pred = w3 * hout
diff = pred - y
e = diff**2
print('z1', z1)
print('z2', z2)
print('hin', hin)
print('hout', hout)
print('pred', pred)
print('diff', diff)
print('ls error', e)

# backwards pass
de_diff = 2 * diff
ddiff_pred = 1
de_pred = de_diff * ddiff_pred
dpred_w3 = hout
de_w3 = de_pred * dpred_w3 # dout_w3 = rs
dpred_hout = w3
de_hout = de_pred * dpred_hout
dhout_hin = hin > 0
de_hin = de_hout * dhout_hin
dhin_z1 = 1
dhin_z2 = 1
de_z1 = de_hin * dhin_z1
de_z2 = de_hin * dhin_z2
dz1_w1 = x1
dz2_w2 = x2
print('dz', dz1_w1, dz2_w2)
de_w1 = de_z1 * dz1_w1
de_w2 = de_z2 * dz2_w2
print('d_diff', de_diff)
print('d_pred', de_diff)
print('d_hout', de_diff)
print('d_hin', de_diff)
print('d_w1:', de_w1)
print('d_w2:', de_w2)
print('d_w3:', de_w3)

### END CODE







import torch
from torchviz import make_dot # install this package 

x1 = torch.tensor([[3.]])
x2 = torch.tensor([[1.]])
y = torch.tensor([9.])
W1 = torch.tensor([[1.]], requires_grad=True)
W2 = torch.tensor([[2.]], requires_grad=True)
W3 = torch.tensor([[1.]], requires_grad=True)
### YOUR CODE HERE - The clamp function may be usefull
nn_x = W3 * torch.clamp(W1 * x1 + W2*x2, min=0)
loss = (y - nn_x).pow(2)
loss.backward()
### END CODE
# print the graph - change naming appropriately
print('d_w1', W1.grad)
print('d_w2', W2.grad)
print('d_w3', W3.grad)
print('Lets show the computation graph')
make_dot(loss, params={'W1': W1, 'W2': W2, 'W3': W3})




import numpy as np
h_in = np.array([[-1, 2, 4]])
d_hout = np.array([[1,2,3]])
print('shapes:', h_in.shape, d_hout.shape)
def relu_grad(d_hout, hin):
    d_hin = None
    ### YOUR CODE HERE
    d_hin = d_hout.copy()
    d_hin[hin<0] = 0
    #d_hin = d_hin * (hin<0)
    
    ### END CODE
    return d_hin

def sigmoid_grad(d_hout, hin):
    d_hin = None
    ### YOUR CODE HERE
    def sigmoid(x):
        return 1./(1+np.exp(-x))
    d_hin =  d_hout * (sigmoid(hin)*(1-sigmoid(hin))) # entrywise multiplication
    ### END CODE
    return d_hin

print('d_hin relu:', relu_grad(d_hout, h_in))
# should be [0, 2, 3]
print('d_hin sigmoid:', sigmoid_grad(d_hout, h_in))
# should be ~ [0.196..., 0.209..., 0.052...]


import torch
from torch import optim
from sklearn.datasets import load_boston
from sklearn import linear_model

print('*'*5, 'Load and Prepare Data', '*'*5)
dataset = load_boston()
# print('dataset', dataset)
X, y = dataset.data, dataset.target
X = (X - X.mean(axis=0))/(X.std(axis=0))
#print('data stats', X.shape, X.mean(axis=0), X.std(axis=0))
ridge=linear_model.Ridge(alpha=0.1, fit_intercept=True)
ridge.fit(X, y)
# print(ridge.coef_, ridge.intercept_)
print('\n', '*'*5, 'Test Sklearn Ridge Regression for Comparison', '*'*5)
print('Ridge Regression Score:', ((ridge.predict(X)-y)**2).mean())

print('\n', '*'*5, 'Make data to torch tensors', '*'*5)
tX = torch.from_numpy(X).float()
ty = torch.from_numpy(y).float().view(-1, 1)


class LR():
    
    def __init__(self):
        pass
        
    def cost(self, X, y, w, b, c=0):
        """ Compute Regularized Least Squares Loss
        
          X: torch.tensor shape (n, d) - Data
          y: torch.tensor shape (n, 1) - Targets
          w: torch.tensor shape (d, 1) - weights
          b: torch.tensor shape (1, 1) - bias weight
          c: scalar, weight decay parameter 
          
          returns (regularized) cost tensor        
        """
        loss = None
        ### YOUR CODE HERE
        pred = X @ w + b 
        loss = torch.mean((pred-y)**2) 
        reg_loss = torch.sum(w**2)
        ### END CODE
        return loss + c * reg_loss
    
    def fit(self, X, y, c=0):
        """ GD Learning Algorithm for Ridge Regression with pytorch
        
        Args:
         X: torch.tensor shape (n, d)
         y: torch.tensor shape (n, 1)
         c: ridge regression weight decay parameter (lambda)
        """
        w = torch.zeros(X.shape[1], 1, requires_grad=True)
        b = torch.zeros(1, 1, requires_grad=True)
        sgd = optim.SGD(params={w, b}, lr=0.1)
        for i in range(100):
            sgd.zero_grad()
            loss = self.cost(X, y, w, b, c=c)
            if i % 10 == 0:
                print('epoch:', i, 'least squares (regularized loss)', loss.item())
            loss.backward()
            sgd.step()
        self.w = w.clone()
        self.b = b.clone()


    def score(self, X, y):
        """ Compute least squares cost for model 
        
        Args:
         X: torch.tensor shape (n, d)
         y: torch.tensor shape (n, 1)
         
        returns least squares score of model on data X with targets y
        """
        score = self.cost(X, y, self.w, self.b, c=0)
        return score

print('\n', '*'*5, 'Run Torch Linear Regression Gradient Descent', '*'*5)

tlr = LR()
tlr.fit(tX, ty, 0.1)
print('pytorch Linear Regression Regression least squares score:', tlr.score(tX, ty).item())




import torch
from torch import optim
from sklearn.datasets import load_boston
from sklearn import linear_model

dataset = load_boston()
X, y = dataset.data, dataset.target
X = (X - X.mean(axis=0))/(X.std(axis=0))
tX = torch.from_numpy(X).float()
ty = torch.from_numpy(y).float().view(-1, 1)

class NN():
    
    def __init__(self):
        pass

    def cost(self, X, y, W1, b1, W2, b2, c=0):
        """ Compute (Regularized) Least Squares Loss of neural net
        The clamp function may be usefull
        
          X: torch.tensor shape (n, d) - Data
          y: torch.tensor shape (n, 1) - Targets
          W1: torch.tensor shape (d, h) - weights
          b1: torch.tensor shape (1, h) - bias weight
          W2: torch.tensor shape (h, 1) - weights
          b2: torch.tensor shape (1, 1) - bias weight
          c: ridge regression weight decay parameter 
    
        returns (weight decay) cost tensor
        """
   
        loss = None
        ### YOUR CODE HERE
        hin = X @ W1 + b1
        hout = hin.clamp(min=0)
        pred = hout @ W2 + b2
        loss = torch.mean((pred-y)**2) 
        reg_loss = c * torch.sum(W1**2) + c * torch.sum(W2**2)
        loss = loss + reg_loss
        ### END CODE
        return loss
    
    def fit(self, X, y, hidden_size=32, c=0.01):   
        """ GD Learning Algorithm for Ridge Regression with pytorch
        
         Args:
         X: torch.tensor shape (n, d)
         y: torch.tensor shape (n, 1)
         hidden_size: int 
         c: float weight decay parameter (lambda)
        """
        input_dim = X.shape[1]        
        W1 = torch.randn(input_dim, hidden_size, requires_grad=True)
        b1 = torch.randn(1, hidden_size, requires_grad=True)
        W2 = torch.randn(hidden_size, 1, requires_grad=True)
        b2 = torch.randn(1, 1, requires_grad=True)
        ### YOUR CODE HERE
        sgd = optim.SGD(params={W1, W2, b1, b2}, lr=0.01)
        for i in range(100):
            sgd.zero_grad()
            loss = self.cost(X, y, W1, b1, W2, b2, c=c)
            if i % 10 == 0:
                print('epoch:', i, 'nn least squares loss', loss.item())
            loss.backward()
            sgd.step()
        ### END CODE
        self.W1 = W1
        self.W2 = W2
        self.b1 = b1
        self.b2 = b2
        
    def score(self, X, y):
        """ Compute least squares cost for model 
        
        Args:
         X: torch.tensor shape (n, d)
         y: torch.tensor shape (n, 1)
         
        returns least squares score of model on data X with targets y
        """
        score = self.cost(X, y, self.W1, self.b1, self.W2, self.b2, c=0)
        return score


net = NN()
net.fit(tX, ty, hidden_size=16, c=0.01)
print('pytorch Neural Net Regression least squares score:', net.score(tX, ty).item())


