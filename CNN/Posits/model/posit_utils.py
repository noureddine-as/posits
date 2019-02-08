import numpy as np
from PySigmoid import *

def posify(x):
    if type(x) == np.ndarray:
        if len(x.shape) == 1:
            return np.array([Posit(y) for y in x])
        else:
            posit_array = []
            for i, _ in enumerate(x):
                posit_array.append(posify(x[i]))
            return np.array(posit_array)
    else: # if type(x) == float | int
        return Posit(x)
    
def posit_sigmoid(x):
    if type(x) == np.ndarray:
        if len(x.shape) == 1:
            return np.array([(y).sigmoid() for y in x])
        else:
            posit_array = []
            for i, l in enumerate(x):
                posit_array.append(posit_sigmoid(l))
            return np.array(posit_array)
    else:
        return (x).sigmoid()
    
##Test
#set_posit_env(8, 0)
#
#sigm = (lambda x: 1 / (1 + np.e ** (-x)))
#
#W = np.random.rand(3,2,3)
#print(sigm(W))
#Wp = posify(W)
#print(posit_sigmoid(Wp))
#print("*******")
#print(np.abs(sigm(W) - posit_sigmoid(Wp)))
#print(np.argmax(Wp))
        
set_posit_env(8,0)
my_posits = posify(np.random.rand(2,2))
out = posify(np.zeros((5, 4)))
out[1:3, 1:3] = my_posits
print((out))


##inputs = np.random.rand(5,8)
##weights = np.random.rand(8,3)
##bias = np.random.rand(3,1)
##a = inputs.dot(weights)+bias.T
##print(a)
### Posit checking
##i = posify(inputs)
##w = posify(weights)
##b = posify(bias)
##A = Posit.fused_matmult(i, w) + b.T
##print(A)
###print(np.abs(a-A))
##print(np.abs((1 / (1 + np.e ** (-a))) - posit_sigmoid(A)))
##print(np.max(A))
##print(A.reshape(3, 5))
##print(posit_sigmoid(A))
#u = np.random.rand(3,1)
#x = posify(u)
#print(np.exp(u))
#print(np.e**x)
#print((np.e**x).reshape(3))
#print(np.sum(x))
#if x.shape[1] == 1:
#    x = x.reshape(3)
#print(Posit.fused_sum(x))