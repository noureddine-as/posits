import numpy as np
import mnist
from model.network import Net
from PySigmoid import *
from model.posit_utils import *

print('Loadind data......')
num_classes = 10
train_images = mnist.train_images() #[60000, 28, 28]
train_labels = mnist.train_labels()
test_images = mnist.test_images()
test_labels = mnist.test_labels()

print('Preparing data......')
train_images = (train_images - np.mean(train_images))/np.std(train_images)
test_images = (test_images - np.mean(test_images))/np.std(test_images)
#train_images = train_images/255
#test_images = test_images/255
training_data = train_images.reshape(60000, 1, 28, 28)
training_labels = np.eye(num_classes)[train_labels]
testing_data = test_images.reshape(10000, 1, 28, 28)
testing_labels = np.eye(num_classes)[test_labels]

print('Converting to posits......')
set_posit_env(8, 0)
training_data = posify(training_data[:5])
testing_data = posify(testing_data[:5])

net = Net()
#print('Training Lenet......')
#net.train(training_data, training_labels, 100, 1, 'weights_posit.pkl')
#print('Testing Lenet......')
#net.test(testing_data, testing_labels, 100)
print('Testing with pretrained weights......')
net.test_with_pretrained_weights(testing_data, testing_labels, 1, 'pretrained_weights.pkl')
print('Predicting with pretrained weights......')
print(net.predict_with_pretrained_weights(testing_data[0], 'pretrained_weights.pkl'))