
# coding: utf-8

# In[1]:


# set variables
import mxnet as mx
import os
import numpy as np
from mxnet import gluon
from mxnet import nd
from time import time
from mxnet.gluon import nn
from mxnet.gluon.model_zoo import vision as models

model_path = '/home/carsmart/users/xiaoming/yichewang/model/model'
model_iteration = 0
image_path = '/home/carsmart/users/xiaoming/yichewang/mytest/'
meta_name_path = '/home/carsmart/users/xiaoming/yichewang/number_name_label.csv'
resize = 224
data_shape = (3, 224, 224)
(mean_r, mean_g, mean_b) = (0.4914, 0.4822, 0.4465)
(std_r, std_g, std_b) = (0.2023, 0.1994, 0.2010)

import argparse

parser = argparse.ArgumentParser(description='manual to this script')
parser.add_argument('--gpus', type=int, default = 1)
parser.add_argument('--top_k', type=int, default = 3)
args = parser.parse_args()
ctx = [mx.gpu(args.gpus)]
top_k = args.top_k

num_test = len(os.listdir(image_path))
batch_size = num_test if num_test <= 64 else 64
assert num_test > 0, 'num_test should greater than 0'
assert batch_size % len(ctx) == 0, 'batch_size % len(ctx) should be 0'


# In[2]:


# model
net_resnet50 = models.get_model(name = 'resnet50_v1', pretrained = False)
def get_net():
    net = nn.HybridSequential()
    with net.name_scope():
        net.add(net_resnet50.features)
        net.add(nn.Dense(1062))
    return net

net = get_net()

net.collect_params().load('/home/carsmart/users/xiaoming/yichewang/model/resnet50/75.params', ctx = ctx)


# In[3]:


# data
os.system('ldconfig /usr/local/cuda/lib64')
os.system('rm -rf ./temp_lst')
os.system('mkdir ./temp_lst')
os.system('python3 /home/carsmart/mxnet/tools/im2rec.py ' +           '--list TRUE --recursive TRUE --test-ratio 1 ' +           './temp_lst/test ' +           image_path)
os.system('python3 /home/carsmart/mxnet/tools/im2rec.py ' +           '--pass-through TRUE --center-crop TRUE ' +           './temp_lst/test ' +           image_path)

test_iter = mx.io.ImageRecordIter(
        path_imglist = './temp_lst/test.lst', 
        path_imgrec = './temp_lst/test.rec', 
        resize = resize, 
        data_shape = data_shape, 
        batch_size = batch_size, 
        rand_mirror = False, 
        rand_crop = False, 
        mean_r = mean_r, 
        mean_g = mean_g, 
        mean_b = mean_b, 
        std_r = std_r, 
        std_g = std_g, 
        std_b = std_b
    )


# In[4]:


# predict
def get_batch(batch, ctx):
    """return data and label on ctx"""
    if isinstance(batch, mx.io.DataBatch):
        data = batch.data[0]
        label = batch.label[0]
    else:
        data, lable = batch
    return (gluon.utils.split_and_load(data, ctx), 
           gluon.utils.split_and_load(label, ctx), 
           data.shape[0])

test_iter.reset()
outputs = []
for i, batch in enumerate(test_iter):
    real_size = batch_size - batch.pad
    data, _, _ = get_batch(batch, ctx)
    outputs.append(np.concatenate(
        [mx.nd.softmax(net(x), axis = 1).as_in_context(mx.cpu()).asnumpy()[0:real_size, :] for x in data]))
if len(outputs) > 1:
    outputs = np.concatenate(outputs)
else:
    outputs = outputs[0]
pred_label = np.argsort(- outputs, axis = 1)


# In[5]:


import pandas as pd

file_num_name_label = '/home/carsmart/users/xiaoming/yichewang/number_name_label_select.csv'
num_name_label = pd.read_csv(file_num_name_label, encoding='utf-8')
file_images = open('./temp_lst/test.lst')
image_name = file_images.readlines()
os.system('rm -rf ./temp_lst')

top_label = pred_label[:, :top_k]


# In[6]:


for i in range(top_label.shape[0]):
    print(image_name[i].strip().split('\t')[2])
    out = num_name_label.iloc[top_label[i, :]]
    out['pred'] = outputs[i, top_label[i, :]]
    print(out, '\n')


# In[ ]:




