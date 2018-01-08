# cars_classification
爬取汽车图片，然后使用resnet-50训练汽车的分类器。车辆总共1062类。
- 汽车图片来自易车网。
- 使用了R version 3.4.3
- python3.5.2
- 安装了GPU版本的mxnet,mxnet的相关教程可以访问[mxnet教程](http://zh.gluon.ai/chapter_preface/index.html).

1. [pictures_prepare.R](https://github.com/linxiaoming1993/cars_classification/blob/master/pictures_prepare.R)是一个图片爬取，存储以及整理的过程，按照要求修改path即可。最后需要调用./mxnet/tools/im2rec.py生成lst和rec文件。

2. [train.ipynb](https://github.com/linxiaoming1993/cars_classification/blob/master/train.ipynb)是模型训练的过程。

3. [predict.py](https://github.com/linxiaoming1993/cars_classification/blob/master/predict.py)是模型预测的过程。
