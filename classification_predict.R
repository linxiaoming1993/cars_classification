# 图片预测
# 需要三个参数，参数一为图片所在文件夹的地址，参数二位模型所在地址
# 参数三位模型对应的iteration
# 下段程序可以直接运行，只需要修改image_path(它是文件夹)
# 最终结果，车牌存储在order_meta_name，对应的概率存储在sorts
PreprocImage <- function(im, mean.img = array(0, dim = c(224, 224, 3))) {
  # crop the image
  shape <- dim(im)
  short.edge <- min(shape[1:2])
  yy <- floor((shape[1] - short.edge) / 2) + 1
  yend <- yy + short.edge - 1
  xx <- floor((shape[2] - short.edge) / 2) + 1
  xend <- xx + short.edge - 1
  croped <- im[yy:yend, xx:xend,,]
  # resize to 224 x 224, needed by input of the model.
  if(length(dim(croped)) > 2){
    dim(croped) <- c(dim(croped)[1:2], 1, dim(croped)[3])
    noise <- imager::imnoise(224, 224, cc = 3, mean = 0, sd = 0.01)
  }else{
    dim(croped) <- c(dim(croped)[1:2], 1, 1)
    noise <- imager::imnoise(224, 224, cc = 1, mean = 0, sd = 0.01)
  }
  resized <- resize(croped, 224, 224)
  
  return(resized)
}
CarsPredict <- function(image_path, model_path, model_iteration, k = 5, meta_name_path){
  library('mxnet')
  library('magrittr')
  library('imager')
  library('stringr')
  model <- mx.model.load(model_path, iteration = model_iteration)
  if(!str_detect(image_path, '/$')){
    image_path <- paste0(image_path, '/') 
  }
  image_list <- paste0(image_path, '/', list.files(image_path))
  num_image <- length(image_list)
  data <- array(integer(0), dim = c(224, 224, 3, num_image))
  for(i in 1:num_image){
    img <- (load.image(image_list[i]) * 255) %>% PreprocImage()
    class(img) <- 'integer'
    # data[, , ,(4 * (i - 1) + 1):(4 * i)] <- aperm(img, c(1, 2, 4, 3))
    data[, , ,i] <- aperm(img, c(1, 2, 4, 3))
    if(!i %% 200){
      print(i)
      gc()
    }
  }
  # 预测
  preds <- predict(model, data / 255, ctx = mx.gpu(1))
  
  # 标签载入
  meta_name <- read.csv(meta_name_path, row.names = 1, stringsAsFactors = F)
  orders <- apply(preds, 2, function(x) order(x, decreasing = TRUE))[1:k, ]
  sorts <- apply(preds, 2, function(x) sort(x, decreasing = TRUE))[1:k, ]
  if(num_image == 1){
    order_meta_name <- meta_name$meta_name[match(orders, meta_name$label)]
  }else{
    order_meta_name <- array(meta_name$meta_name[match(orders, meta_name$label)], dim = dim(orders))
  }
  return(list(car = order_meta_name, probability = sorts))
}

model_path <- '/home/carsmart/users/xiaoming/models/yichewang/resnet-50/model'
model_iteration <- 30
image_path <- '/home/carsmart/users/xiaoming/yichewang/mytest'
meta_name_path <- '/home/carsmart/users/xiaoming/yichewang/number_name_label.csv'
cars_predict <- CarsPredict(image_path, model_path, model_iteration, meta_name_path = meta_name_path)

