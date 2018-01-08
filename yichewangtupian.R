library('RCurl')
library('rvest')
library('magrittr')
library('stringr')
library('data.table')
library('parallel')

# 存储有易车网各个品牌车链接的url
url <- 'http://api.car.bitauto.com/CarInfo/getlefttreejson.ashx?tagtype=baojia&pagetype=masterbrand&objid=2'
header <- 'http://photo.bitauto.com'
path <- 'C:/Users/xiaoming/Documents/yichewangtupian/'

master <- getURL(url) %>% str_extract_all('id:(.*?)url') %>% .[[1]] %>% str_split('[:,]') %>% 
  lapply(function(x){
    data.frame(id = x[2], name = substr(x[4], 2, nchar(x[4]) - 1), stringsAsFactors = F)
  })
master <- do.call(rbind, master)
write.csv(master, paste0(path, 'meta_name.csv'), row.names = F)

SerialYearStyle <- function(serial_id, serial){
  # 由serial_url得到serial_year_style_url
  serial_url <- serial$url[serial_id]
  serial_data <- read_html(serial_url) %>% html_nodes('body') %>% html_nodes('div.treeMainv1') %>% 
    html_nodes('ul.nav') %>% as.character() %>% str_extract_all('href(.*)[0-9]款') %>% unlist()
  if(is.null(serial_data)){
    return('no photo')
  }
  serial_year_url <- header %>%
    paste0(serial_data %>% str_extract('/(.*?)photoanchor'))
  year <- serial_data %>% str_extract('[0-9]+款')
  serial_year_style <- lapply(1:length(serial_year_url), function(year_id, serial_year_url, year){
    serial_year_data <- read_html(serial_year_url[year_id]) %>% html_nodes('dl.dl-sty-set') %>% 
      html_nodes('ul#carmodelcontainer') %>% html_nodes('div.color-w-box') %>% html_children()
    serial_year_style_url <- serial_year_data %>% html_attr('href')
    serial_year_style <- serial_year_data %>% html_children() %>% html_attr('title') %>% na.omit() %>% as.character()
    tryCatch(data.frame(brand = serial$brand[serial_id], serial = serial$serial[serial_id], cartype = serial$cartype[serial_id], year = year[year_id], serial_year_style = serial_year_style, url = paste0(header, serial_year_style_url), stringsAsFactors = F), error = function(e) return('no photo'))
  }, serial_year_url, year)
  serial_year_style <- do.call(rbind, serial_year_style)
  serial_year_style
}

SerialYearStylePhoto <- function(style_id, serial_year_style, path, master_id){
  if(! file.exists(paste0(path, master_id, '/', style_id))){
    dir.create(paste0(path, master_id, '/', style_id))
  }
  photo_id <<- 1
  serial_year_style_url <- serial_year_style$url[style_id]
  appearance_url <- read_html(serial_year_style_url) %>% html_nodes('h5.h5-sep') %>% 
    str_extract('href(.*?)外观') %>% na.omit %>% str_extract('/model(.*?)photoanchor')
  appearance_data <- paste0(header, appearance_url) %>% read_html()
  photo_url <- appearance_data %>% html_nodes('div.row') %>% html_nodes('div.col-xs-3') %>% 
    html_nodes('div.img') %>% html_children() %>% html_children() %>% html_attr('src')
  lapply(1:length(photo_url), function(x, master_id){
    try(downloader::download(photo_url[x], paste0(path, master_id, '/', style_id, '/', photo_id, '.jpg'), mode = 'wb', quiet = TRUE), silent = T)
    photo_id <<- photo_id + 1
  }, master_id)
  while(1){
    next_page <- appearance_data %>% html_nodes('div.pagination') %>% html_nodes('a.next_on') %>% html_attr('href')
    if(length(next_page)){
      appearance_data <- paste0(header, next_page) %>% read_html()
      photo_url <- appearance_data %>% html_nodes('div.row') %>% html_nodes('div.col-xs-3') %>% 
        html_nodes('div.img') %>% html_children() %>% html_children() %>% html_attr('src')
      lapply(1:length(photo_url), function(x, master_id){
        try(downloader::download(photo_url[x], paste0(path, master_id, '/', style_id, '/', photo_id, '.jpg'), mode = 'wb', quiet = TRUE), silent = T)
        photo_id <<- photo_id + 1
      }, master_id)
    }else{
      break
    }
  }
  Sys.sleep(sample(1:3, 1))
}

MasterPhoto <- function(master_id, master, path){
  library('RCurl')
  library('rvest')
  library('magrittr')
  library('stringr')
  # 由master_url得到内部的photo_url
  master_id <- master$id[master_id]
  master_url <- paste0(header, '/master/', master_id, '/')
  if(! file.exists(paste0(path, master_id))){
    dir.create(paste0(path, master_id))
  }
  master_data <- read_html(master_url) %>% html_nodes('body') %>% html_nodes('div.main-inner-section')
  brand_name <- master_data %>% html_nodes('h5.h5-sep') %>% html_text() %>% str_replace_all('>>', '')
  if(! length(brand_name)){
    brand_name <- '0'
  }
  serial <- lapply(1:length(brand_name), function(brand_id, x, brand_name){
    if(! length(x)){
      return(0)
    }
    x <- x[[brand_id]] %>% html_nodes('div.col-xs-3')
    cartype <- x %>% html_attr('cartype')
    serial_name <- x %>% html_nodes('li.name') %>% html_text()
    serial_url <- x %>% html_nodes('li.name') %>% html_children() %>% html_attr('href')
    data.frame(brand = brand_name[brand_id], serial = serial_name, cartype = cartype, url = paste0(header, serial_url), stringsAsFactors = F)
  }, master_data %>% html_nodes('div.row'), brand_name)
  serial <- do.call(rbind, serial)
  if(class(serial) == 'matrix'){
    return(0)
  }
  serial_year_style <- lapply(1:nrow(serial), SerialYearStyle, serial)
  serial_year_style <- do.call(rbind, serial_year_style)
  if(serial_year_style[1, ] == 'no photo' || (length(which(serial_year_style$brand == 'no photo')))){
    if(is.null(names(serial_year_style))){
      return(0)
    }
    serial_year_style <- serial_year_style[-which(serial_year_style$brand == 'no photo'), ]
  }
  if(nrow(serial_year_style) == 0){
    return(0)
  }
  serial_year_style$master <- master_id
  write.csv(serial_year_style, paste0(path, 'serial_year_style', '/', master_id, '.csv'))
  lapply(1:nrow(serial_year_style), function(style_id, serial_year_style, path, master_id){
    tryCatch(SerialYearStylePhoto(style_id, serial_year_style, path, master_id), error = function(e){
      Sys.sleep(2)
      e
    })
  }, serial_year_style, path, master_id)

  # 停5秒
  Sys.sleep(5)
  serial_year_style
}

master_serial_year_style <- lapply(155:nrow(master), function(master_id, master, path){
  tryCatch(MasterPhoto(master_id, master, path), error = function(e) return('0'))
}, master, path)

## 整理meta_name_all
files_serial_year_style <- list.files(paste0(path, 'serial_year_style'))
meta_name1 <- lapply(1:length(files_serial_year_style), function(x){
  temp <- read.csv(paste0('C:/Users/xiaoming/Documents/yichewangtupian/serial_year_style/', files_serial_year_style[x]), stringsAsFactors = F, row.names = 1)
  temp$file_num <- 1:nrow(temp)
  num <- lapply(1:nrow(temp), function(y){
    length(list.files(paste0('C:/Users/xiaoming/Documents/yichewangtupian/', str_split(files_serial_year_style[x], '\\.')[[1]][1], '/', y)))
  })
  temp$picture_num <- unlist(num)
  temp
})
meta_name1 <- do.call(rbind, meta_name1)
names(meta_name) <- c('master', 'master_name')
meta_name_all <- merge(meta_name, meta_name1)
write.csv(meta_name_all, paste0(path, 'meta_name_all.csv'), row.names = F)

# 整理图片，按照master_brand_serial存储图片。
meta_name_all <- read.csv(paste0(path, 'meta_name_all.csv'), stringsAsFactors = F)
file_name <- paste(meta_name_all$master_name, meta_name_all$brand, meta_name_all$serial, sep = '_') %>% unique()
dir.create(paste0(path, 'data'))
# 手动把所有master图片文件移动到data文件里面去
dir.create(paste0(path, 'master_brand_serial'))
lapply(file_name, function(file_name){
  dir.create(paste0('C:/Users/xiaoming/Documents/yichewangtupian/master_brand_serial/', file_name))
})
lapply(1:nrow(meta_name_all), function(num, meta_name_all){
  file_name <- meta_name_all[num, ]
  list_pictures <- list.files(paste0('C:/Users/xiaoming/Documents/yichewangtupian/data/', file_name$master, '/', file_name$file_name))
  file.copy(paste0('C:/Users/xiaoming/Documents/yichewangtupian/data/', file_name$master, '/', file_name$file_name, '/', list_pictures), 
            paste0('C:/Users/xiaoming/Documents/yichewangtupian/master_brand_serial/', 
                   paste(file_name$master_name, file_name$brand, file_name$serial, sep = '_')))
  file.rename(paste0('C:/Users/xiaoming/Documents/yichewangtupian/master_brand_serial/', 
                     paste(file_name$master_name, file_name$brand, file_name$serial, sep = '_'), '/', list_pictures), 
              paste0('C:/Users/xiaoming/Documents/yichewangtupian/master_brand_serial/', 
                     paste(file_name$master_name, file_name$brand, file_name$serial, sep = '_'), '/', file_name$file_name, '_', list_pictures))
}, meta_name_all)

## 整理选择出来的图片的num
name_num <- read.csv(paste0(path, 'name_num.csv'), stringsAsFactors = F)
file_path <- paste0(path, 'master_brand_serial_select')
list_file <- list.files(file_path)
lapply(list_file, function(file_name, name_num){
  file.rename(paste0(file_path, '/', file_name), paste0(file_path, '/', name_num$num[match(file_name, name_num$name)]))
}, name_num)

num <- list.files(paste0(path, 'master_brand_serial_select'))
file.rename(paste0(path, 'master_brand_serial_select/', num), 
            paste0(path, 'master_brand_serial_select/', stringr::str_pad(num, 4, 'left', '0')))

# 程序报错，原因是有些图片爬取过程出错了。
# 将不能读取的图片删除
library(imager)
file_path <- paste0(path, 'master_brand_serial_select/')
dim_picture <- matrix(0L, ncol = 3, nrow = 1012806)
name_picture <- character(1012806)
i <- 1
file_name <- list.files(file_path)
lapply(file_name, function(file_name, file_path){
  picture_path <- paste0(file_path, file_name, '/', list.files(paste0(file_path, file_name)))
  lapply(picture_path, function(picture_path){
    print(i)
    name_picture[i] <<- picture_path
    try(dim_picture[i, ] <<- dim(load.image(picture_path))[-3], TRUE)
    i <<- i + 1
  })
}, file_path)
delete <- which(dim_picture[, 1] == 0)
delete_name <- name_picture[delete]
# > length(delete)
# [1] 78
write.csv(delete_name, paste0(path, 'delete_name.csv'))
file.remove(delete_name)

# 将图片从本地复制到带有GPU的机器上，然后运行
# sudo python3 /home/carsmart/mxnet/tools/im2rec.py --list TRUE --recursive TRUE --train-ratio 0.8 --test-ratio 0 /home/carsmart/users/xiaoming/yichewang/train_lst/train /home/carsmart/users/xiaoming/yichewang/master_brand_serial_select/
# sudo python3 /home/carsmart/mxnet/tools/im2rec.py --pass-through TRUE --center-crop TRUE /home/carsmart/users/xiaoming/yichewang/train_lst/train /home/carsmart/users/xiaoming/yichewang/master_brand_serial_select/
