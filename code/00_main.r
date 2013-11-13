# 00_main.r
# :purpose: Run clustering on ta-feng dataset for demo
# :author: jon.sedar@applied.ai
# :date: Thu 03 Oct 2013
# :note: Roughly 700 LOC throughout all scripts


### header ---------------------------------------------------------------------
require(stringr)

### locations
proj <- str_c("~/Documents/workspace/dublinR/DublinR_ClusteringMiniProject/")
dirs <- c(str_c(proj,"code/"),str_c(proj,"data/"),str_c(proj,"img/"))
names(dirs) <- c("code","data","img")

require(data.table)
require(Hmisc)
require(ggplot2)
require(scales)
require(grid)
require(reshape2)
require(lubridate)
require(mclust)
require(mix)
require(zoo)


### 01 load data ---------------------------------------------------------------
source(str_c(dirs['code'],'01_sourcing.r'))
dt <- get_data()


### 02 basic checks, final cleaning, viz ---------------------------------------
source(str_c(dirs['code'],'02_exploration.r'))


### 03 feature creation --------------------------------------------------------
source(str_c(dirs['code'],'03_featurecreation.r'))


### 00 pitstop to load / reload data post feature creation ---------------------
cst <- data_loader(str_c(dirs['data'],"cst.rds"),loadraw=F)
write.csv(cst,str_c(dirs['data'],'cst.csv'),row.names=F,quote=F)


### 04 feature selection -------------------------------------------------------
source(str_c(dirs['code'],'04_featureselection.r'))



### 05 Run EM with GMM ---------------------------------------------------------
source(str_c(dirs['code'],'05_clustering.r'))


