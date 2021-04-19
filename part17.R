# ===========================先导：导入包=======================================

# pkgs <- c("rpart","rpart.plot","party","randomForest","e1071")
# install.packages(pkgs,depend=TRUE)

# ============================17.1-数据准备=====================================
rm(list = ls()) # 清除所有变量
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc,ds,sep="")

breast <- read.table(url,sep=",",header=FALSE,na.strings = "?")
names(breast) <- c("ID","clumpThickness","sizeUniformity","shapeUniformity","maginalAdesion",
                   "singleEpithelialCellSize","bareNuclei","blandChromatin","normalNucleoli",
                   "mitosis","class")

df <- breast[-1] # 负号表示剔除那一列，此处就是剔除第一列
df$class <- factor(df$class,levels = c(2,4),labels = c("benign","malignant"))
set.seed(1234)
train <- sample(nrow(df),0.7*nrow(df)) # nrow是行数，这里就表示随机抽取哪几行
df.train <- df[train,] # 选取抽出来的那些行
df.validate <- df[-train,] # 预测集就是剔除的那些行
table(df.train$class) # 统计训练集中类型的数量
table(df.validate$class) # 统计预测集中类型的数量


# ============================17.5-随机森林=====================================
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(class~.,data = df.train,na.action = na.roughfix,importance=TRUE) # 拟合
fit.forest
importance(fit.forest,type = 2) # 检验哪个变量重要
forest.pred <- predict(fit.forest,df.validate) # 预测
forest.pref <- table(df.validate$class,forest.pred,dnn = c("Actual","Predicted")) 
forest.pref


# ============================17.6-支持向量机===================================
library(e1071)
set.seed(1234)
fit.svm <- svm(class~.,data = df.train)
fit.svm
svm.pred <- predict(fit.svm,na.omit(df.validate))
svm.pref <- table(na.omit(df.validate)$class,
                  svm.pred,dnn = c("Actual","Predicted"))
svm.pref
