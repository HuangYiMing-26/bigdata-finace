# 脚本信息----------------------------------------------------------------------
# 数据分析脚本
# created @ 2021.4.14 By HYM

# 数据分析和数据挖掘------------------------------------------------------------

# 思路：
# 1. 先用主成分分析法，降维和挖掘潜在结构（手动降维了）
# 2. 回归分析（OLS）
# 3. 机器学习预测（监督学习法）


# 剔除一些变量做分析


# 逻辑回归：仔细看了一下借款类型，因为Over due的数据量太少了，挺难用logisit回归
#fit <- glm(status~gender+marriage, family=binomial, data=loandata[loandata$status=="Over due"| loandata$status=="Closed",])




# 回归分析——先做基准回归，后面看看还怎么修改；结果挺好看的
fit1 <- lm(availableCredits~sumCreditPoint
           +age
           #+region
           #+gender
           #+officeDomain
           +hasHouse+houseLoan+hasCar+carLoan
           +workYears
           +marriage
           ,
           data=loandata[loandata$status != 'Failed',])
summary(fit1)

fit2 <- lm(availableCredits~sumCreditPoint
           +age
           +region
           #+gender
           #+officeDomain
           +hasHouse+houseLoan+hasCar+carLoan
           +workYears
           +marriage
           ,
           data=loandata[loandata$status != 'Failed',])

fit3 <- lm(availableCredits~sumCreditPoint
           +age
           +region
           +gender
           #+officeDomain
           +hasHouse+houseLoan+hasCar+carLoan
           +workYears
           +marriage
           ,
           data=loandata[loandata$status != 'Failed',])

fit4 <- lm(availableCredits~sumCreditPoint
           +age
           +region
           +gender
           +officeDomain
           +hasHouse+houseLoan+hasCar+carLoan
           +workYears
           +marriage
           ,
           data=loandata[loandata$status != 'Failed',])

stargazer(fit1,fit2,fit3,fit4,title = "results", align = F, type = "text", no.space = TRUE, out = "回归结果.html")
# plot(fit)


# 聚类(无监督学习)


# 决策树或随机森林(监督学习)——先做这个吧----------------------------------------

# 思路：
# 1. 将已完成和违约的70%-80%样本作为训练集全部放入随机森林中求出模型，求出准确度；
# 2. 将进行中的样本放入模型中进行预测，求出哪些未来会违约，哪些会完成；

# 定义随机种子
set.seed(1234)

# 数据归一化处理

loandata$S.age <- scale(loandata$age)
loandata$S.sumCreditPoint <- scale(loandata$sumCreditPoint)

# 选取用于预测的变量名
select.column <- c("officeDomain","region"
                   ,"S.sumCreditPoint","S.age"
                   #,"hasHouse","houseLoan","hasCar","carLoan"
                   ,"status")
loandata$status <- as.character(loandata$status) # 换成字符串容易抽样
loandata.randomforest <- loandata[loandata$status=="Closed" |
                                    loandata$status=="Over due",select.column]
loandata.randomforest$status <- factor(loandata.randomforest$status) # 抽样后换回因子
train <- sample(nrow(loandata.randomforest),0.7*nrow(loandata.randomforest))
df.train <- loandata.randomforest[train,]
df.validate <- loandata.randomforest[-train,]
table(df.train$status)
table(df.validate$status)
fit.forest <- randomForest(status~.,data = df.train,na.action = na.roughfix,importance=TRUE) # 拟合
sink('Output.txt',append = TRUE)
fit.forest
sink()
importance(fit.forest,type = 2) # 检验哪个变量重要
forest.pred <- predict(fit.forest,df.validate) # 预测
forest.pref <- table(df.validate$status,forest.pred,dnn = c("Actual","Predicted"))
sink('Output.txt',append = TRUE)
forest.pref # 准确率有98%
sink()

loandata.predict <- loandata[loandata$status=="In progress",select.column]
loandata.predict$status <- NULL # 删除标签类型
# loandata.predict$status <- factor(loandata.predict$status)
progress.pred <- predict(fit.forest,loandata.predict) # 预测
sink('Output.txt',append = TRUE)
table(progress.pred)
sink()


# 词云文本情绪分析--------------------------------------------------------------

f <- loandata$description[loandata$status == "Over due"]
seg <- qseg[f] #使用qseg类型分词，并把结果保存到对象seg中
seg <- seg[nchar(seg)>1] #去除字符长度小于2的词语
seg <- table(seg) #统计词频
seg <- seg[!grepl('[0-9]+',names(seg))] #去除数字
seg <- seg[!grepl('a-zA-Z',names(seg))] #去除字母
length(seg) #查看处理完后剩余的词数
seg <- sort(seg, decreasing = TRUE)[1:100] #降序排序，并提取出现次数最多的前100个词语
# seg #查看100个词频最高的
Overdue.data=data.frame(seg)
wordcloud2(Overdue.data, size = 1, minSize = 0, gridSize =  0,
           fontFamily = "微软雅黑", fontWeight = 'normal',
           color = "random-dark", backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, rotateRatio = 0.4,
           shape = 'circle', ellipticity = 0.65, widgetsize = NULL)


f <- loandata$description[loandata$status == "Closed"]
seg <- qseg[f] #使用qseg类型分词，并把结果保存到对象seg中
seg <- seg[nchar(seg)>1] #去除字符长度小于2的词语
seg <- table(seg) #统计词频
seg <- seg[!grepl('[0-9]+',names(seg))] #去除数字
seg <- seg[!grepl('a-zA-Z',names(seg))] #去除字母
length(seg) #查看处理完后剩余的词数
seg <- sort(seg, decreasing = TRUE)[1:100] #降序排序，并提取出现次数最多的前100个词语
# seg #查看100个词频最高的
Closed.data=data.frame(seg)

wordcloud2(Closed.data, size = 1, minSize = 0, gridSize =  0,
           fontFamily = "微软雅黑", fontWeight = 'normal',
           color = "random-dark", backgroundColor = "white",
           minRotation = -pi/4, maxRotation = pi/4, rotateRatio = 0.4,
           shape = 'circle', ellipticity = 0.65, widgetsize = NULL)