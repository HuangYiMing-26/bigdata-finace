# 脚本信息----------------------------------------------------------------------
# 描述性统计
# created @ 2021.4.14 By HYM

# 描述性统计--------------------------------------------------------------------

# 思路：
# - 看一下不同借款状态的人数分布情况；
# - 对未逾期和逾期的人进行简单的数据描述，并且画图；
# - 探究两两类型变量之间是否有相关性；

# 描述分析是最基本的分析统计方法，在实际工作中也是应用最广的分析方法。分为两大类：
# 数据描述和指标统计
# 数据描述：用来对数据进行基本情况的刻画，包括：数据总数、时间跨度、时间粒度、空间范围、
# 数据来源等。如果是建模，那么还要看数据的极值、分布、离散度等内容。
# 指标统计：用来做报告，分析实际情况的数据指标，可粗略分为四大类：
# 变化、分布、对比、预测；




# 全样本的借款状态人数分布情况--------------------------------------------------

# 描述一下数据的总数

table(loandata$status)
sink('Output.txt',append = FALSE)
prop.table(table(loandata$status))*100 # 显示比例
sink()
plot(loandata$status, main="样本中借款状态的情况",xlab="借款状态",ylab="频数")

sink('Output.txt',append = TRUE)
prop.table(table(loandata$status[loandata$status=='Over due'|loandata$status=='Closed']))*100
sink()

# 探讨不同借款状态的性别分布
sink('Output.txt',append = TRUE)
xtabs(~ gender+status,data=loandata)
sink()
ggplot(data=loandata, aes(x=status,fill=gender)) +  
  geom_bar() + labs(title = "借款状态的性别分布情况",x="借款状态",y="人数") + 
  mytheme
ggsave(file='借款状态的性别分布情况.png',width = 5,height = 4)


# 探讨不同婚恋状态和借款状态的分布
sink('Output.txt',append = TRUE)
xtabs(~marriage + status, data=loandata)
sink()
ggplot(data=loandata, aes(x=status,fill=marriage)) +  
  geom_bar() + labs(title = "借款状态的婚恋分布情况",x="借款状态",y="人数") + 
  mytheme
ggsave(file='借款状态的婚恋分布情况.png',width = 5,height = 4)

# 探讨不同借款状态、性别和婚恋的关系
sink('Output.txt',append = TRUE)
xtabs(~ gender+marriage+status,data=loandata)
sink()

# 探讨不同借款类型和借款状态之间的关系
sink('Output.txt',append = TRUE)
xtabs(~ borrowType+status,data=loandata)
sink()
ggplot(data=loandata, aes(x=status,fill=borrowType)) +  
  geom_bar() + labs(title = "借款状态的类型分布情况",x="借款状态",y="人数") + 
  mytheme
ggsave(file='借款状态的类型分布情况.png',width = 5,height = 4)

# 探讨不同工作单位和借款状态之间的关系
sink('Output.txt',append = TRUE)
xtabs(~ officeType+status,data=loandata)
sink()
ggplot(data=loandata, aes(x=status,fill=officeType)) +  
  geom_bar() + labs(title = "借款状态的工作单位分布情况",x="借款状态",y="人数") + 
  mytheme
ggsave(file='借款状态的工作单位分布情况.png',width = 5,height = 4)

# 探讨不同工作类型和借款状态之间的关系
sink('Output.txt',append = TRUE)
xtabs(~ jobType+status,data=loandata)
sink()
ggplot(data=loandata, aes(x=status,fill=jobType)) +  
  geom_bar() + labs(title = "借款状态的工作类型分布情况",x="借款状态",y="人数") + 
  mytheme
ggsave(file='借款状态的工作类型分布情况.png',width = 5,height = 4)

# 探讨不同行业和借款状态之间的关系
sink('Output.txt',append = TRUE)
xtabs(~ officeDomain+status,data=loandata)
sink()
ggplot(data=loandata, aes(x=status,fill=officeDomain)) +  
  geom_bar() + labs(title = "借款状态的行业分布情况",x="借款状态",y="人数") + 
  mytheme
ggsave(file='借款状态的行业分布情况.png',width = 5,height = 4)

# 探讨学历和借款状态之间的关系
sink('Output.txt',append = TRUE)
xtabs(~ graduation+status,data=loandata)
sink()
ggplot(data=loandata, aes(x=status,fill=graduation)) +  
  geom_bar() + labs(title = "借款状态的学历分布情况",x="借款状态",y="人数") + 
  mytheme
ggsave(file='借款状态的学历分布情况.png',width = 5,height = 4)


# 探讨工资和借款状态之间的关系
sink('Output.txt',append = TRUE)
xtabs(~ salary+status,data=loandata)
sink()
ggplot(data=loandata, aes(x=status,fill=salary)) +  
  geom_bar() + labs(title = "借款状态的工资分布情况",x="借款状态",y="人数") + 
  mytheme
ggsave(file='借款状态的工资分布情况.png',width = 5,height = 4)

# 探讨地区和借款状态之间的关系
sink('Output.txt',append = TRUE)
xtabs(~ region+status,data=loandata)
sink()
ggplot(data=loandata, aes(x=status,fill=region)) +  
  geom_bar() + labs(title = "借款状态的省份分布情况",x="借款状态",y="人数") + 
  mytheme
ggsave(file='借款状态的省份分布情况.png',width = 5,height = 4)

# 探讨工作年限和借款状态之间的关系
sink('Output.txt',append = TRUE)
xtabs(~ workYears+status,data=loandata)
sink()
ggplot(data=loandata, aes(x=status,fill=workYears)) +  
  geom_bar() + labs(title = "借款状态的工作年限分布情况",x="借款状态",y="人数") + 
  mytheme
ggsave(file='借款状态的工作年限分布情况.png',width = 5,height = 4)

# 房、车、房贷、车贷和借款状态的关系
sink('Output.txt',append = TRUE)
xtabs(~ hasHouse+houseLoan+hasCar+carLoan+status,data=loandata)
sink()



# 探讨年龄和借款状态之间的关系
sink('Output.txt',append = TRUE)
xtabs(~ age+status,data=loandata)
sink()

# 各个变量之间的分布大概情况
# summary(loandata)


# 画三种借款状态的审批额散点图
ggplot(data=loandata[loandata$status=="Over due",],aes(x=c(1:nrow(loandata[loandata$status=="Over due",])),y=availableCredits),col=pal_npg())+geom_point(aes(col=gender))+labs(title = "逾期状态的借款额",x="ID",y="借款额")+ mytheme
ggsave(file='逾期状态的借款额.png',width = 5,height = 4)
ggplot(data=loandata[loandata$status=="Closed",],aes(x=c(1:nrow(loandata[loandata$status=="Closed",])),y=availableCredits))+geom_point(aes(col=gender))+labs(title = "借款结束借款额",x="ID",y="借款额") + mytheme
ggsave(file='借款结束借款额.png',width = 5,height = 4)
ggplot(data=loandata[loandata$status=="In progress",],aes(x=c(1:nrow(loandata[loandata$status=="In progress",])),y=availableCredits))+geom_point(aes(col=gender))+labs(title = "借款进行中借款额",x="ID",y="借款额") + mytheme
ggsave(file='借款进行中借款额.png',width = 5,height = 4)

# 画年龄和借款状态图
x <- xtabs(~ age+status,data=loandata)
x <- data.frame(x)
ggplot(data=x,aes(x=age, y=Freq,fill = status))+
  geom_bar(position = 'dodge',stat="identity")+
  labs(title = "不同年龄的借款状态分布",x="年龄",y="频数") + 
  mytheme
ggsave(file='不同年龄的借款状态分布.png')

# 对审批数额的描述性统计--------------------------------------------------------

sink('Output.txt',append = TRUE)
describe(availableCredits~status,data=loandata)
sink()



