# 脚本属性 ---------------------------------------------------------------------

## 这是数据预处理文件。
## created @ 2021.4.7 by HYM
## written By R-4.0.5 via UTF-8
## using Macos-11.2


# 清理内存变量------------------------------------------------------------------

rm(list=ls())



# 读取数据 ---------------------------------------------------------------------

# 数据是ANSI编码格式
# 手动将文件名改成：Loan data，并且将文件的编码改为UTF8。

rawdata <- read.csv("Loan data.csv", header = TRUE, sep = ",", fill = TRUE, 
                    encoding = "UTF-8",stringsAsFactors = FALSE,na.strings = c(""))


# 数据清洗 ---------------------------------------------------------------------

# 目的：
# 1. 将标签类型的变量转换成因子
# 2. 将乱七八糟的描述转换成合适的描述
# 3. 处理缺失值
#   1. 类别型变量的空值全都用“未填写”填补了；
#   2. 尝试一下不用“未填写”填补，即留空；



# 剔除无用的变量
mydata <- names(rawdata) %in% c("userId","nickName","realName","idNo","office",
                                "interestDate","closeTime","userLoanRecordId","carBrand"
                                ,"repayType","homeTown","university","loanId"
                                ,"title","displayLoanType","verifyState"
                                ,"productName","readyTime","repaidByGuarantorTime"
                                ,"hasOthDebt","repaySource","productRepayType","overDueDate"
                                ,"graduatedYear","officeScale","position","city","borrowStatus"
                                ,"totalCount","successCount","alreadyPayCount","failedCount"
                                ,"notPayoffCount","overdueCount","withdrawAmount","repaidTotalAmount"
                                ,"alreadyPayPrincipal",'notPayPrincipal','alreadyPayInterest'
                                ,'notPayInterest','overdueAmount','interest','months','surplusAmount'
                                ,'leftMonths','repaidByGuarantor','borrowStatus','borrowAmount'
                                ,'amount','creditLevel')
# 因为城市太多了，不考虑城市

newdata <- rawdata[!mydata]
loandata <- newdata

# 关闭旧数据库，以省空间
rm(rawdata)
rm(mydata)
rm(newdata)

# 保存数据库


# R语言可以自动将分类变量处理为虚拟变量

# attach(loandata)

# 将字符变量转换成因子

# 处理性别---------------------------------------------------------------------

loandata$gender[loandata$gender == "男"] <- "male"
loandata$gender[loandata$gender == "女"] <- "female"
loandata$gender <- factor(loandata$gender,ordered = FALSE,labels = c("male","female"))

# 处理婚恋---------------------------------------------------------------------

loandata$marriage[loandata$marriage=="None"] <- NA
loandata$marriage <- factor(loandata$marriage,ordered = FALSE,labels = c("Divorced","Married","Unmarried","Widowed"))

# 处理学历---------------------------------------------------------------------

loandata$graduation[loandata$graduation == ", \u672c\u79d1"] <- "本科" # ，本科
loandata$graduation[loandata$graduation == "\u672c\u79d1,"] <- "本科" # 本科，
loandata$graduation[loandata$graduation == ", \u5927\u4e13"] <- "大专" # ，大专
loandata$graduation[loandata$graduation == ", \u9ad8\u4e2d\u6216\u4ee5\u4e0b"] <- "高中或以下" # ，高中或以下
loandata$graduation[loandata$graduation == "\u672c\u79d1"] <- "本科" # 本科
loandata$graduation[loandata$graduation == "\u5927\u4e13"] <- "大专" # 大专
loandata$graduation[loandata$graduation == "\u5927\u4e13,"] <- "大专" # 大专，
loandata$graduation[loandata$graduation == "\u5927\u4e13, \u9ad8\u4e2d\u6216\u4ee5\u4e0b"] <- "5" 
loandata$graduation[loandata$graduation == "\u9ad8\u4e2d\u6216\u4ee5\u4e0b"] <- "高中或以下" # 高中或以下
loandata$graduation[loandata$graduation == "\u9ad8\u4e2d\u6216\u4ee5\u4e0b,"] <- "高中或以下" # 高中或以下，
loandata$graduation[loandata$graduation == "\u5176\u4ed6\u501f\u6b3e"] <- "其他" # 其他
loandata$graduation[loandata$graduation == "\u6295\u8d44\u521b\u4e1a"] <- "其他" 
loandata$graduation[loandata$graduation == "\u7814\u7a76\u751f\u6216\u4ee5\u4e0a"] <- "研究生或以上" # 研究生或以上
loandata$graduation[loandata$graduation == "\u88c5\u4fee\u501f\u6b3e"] <- "其他" 
loandata$graduation[is.na(loandata$graduation)] <- "其他"
loandata$graduation[loandata$graduation == "其他"] <- NA
loandata$graduation <- factor(loandata$graduation,levels = c("研究生或以上","本科","大专","高中或以下"), labels = c("Postgraduate or above","Undergraduate","Junior college","Senior high school or below"))

# 处理出生日期-----------------------------------------------------------------

loandata$birthDay <- as.Date(loandata$birthDay,format='%Y/%m/%d')

# 处理评级分-------------------------------------------------------------------

loandata$sumCreditPoint <-as.numeric(loandata$sumCreditPoint)

# loandata$creditLevel <- factor(loandata$creditLevel,level=c("AA","A","B","C","D","E","HR"))

# 处理可贷额度-----------------------------------------------------------------

loandata$availableCredits <- as.numeric(loandata$availableCredits)

# 处理工作年限-----------------------------------------------------------------

loandata$workYears[loandata$workYears == ", 1-3\u5e74（\u542b）"] <- "1-3年(含)" # 1-3年(含)
loandata$workYears[loandata$workYears == ", 1\u5e74（\u542b）\u4ee5\u4e0b"] <- "1年（含）以下" # 1年（含）以下
loandata$workYears[loandata$workYears == ", 3-5\u5e74（\u542b）"] <- "3-5年（含）" # 3-5年（含）
loandata$workYears[loandata$workYears == ", 5\u5e74\u4ee5\u4e0a"] <- "5年以上" # 5年以上
loandata$workYears[loandata$workYears == "1-3\u5e74（\u542b）"] <- "1-3年(含)" # 1-3年(含)
loandata$workYears[loandata$workYears == "1-3\u5e74（\u542b）,"] <- "1-3年(含)" # 1-3年(含)
loandata$workYears[loandata$workYears == "1-3\u5e74（\u542b） ,"] <- "1-3年(含)" # 1-3年(含)
loandata$workYears[loandata$workYears == "1-3\u5e74（\u542b）, 1-3\u5e74（\u542b）"] <- "1-3年(含)" # 1-3年(含)
loandata$workYears[loandata$workYears == "1\u5e74（\u542b）\u4ee5\u4e0b"] <- "1年（含）以下" # 1年（含）以下
loandata$workYears[loandata$workYears == "1\u5e74（\u542b）\u4ee5\u4e0b,"] <- "1年（含）以下" # 1年（含）以下
loandata$workYears[loandata$workYears == "1\u5e74（\u542b）\u4ee5\u4e0b, 1\u5e74（\u542b）\u4ee5\u4e0b"] <- "1年（含）以下" # 1年（含）以下
loandata$workYears[loandata$workYears == "3-5\u5e74（\u542b）"] <- "3-5年（含）" # 3-5年
loandata$workYears[loandata$workYears == "3-5\u5e74（\u542b）,"] <- "3-5年（含）" # 3-5
loandata$workYears[loandata$workYears == "3-5\u5e74（\u542b）, 3-5\u5e74（\u542b）"] <- "3-5年（含）" # 3-5
loandata$workYears[loandata$workYears == "3-5\u5e74（\u542b）, 5\u5e74\u4ee5\u4e0a"] <- "5年以上" # 5+
loandata$workYears[loandata$workYears == "5\u5e74\u4ee5\u4e0a"] <- "5年以上" # 5+
loandata$workYears[loandata$workYears == "5\u5e74\u4ee5\u4e0a ,"] <- "5年以上" # 5+
loandata$workYears[loandata$workYears == "5\u5e74\u4ee5\u4e0a,"] <- "5年以上" # 5+
loandata$workYears[loandata$workYears == "5\u5e74\u4ee5\u4e0a, 5\u5e74\u4ee5\u4e0a"] <- "5年以上" # 5+
loandata$workYears[loandata$workYears == "\u65e0"] <- "无工作经验" # 无工作经验
#loandata$workYears[is.na(loandata$workYears)] <- "未填写" # 未填写
#loandata$workYears[loandata$workYears == "未填写"] <- NA # 未填写
loandata$workYears <- factor(loandata$workYears,levels = c("5年以上","3-5年（含）","1-3年(含)","1年（含）以下","无工作经验"),labels = c("More than 5 years","3-5 years (inclusive)","1-3 years (inclusive)","Less than 1 year (inclusive)","No working experience"))

# 处理相关的布尔类型------------------------------------------------------------
loandata$hasHouse <- factor(loandata$hasHouse)
loandata$houseLoan <- factor(loandata$houseLoan)
loandata$hasCar <- factor(loandata$hasCar)
loandata$carLoan <- factor(loandata$carLoan)

# 处理工作类型------------------------------------------------------------------

loandata$jobType[loandata$jobType == "\u8bf7\u9009\u62e9"] <- "未填写"
loandata$jobType[loandata$jobType == "\u7f51\u5546"] <- "网络商家"
loandata$jobType[loandata$jobType == "未填写"] <- NA
loandata$jobType <- factor(loandata$jobType,ordered = FALSE,labels = c("The working class","Other","Private business owner","The electronic commerce"))

#处理工作地点类型---------------------------------------------------------------

loandata$officeType[loandata$officeType == ", \u5730\u65b9\u56fd\u8d44\u59d4\u76f4\u5c5e\u4f01\u4e1a"] <- "地方国资委直属企业"
loandata$officeType[loandata$officeType == ", \u4e2a\u4f53\u7ecf\u8425\u8005"] <- "个体经营者"
loandata$officeType[loandata$officeType == ", \u5176\u4ed6"] <- "其他"
loandata$officeType[loandata$officeType == ", \u4e16\u754c500\u5f3a（\u5305\u62ec\u5408\u8d44\u4f01\u4e1a\u53ca\u4e0b\u7ea7\u5355\u4f4d）"] <- "世界500强（包括合资企业及下级单位）"
loandata$officeType[loandata$officeType == ", \u4e8b\u4e1a\u5355\u4f4d"] <- "事业单位"
loandata$officeType[loandata$officeType == ", \u5916\u8d44\u4f01\u4e1a（\u5305\u62ec\u5408\u8d44\u4f01\u4e1a）"] <- "外资企业（包括合资企业）"
loandata$officeType[loandata$officeType == ", \u592e\u4f01（\u5305\u62ec\u4e0b\u7ea7\u5355\u4f4d）"] <- "央企（包括下级单位）"
loandata$officeType[loandata$officeType == ", \u4e00\u822c\u6c11\u8425\u4f01\u4e1a"] <- "一般民营企业"
loandata$officeType[loandata$officeType == ", \u4e00\u822c\u4e0a\u5e02\u516c\u53f8（\u5305\u62ec\u56fd\u5916\u4e0a\u5e02）"] <- "一般上市公司（包括国外上市）"
loandata$officeType[loandata$officeType == "\u5730\u65b9\u56fd\u8d44\u59d4\u76f4\u5c5e\u4f01\u4e1a"] <- "地方国资委直属企业"
loandata$officeType[loandata$officeType == "\u5730\u65b9\u56fd\u8d44\u59d4\u76f4\u5c5e\u4f01\u4e1a,"] <- "地方国资委直属企业"
loandata$officeType[loandata$officeType == "\u5730\u65b9\u56fd\u8d44\u59d4\u76f4\u5c5e\u4f01\u4e1a, \u5730\u65b9\u56fd\u8d44\u59d4\u76f4\u5c5e\u4f01\u4e1a"] <- "地方国资委直属企业"
loandata$officeType[loandata$officeType == "\u4e2a\u4f53\u7ecf\u8425\u8005"] <- "个体经营者"
loandata$officeType[loandata$officeType == "\u4e2a\u4f53\u7ecf\u8425\u8005, \u4e2a\u4f53\u7ecf\u8425\u8005"] <- "个体经营者"
loandata$officeType[loandata$officeType == "\u4e2a\u4f53\u7ecf\u8425\u8005, \u4e00\u822c\u6c11\u8425\u4f01\u4e1a"] <- "个体经营者"
loandata$officeType[loandata$officeType == "\u56fd\u5bb6\u673a\u5173"] <- "国家机关"
loandata$officeType[loandata$officeType == "\u5176\u4ed6"] <- "其他"
loandata$officeType[loandata$officeType == "\u4e16\u754c500\u5f3a（\u5305\u62ec\u5408\u8d44\u4f01\u4e1a\u53ca\u4e0b\u7ea7\u5355\u4f4d）"] <- "世界500强（包括合资企业及下级单位）"
loandata$officeType[loandata$officeType == "\u4e16\u754c500\u5f3a（\u5305\u62ec\u5408\u8d44\u4f01\u4e1a\u53ca\u4e0b\u7ea7\u5355\u4f4d）, \u4e16\u754c500\u5f3a（\u5305\u62ec\u5408\u8d44\u4f01\u4e1a\u53ca\u4e0b\u7ea7\u5355\u4f4d）"] <- "世界500强（包括合资企业及下级单位）"
loandata$officeType[loandata$officeType == "\u4e8b\u4e1a\u5355\u4f4d"] <- "事业单位"
loandata$officeType[loandata$officeType == "\u4e8b\u4e1a\u5355\u4f4d ,"] <- "事业单位"
loandata$officeType[loandata$officeType == "\u4e8b\u4e1a\u5355\u4f4d,"] <- "事业单位"
loandata$officeType[loandata$officeType == "\u4e8b\u4e1a\u5355\u4f4d, \u4e8b\u4e1a\u5355\u4f4d"] <- "事业单位"
loandata$officeType[loandata$officeType == "\u5916\u8d44\u4f01\u4e1a（\u5305\u62ec\u5408\u8d44\u4f01\u4e1a）, \u5916\u8d44\u4f01\u4e1a（\u5305\u62ec\u5408\u8d44\u4f01\u4e1a）"] <- "外资企业（包括合资企业）"
loandata$officeType[loandata$officeType == "\u592e\u4f01（\u5305\u62ec\u4e0b\u7ea7\u5355\u4f4d）, \u592e\u4f01（\u5305\u62ec\u4e0b\u7ea7\u5355\u4f4d）"] <- "央企（包括下级单位）"
loandata$officeType[loandata$officeType == "\u4e00\u822c\u6c11\u8425\u4f01\u4e1a ,"] <- "一般民营企业"
loandata$officeType[loandata$officeType == "\u4e00\u822c\u6c11\u8425\u4f01\u4e1a,"] <- "一般民营企业"
loandata$officeType[loandata$officeType == "\u4e00\u822c\u6c11\u8425\u4f01\u4e1a, \u4e00\u822c\u6c11\u8425\u4f01\u4e1a"] <- "一般民营企业"
loandata$officeType[loandata$officeType == "\u4e00\u822c\u4e0a\u5e02\u516c\u53f8（\u5305\u62ec\u56fd\u5916\u4e0a\u5e02）, \u4e00\u822c\u4e0a\u5e02\u516c\u53f8（\u5305\u62ec\u56fd\u5916\u4e0a\u5e02）"] <- "一般上市公司（包括国外上市）"
#loandata$officeType[is.na(loandata$officeType)] <- "未填写" # 未填写
loandata$officeType <- factor(loandata$officeType, ordered = FALSE, labels = c("地方国资委直属企业","个体经营者","国家机关","其他",
                                                                               "世界500强（包括合资企业及下级单位）","事业单位","外资企业（包括合资企业）"
                                                                               ,"无","央企（包括下级单位）","一般民营企业","一般上市公司（包括国外上市）"))


# 处理借款类型-----------------------------------------------------------------

loandata$borrowType[loandata$borrowType == "SELF"] <- "个人消费"
loandata$borrowType[loandata$borrowType == "EDUCAITON"] <- "教育培训"
loandata$borrowType[loandata$borrowType == "SHORTTERM"] <- "短期周转"
loandata$borrowType[loandata$borrowType == "VENTURE"] <- "投资创业"
loandata$borrowType[loandata$borrowType == "\u77ed\u671f\u5468\u8f6c, \u4e2a\u4eba\u6d88\u8d39"] <- "个人消费"
loandata$borrowType[loandata$borrowType == "\u77ed\u671f\u5468\u8f6c, \u8d2d\u8f66\u501f\u6b3e"] <- "短期周转"
loandata$borrowType[loandata$borrowType == "\u77ed\u671f\u5468\u8f6c, \u5176\u4ed6\u501f\u6b3e"] <- "短期周转"
loandata$borrowType[loandata$borrowType == "\u77ed\u671f\u5468\u8f6c, \u6295\u8d44\u521b\u4e1a"] <- "短期周转"
loandata$borrowType[loandata$borrowType == "\u77ed\u671f\u5468\u8f6c, \u88c5\u4fee\u501f\u6b3e"] <- "短期周转"
loandata$borrowType[loandata$borrowType == "\u4e2a\u4eba\u6d88\u8d39 "] <- "个人消费"
loandata$borrowType[loandata$borrowType == "\u4e2a\u4eba\u6d88\u8d39, \u8d2d\u8f66\u501f\u6b3e"] <- "购车借款"
loandata$borrowType[loandata$borrowType == "\u8d2d\u8f66\u501f\u6b3e, \u77ed\u671f\u5468\u8f6c"] <- "购车借款"
loandata$borrowType[loandata$borrowType == "\u8d2d\u8f66\u501f\u6b3e, \u4e2a\u4eba\u6d88\u8d39"] <- "购车借款"
loandata$borrowType[loandata$borrowType == "\u8d2d\u8f66\u501f\u6b3e, \u88c5\u4fee\u501f\u6b3e"] <- "其他借款"
loandata$borrowType[loandata$borrowType == "\u5176\u4ed6\u501f\u6b3e "] <- "其他借款"
loandata$borrowType[loandata$borrowType == "\u5176\u4ed6\u501f\u6b3e, \u77ed\u671f\u5468\u8f6c"] <- "短期周转"
loandata$borrowType[loandata$borrowType == "\u6295\u8d44\u521b\u4e1a, \u77ed\u671f\u5468\u8f6c"] <- "短期周转"
loandata$borrowType[loandata$borrowType == "\u88c5\u4fee\u501f\u6b3e, \u533b\u7597\u652f\u51fa"] <- "装修借款"
#loandata$borrowType[is.na(loandata$borrowType)] <- "未填写"
#loandata$borrowType[loandata$borrowType == "未填写"] <- NA
loandata$borrowType <- factor(loandata$borrowType, ordered = FALSE, labels = c("Short-term borrowing",
                                                                               "Personal consumption","The car loan","Housing loan","Wedding loan","Education and training",
                                                                               "Other borrowing","Investment","Health spending","Decorate loan"))

# 处理借款状态类型--------------------------------------------------------------

loandata$status[loandata$status == "OVER_DUE"] <- "BAD_DEBT"
loandata$status[loandata$status == "WAIT_OPEN"] <- "IN_PROGRESS"
loandata$status <- factor(loandata$status, ordered = FALSE, labels = c("Over due","Closed","Failed","In progress"))

# 处理行业----------------------------------------------------------------------

loandata$officeDomain[loandata$officeDomain == ", IT"] <- "IT"
loandata$officeDomain[loandata$officeDomain == ", \u623f\u5730\u4ea7\u4e1a"] <- "房地产业"
loandata$officeDomain[loandata$officeDomain == ", \u5efa\u7b51\u5de5\u7a0b"] <- "建筑工程"
loandata$officeDomain[loandata$officeDomain == ", \u4ea4\u901a\u8fd0\u8f93\u4e1a"] <- "交通运输业"
loandata$officeDomain[loandata$officeDomain == ", \u91d1\u878d/\u6cd5\u5f8b"] <- "金融/法律"
loandata$officeDomain[loandata$officeDomain == ", \u96f6\u552e/\u6279\u53d1"] <- "零售/批发"
loandata$officeDomain[loandata$officeDomain == ", \u5a92\u4f53/\u5e7f\u544a"] <- "媒体/广告"
loandata$officeDomain[loandata$officeDomain == ", \u5a31\u4e50\u670d\u52a1\u4e1a"] <- "娱乐服务业"
loandata$officeDomain[loandata$officeDomain == ", \u653f\u5e9c\u673a\u5173"] <- "政府机关"
loandata$officeDomain[loandata$officeDomain == ", \u5236\u9020\u4e1a"] <- "制造业"
loandata$officeDomain[loandata$officeDomain == "IT, IT"] <- "IT"
loandata$officeDomain[loandata$officeDomain == "\u9910\u996e/\u65c5\u9986\u4e1a, \u9910\u996e/\u65c5\u9986\u4e1a"] <- "餐饮/旅馆业"
loandata$officeDomain[loandata$officeDomain == "\u5efa\u7b51\u5de5\u7a0b,"] <- "建筑工程"
loandata$officeDomain[loandata$officeDomain == "\u4ea4\u901a\u8fd0\u8f93\u4e1a, \u4ea4\u901a\u8fd0\u8f93\u4e1a"] <- "交通运输业"
loandata$officeDomain[loandata$officeDomain == "\u6559\u80b2/\u57f9\u8bad,"] <- "教育/培训"
loandata$officeDomain[loandata$officeDomain == "\u6559\u80b2/\u57f9\u8bad, \u6559\u80b2/\u57f9\u8bad"] <- "教育/培训"
loandata$officeDomain[loandata$officeDomain == "\u96f6\u552e/\u6279\u53d1, \u96f6\u552e/\u6279\u53d1"] <- "零售/批发"
loandata$officeDomain[loandata$officeDomain == "\u5a92\u4f53/\u5e7f\u544a, \u5a92\u4f53/\u5e7f\u544a"] <- "媒体/广告"
loandata$officeDomain[loandata$officeDomain == "\u519c\u4e1a, \u519c\u4e1a"] <- "农业"
loandata$officeDomain[loandata$officeDomain == "\u5176\u5b83, IT"] <- "IT"
loandata$officeDomain[loandata$officeDomain == "\u5176\u5b83, \u5176\u5b83"] <- "其它"
loandata$officeDomain[loandata$officeDomain == "\u533b\u7597/\u536b\u751f/\u4fdd\u5065,"] <- "医疗/卫生/保健"
loandata$officeDomain[loandata$officeDomain == "\u5a31\u4e50\u670d\u52a1\u4e1a,"] <- "娱乐服务业"
loandata$officeDomain[loandata$officeDomain == "\u653f\u5e9c\u673a\u5173, \u653f\u5e9c\u673a\u5173"] <- "政府机关"
loandata$officeDomain[loandata$officeDomain == "\u5236\u9020\u4e1a ,"] <- "制造业"
loandata$officeDomain[loandata$officeDomain == "\u5236\u9020\u4e1a,"] <- "制造业"
loandata$officeDomain[loandata$officeDomain == "\u5236\u9020\u4e1a, \u5236\u9020\u4e1a"] <- "制造业"
loandata$officeDomain[loandata$officeDomain == "\u5efa\u7b51\u5de5\u7a0b, \u5efa\u7b51\u5de5\u7a0b"] <- "建筑工程"
loandata$officeDomain[loandata$officeDomain == "计算机系统"] <- "IT"
loandata$officeDomain[loandata$officeDomain == "房地产业"] <- "The real estate industry"
loandata$officeDomain[loandata$officeDomain == "建筑工程"] <- "Building works"
loandata$officeDomain[loandata$officeDomain == "交通运输业"] <- "Transportation"
loandata$officeDomain[loandata$officeDomain == "金融/法律"] <- "Finance/Law"
loandata$officeDomain[loandata$officeDomain == "零售/批发"] <- "Retail/Wholesale"
loandata$officeDomain[loandata$officeDomain == "媒体/广告"] <- "Media/Advertising"
loandata$officeDomain[loandata$officeDomain == "娱乐服务业"] <- "Entertainment service"
loandata$officeDomain[loandata$officeDomain == "政府机关"] <- "The government authority"
loandata$officeDomain[loandata$officeDomain == "制造业"] <- "Manufacturing"
loandata$officeDomain[loandata$officeDomain == "餐饮/旅馆业"] <- "Restaurant/Hotel industry"
loandata$officeDomain[loandata$officeDomain == "教育/培训"] <- "Education/Training"
loandata$officeDomain[loandata$officeDomain == "农业"] <- "Agricultural"
loandata$officeDomain[loandata$officeDomain == "其它"] <- "Others"
loandata$officeDomain[loandata$officeDomain == "医疗/卫生/保健"] <- "Medical/Hygiene/Health care"
loandata$officeDomain[loandata$officeDomain == "能源业"] <- "The energy sector"
loandata$officeDomain[loandata$officeDomain == "公共事业"] <- "Public utilities"
loandata$officeDomain[loandata$officeDomain == "公益组织"] <- "Public welfare organization"
loandata$officeDomain[loandata$officeDomain == "体育/艺术"] <- "Physical Education/Art"
loandata$officeDomain[loandata$officeDomain == "无"] <- NA
# loandata$officeDomain[is.na(loandata$officeDomain)] <- "未填写"
# loandata$officeDomain[loandata$officeDomain == "未填写"] <- NA
loandata$officeDomain <- factor(loandata$officeDomain, ordered = FALSE)




# 工资水平处理----------------------------------------------------------------------

loandata$salary <- gsub('[,]', '', loandata$salary)
loandata$salary <- gsub('[ ]', '', loandata$salary)
loandata$salary[loandata$salary == "10000-20000\u514310000-20000\u5143"] <- "10000-20000元"
loandata$salary[loandata$salary == "2000-5000\u51432000-5000\u5143"] <- "2000-5000元"
loandata$salary[loandata$salary == "20000-50000\u514320000-50000\u5143"] <- "20000-50000元"
loandata$salary[loandata$salary == "5000-10000\u514310000-20000\u5143"] <- "5000-10000元"
loandata$salary[loandata$salary == "5000-10000\u51435000-10000\u5143"] <- "5000-10000元"
loandata$salary[loandata$salary == "50000\u5143\u4ee5\u4e0a5000-10000\u5143"] <- "5000-10000元"
loandata$salary[loandata$salary == "50000\u5143\u4ee5\u4e0a50000\u5143\u4ee5\u4e0a"] <- "50000元以上"
loandata$salary[loandata$salary == "1000\u5143\u4ee5\u4e0b"] <- "2000元以下"
loandata$salary[loandata$salary == "1001-2000\u5143"] <- "2000元以下"
#loandata$salary[is.na(loandata$salary)] <- "未填写"
#loandata$salary[loandata$salary == "未填写"] <- NA
loandata$salary <- factor(loandata$salary,levels = c("50000元以上","20000-50000元","10000-20000元","5000-10000元","2000-5000元","2000元以下"),
                          labels = c("More than 50000 yuan","20000-50000 yuan","10000-20000 yuan"
                                     ,"5000-10000 yuan","2000-5000 yuan","Less than 2000 yuan"))



# 省份处理----------------------------------------------------------------------

loandata$province <- gsub('[省]','', loandata$province)
loandata$province <- gsub('[请选择]','', loandata$province)
loandata$province <- gsub('[市]','', loandata$province)
loandata$province <- gsub('[,]','', loandata$province)
loandata$province <- gsub('[ ]','', loandata$province)
loandata$province[loandata$province == "\u8fbe\u5dde"] <- "四川"
loandata$province[loandata$province == "\u5e7f\u897f\u58ee\u65cf\u81ea\u6cbb\u533a\u5357\u5b81 "] <- "广西"
loandata$province[loandata$province == "\u5e7f\u897f\u58ee\u65cf\u81ea\u6cbb\u533a"] <- "广西"
loandata$province[loandata$province == "\u5e7f\u5dde"] <- "广东"
loandata$province[loandata$province == "\u90af\u90f8"] <- "河北"
loandata$province[loandata$province == "\u5609\u5174"] <- "浙江"
loandata$province[loandata$province == "\u5b81\u590f\u56de\u65cf\u81ea\u6cbb\u533a"] <- "宁夏"
loandata$province[loandata$province == "\u5185\u8499\u53e4\u81ea\u6cbb\u533a"] <- "内蒙古"
loandata$province[loandata$province == "\u53a6\u95e8"] <- "福建"
loandata$province[loandata$province == "\u4e0a\u6d77\u4e0a\u6d77"] <- "上海"
loandata$province[loandata$province == "\u6df1\u5733"] <- "广东"
loandata$province[loandata$province == "\u82cf\u5dde"] <- "江苏"
loandata$province[loandata$province == "\u897f\u85cf\u81ea\u6cbb\u533a"] <- "西藏"
loandata$province[loandata$province == "\u957f\u6c99"] <- "湖南"
loandata$province[loandata$province == "\u5e7f\u897f\u58ee\u65cf\u81ea\u6cbb\u533a\u5357\u5b81"] <- "广西"
loandata$province[loandata$province == "\u5510\u5c71"] <- "河北"
loandata$province[loandata$province == "\u65e0\u9521"] <- "江苏"
loandata$province[loandata$province == "\u65b0\u7586\u7ef4\u543e\u5c14\u81ea\u6cbb\u533a"] <- "新疆"
loandata$province[loandata$province == ""] <- NA
loandata$province[loandata$province == 0] <-NA
# loandata$province[is.na(loandata$province)] <- "未填写"
loandata$province[loandata$province == "广东"] <- "GuangDong"
loandata$province[loandata$province == "\u5b89\u5fbd"] <- "AnHui"
loandata$province[loandata$province == "\u6fb3\u95e8"] <- "Macau"
loandata$province[loandata$province == "\u5317\u4eac"] <- "BeiJing"
loandata$province[loandata$province == "\u798f\u5efa"] <- "FuJian"
loandata$province[loandata$province == "\u7518\u8083"] <- "GanSu"
loandata$province[loandata$province == "\u5e7f\u897f"] <- "GuangXi"
loandata$province[loandata$province == "\u8d35\u5dde"] <- "GuiZhou"
loandata$province[loandata$province == "\u6d77\u5357"] <- "HaiNan"
loandata$province[loandata$province == "\u6cb3\u5317"] <- "HeiBei"
loandata$province[loandata$province == "\u6cb3\u5357"] <- "HeNan"
loandata$province[loandata$province == "\u9ed1\u9f99\u6c5f"] <- "HeiLongJiang"
loandata$province[loandata$province == "\u6e56\u5317"] <- "HuBei"
loandata$province[loandata$province == "\u6e56\u5357"] <- "HuNan"
loandata$province[loandata$province == "\u5409\u6797"] <- "JiLin"
loandata$province[loandata$province == "\u6c5f\u897f"] <- "JiangXi"
loandata$province[loandata$province == "\u8fbd\u5b81"] <- "LiaoNing"
loandata$province[loandata$province == "\u5185\u8499\u53e4"] <- "NeiMengGu"
loandata$province[loandata$province == "\u5b81\u590f"] <- "NingXia"
loandata$province[loandata$province == "\u9752\u6d77"] <- "QingHai"
loandata$province[loandata$province == "\u5c71\u4e1c"] <- "ShanDong"
loandata$province[loandata$province == "\u5c71\u897f"] <- "ShanXi"
loandata$province[loandata$province == "\u9655\u897f"] <- "ShaanXi"
loandata$province[loandata$province == "\u4e0a\u6d77"] <- "ShangHai"
loandata$province[loandata$province == "\u56db\u5ddd"] <- "SiChuan"
loandata$province[loandata$province == "\u53f0\u6e7e"] <- "TaiWan"
loandata$province[loandata$province == "\u5929\u6d25"] <- "TianJin"
loandata$province[loandata$province == "\u897f\u85cf"] <- "XiZang"
loandata$province[loandata$province == "\u9999\u6e2f"] <- "HongKong"
loandata$province[loandata$province == "\u65b0\u7586"] <- "XinJiang"
loandata$province[loandata$province == "\u4e91\u5357"] <- "YunNan"
loandata$province[loandata$province == "\u6d59\u6c5f"] <- "ZheJiang"
loandata$province[loandata$province == "\u91cd\u5e86"] <- "ChongQing"
loandata$province[loandata$province == "\u6c5f\u82cf"] <- "JiangSu"
loandata$province <- factor(loandata$province,ordered = FALSE)

loandata$age <- year(Sys.Date())-year(loandata$birthDay) # 计算年龄

loandata <- loandata[-2] # 删除出生日期

# 地区处理---------------------------------------------------------------------

loandata$region[loandata$province=='HeiLongJiang'] <- 'NorthEast of China'
loandata$region[loandata$province=='JiLin'] <- 'NorthEast of China'
loandata$region[loandata$province=='LiaoNing'] <- 'NorthEast of China'
loandata$region[loandata$province=='ShangHai'] <- 'East of China'
loandata$region[loandata$province=='JiangSu'] <- 'East of China'
loandata$region[loandata$province=='ZheJiang'] <- 'East of China'
loandata$region[loandata$province=='AnHui'] <- 'East of China'
loandata$region[loandata$province=='FuJian'] <- 'East of China'
loandata$region[loandata$province=='JiangXi'] <- 'East of China'
loandata$region[loandata$province=='ShanDong'] <- 'East of China'
loandata$region[loandata$province=='TaiWan'] <- 'East of China'
loandata$region[loandata$province=='BeiJing'] <- 'North of China'
loandata$region[loandata$province=='TianJin'] <- 'North of China'
loandata$region[loandata$province=='ShanXi'] <- 'North of China'
loandata$region[loandata$province=='HeBei'] <- 'North of China'
loandata$region[loandata$province=='NeiMengGu'] <- 'North of China'
loandata$region[loandata$province=='HeNan'] <- 'Central of China'
loandata$region[loandata$province=='HuBei'] <- 'Central of China'
loandata$region[loandata$province=='HuNan'] <- 'Central of China'
loandata$region[loandata$province=='GuangDong'] <- 'South of China'
loandata$region[loandata$province=='GuangXi'] <- 'South of China'
loandata$region[loandata$province=='HaiNan'] <- 'South of China'
loandata$region[loandata$province=='HongKong'] <- 'South of China'
loandata$region[loandata$province=='Macau'] <- 'South of China'
loandata$region[loandata$province=='SiChuan'] <- 'SouthWest of China'
loandata$region[loandata$province=='GuiZhou'] <- 'SouthWest of China'
loandata$region[loandata$province=='YunNan'] <- 'SouthWest of China'
loandata$region[loandata$province=='ChongQing'] <- 'SouthWest of China'
loandata$region[loandata$province=='XiZang'] <- 'SouthWest of China'
loandata$region[loandata$province=='ShaanXi'] <- 'NorthWest of China'
loandata$region[loandata$province=='GanSu'] <- 'NorthWest of China'
loandata$region[loandata$province=='QingHai'] <- 'NorthWest of China'
loandata$region[loandata$province=='NingXia'] <- 'NorthWest of China'
loandata$region[loandata$province=='XinJiang'] <- 'NorthWest of China'
loandata$region <- factor(loandata$region,ordered = FALSE)

# 用地区代替了省份之后，省份就可以剔除了
loandata <- subset(loandata, select = -c(province))



# 处理异常值--------------------------------------------------------------------
# 识别方法：箱线图
# 处理方法：直接删除(大约删除了9065个)

# 注意：借款失败的组别中，仍然有审批的额度，但是可能由于别的原因，使他们不能成功借钱，
#       但是我们这里不考虑

loandata$availableCredits[loandata$status=='Failed'&loandata$availableCredits!=0] <- 0
loandata$availableCredits[loandata$status=='Over due'&loandata$availableCredits==0] <- NA
loandata$availableCredits[loandata$status=='Closed'&loandata$availableCredits==0] <- NA
loandata$availableCredits[loandata$status=='In progress'&loandata$availableCredits==0] <- NA

OutVals <- boxplot.stats(loandata$availableCredits[loandata$status=="In progress"])$out
loandata$availableCredits[loandata$status=="In progress" & loandata$availableCredits>=min(OutVals)] <- NA
OutVals <- boxplot.stats(loandata$availableCredits[loandata$status=="Over due"])$out
loandata$availableCredits[loandata$status=="Over due" & loandata$availableCredits >= min(OutVals)] <- NA
OutVals <- boxplot.stats(loandata$availableCredits[loandata$status=="Closed"])$out
loandata$availableCredits[loandata$status=="Closed" & loandata$availableCredits >= min(OutVals)] <- NA
rm(OutVals)

# 写文件------------------------------------------------------------------------

#write.csv(loandata,file="data.csv",row.names = F)



# 探索缺失值--------------------------------------------------------------------

#md.pattern(loandata)
#aggr(loandata[1:10], prop=FALSE, numbers=TRUE)

