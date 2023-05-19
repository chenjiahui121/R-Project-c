setwd("D://CHENJIAHUI//大二下//数据科学//大作业")
data<-read.csv(".//dataformodel.csv")
question<-read.csv(".//questions.csv")
question$ClosedByAttorneyUno <- ifelse(question$ClosedByAttorneyUno=="NULL",0,1)
#表格合并
questiondf<-question[,c("QuestionUno","ClosedByAttorneyUno")]
# 使用merge函数根据QuestionUno列匹配数据
data <- merge(questiondf, data, by = "QuestionUno", all.x = FALSE)
class(data$Category)#查看原始类型
class(data$StateAbbr)
data$Category<-as.factor(data$Category)#转化为因子形式
data$StateAbbr<-as.factor(data$StateAbbr)
#调整时间格式
data$Time_m <- as.numeric(as.POSIXlt(data$Time_m, format = "%H:%M"))
summary(data$StateAbbr)#转化后数据进行查看
summary(data$Time_m)
column_counts <-sapply(data, function(x) sum(!is.na(x), na.rm = TRUE))
column_counts_df <- data.frame(Column = names(column_counts), Count = column_counts)
data <- na.omit(data)
#整理后数据描述表格生成
library(knitr)
library(kableExtra)
summary_stats <- summary(data)
# 将summary_stats转换为数据框
summary_table <- as.data.frame(summary_stats)
# 使用kable()函数生成表格
kable(summary_table, format = "html", table.attr = "class='table'")
#进行分类模型测试
#逻辑回归模型
library(stats)
model_glm_rate <- glm(ClosedByAttorneyUno ~ SRate+length+AQRate+Time_m+CRate, data = data, family = binomial())
model_glm_factor <- glm(ClosedByAttorneyUno ~ StateAbbr+length+AQRate+Time_m+Category, data = data, family = binomial())
summary(model_glm_rate)
summary(model_glm_fator)
#卡方检验
result_time_m <- chisq.test(table(data$ClosedByAttorneyUno,data$Time_m))
result_state<-chisq.test(table(data$ClosedByAttorneyUno,data$StateAbbr))
result_length<-chisq.test(table(data$ClosedByAttorneyUno,data$length))
result_Cate<-chisq.test(table(data$ClosedByAttorneyUno,data$Category))
result_AQrate<-chisq.test(table(data$ClosedByAttorneyUno,data$AQRate))
#决策树
library(rpart)
# 构建决策树模型
model_rtree <- rpart(ClosedByAttorneyUno ~ StateAbbr+length+AQRate+Time_m+Category, data = data, method = "class")
summary(model_rtree)
#随机森林
library(randomForest)
model_forest <- randomForest(ClosedByAttorneyUno ~ StateAbbr+length+AQRate+Time_m+Category, data = data)
#SVM
library(e1071)
model_svm <- svm(ClosedByAttorneyUno ~ StateAbbr+length+AQRate+Time_m+Category, data = data)
#神经网络
library(neuralnet)
model_neural <- neuralnet(ClosedByAttorneyUno ~ StateAbbr+length+AQRate+Time_m+Category, data = data)
