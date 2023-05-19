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
summary(model_glm_factor)
#卡方检验
result_time_m <- chisq.test(table(data$ClosedByAttorneyUno,data$Time_m))
result_state<-chisq.test(table(data$ClosedByAttorneyUno,data$StateAbbr))
result_length<-chisq.test(table(data$ClosedByAttorneyUno,data$length))
result_Cate<-chisq.test(table(data$ClosedByAttorneyUno,data$Category))
result_AQrate<-chisq.test(table(data$ClosedByAttorneyUno,data$AQRate))
#训练集测试集划分
data$ClosedByAttorneyUno<-as.factor(data$ClosedByAttorneyUno)
library(caret)
index <- createDataPartition(data$ClosedByAttorneyUno, p = 0.7, list = FALSE)
# 根据索引划分训练集和测试集
train_data <- data[index, ]
test_data <- data[-index, ]
#逻辑回归模型
model_glm_factor <- glm(ClosedByAttorneyUno ~ StateAbbr+length+AQRate+Time_m+Category, data = train_data, family = binomial())
predicted_glm <- predict(model_glm_factor, newdata = test_data)
predicted_glm <- ifelse(predicted_glm >= 0.5, "1", "0")
# 根据预测结果和实际标签生成混淆矩阵
confusion_matrix_glm <- table(predicted_glm, test_data$ClosedByAttorneyUno)
# 计算正确率
accuracy_glm <- sum(diag(confusion_matrix_glm)) / sum(confusion_matrix_glm)
#决策树
library(rpart)
# 构建决策树模型
model_rtree <- rpart(ClosedByAttorneyUno ~ StateAbbr+length+AQRate+Time_m+Category,data=data)
summary(model_rtree)
predicted_rtree <- predict(model_rtree, newdata = test_data,type="class")
confusion_matrix_rtree <- table(predicted_rtree, test_data$ClosedByAttorneyUno)
# 计算正确率
accuracy_rtree <- sum(diag(confusion_matrix_rtree)) / sum(confusion_matrix_rtree)
#随机森林
library(randomForest)
model_forest <- randomForest(ClosedByAttorneyUno ~ StateAbbr+length+AQRate+Time_m+Category, data = train_data)
predicted_forest <- predict(model_forest, newdata = test_data,type="class")
confusion_matrix_forest <- table(predicted_forest, test_data$ClosedByAttorneyUno)
# 计算正确率
accuracy_forest <- sum(diag(confusion_matrix_forest)) / sum(confusion_matrix_forest)
#SVM
library(e1071)
model_svm <- svm(ClosedByAttorneyUno ~ StateAbbr+length+AQRate+Time_m+Category, data = train_data)
predicted_svm <- predict(model_svm, newdata = test_data,type="class")
confusion_matrix_svm <- table(predicted_svm, test_data$ClosedByAttorneyUno)
# 计算正确率
accuracy_svm <- sum(diag(confusion_matrix_svm)) / sum(confusion_matrix_svm)
#神经网络
library(neuralnet)
model_neu <- neuralnet(ClosedByAttorneyUno ~ StateAbbr+length+AQRate+Time_m+Category, data = train_data)
predicted_neu <- predict(model_neu, newdata = test_data,type="class")
confusion_matrix_neu <- table(predicted_neu, test_data$ClosedByAttorneyUno)
# 计算正确率
accuracy_neu <- sum(diag(confusion_matrix_neu)) / sum(confusion_matrix_neu)