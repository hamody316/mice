# mice
Application of generalized linear model based on multiple imputation in renal disease research

# 数据整理
导入数据
library(openxlsx)
data<-read.xlsx("data.xlsx",sheet = 1)

定义因子变量
体质指数
data$BMI<-factor(data$BMI,levels = c(2,1,3))
性别
data$Gender<-factor(data$Gender)

# 利用完整数据集拟合广义线性模型
全进入法
fit<-glm(sAKI~Gender+Age+BMI+NLR+BUN+Cr+UA+Na+K+Lactate,
         family = binomial,data)
summary(fit)
逐步回归法
summary(step(fit))

# 构造缺失值及特征描述
设定缺失值的比例5%
library(simFrame)
nac<-NAControl(NArate=0.05) #5%
指定缺失的变量所在列
data_na<-setNA(data[c(2:4,8,10,13)],nac)
data_na<-cbind(data_na,data[c(1,5:7,9,11,12)])

## 利用含缺失值的数据集拟合广义线性模型
fit_na<-glm(sAKI ~ Gender + Age + BMI + Cr + Na + Lactate , 
            family = binomial,data_na)
summary(fit_na)

# 基于多重填补的广义线性模型
进行缺失值的多重填补
library(mice)
data_mice<-mice(data_na,m=10,seed = 725,
                meth = c("logreg","pmm","polyreg","pmm","pmm","pmm",
                         "pmm","pmm","pmm","pmm","pmm","pmm","pmm"))

对10个数据集进行回归分析
fit_mice<-with(data_mice,glm(sAKI ~ Gender + Age + BMI + Cr + Na + Lactate , 
                             family = binomial))
合并结果
fit_mice_pool<-pool(fit_mice)
展示结果
summary(fit_mice_pool)

# 对比基于原始数据和基于多重填补数据的回归系数
基于原始数据的回归系数
fit_new<-glm(sAKI ~ Gender + Age + BMI + Cr + Na + Lactate , 
             family = binomial,data = data)
res<-summary(fit_new)
beta1<-as.data.frame(res$coefficients[,1])
基于多重填补数据的回归系数
beta2<-as.data.frame(summary(fit_mice_pool)$estimate)
两套回归系数的差值
beta_dif<-as.data.frame(beta1-beta2)
Table<-cbind.data.frame(beta1,beta2,beta_dif)
Table
