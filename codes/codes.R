rm(list=ls()) #清空所有变量
options(digits=4) #数字位数设置
options(scipen=100) #设置科学计数法，scipen=1代表从十万之后开始科学计数
Sys.setlocale("LC_ALL", "Chinese") #修改R系统语言为中文
graphics.off()

library(forecast)
library(tseries) 
library(FinTS) 
library(fBasics) 
library(knitr) 
library(rugarch)
library(kableExtra) 
library(fGarch)

data <- read.csv("./data/cybz.csv",header=T)
cybz <- data[, 5]
cybz.date <- as.Date(data[,1])
# cybz <- c(cybz)
# cybz.rt <- diff(log(cybz))
# cybz.rt <- c(0.016884823, cybz.rt)
# data[,8] <- cybz.rt
# write.csv(data,"./data/cybz.csv",row.names = FALSE)
cybz.rt <- data[, 8]

cybz.rt1 <- basicStats(cybz.rt)
cybz.rt2 <- cbind("统计量值",as.data.frame(t(cybz.rt1[c(7,14,15,16),])))
colnames(cybz.rt2) <- c("统计量","均值","标准差","偏态系数","峰态系数")

knitr::kable(cybz.rt2,row.names =F, align = c("l", "c", "c", "c", "c"),
caption="创业板指对数收益率描述性分析结果",longtable = TRUE, booktabs = TRUE,linesep="",escape = F)%>%kable_styling(full_width = T)%>%column_spec(1:5,width=c("3cm","2.5cm","2.5cm","2.5cm","2.5cm"))

par(mfrow=c(2,1))
plot(cybz.date,cybz,type="l",xlab="日期",ylab="结算价",ylim=c(500,4000),cex.lab=1.5,cex.axis=1.5)
plot(cybz.date,cybz.rt,type="l",xlab="日期",cex.main=2,ylab="对数收益率",ylim=c(-0.1,0.1),las=0,cex.lab=1.5,cex.axis=1.5)
graphics.off()

# 创业板指数选样指标为一段时期(一般为六个月)平均流通市值的比重和平均成交金额的比
# 重。选样时先计算入围个股平均流通市值占创业板市场比重和平均成交金额占创业板市场
# 比重，再将上述指标按2：1的权重加权平均，计算结果从高到低排序，在参考公司治理结
# 构、经营状况等因素后，按照缓冲区技术选取创业板指数成份股。

# 纯随机性检验
for (i in 1:2) {print(Box.test(cybz.rt,lag = 6*i,type = 'Lj'))} #非白噪声

# 平稳性检验
pp.test(cybz.rt) #平稳

par(mfrow=c(1,2))
hist(cybz.rt,main=NULL,breaks = 50,xlab="创业板指收益率",ylab="频率",cex.axis=0.8, cex.lab=0.9)
qqnorm(cybz.rt,main =NULL,xlab="理论分位数",ylab="样本分位数",cex.axis=0.8, cex.lab=0.9)
qqline(cybz.rt)

# 正态性检验
jarque.bera.test(cybz.rt)   #非正态

# 自相关性检验
par(mfrow=c(2,1))
acf(cybz.rt,main="ACF",xlab="滞后期",ylab="ACF值",cex.axis=1.5, cex.lab=1.5,cex.main=2) #自相关图
pacf(cybz.rt,main="PACF",xlab="滞后期",ylab="PACF值",cex.axis=1.5, cex.lab=1.5,cex.main=2) #偏自相关图
graphics.off()

# 拟合ARMA模型
auto.arima(cybz.rt)
fit <- arima(cybz.rt,order = c(1,0,1))
cybz.rt.r <- stats::residuals(fit)
for (i in 1:2) {print(Box.test(cybz.rt.r,lag = 6*i,type = 'Lj'))} 
plot(cybz.date,cybz.rt.r,type="l",xlab="日期",ylab="残差",cex.axis=0.7, cex.lab=0.8)


# Portmanteau Q检验（ARCH效应检验） 
for (i in 1:6) {print(Box.test(cybz.rt.r^2,lag = i,type = 'Lj'))}#有ARCH效应

# # 拟合GARCH(1,1)模型，高斯分布
# model = garchFit(~1+garch(1,1),data=cybz.rt.r,trace=F)
# summary(model)

# # 拟合GARCH(1,1)模型，t分布
# model2 = garchFit(~1+garch(1,1),data=cybz.rt.r,trace=F,cond.dist="std")
# summary(model2)

# # 拟合GARCH(1,1)模型，GED分布
# model4 = garchFit(~1+garch(1,1),data=cybz.rt.r,trace=F,cond.dist="ged")
# summary(model4)

# 拟合GARCH(1,1)模型，有偏t分布
model3 = garchFit(~1+garch(1,1),data=cybz.rt.r,trace=F,cond.dist="sstd")
summary(model3)

vol = volatility(model3) #模型的拟合波动率序列
resi=residuals(model3,standardize=T) #标准化残差

upp = -0.000027159 + 2*vol
low = -0.000027159 - 2*vol
par(mfrow=c(1,1))
plot(cybz.date,cybz.rt,type="l",xlab="日期",cex.main=2,ylab="对数收益率",ylim=c(-0.1,0.1),las=0,cex.lab=1.5,cex.axis=1.5)
lines(cybz.date,upp,lty=2,col='red')
lines(cybz.date,low,lty=2,col='red')
abline(h = c(-0.000027159)) ##95%点预测区间

par(mfcol=c(2,2))
acf(resi,lag=6)
pacf(resi,lag=6)
acf(resi^2,lag=6)
pacf(resi^2,lag=6)
graphics.off()


# garch1.1 <- ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),mean.model=list(armaOrder=c(1,1)),distribution="std")
# garch1.1fit <- ugarchfit(spec=garch1.1,data=cybz.rt)
# plot(garch1.1fit)

# 拟合GARCH(1,1)模型之后残差序列自相关检验
for (i in 1:6) {print(Box.test(resi^2,lag = i,type = 'Lj'))}
