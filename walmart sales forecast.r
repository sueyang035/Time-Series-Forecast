library(ggfortify)
library(ggplot2)
library(forecast)   # for the "ma" moving average function
library(ggfortify)  # for autoplot
library(fBasics)
library(dplyr)

library(lubridate)
library(stats) #box pot 
library(lmtest)

library(fpp2)
library(fUnitRoots)
library(tseries)
library(TSA) 


#read data and convert date type 
w = read.csv('walmart.csv')
head(w)
class(w$Date)
w$Date = as.Date(w$Date, format = "%d-%m-%Y")
class(w$Date)


# Group by Date and summarize
w_grouped <- w %>%
  group_by(Date) %>%
  summarise(
    Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE),
    Holiday_Flag = max(Holiday_Flag),        # 1 if any store had a holiday
    Temperature = mean(Temperature, na.rm = TRUE),
    Fuel_Price = mean(Fuel_Price, na.rm = TRUE),
    CPI = mean(CPI, na.rm = TRUE),
    Unemployment = mean(Unemployment, na.rm = TRUE)
  ) %>%
  arrange(Date)


# adjust sales using CPI 
base_cpi <- first(w_grouped$CPI)


cpi_ts = ts(w_grouped$CPI,start=c(2010,6),frequency=52)
autoplot(cpi_ts)

cpi_ts[1]

sales_ts = ts(w_grouped$Weekly_Sales, start =c(2010,6),frequency=52 )
autoplot(sales_ts)


sales_adj = (sales_ts/cpi_ts) * cpi_ts[1]

sales_adj_ts = ts(sales_adj,start =c(2010,6),frequency=52)


autoplot(sales_adj_ts)

acf(sales_adj_ts,lag.max=104)
pacf(sales_adj_ts,lag.max=104)
eacf(diff(sales_adj_ts,lag=52))


## mannaul model 

fit1
fit1 = Arima(sales_adj_ts, order=c(1,0,0),seasonal = c(0,1,0))

autoplot(fit1$residuals)
acf(fit1$residuals,lag=52)
pacf(fit1$residuals,lag=52)

coeftest(fit1)
Box.test(fit1$residuals,lag=40,type='Ljung')

fit2 = Arima(sales_adj_ts, order=c(1,0,1),seasonal = c(0,1,0))
acf(fit2$residuals,lag=52)
pacf(fit2$residuals,lag=52)

coeftest(fit2)
Box.test(fit2$residuals,lag=20,type='Ljung')
Box.test(fit_auto1$residuals,lag=20,type='Ljung') 

# auto arima      
fit_auto1 = auto.arima(sales_adj_ts,ic='bic')
coeftest(fit_auto1)

fit_auto2 = auto.arima(sales_adj_ts,ic='aic')
fit_auto2

#compare the models 
AIC(fit1,fit2,fit_auto1,fit_auto2)
BIC(fit1,fit2,fit_auto1,fit_auto2)

fit1
fit2
fit_auto1
fit_auto2


### backtest 

length(sales_adj_ts) * 0.9

train = window(sales_adj_ts, end =c(time(sales_adj_ts)[128]))

test = window(sales_adj_ts, start =c(time(sales_adj_ts)[129]))
fit_train = Arima(train, order=c(1,0,0),seasonal = c(0,1,0))

fc = forecast(fit_train, h =15 )

fit_train1 = Arima(train,order=c(0,0,0),seasonal=c(0,1,0))
fc1 =forecast(fit_train1,h=15)

accuracy(fc,test)
accuracy(fc1,test)

autoplot(fc) +
  autolayer(train, series = "Training Data", color = "black") +
  autolayer(test, series = "Test Data", color = "red") +
  ggtitle("ARIMA Forecast vs Training and Test Data") +
  xlab("Time") + ylab("Value") +
  theme_minimal()

fit_auto_train = Arima(train, order=c(1,1,1),seasonal = c(0,1,0))
fc_auto = forecast(fit_auto_train, h =15 )
accuracy(fc_auto,test)

fit2_train = Arima(train, order=c(1,0,1),seasonal = c(0,1,0))

fc2 = forecast(fit2_train, h =15 )


accuracy(fc2,test)


# forecast for the next 10 weeks


fc_10weeks = forecast(fit2, h =10 )
fc_10weeks
autoplot(fc_10weeks) +
  autolayer(sales_adj_ts, series = "Training Data", color = "black") +
  ggtitle("Sales Forecast for 10 Weeks") +
  xlab("Time") + ylab("Value") +
  theme_minimal()



### create several holiday dummy lag 
# use holiday & fuel as predictors 

library(dplyr)
library(forecast)

 #1. based on the ccf, holiday flag at lag -1 is has high correlation with 
#sales this makes since because 
ccf(w_grouped$Weekly_Sales,w_grouped$Holiday_Flag)
ccf(w_grouped$Weekly_Sales,w_grouped$Fuel_Price)
# 2.  Create lags and interaction, then remove NA rows
w_grouped <- w_grouped %>%
  mutate(
    Holiday_Lead1 = lead(Holiday_Flag, 1) ,
    Fuel_Holiday_Lead = Fuel_Price * Holiday_Lead1,
  ) %>%
  na.omit()   # remove rows with NA from lags

par(mfrow=c(1,1), mar=c(5,4,4,2))

ccf(w_grouped$Weekly_Sales,w_grouped$Holiday_Lead1)

# 3. Create time series after dropping NAs
sales_ts <- ts(w_grouped$Weekly_Sales, start=c(2010,6),  frequency = 52)
cpi_ts <- ts(w_grouped$CPI, start=c(2010,6) ,frequency = 52)

head(sales_ts)

fuel_ts = ts(w$Fuel_Price, frequency=52)
autoplot(fuel_ts)
autoplot(sales_adj_ts)
cpi_ts[1]
head(cpi_ts)
tail(cpi_ts)
# Adjust sales by CPI
sales_adj_ts <- (sales_ts * 167.7309) / cpi_ts

autoplot(sales_adj_ts)

head(w_grouped)
#4 . Create xreg matrix (all rows now aligned)

xreg1 = w_grouped$Fuel_Price
xreg2 = w_grouped$Holiday_Lead1
xreg3 = w_grouped$Fuel_Holiday_Lead

xreg4 <- as.matrix(w_grouped[, c(
 'Holiday_Lead1', 'Fuel_Price'
)]) 
xreg5 = as.matrix(w_grouped[, c('Fuel_Price', 'Fuel_Holiday_Lead')])


mean(xreg1)

xreg1_f = rep(mean(xreg1), 10)
xreg2_f = rep(mean(xreg2), 10)
xreg3_f = rep(mean(xreg3), 10)

# Calculate column means
m <- colMeans(xreg5)

# Repeat for 10 future periods
xreg5_f <- matrix(rep(m, each = 10), ncol = length(m), byrow = FALSE)


coeftest(fit3)


# 5. Check lengths to make sure they match
length(sales_adj_ts)
nrow(xreg5)
length(xreg1)

#6 fit model, using fuel only


fit1 =Arima(sales_adj_ts, order=c(0,0,0),seasonal = c(0,1,0), xreg = xreg1)

acf(fit1$residuals)
pacf(fit1$residuals)
eacf(fit1$residuals)

Box.test(fit1$residuals,lag=20, type='Ljung')

fit1
coeftest(fit1)
#AIC=2854.46   AICc=2854.59   BIC=2859.45


fit2 =Arima(sales_adj_ts, order=c(1,0,0),seasonal = c(0,0,1), xreg = xreg2)

acf(fit2$residuals,lag.max = 104)
pacf(fit2$residuals,lag.max=104)
eacf(fit2$residuals)
Box.test(fit2$residuals,lag=20, type='Ljung')
fit2
coeftest(fit2)

#AIC=4710.8   AICc=4711.24   BIC=4725.58


fit3 =Arima(sales_adj_ts, order=c(0,0,0),seasonal = c(0,1,0), xreg = xreg3)
fit3_ar =Arima(sales_adj_ts, order=c(1,0,0),seasonal = c(0,1,0), xreg = xreg3)
acf(fit3$residuals)
pacf(fit3$residuals)
autoplot(fit3$residuals)

Box.test(fit3_ar$residuals,lag=40, type='Ljung')
Box.test(fit3$residuals,lag=20, type='Ljung')


fit3
coeftest(fit3)


#AIC=2854.8   AICc=2855.08   BIC=2862.3

# see how holiday modifies the fuel price slope. 

fit5 =Arima(sales_adj_ts, order=c(0,0,0),seasonal = c(0,1,0), xreg = xreg5)
acf(fit5$residuals,lag.max = 10)
pacf(fit5$residuals)
coeftest(fit5)
fit5

fit4 = Arima(sales_adj_ts, order=c(0,0,0),seasonal = c(0,0,1), xreg = xreg4)
fit4
coeftest(fit4)
## test 

#forecast using the best model 
fc_10weeks = forecast(fit3, xreg = xreg3_f, h =10 )

fc_10weeks

fc = forecast(fit1,h=10)
fc

autoplot(fc_10weeks) +
  autolayer(sales_adj_ts, series = "Training Data", color = "black") +
  ggtitle("Aggregated Sales Forecast with/without regressor") +
  xlab("Time") + ylab("Value") +
  theme_minimal() + autolayer(fc,color='red')


autoplot(sales_adj_ts) + ggtitle("Aggregated Sales Time Series")
autoplot(w4_adj) + ggtitle("Store 4 Sales Time Series")


AIC(fit2,fit4)
AIC(fit1,fit3,fit3_ar,fit5)

BIC(fit2,fit4)
BIC(fit1,fit3,fit3_ar, fit5)





#backteset 

source("backtest.R")


backtest(fit3_ar,sales_adj_ts, h=1, orig=.9*length(sales_adj_ts))
backtest(fit1, sales_adj_ts, h=1, orig=.9*length(sales_adj_ts))
backtest(fit3, sales_adj_ts, h=1, orig=.9*length(sales_adj_ts))
backtest(fit5, sales_adj_ts, h=1, orig=.9*length(sales_adj_ts))

#test split 





length(sales_adj_ts) * 0.9

train = window(sales_adj_ts, end =c(time(sales_adj_ts)[127]))

test = window(sales_adj_ts, start =c(time(sales_adj_ts)[128]))

length(xreg1)
xreg1_train = xreg1[1:127]
xreg1_test = xreg1[128:142]

fit1_train = Arima(train, order=c(0,0,0),seasonal = c(0,1,0),xreg = xreg1_train )

fc1 = forecast(fit1_train, xreg = xreg1_test, h =15 )

accuracy(fc1,test)


autoplot(fc1) +
  autolayer(train, series = "Training Data", color = "black") +
  autolayer(test, series = "Test Data", color = "red") +
  ggtitle("ARIMA Forecast vs Training and Test Data") +
  xlab("Time") + ylab("Value") +
  theme_minimal()


xreg3_train = xreg3[1:127]
xreg3_test = xreg3[128:142]

fit3_train = Arima(train, order=c(0,0,0),seasonal = c(0,1,0),xreg = xreg3_train )
fit3_train1 = Arima(train, order=c(1,0,0),seasonal = c(0,1,0),xreg=xreg3_train)
pacf(fit3_train$residuals)

#try backtest function. 

fc3 = forecast(fit3_train, xreg = xreg3_test, h =15 )
fc3_ar = forecast(fit3_train1, xreg = xreg3_test, h =15 )
accuracy(fc3,test)
accuracy(fc3_ar,test)

autoplot(fc3) +
  autolayer(train, series = "Training Data", color = "black") +
  autolayer(test, series = "Test Data", color = "red") +
  ggtitle("fit3 Forecast vs Training and Test Data") +
  xlab("Time") + ylab("Value") +
  theme_minimal()



#------- 
  
xreg2_train = xreg2[1:127]
xreg2_test = xreg2[128:142]

fit2_train = Arima(train, order=c(1,0,0),seasonal = c(0,0,1),xreg = xreg2_train )

fc2 = forecast(fit2_train, xreg = xreg2_test, h =15 )



autoplot(fc2) +
  autolayer(train, series = "Training Data", color = "black") +
  autolayer(test, series = "Test Data", color = "red") +
  ggtitle("fit2 Forecast vs Training and Test Data") +
  xlab("Time") + ylab("Value") +
  theme_minimal()


##
xreg5_train = xreg5[1:127,]
xreg5_test = xreg5[128:142,]

length(xreg5_test)

fit5_train = Arima(train, order=c(0,0,0),seasonal = c(0,1,0),xreg = xreg5_train)
fc5 = forecast(fit5_train, xreg = xreg5_test, h =15 )
autoplot(fc5) +
  autolayer(train, series = "Training Data", color = "black") +
  autolayer(test, series = "Test Data", color = "red") +
  ggtitle("Fit5 Forecast vs Training and Test Data") +
  xlab("Time") + ylab("Value") +
  theme_minimal()


accuracy(fc1,test)
accuracy(fc3_ar,test)
accuracy(fc3,test)
accuracy(fc5,test)




# 5. Fit ARIMA with external regressors, test with auto arima 
fit_auto <- auto.arima(sales_adj_ts, xreg = xreg5,ic='aic')
fit_auto
coeftest(fit_auto)

acf(fit_auto$residuals)
pacf(fit_auto1$residuals)
Box.test(fit_auto2$rvesiduals,lag=20,type='Ljung')



## rank store performance 


df_store = w %>% group_by(Store) %>% summarise(avg_sales = mean(Weekly_Sales))
head(df_store)


df_sorted <- df_store[order(df_store$avg_sales, decreasing = TRUE), ]

df_sorted 

#store 1 time series 
w4 = subset(w, Store==4)
head(w4)

w1 = subset(w,Store==1)
w2 = subset(w,Store==2)
w3= subset(w,Store==3)

w1_ts = ts(w1$Weekly_Sales,start =c(2010,6),frequency=52)
cpi_ts = ts(w1$CPI,start =c(2010,6),frequency=52)
cpi_ts[1]
w1_adj = (w1_ts/cpi_ts) * cpi_ts[1]
autoplot(w1_adj)

w2_ts = ts(w2$Weekly_Sales,start =c(2010,6),frequency=52)
cpi_ts = ts(w2$CPI,start =c(2010,6),frequency=52)
cpi_ts[1]
w2_adj = (w2_ts/cpi_ts) * cpi_ts[1]
autoplot(w2_adj)


w3_ts = ts(w3$Weekly_Sales,start =c(2010,6),frequency=52)
cpi_ts = ts(w3$CPI,start =c(2010,6),frequency=52)
cpi_ts[1]
w3_adj = (w3_ts/cpi_ts) * cpi_ts[1]
autoplot(w3_adj)







tem_ts = ts(w4$Temperature,frequency=52)
autoplot(tem_ts)

w4 <- w4%>%
  mutate(
    Holiday_Lead1 = lead(Holiday_Flag, 1) ,
    Fuel_Holiday_lead = Fuel_Price * Holiday_Lead1,
    Tem_Holiday_lead = Temperature* Holiday_Lead1
  ) %>%
  na.omit()   # remove rows with NA from lags

w4_ts = ts(w4$Weekly_Sales,start =c(2010,6),frequency=52)

cpi_ts = ts(w4$CPI,start =c(2010,6),frequency=52)

cpi_ts[1]
w4_adj = (w4_ts/cpi_ts) * 126.4421
autoplot(w4_adj)



plot(diff(w4_adj))


adfTest(w4_adj,type='c') #non stationary
adfTest(w4_adj,type='ct') 

kpss.test(w4_adj,null='Level')  

fitauto = auto.arima(w4_adj)
fitauto

acf(w4_adj)


m <- colMeans(xreg)

# Repeat for 10 future periods
xreg_f <- matrix(rep(m, each = 10), ncol = length(m), byrow = FALSE)

xreg_f= xreg[40:49,]

xreg


f_1 = forecast(fit_1, xreg =xreg_f, h =10 )

autoplot(f_1) + autolayer(f_2,color='red') +  ggtitle("Store 4 Sales Forecast with/without regressor") 

f = forecast(fit, xreg =xreg_f, h =10 )
autoplot(f)

f_2 = forecast(fit_2,h=10)

f_1
f_2




autoplot(f_2) +
  autolayer(w4_adj, series = "Training Data", color = "black") +
  ggtitle("Store 4 Sales Forecast for 10 Weeks") +
  xlab("Time") + ylab("Value") +
  theme_minimal()
autoplot(f_2)


# trend satationary 

tem_ts =  ts(w4$Temperature,start =c(2010,6),frequency=52)
holiday_ts = ts(w4$Holiday_Flag,start =c(2010,6),frequency=52)
fuel_ts = ts(w4$Fuel_Price,start =c(2010,6),frequency=52)

Holiday_Flag_Fuel = w4$Holiday_Flag * w4$Fuel_Price
Holiday_Flag_Tem = w4$Holiday_Flag * w4$Temperature

cor(xreg)
xreg_new <- as.matrix(w4[, c(
 'Tem_Holiday_lead','Temperature'
)])

xreg_new <- as.matrix(w4[, c(
  'Holiday_Lead1', "Fuel_Price",
 'Tem_Holiday_lead'
)])

cor(xreg)

xreg<- as.matrix(w4[, c(
"Tem_Holiday_lead", 'Holiday_Lead1'
)]) 



length(w4_adj) == nrow(xreg_new)
cor(xreg_new)

summary(xreg_new)
any(is.na(xreg_new))
any(!is.finite(xreg_new))

dfw4 = diff(w4_adj,lag=52)


length(w4_adj)


autoplot(dfw4)

fitauto = auto.arima(w4_adj)

fitauto


length(dfw4)
length(xreg)


fit_1 = Arima(w4_adj, order=c(1,0,1) ,xreg=xreg)

fit_1

fit_11 = Arima(w4_adj, order =c(1,0,1), seasonal = c(0,0,1), xreg=xreg)
fit_11
coeftest(fit_1)
autoplot(fit_11$residuals) 

acf(fit_1$residuals)
pacf(fit_1$residuals)
eacf(fit_1$residuals)

Box.test(fit_1$residuals, lag = 40, type = 'Ljung')

fit = auto.arima(w4_adj, xreg = xreg, seasonal = TRUE)
fit

fit_2

fit_2 = Arima(w4_adj,order = c(1,0,1), seasonal = c(0,1,0))
acf(fit_2$residuals)
pacf(fit_2$residuals)

Box.test(fit_2$residuals, lag = 40, type = 'Ljung')

fit_1


backtest(fit_1, w4_adj, h=1, orig=.9*length(w4_adj))
backtest(fit_2, w4_adj, h=1, orig=.9*length(w4_adj))

backtest(fitauto,w4_adj,h=1,orig=.9*length(w4_adj))

length(w4_adj)

pacf(fit_1$residuals)
pacf(fit_2$residuals)

AIC(fit_1,fit_2)


fit_2
coeftest(fit_2)

autoplot(fuel_ts)


142*0.9
summary(xreg)


train = window(w4_adj, end =c(time(w4_adj)[127]))

test = window(w4_adj, start =c(time(w4_adj)[128]))

xreg_train = xreg[1:127,]
xreg_test = xreg[128:142,]


fit_1_train= Arima(train, order=c(1,0,1), xreg = xreg_train)


fc_1 = forecast(fit_1_train, xreg = xreg_test, h =15 )
autoplot(fc_1) +
  autolayer(train, series = "Training Data", color = "black") +
  autolayer(test, series = "Test Data", color = "red") +
  ggtitle("Regressor Forecast vs Training and Test Data") +
  xlab("Time") + ylab("Value") +
  theme_minimal()



fit_2_train= Arima(train, order=c(1,0,1),seasonal = c(0,1,0))


fc_2 = forecast(fit_2_train, h =15 )
autoplot(fc_2) +
  autolayer(train, series = "Training Data", color = "black") +
  autolayer(test, series = "Test Data", color = "red") +
  ggtitle("ARIMA Forecast vs Training and Test Data") +
  xlab("Time") + ylab("Value") +
  theme_minimal()

accuracy(fc_1,test)
accuracy(fc_2,test)


train_s = window(train, start = start(train) + 52/frequency(train))
xreg_s  = window(xreg_train, start = start(train) + 52/frequency(train))

k=52
xreg_s  <- xreg_train[(k+1):nrow(xreg_train), ]
train_s <- train[(k+1):length(train)]

fit_1_s = Arima(train_s, order=c(1,0,1), xreg=xreg_s)
fit_2_s = Arima(train_s, order=c(1,0,1), seasonal=c(0,1,0))

AIC(fit_1_s, fit_2_s)



library(seasonal)

# seasonal adjust time series using X11. 
sa_series <- seas(sales_adj_ts,x11 = "")

sa_series <- stl(sales_adj_ts, s.window = "periodic")
plot(sa_series)


# try VAR on two or three of the series?

library(forecast)
fit <- auto.arima(sales_adj_ts, xreg = fourier(sales_adj_ts, K = 4))

sa_series <- stl(sales_adj_ts, s.window = "periodic")


seasonally_adjusted <- sales_adj_ts - sa_series$time.series[, "seasonal"]

seasonally_adjusted_ts <- ts(seasonally_adjusted,
                             start = start(sales_adj_ts),
                             frequency = frequency(sales_adj_ts))


fit = auto.arima(seasonally_adjusted_ts)
fit

adfTest(seasonally_adjusted_ts, type = 'c') #stationary 


autoplot(seasonally_adjusted_ts)

pacf(seasonally_adjusted_ts)

autoplot(fit$residuals)

fit1 = Arima(seasonally_adjusted_ts, order=c(1,0,1))
fit1
autoplot(fit1$residuals)



library(vars)

df <- cbind(r3,r4)
df <- na.omit(df)

lag <- VARselect(df, lag.max=8)

### test on store 4 

lag 

var = VAR(df,p=8,type='const')
var
coeftest(var)

serial.test(var, type='PT.asymptotic')



w8= subset(w,Store==8)

w8_ts = ts(w8$Weekly_Sales,start =c(2010,6),frequency=52)
cpi_ts = ts(w8$CPI,start =c(2010,6),frequency=52)
cpi_ts[1]
w8_adj = (w8_ts/cpi_ts) * cpi_ts[1]
autoplot(w8_adj)



sa8 <- stl(w8_adj, s.window = "periodic")
seas_adj8 <- w8_adj - sa8$time.series[, "seasonal"]
autoplot(seas_adj8)

adfTest(seas_adj8,type='c') 

kpss.test(seas_adj8,null='Level')

# kpss null: stationary alt:unit root 


## store 7, 8  is stationary after removing seasonality 

w4=subset(w,Store==4)

w4_ts = ts(w4$Weekly_Sales,start =c(2010,6),frequency=52)
cpi_ts = ts(w4$CPI,start =c(2010,6),frequency=52)
cpi_ts[1]
w4_adj = (w4_ts/cpi_ts) * cpi_ts[1]
autoplot(w4_adj)

adfTest(w4_adj,type='c') 
kpss.test(w4_adj,null='Level')



w3=subset(w,Store==3)

w3_ts = ts(w3$Weekly_Sales,start =c(2010,6),frequency=52)
cpi_ts = ts(w3$CPI,start =c(2010,6),frequency=52)
cpi_ts[1]
w3_adj = (w3_ts/cpi_ts) * cpi_ts[1]
autoplot(w3_adj)

adfTest(w3_adj,type='c') 
kpss.test(w3_adj,null='Level')



fitw4 = lm(w4_adj ~ time(w4_adj) )

r4 = fitw4$residuals



sa3 <- stl(w3_adj, s.window = "periodic")
seas_adj3 <- w3_adj - sa3$time.series[, "seasonal"]
autoplot(seas_adj3)
adfTest(seas_adj3,type='c') 
kpss.test(seas_adj3,null='Level')


df3 = diff(seas_adj3)
df4 = diff(seas_adj4)

autoplot(diff(w3_adj,lag=52)) 

autoplot(diff(w4_adj,lag=52)) 


fitw3 = lm(w3_adj ~ time(w3_adj) )

r3 = fitw3$residuals
plot(r4)
adfTest(r4,type='c') 
kpss.test(r4,null='Level')



