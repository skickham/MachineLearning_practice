############################
##### Time Series Analysis##
############################
############################

# Examples of time series 

# 1D random walks

# Bernoulli random walk
# rbinom with size = 1 is nothing but Bernoulli distribution 
epsilon_ber <- 2*rbinom(100,1,p=0.5)-1     # converting the values 0, 1 to -1, 1
x_ber <- cumsum(epsilon_ber)
plot(x_ber, type='l')

# Gaussian random walk

epsilon_norm <- rnorm(100, 0, sd=0.1)
x_norm <- cumsum(epsilon_norm)
plot(x_norm, type='l')


# 2D random walks

# Bernoulli random walk

N<-10000

epsilon1_ber <-  2 * rbinom(N,1,p=0.5) - 1  
epsilon2_ber <-  2 * rbinom(N,1,p=0.5) - 1
x_ber <-  cumsum(epsilon1_ber)
y_ber <-  cumsum(epsilon2_ber)
plot(x_ber, y_ber, type='l')

# Gaussian random walk

N<-10000

epsilon1_norm <-  rnorm(N,0,0.1) 
epsilon2_norm <-  rnorm(N,0,0.1)
x_norm <-  cumsum(epsilon1_norm)
y_norm <-  cumsum(epsilon2_norm)
plot(x_norm, y_norm, type='l')

##################
## SnP500
##################

# SnP500 index price series
library(quantmod)


sp500<-new.env()
startDate = as.Date('1960-01-04')
endDate   = as.Date('2017-05-23')
quantmod::getSymbols('^GSPC',env=sp500, src='yahoo', from=startDate,
           to=endDate,auto.assign=T)   # im confused it wont come out, why??????

# No dividend for a stock index, but useful for the individual stocks
quantmod::getDividends('^GSPC', env=sp500, src='yahoo', from=startDate,
           to=endDate,auto.assign=T)
# No split for a stock index, but useful for the individual stocks
quantmod::getSplits('^GSPC', env=sp500, src='yahoo',from=startDate,to=endDate,auto.assign=T)


head(sp500$GSPC)
tail(sp500$GSPC)
#header: GSPC.Open GSPC.High GSPC.Low GSPC.Close GSPC.Volume GSPC.Adjusted

class(sp500$GSPC)

sp_prc<-sp500$GSPC[,6]   # becomes a time series of stock price
# be careful not to add timeseries with incorrect labels 
class(sp_prc)    # xts, zoo      'zoo' stands for Z's ordered observations
# xts can handle irregular period time series as well as regular time series

#1960-01-04         59.91
#2017-05-23       2394.02
# total return    (2394.02-59.91)/59.91 ~ 38.96
# daily return  
plot(sp_prc)
ret_sp500_daily<-(sp_prc-lag(sp_prc))/lag(sp_prc)

#or 
ret_sp500_daily<-diff(sp_prc)/lag(sp_prc)    # difference time series removes overll trend

plot(ret_sp500_daily)
mean(ret_sp500_daily,na.rm=T) * 252     # annualized return  # should be about 8% b/c ...
# 0.0770374 ~ 7.7%
# This excludes the yearly dividends paid out to the investors
# the SP500's dividend yield is currently ~2%, the longer term average is 3-5%.
# total return is >= 10%/year

# to get the time series with dividends, use the ETF ticker = SPY

spy<-new.env()
quantmod::getSymbols('SPY',env=spy, src='yahoo', from=as.Date('1993-01-29'),
           to=as.Date('2017-05-23'))

quantmod::getDividends('SPY',env=spy, src='yahoo', from=as.Date('1993-01-29'),
           to=as.Date('2017-05-23'),auto.assign=T)

quantmod::getSplits('SPY',env=spy, src='yahoo', from=as.Date('1993-01-29'),
             to=as.Date('2017-05-23'),auto.assign=T)

head(spy$SPY)
spy_prc <- spy$SPY[,6]
ret_spy_daily <- diff(spy_prc)/lag(spy_prc)
ret_spy_div   <- spy$SPY.div/spy_prc
mean(ret_spy_daily, na.rm=T) * 252    # ~ 8.71%/year
mean(ret_spy_div,na.rm=T) * 4 # one dividend per quarter 1.80%/year
# Total is about 10.51%/year from 1993-01-29 to 2017-05-23

# rollmean   and smoothing
sp_avg20 = rollmean(sp_prc, 20, align='center')   # center rolling average
plot(sp_avg20)

sp_avg100 = rollmean(sp_prc, 100, align='center')
plot(sp_avg100)

ret_sp500_daily<-ret_sp500_daily[2:length(ret_sp500_daily)]  # removing the first nan element
ret_sp500_avg20 = rollmean(ret_sp500_daily, 20, align='center')
plot(ret_sp500_avg20)

ret_sp500_avg100 = rollmean(ret_sp500_daily, 100, align='center')
plot(ret_sp500_avg100)       # The 100 trading days moving average is smoother than the 20 days version

# The daily return of SP500 has a big fat tail 
qqnorm(ret_sp500_daily)
qqline(ret_sp500_daily)   # even stock time series has fat tail (?) 

# The 1987 Black Monday drops 20% in a day, but its effect is smoothed out after 100-day moving window
# Now the most significant drop becomes the finanical crisis during 2008-2009

###################
# AR(1) example ###
###################


N <- 100
sigma <- 0.1
epsilon = rnorm(N, 0, sigma)
X <- numeric(N)
theta <- 0.2
X[1]  <- 0.0
for (i in 2:N) {
  X[i] <- theta * X[i-1] + epsilon[i]  
}
plot(X,type='l')


#####################
# MA(1) Example  ####
#####################


N <- 100
sigma <- 0.1
epsilon = rnorm(N, 0, sigma)
Y <- numeric(N)
phi <- 0.6
Y[1]  <- 0.0
for (i in 2:N) {
  Y[i] <- epsilon[i] + phi * epsilon[i-1]  
}
plot(Y,type='l')


###########################
# ADF test for stationarity
###########################

library(tseries)
# test for stationary
# adf.test
adf.test(sp_prc)
#Augmented Dickey-Fuller Test

#data:  sp_prc
#Dickey-Fuller = -0.5964, Lag order = 24, p-value = 0.9775
#alternative hypothesis: stationary

# ~97.7% probability it is NOT a stationary time series
# This can be seen by the naked eye

adf.test(ret_sp500_daily)

#data:  ret_sp500_daily
#Dickey-Fuller = -24.613, Lag order = 24, p-value = 0.01
#alternative hypothesis: stationary

#Warning message:
#  In adf.test(ret_sp500_daily) : p-value smaller than printed p-value

# About < 1% chance to be a non-stationary time series

sp_logPrc <- log(sp_prc)
logRet_sp500_daily <- diff(sp_logPrc)
logRet_sp500_daily <- logRet_sp500_daily[2:length(logRet_sp500_daily)]

adf.test(logRet_sp500_daily)

#Augmented Dickey-Fuller Test

#data:  logRet_sp500_daily
#Dickey-Fuller = -24.35, Lag order = 24, p-value = 0.01
#alternative hypothesis: stationary

#Warning message:
#  In adf.test(logRet_sp500_daily) : p-value smaller than printed p-value


### Some simulated AR(1) time series to demonstrate 
### the usage of augumented Fuller-Dickey test

#
N <- 100
sigma <- 0.1
epsilon = rnorm(N, 0, sigma)
X <- numeric(N)
theta <- 0.2
X[1]  <- 2.0
for (i in 2:N) {
    X[i] <- theta * X[i-1] + epsilon[i]  
}

adf.test(X)
# The chance to be non-stationary is < 1%

set.seed(0)
N <- 100
sigma <- 0.1
epsilon = rnorm(N, 0, sigma)
X <- numeric(N)
theta <- 0.95
X[1]  <- 2.0
for (i in 2:N) {
  X[i] <- theta * X[i-1] + epsilon[i]  
  
}

adf.test(X)
# > 46% chance it is a non-stationary time series
# actually such an AR(1) time series is indeed stationary
################
# The accuracy of adf.test depends on the sample size
################

set.seed(0)
N <- 1000
sigma <- 0.1
epsilon = rnorm(N, 0, sigma)
X <- numeric(N)
theta <- 0.95
X[1]  <- 2.0
for (i in 2:N) {
  X[i] <- theta * X[i-1] + epsilon[i]  
}

adf.test(X)
# Increasing N to 1000, p-value drops to <1%

# Conclusion:  The accuracy of the adf.test depends on the sample size
# Do not apply the statistical test blindly

set.seed(0)
N <- 100
sigma <- 0.01
epsilon = rnorm(N, 0, sigma)
X <- numeric(N)
theta <- 0.95
X[1]  <- 2.0
for (i in 2:N) {
  X[i] <- theta * X[i-1] + epsilon[i]  
  
}

adf.test(X)
# keeping N to be 100, but dropping the noise sd to 0.01
# now p-value < 1%!
# not only sample size that matters, noise also matters!

# Conclusion: statistical test is often not bullet-proof. It depends on the sample sizes
# and the standard error of the noise
# In real life, the standard error of the noise is not known a priori. They have to be estimated.

################
# acf
# With the above simulated time series, theta = 0.5
################

N <- 100
sigma <- 0.1
epsilon = rnorm(N, 0, sigma)
X <- numeric(N)
theta <- 0.5
X[1]  <- 2.0
for (i in 2:N) {
  X[i] <- theta * X[i-1] + epsilon[i]  
}

acf(X, lag.max=10)  # lag.max = 10 to plot only the 10 terms
# anything within the confidence band, youre not sure if its zero or not


N <- 1000
sigma <- 0.1
epsilon = rnorm(N, 0, sigma)
X <- numeric(N)
theta <- 0.5
X[1]  <- 2.0
for (i in 2:N) {
  X[i] <- theta * X[i-1] + epsilon[i]  
  
}

#############################
# AutoCorrelations of SP500
#############################

################################
# No long term auto-corrleation
################################

########################## side note: I keep getting a "missing values" error and dont know what to do about it


acf(X, lag.max=10)  # The noise in the auto-correlation function reduces as N goes large

# acf on the sp500 daily return
acf(ret_sp500_daily, lag.max=100)   # The stock index is generally efficient, there is no long term pattern
# in the long term, the stock market is unpredictable
#################################
# 100 day-window autocorrelation
#################################

result_theta<-rollapply(as.vector(ret_sp500_daily),width=100,FUN=acf,lag.max=1,
                        type='correlation',plot=FALSE, align='right')
# result_theta is a matrix
M<-length(result_theta[,1])
thetas <- numeric(M)

for (i in 1:M) {
  thetas[i] <- result_theta[i,]$acf[2]
}


thetas <- as.xts(thetas, order.by=index(sp500_vola_daily[100:(M+99)]))
plot(thetas)


######################################
#  Analysis involving spx volatility
######################################

sp500_vola_daily <-rollapply(ret_sp500_daily, width=100, FUN=sd)
plot(sp500_vola_daily)


# Some EDA on the relationship between volatility and thetas
return_vola_100 <- sp500_vola_daily[100:(M+99)]    # objet M not found!?!?!? wtf
return_vola     <- as.vector(return_vola_100)
return_autocorr <- as.vector(thetas)
return_daily_100    <- as.vector(rollmean(ret_sp500_daily, 100, align='right'))
plot(return_daily_100, return_vola)
plot(return_vola, return_autocorr)

plot(return_daily_100, type='l')
plot(return_vola_100, type='l')

## the damn scatter plot tells you how peple over react and the market tends to bounce back or something....??? dot get it


adf.test(return_vola_100)  #likely to be stationary
# What if we fit an arima model by calling auto.arima

#################################
# Model Selection by auto.arima
#################################

#install.packages('forecast')
library(forecast)

auto.arima(return_vola_100)  # default kpss test rejects the null hypothesis of stationary time series

#Series: return_vola_100 
#ARIMA(4,1,4)    # opposite of acf tests


#Coefficients:
#  ar1     ar2     ar3     ar4     ma1      ma2      ma3      ma4
#-0.4447  0.3120  0.4706  0.4231  0.5465  -0.1300  -0.3866  -0.4572
#s.e.   0.0914  0.0718  0.0548  0.0833  0.0895   0.0749   0.0462   0.0702

#sigma^2 estimated as 3.455e-08:  log likelihood=102634.8
#AIC=-205251.5   AICc=-205251.5   BIC=-205183.4

auto.arima(return_vola_100, test='adf')





#Series: return_vola_100 
#ARIMA(0,0,5) with non-zero mean 
# this one is more consistent with the reality somehow....it is more consistent and mroe good
# give better results


#Coefficients:
#  ma1     ma2     ma3     ma4     ma5   mean
#2.1842  2.9673  2.7398  1.6827  0.6009  0.009
#s.e.  0.0085  0.0144  0.0139  0.0101  0.0061  0.000

#sigma^2 estimated as 2.746e-07:  log likelihood=87804.66
#AIC=-175595.3   AICc=-175595.3   BIC=-175542.3

# sample the volatility series to avoid samples using the same raw data

sample <- seq(100, 14299, 100)
ret_vola_sampled <- return_vola_100[sample]

auto.arima(ret_vola_sampled, test='adf', trace=T)  #ARIMA(1,0,0) with non-zero mean
# compared with
auto.arima(ret_vola_sampled, trace=T)  # ARIMA(1,1,1) by using the kpss test intead of adf test

# 
auto.arima(ret_vola_sampled^2, test='adf', trace=T)   # Don't forget Box-Cox!
# The AR coefficient drops in the new model

acf(ret_vola_sampled^2)

#######################
# pacf vs acf
#######################

# pacf

N <- 1000
epsilon <- rnorm(N, 0, 0.1)
X <- numeric(N)
for (i in 3:N) {
  X[i] <- 0.5 * X[i-2] + epsilon[i]   # the x[i-2] term makes it multilinear perhaps thats what aiko said
}

# theta_1 = 0, theta_2 = 0.5

# compare acf vs pacf 
acf(X, lag.max=20)
pacf(X, lag.max=20) # the way to correct the acf function somehow

#############################
# Simplicity of acf with ma
#############################

# On the ACF of an MA(2) series
N <- 1000
epsilon <- rnorm(N, 0, 0.1)
X <- numeric(N)
for (i in 3:N) {
  X[i] <- epsilon[i] + 1.0 * epsilon[i-1] - 0.5 * epsilon[i-2]
}

acf(X, lag.max=20)  # Notice that acf drops to nearly zero after (including) k=3

###############################################
# compute the theoretical acf of arma series
###############################################

# compute the theoretical ACF from an ARMA TS
plot(ARMAacf(ar = 0, ma=c(1.0, -0.5), lag.max=20, pacf=FALSE))
plot(ARMAacf(ar = 0, ma=c(1.0, -0.5), lag.max=20, pacf=TRUE))

####################################
# Fitting an arima series manually
####################################

arima(sp_prc, order=c(1,1,0))
#arima(x = sp_prc, order = c(1, 1, 0))

#Coefficients:
#  ar1
#-0.0521
#s.e.   0.0083

#sigma^2 estimated as 79.84:  log likelihood = -52008.25,  aic = 104020.5
 
in
arima(sp_prc, order=c(1,0,0))
# Error in arima(sp_prc, order = c(1, 0, 0)) : 
# non-stationary AR part from CSS

arima(sp_prc, order=c(1,1,1))
#arima(x = sp_prc, order = c(1, 1, 1))

#Coefficients:
#  ar1      ma1
#0.6758  -0.7301
#s.e.  0.0510   0.0473

#sigma^2 estimated as 79.62:  log likelihood = -51988.93,  aic = 103983.9

fit<-arima(sp_prc, order=c(2,1,0))
#summary(fit)
#Call:
#  arima(x = sp_prc, order = c(2, 1, 0))

#Coefficients:
#  ar1      ar2
#-0.0540  -0.0361
#s.e.   0.0083   0.0083

#sigma^2 estimated as 79.73:  log likelihood = -51998.86,  aic = 104003.7

#Training set error measures:
#  ME     RMSE      MAE        MPE      MAPE      MASE          ACF1
#Training set 0.1739866 8.928983 4.428399 0.02225356 0.6800208 0.9995924 -0.0008523576


#RMSE stands for Root Mean Squared Error


####################
####  Forecast
####################
#install.packages('forecast')
library(forecast)

output<-forecast(fit,h=252)   # RWF = random walk focus
plot(output)

names(output)
head(output$lower)
head(output$upper)

#80% and 95% confident band

#One may fit an arima model automatically by calling

fit <- auto.arima(sp_prc, test='adf')
summary(fit)
fitted.values(fit)
residuals(fit)

#Then doing the forecast

output<-forecast(fit,h=252)
plot(output)


# predict without fitting a time series model

plot(naive(sp_prc[1:4000],h=200))
# naive is the wrapper of rwf

#############################################
# computing out-of-sample forecast accuracy
#############################################

L <- length(ret_sp500_daily)-5000  # shrink the length to avoid row index error
x <- ret_sp500_daily[1:L]
fit2<-auto.arima(x)
fc <- forecast(fit2, h=200)  # the output would have error if the number of row > 1e4
names(fc)
accuracy(fc, ret_sp500_daily[(L+1):(L+200)])   # can run the accurcy...cooly

L    <- length(sp_prc) - 5000
x    <- sp_prc[1:L]

###############
## adf vs kpss
###############

fit.adf <- auto.arima(x,test='adf')
fc.adf  <- forecast(fit.adf, h=200)
accuracy(fc.adf, sp_prc[(L+1):(L+200)])
plot(fc.adf)

fit.kpss <- auto.arima(x)  # using the default kpss test
fc.kpss  <- forecast(fit.kpss, h=200)
accuracy(fc.kpss, sp_prc[(L+1):(L+200)])
plot(fc.kpss)

#################
# nonlinear time series with nnetar, the neural network auto-regressive forecast
# fitting nn-ar time series
#################
# nnetar is from the forecast package

nn_model <- nnetar(ret_sp500_daily[2:L,1])
nn_model$p     # 33
nn_model$P     # 0
nn_model$size  #17 = round((33+0+1)/2) neuron size
fitted.nn<-fitted.nnetar(nn_model)
residuals.nnetar<-residuals.nnetar(nn_model)
sqrt(mean(residuals.nnetar**2,na.rm=T))  # 0.007213
accuracy(nn_model)
fc_nn <- forecast(nn_model, h=200)
accuracy(fc_nn, ret_sp500_daily[(L+1):(L+200), 1])
                 #ME        RMSE         MAE      MPE     MAPE      MASE        ACF1
#Training set 7.500949e-06 0.007213015 0.005045455      NaN      Inf 0.6409732 -0.00518907
#Test set     1.865454e-03 0.011639015 0.008362533 106.1239 112.2939 1.0623739          NA
# compared with 
arima_model <- auto.arima(ret_sp500_daily[2:L,1])
accuracy(arima_model)
fc_arima<- forecast(arima_model, h=200)
accuracy(fc_arima, ret_sp500_daily[(L+1):(L+200), 1])
                  #ME        RMSE         MAE      MPE     MAPE      MASE         ACF1
#Training set 7.633745e-07 0.008459978 0.005867954      NaN      Inf 0.7454633 -0.000103191
#Test set     7.240823e-04 0.011450707 0.008191159 98.95991 99.03729 1.0406025           NA

#neural network model has a slightly better in-sample performance, but works worse out-of-sample
