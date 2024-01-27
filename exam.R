

install.packages("D:/_CBS/CBS_4/Predictive Analytics (CDSCO1005U.LA_F22)/_Problem Sets/other/fpp3package-master", 
                 repos = NULL, 
                 type = "source")

install.packages("fpp3",dependencies = TRUE)
install.packages("hms",dependencies = TRUE, type = "source")
install.packages("fpp",dependencies = TRUE, type = "source")
install.packages("chron",dependencies = TRUE, type = "source")
install.packages("Rtools",dependencies = TRUE)

library(readr)
library(hms)
library(chron)
library(dplyr)
library(fpp3)
library(fpp)
library(lubridate)
library(parsedate)
library(urca)
library(strucchange)


setwd("D:/_CBS/CBS_4/Predictive Analytics (CDSCO1005U.LA_F22)/_EXAM")
getwd()
wspeed <- read.csv("wthr_lousakies_5.csv", sep=';',header = TRUE, colClasses = c('Date', 'character', 'double', 'character')) 
#rename(wspeed, Date = ï..Date )
names(wspeed)[names(wspeed) == 'ï..Date'] <- 'Date'

str(wspeed)
sapply(wspeed$Wind.Speed,sd, mean, na.rm=TRUE)
# get summary statistics
summary(wspeed$Wind.Speed)
sd(wspeed$Wind.Speed)
tail(guess_formats(wspeed$Time, "T"),20)

wspeed
wspeed$Time@hour
tail(wspeed$Time)
colnames(wspeed)
#spec(wspeed)
summary(wspeed$Wind.Speed)

t.test(wspeed$Wind.Speed, alternative = "two.sided", mu = 12.01)

### CREATE UTC TIME ######################
wspeed$Time <- hms(wspeed$Time)
is_hms(wspeed$Time)

secs <- seconds(wspeed$Time)
secs
time <- seconds_to_period(secs)
time

time <- seconds(wspeed$Time)
time
days = day(seconds_to_period(time))
days
days <- as.numeric(as.character(days))
Hours = hour(seconds_to_period(time))
Hours
Hours <- as.numeric(as.character(Hours))
Minutes = minute(seconds_to_period(time))
Minutes <- as.numeric(as.character(Minutes))
typeof(Minutes)

wspeed["day"] <- days
wspeed["hours"] <- Hours
wspeed["minutes"] <- Minutes

wspeed$day<- as.numeric(as.character(wspeed$day))
wspeed$hours<- as.numeric(as.character(wspeed$hours))
wspeed$minutes<- as.numeric(as.character(wspeed$minutes))                        
sapply(wspeed, class)
wspeed
with(wspeed, ymd_hm(paste("2022", "6", days+10, hours, minutes, sep= ':')))
time <- with(wspeed, ymd_hm(paste("2022", "6", days+10, Hours, Minutes, sep= ':')))
time
wspeed["Time_UTC"] <- time
wspeed<-wspeed[ , !names(wspeed) %in% c("Date","day","hours","minutes")] 

##############################################################################################

wspeed

ws_ts <- wspeed %>%
  as_tsibble(index = Time_UTC)

tail(ws_ts,20)

ws_ts
tail(ws_ts,20)
str(ws_ts)

autoplot(ws_ts, aes(seconds(Time),Wind.Speed)) + geom_line()
autoplot(ws_ts,Wind.Speed) + labs(title = "Time Series plot of Wind Speed Data")
ggplot(ws_ts, aes(seconds(Time),Wind.Speed )) + geom_line()


ws_ts %>%
  ACF(Wind.Speed, lag_max = 144) %>% autoplot()


# Density and histogram around the mean plots
a <- ggplot(ws_ts, aes(x = Wind.Speed))
a + geom_density() +
  geom_vline(aes(xintercept = mean(Wind.Speed)), 
             linetype = "dashed", size = 0.6)

a + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(Wind.Speed)), 
             linetype = "dashed", size = 0.6)

# entropy (Shannon)
feat_spectral(ws_ts$Wind.Speed)
# box-pierce, ljung-box tests

ws_ts %>% features(Wind.Speed, box_pierce, lag = 6, dof = 0)
ws_ts %>% features(Wind.Speed, ljung_box, lag = 6, dof = 0)
# Box-Cox transformation
ws_ts %>%
  features(Wind.Speed, guerrero)

ws_bc <- ws_ts %>%
  autoplot(box_cox(Wind.Speed, 1.3))

ws_bc <- ws_ts %>%
  mutate(bc = box_cox(Wind.Speed, 1.3))

# log transformation
ws_lg <- ws_ts %>%
  mutate(lg = log(Wind.Speed))
ws_lg

# check lambda method
lambda <- BoxCox.lambda(ws_ts)
lambda

# 
# plot the density and histogram of transformed time series

ws_bc
b <- ggplot(ws_bc, aes(x = bc))
b + geom_density() +
  geom_vline(aes(xintercept = mean(bc)), 
             linetype = "dashed", size = 0.6)


b + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(bc)), 
             linetype = "dashed", size = 0.6)


# Classical decomposition

# ######## Additive
ws_ts %>%
  model(
    classical_decomposition(Wind.Speed, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of Wind Spead ts")

# ######## Multiplicative #####

ws_ts %>%
  model(
    classical_decomposition(Wind.Speed, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical multiplicative decomposition of Wind Spead ts")
# ######## Additive after bc transformation #######
ws_bc %>%
  model(
    classical_decomposition(bc, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of Wind Spead ts")


# ####### multiplicative after bc transformation ######
ws_bc %>%
  model(
    classical_decomposition(bc, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical multiplicative decomposition of Wind Spead ts")


# ######## Additive after log transformation #######
ws_lg %>%
  model(
    classical_decomposition(lg, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of Wind Spead ts")

# ####### multiplicative after log transformation ######
ws_lg %>%
  model(
    classical_decomposition(lg, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical multiplicative decomposition of Wind Spead ts")
#########################
# KPSS ADF

#KPSS deterministic constant or with linear trend
summary(ur.kpss(ws_bc$bc, type = "mu", lags = 'long'))
#summary(ur.kpss(ws_bc$Wind.Speed, type = "mu", lags = 'long'))
summary(ur.kpss(ws_bc$bc, type = "tau"))


# ADF
summary(ur.df(ws_bc$bc, type = "none"))
summary(ur.df(ws_bc$bc, type = "drift"))
summary(ur.df(ws_bc$bc, type = "trend"))
# Wind.Speed

# KPSS with first difference

# ws_bc %>%
#   mutate(diff_ws = difference(Wind.Speed))
summary(ur.kpss(diff(ws_bc$bc), type = "mu"))

summary(ur.kpss(diff(ws_bc$bc, lag = 17), type = "tau"))
# -> stationary series with deterministic constant when testing for 17-lag periods
summary(ur.kpss(diff(diff(ws_bc$bc)), type = "mu"))


# ADF with first difference
summary(ur.df(diff(ws_bc$bc), type = "none"))


##########################

# take correlograms for diferentiated data

ws_bc %>%
  ACF(diff(bc), lag_max = 24) %>% autoplot()

ws_bc %>%
  PACF(diff(bc), lag_max = 24) %>% autoplot()

###  auto ARIMA #####

summary(auto.arima(ws_bc$bc, ic = "aic"))
summary(auto.arima(ws_bc$bc, ic = "bic"))


# fit the first ARIMA and check residuals ###

model_1 <- ws_bc %>%
  model(ARIMA(bc ~ 1 + pdq(1, 1, 2)))
model_1 %>% report()

model_1 %>% gg_tsresiduals()
augment(model_1) %>% features(bc, ljung_box, dof = 2, lag = 10)

(lb4 <- ljung_box(residuals(model_1)$.resid, lag = 10, dof = 4))

# ETS #########################

fc4 = ets(ws_bc$bc, model = "ZZZ")
fc5<-  ets(wspeed$Wind.Speed, model = "AAdN")
fc5 %>% gg_tsresiduals()

## (A, Ad, N ) ##############

dcmp <- ws_ts %>%
  model(STL(Wind.Speed)) %>%
  components()

dcmp %>%
  #as_tsibble() %>%
  autoplot(season_adjust)

ets_1<-ets(wspeed$Wind.Speed,model = "AAN" )
res_2 <- residuals(ets_1)
res_2
autoplot(res_2)

ws_ts %>%
  ACF(res_2, lag_max = 25) %>% autoplot()

a1 <- ggplot(ws_ts, aes(x = res_2))
a1 + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(res_2)), 
             linetype = "dashed", size = 0.6)

# ETS (ANN) ##########
ets_2<-ets(wspeed$Wind.Speed,model = "ANN" )
res_3 <- residuals(ets_2)
res_3
autoplot(res_3)

ws_ts %>%
  ACF(res_3, lag_max = 25) %>% autoplot()

a1 <- ggplot(ws_ts, aes(x = res_3))
a1 + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(res_2)), 
             linetype = "dashed", size = 0.6)

## ETS Ljung Box tests ###
(aan <- ljung_box(res_2, lag = 10, dof = 3))
(ann <- ljung_box(res_3, lag = 10, dof = 3))
##############

# ACF(res_2)
# res_2 %>% gg_tsresidualsals()
# ets_1 %>% gg_tsresiduals()



#%>%
stletsdamped 

re<- residuals(stletsdamped)
autoplot(re) + xlab("Time") + ylab("") +
   ggtitle("Residuals from ETS(A,Ad,N)")

res <- residuals(stletsdamped)
model_1 %>% gg_tsresiduals()



###############################
## breakpoints struct. breaks ###

ws <- ws_ts %>%
  #filter(Country == "Afghanistan") %>%
  as_tsibble() %>%
  select( Wind.Speed) %>% 
  mutate( Lag0 = Wind.Speed, Lag1 = lag(Wind.Speed), Time = Time_UTC) 

qlr <- Fstats(Lag0 ~ Lag1, data = as.ts(ws), from = 0.15)
plot(qlr)

test <- sctest(qlr, type = "supF")

breakpoints(qlr, alpha = 0.05)

plot(qlr, alpha = 0.1, main = "F Statistics") 
lines(breakpoints(qlr))

#########################
## Create model split train/ test ##################
# We want to forecast and test on the last 8 hours of our time series
# which is a period of 96 5-min points.
rows=nrow(ws_bc)
index=0.8044806517311609*rows

### Forecasting ARIMA(1,1,2)
train1 <- ws_ts[(1:index),]
test1 <- ws_ts[-(1:index),]
firstd<-diff(train1$Wind.Speed)
#model_tr1 <-

model_tr1 <- train1 %>%
  model(ARIMA(Wind.Speed ~  pdq(1, 1, 2)))

model_tr1%>%
forecast(h = 96)%>%
autoplot(ws_ts)

fit1 <- forecast(model_tr1, h = 96)

#autoplot(y = c(model_tr1, test1))%>%
#model_tr1 %>% report()

#############################
### Forecasting ETS( AAN )
train2 <- ws_ts[(1:index),]
test2 <- ws_ts[-(1:index),]


ets_1_tr<-ets(train2$Wind.Speed,model = "AAN" )
ets_1_tr
#ets_1_tr%>%
  plot(forecast(ets_1_tr,h = 96))

fit2 <- forecast(ets_1_tr,h = 96)
 

#############################
### Forecasting ETS( ANN )
train3 <- ws_ts[(1:index),]
test3 <- ws_ts[-(1:index),]

ets_2_tr<-ets(train3$Wind.Speed,model = "ANN" )

plot(forecast(ets_2_tr,h = 96))
fit3 <- forecast(ets_2_tr,h = 96)

# train1
# nrow(train1)
# test1
# nrow(test1)

# ACCURACY #######################
fit2
test2
accuracy(fit1,test1)
accuracy(fit2, test2$Wind.Speed)
accuracy(fit3, test3$Wind.Speed)
#################

####### Bind series to plot together
# tseries_diff1 <- diff(tseries, lag = 1)
# tm <- cbind(tseries, tseries_diff1)
# head(tm)
################


rlang::last_error()
rlang::last_trace()
