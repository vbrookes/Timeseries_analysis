## ----setup, include=FALSE------------------------------------------------
# Chunk 1 ----
knitr::opts_chunk$set(echo = TRUE)

pagebreak <- function() {
  if(knitr::is_latex_output())
    return("\\newpage")
  else
    return('<div style="page-break-before: always;" />')
}



## ----wrap-hook, messages = FALSE-----------------------------------------
# Chunk 2 ----

library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)


## ------------------------------------------------------------------------
# Chunk 3 ----

library(tseries)
library(vars)
library(forecast)
library(RCurl)


## ------------------------------------------------------------------------
# Chunk 4 ----

dataSource_CPV = getURL("https://raw.githubusercontent.com/vbrookes/Timeseries_analysis/master/Parvo_TS_clean.csv")
ParvoD <- read.csv(text = dataSource_CPV, header = T)
summary(ParvoD)
str(ParvoD)
head(ParvoD)
tail(ParvoD)



## ------------------------------------------------------------------------
## Duplication
# Chunk 5 ----

which(duplicated(ParvoD))  # anyDuplicated() does the same thing 
AllCompleteData = unique(ParvoD) # Can check that the dataset is the same length

#missing values in entire data set
ParvoD$complete<-complete.cases(ParvoD) # shows you if there are missing values in the row. If row is complete=TRUE
length(ParvoD$complete)
missingData <- ParvoD[which(ParvoD$complete == "FALSE"),]
missingData



## ------------------------------------------------------------------------
# Chunk 6 ----

length(ParvoD$complete) # total events
str(ParvoD)

sum(ParvoD$Cases) # total cases
sum(ParvoD$Events) # total events

## Save Case.Date to Date in a new column, with as.Date format
ParvoD$Date = as.Date(ParvoD$Case.Date, "%d/%m/%Y")

## Remove rows with no date (this should also have been detected in the previous chunk of code).
ParvoD <- subset(ParvoD,!(is.na(ParvoD["Date"]) ))

## Add columns that are by week or month
ParvoD$byWeek = cut(ParvoD$Date, breaks="1 week") 
ParvoD$byMonth = cut(ParvoD$Date, breaks="1 month") 

## find the minimum and maximum dates of observations
min(ParvoD$Date, na.rm = T)
max(ParvoD$Date, na.rm = T)



## ------------------------------------------------------------------------
# Chunk 7 ----

#### Aggregate data by week
ParvoW = ddply(ParvoD, c("byWeek"), summarise,
                     Cases = sum(Cases),
                     Events = sum(Events))
ParvoW$byWeek <-  as.Date(ParvoW$byWeek, format = "%Y-%m-%d")
str(ParvoW)
summary(ParvoW)

ParvoM = ddply(ParvoD, c("byMonth"), summarise,
                     Cases = sum(Cases),
                     Events = sum(Events))
ParvoM$byMonth <-  as.Date(ParvoM$byMonth, format = "%Y-%m-%d")
str(ParvoM)
summary(ParvoM)



## ------------------------------------------------------------------------
# Chunk 8 ----

# Create time sequence with 1 week intervals
data.length <- length(ParvoW$byWeek)
min.date = min(ParvoW$byWeek)
max.date = max(ParvoW$byWeek)

# Check length
length(ParvoW$Events)

all.dates <- seq(min.date, max.date, by="week")
all.dates.frame <- data.frame(list(byWeek=all.dates))  # Make it into a data frame so that we can merge it 

# Merge data with weekly Parvo data
ParvoW <- merge(all.dates.frame, ParvoW, all = T)
# Change NAs to 0
ParvoW$Cases[is.na(ParvoW$Cases)] <- 0
ParvoW$Events[is.na(ParvoW$Events)] <- 0

# summary
summary(ParvoW)
# check structure
str(ParvoW)
# Check length
length(ParvoW$Events) # Note that in this dataset, there must be 2 weeks with 0 cases (because this is two weeks longer).



## ------------------------------------------------------------------------
# Chunk 9 ----

# Create time sequence with 1 month intervals
data.lengthM <- length(ParvoM$byMonth)
min.dateM = min(ParvoM$byMonth)
max.dateM = max(ParvoM$byMonth)

# Check length
length(ParvoM$Cases)

all.dates <- seq(min.dateM, max.dateM, by="month")
all.dates.frame <- data.frame(list(byMonth=all.dates))  # Make it into a data frame so that we can merge it 

# Merge data with monthly Parvo data
ParvoM <- merge(all.dates.frame, ParvoM, all = T)
# Change NAs to 0
ParvoM$Cases[which(is.na(ParvoM$Cases))] <- 0
ParvoM$Events[which(is.na(ParvoM$Events))] <- 0

# summary
summary(ParvoM)
# check structure
str(ParvoM)
write.csv(ParvoM, 'D:/Users/vbrookes/Dropbox (Sydney Uni)/Parvo_timeseries/Parvo_Month.csv')
# Check length
length(ParvoM$Events) 



## ------------------------------------------------------------------------
# Chunk 10 ----

themeVB = theme(axis.text.x = element_text(colour = "black", angle = 90, hjust=1,vjust=0.5, size = 11), 
                axis.line = element_line(colour = "black"), 
                axis.text.y = element_text(colour = "black"),
                axis.ticks = element_line(colour = "black"),
                axis.title.y = element_text(vjust=1.5),
                panel.grid.major = element_line(colour = "grey93"),
                panel.grid.minor = element_line(colour = "white"),
                panel.background = element_blank(),
                legend.background = element_rect(colour = NA), 
                legend.key = element_rect(fill = 'transparent'))



## ------------------------------------------------------------------------
# Chunk 11 ----

ggplot(ParvoW, aes(x = byWeek, y = Events, group = 1)) +
  geom_bar(stat="identity", width = 0.5, colour = "black") +
  stat_smooth(aes(y = Events), method='auto', level=0.95) + 
  themeVB +
  scale_x_date() +
  xlab("Year") +
  ylab("Events") 

ggplot(ParvoM, aes(x = byMonth, y = Events, group = 1)) +
  geom_bar(stat="identity", width = 0.5, colour = "black") +
  stat_smooth(aes(y = Events), method='auto', level=0.95) + 
  themeVB +
  scale_x_date() +
  xlab("Year") +
  ylab("Events") 




## ------------------------------------------------------------------------
# Chunk 12 ----

Parvo_ts_data <- ts(ParvoW$Events, start = c(2009, 40), frequency =52)

Parvo_ts_dataM <- ts(ParvoM$Events, start = c(2009, 10), frequency =12)



## ------------------------------------------------------------------------
# Chunk 13 ----

# Weekly ts dataset
fit_Parvo_data = ts_all_model <- lm(Parvo_ts_data ~ time(Parvo_ts_data) + factor(cycle(Parvo_ts_data)))
summary(fit_Parvo_data)
confint(fit_Parvo_data)

# Monthly ts dataset
fit_Parvo_dataM = ts_all_model <- lm(Parvo_ts_dataM ~ time(Parvo_ts_dataM) + factor(cycle(Parvo_ts_dataM)))
summary(fit_Parvo_dataM)
confint(fit_Parvo_dataM)



## ------------------------------------------------------------------------
# Chunk 14 ----

# Weekly ts dataset
Decompose_ts <- decompose(Parvo_ts_data)
summary(Decompose_ts)
plot(Decompose_ts)

plot(Decompose_ts$trend + Decompose_ts$random, ylab = "trend + random components")  # can also plot combinations

Decompose_ts_loess <- stl(Parvo_ts_data, s.window="periodic")
plot(Decompose_ts_loess)

# Monthly ts dataset
Decompose_tsM <- decompose(Parvo_ts_dataM)
summary(Decompose_tsM)
plot(Decompose_tsM)

Decompose_ts_loessM <- stl(Parvo_ts_dataM, s.window="periodic")
plot(Decompose_ts_loessM)

## There looks like seasonality. Add a week and month column to ParvoW to investigate further
ParvoW$Year = year(ParvoW$byWeek)
ParvoW$Week = week(ParvoW$byWeek)
ParvoW$Month = month(ParvoW$byWeek)

ggplot(ParvoW, aes(y=Events, x = Week)) + geom_boxplot(aes(group=Week), fill = 'grey90') + themeVB 
ggplot(ParvoW, aes(y=Events, x = Month)) + geom_boxplot(aes(group=Month), fill = 'grey90') + themeVB 
ggplot(ParvoW, aes(y=Events, x = Year)) + geom_boxplot(aes(group=Year), fill = 'grey90') + themeVB 


## ------------------------------------------------------------------------
# Chunk 15 ----

### Compare the seasonality plots

plot(Decompose_tsM$seasonal) 
lines(Decompose_ts$seasonal, col = 'red') 



## ------------------------------------------------------------------------
# Chunk 16 ----

## Tests for stationarity
# Weekly data
Parvo_ts_data %>% ggtsdisplay(theme = themeVB)

ndiffs(Parvo_ts_data) # 1 difference estimated to induce stationarity
nsdiffs(Parvo_ts_data) # No differencing required to induce stationarity

Parvo_ts_data %>% diff() %>% ggtsdisplay(theme = themeVB) # Assess differenced ts and ACF and PACF plots.

# Monthly data
Parvo_ts_dataM %>% ggtsdisplay(theme = themeVB)

ndiffs(Parvo_ts_dataM) # 1 difference estimated to induce stationarity
nsdiffs(Parvo_ts_dataM) # No differencing required to induce stationarity

Parvo_ts_dataM %>% diff() %>% ggtsdisplay(theme = themeVB) # Assess differenced ts and ACF and PACF plots.



## ------------------------------------------------------------------------
# Chunk 17 ----

## Weekly time-series tests for stationarity
# 1. 
Box.test(Parvo_ts_data, lag = 52, type="Ljung-Box") 

# 2.
adf.test(Parvo_ts_data, alternative = "stationary", k = 104) 

# 3.
kpss.test(Parvo_ts_data, null = 'Trend') # The null hypothesis is a time trend with stationary error.
kpss.test(Parvo_ts_data, null = 'Level') # The null hypothesis is that the series is white noise.

## Monthly time-series tests for stationarity
# 1. 
Box.test(Parvo_ts_dataM, lag = 12, type="Ljung-Box") 

# 2.
adf.test(Parvo_ts_dataM, alternative = "stationary", k = 24) 

# 3.
kpss.test(Parvo_ts_dataM, null = 'Trend') # The null hypothesis is a time trend with stationary error.
kpss.test(Parvo_ts_dataM, null = 'Level') # The null hypothesis is that the series is white noise.



## ------------------------------------------------------------------------
# Chunk 18 ----

## Weekly time-series tests for stationarity
# 1. 
Box.test(diff(Parvo_ts_data), lag=52, type="Ljung-Box") 

# 2.
adf.test(diff(Parvo_ts_data), alternative = "stationary", k = 104) 

# 3.
kpss.test(diff(Parvo_ts_data), null = 'Trend') # The null hypothesis is a time trend with stationary error.
kpss.test(diff(Parvo_ts_data), null = 'Level') # The null hypothesis is that the series is white noise.

## Monthly time-series tests for stationarity
# 1. 
Box.test(diff(Parvo_ts_dataM), lag=12, type="Ljung-Box") 

# 2.
adf.test(diff(Parvo_ts_dataM), alternative = "stationary", k = 24) 

# 3.
kpss.test(diff(Parvo_ts_dataM), null = 'Trend') # The null hypothesis is a time trend with stationary error.
kpss.test(diff(Parvo_ts_dataM), null = 'Level') # The null hypothesis is that the series is white noise.



## ------------------------------------------------------------------------
# Chunk 19 ----

## Examine ACF and PACF plots of differenced time-series again to estimate p, P, q and Q.
# Weekly data
Parvo_ts_data %>% diff() %>% ggtsdisplay(theme = themeVB) # Assess differenced ts and ACF and PACF plots.

# Monthly data
Parvo_ts_dataM %>% diff() %>% ggtsdisplay(theme = themeVB) # Assess differenced ts and ACF and PACF plots.



## ------------------------------------------------------------------------
# Chunk 20 ----

Auto_Arima = auto.arima(Parvo_ts_data)
summary(Auto_Arima)
plot(forecast(Auto_Arima, 100))
confint(Auto_Arima) 
checkresiduals(Auto_Arima)


## ------------------------------------------------------------------------
# Chunk 21 ----

Auto_ArimaM = auto.arima(Parvo_ts_dataM)
summary(Auto_ArimaM)
plot(forecast(Auto_ArimaM,24))
confint(Auto_ArimaM) 
checkresiduals(Auto_ArimaM)


## ------------------------------------------------------------------------
# Chunk 22 ----

### Weekly time series models
## Weekly ARIMA: (2, 1, 1) (1-3, 0, 1-3) [52]
## fitted: (3,1,1)(1,0,0)[52] with drift, AICc=1834.24

fit1 = Arima(Parvo_ts_data, order = c(2, 1, 1), seasonal = c(1, 0, 0))
summary(fit1)
plot(forecast(fit1,100))
confint(fit1) 
checkresiduals(fit1)

fit2 = Arima(Parvo_ts_data, order = c(2, 1, 1), seasonal = c(0, 0, 1))
summary(fit2)
plot(forecast(fit2,100))
confint(fit2) 
checkresiduals(fit2)

fit3 = Arima(Parvo_ts_data, order = c(2, 1, 1), seasonal = c(1, 0, 0), include.drift = T)  # *** This model is the best weekly model
summary(fit3)
plot(forecast(fit3,100))
confint(fit3) 
checkresiduals(fit3)

### monthly time series models
## Monthly ARIMA: (0-2, 1, 0-2) (0-1, 0, 2) [12]
## Auto fitted: (2,1,1)(2,0,0)[12], AICc=632.61

fit4 = Arima(Parvo_ts_dataM, order = c(1, 1, 1), seasonal = c(1, 0, 0))
summary(fit4)
plot(forecast(fit4, 24))
confint(fit4) 
checkresiduals(fit4)

fit5 = Arima(Parvo_ts_dataM, order = c(2, 1, 1), seasonal = c(0, 0, 1))
summary(fit5)
plot(forecast(fit5, 24))
confint(fit5) 
checkresiduals(fit5)

fit6 = Arima(Parvo_ts_dataM, order = c(2, 1, 1), seasonal = c(2, 0, 0), include.drift = T)
summary(fit6)
plot(forecast(fit6, 24))
confint(fit6) 
checkresiduals(fit6)

fit7 = Arima(Parvo_ts_dataM, order = c(1, 1, 0), seasonal = c(1, 0, 0))
summary(fit7)
plot(forecast(fit7, 24))
confint(fit7) 
checkresiduals(fit7)



## ------------------------------------------------------------------------
# Chunk 23 ----

dataSource_Rain = getURL("https://raw.githubusercontent.com/vbrookes/Timeseries_analysis/master/Mudgee_Monthly_rainfall.csv")
RainfallM = read.csv(text = dataSource_Rain, header = T)
summary(RainfallM)
str(RainfallM)
head(RainfallM)
tail(RainfallM)

RainfallM$Date = as.character(RainfallM$Date, format = "%d/%m/%Y")
RainfallM$Date = as.Date(RainfallM$Date, "%d/%m/%Y")

RainfallM$byMonth = cut(RainfallM$Date, breaks="1 month") 
RainfallM$byMonth <-  as.Date(RainfallM$byMonth, format = "%Y-%m-%d")

# summary
summary(RainfallM)
# check structure
str(RainfallM)
# Check length
length(RainfallM$Dates)

ggplot(RainfallM, aes(x = byMonth, y = Rain_mm, group = 1)) +
  geom_bar(stat="identity", width = 0.5, colour = "black") +
  stat_smooth(aes(y = Rain_mm), method='auto', level=0.95) + 
  themeVB +
  scale_x_date() +
  xlab("Year") +
  ylab("Rainfall") 


Rainfall_ts_data <- ts(RainfallM$Rain_mm, start = c(2009, 10), frequency =12)

Decompose_tsRainM <- decompose(Rainfall_ts_data)
summary(Decompose_tsRainM)
plot(Decompose_tsRainM)



## ------------------------------------------------------------------------
# Chunk 24 ----

# Assess trend and seasonality
fit_Rain_dataM = lm(Rainfall_ts_data ~ time(Rainfall_ts_data) + factor(cycle(Rainfall_ts_data)))
summary(fit_Rain_dataM)
confint(fit_Rain_dataM)

# Tests for stationarity
Rainfall_ts_data %>% ggtsdisplay(theme = themeVB)

# We  run `ndiffs` and `nsdiffs` (seasonal) from the `forecast` package to assess how many first differences are needed to induce stationarity.
ndiffs(Rainfall_ts_data)
nsdiffs(Rainfall_ts_data)

## Monthly time-series tests for stationarity
# 1. 
Box.test(Rainfall_ts_data, lag = 12, type="Ljung-Box") 

# 2.
adf.test(Rainfall_ts_data, alternative = "stationary", k = 24) 

# 3.
kpss.test(Rainfall_ts_data, null = 'Trend') 
kpss.test(Rainfall_ts_data, null = 'Level') 



## ------------------------------------------------------------------------
# Chunk 25 ----

# Make a matrix of lagged predictors (0, 1, 2 or 3 lags).
RainLag <- cbind(
    AdLag0 = Rainfall_ts_data,
    AdLag1 = stats::lag(Rainfall_ts_data, k = 1),
    AdLag2 = stats::lag(Rainfall_ts_data, k = 2),
    AdLag3 = stats::lag(Rainfall_ts_data, k = 3)) %>%
  head(NROW(Rainfall_ts_data))

# Restrict data so models use same fitting period
fit1 <- auto.arima(Parvo_ts_dataM[4:70], xreg=RainLag[4:70,1])
fit2 <- auto.arima(Parvo_ts_dataM[4:70], xreg=RainLag[4:70,1:2])
fit3 <- auto.arima(Parvo_ts_dataM[4:70], xreg=RainLag[4:70,1:3])
fit4 <- auto.arima(Parvo_ts_dataM[4:70], xreg=RainLag[4:70,1:4])

c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])

fit <- auto.arima(Parvo_ts_dataM, xreg=RainLag[,1:3])
summary(fit)
confint(fit)
checkresiduals(fit)

# We then sequentially fir all rainfall lagged data to the best fitting monthly model from the previous section (all tested lags not shown here). The final model includes 3 months of lag and has the lowest AIC of all the models.
fitA <- Arima(Parvo_ts_dataM, order = c(2, 1, 1), seasonal = c(2, 0, 0), xreg=RainLag[,1:4])
summary(fitA)
confint(fitA)
checkresiduals(fitA)

# We use the last fitted model for forecasting. The forecast is fitted with trimmed data otherwise error message: 'Upper prediction intervals are not finite.' due to NAs in lagged rainfall data.
fcast <- forecast(fitA, h = 20, xreg=RainLag[4:70,1:4])
    
autoplot(fcast) + xlab("Year") + ylab("Events")


## ------------------------------------------------------------------------
# Chunk 26 ----

ccf(Parvo_ts_dataM, Rainfall_ts_data, type = 'correlation')


## ------------------------------------------------------------------------
# Chunk 27 ----

# Difference Parvo data because model is VARMA (no I)
ParvoM_diff = diff(Parvo_ts_dataM)

Differenced_data <- cbind(ParvoM_diff, Rainfall_ts_data) # Combine in a dataframe with the rainfall data.
Differenced_data[is.na(Differenced_data)] <- 0 # Convert first value from NA to 0


## ------------------------------------------------------------------------
# Chunk 28 ----

# estimate orders for AR(p) and MA(q) between parvo events and rainfall
library(vars)
VARselect(Differenced_data, lag.max = 12, type = "const") # AR(p) = 1 if use SC (BIC), or p = 4 if use the other information criteria.

# Automated model fit
Mod1 <- VAR(Differenced_data, p=1, type = "const") # automated
Mod1
summary(Mod1)
serial.test(Mod1, lags.pt=12)

Mod2 <- VAR(Differenced_data, p=4, type = "const") # automated
Mod2
summary(Mod2)
serial.test(Mod2, lags.pt=12)

# we plot forecasts from each predictive equation
forecast(Mod1) %>%
  autoplot() + xlab("Year")

forecast(Mod2) %>%
  autoplot() + xlab("Year")

