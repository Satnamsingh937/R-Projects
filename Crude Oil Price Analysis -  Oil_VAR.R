
pack1<-c("car","readxl","ggplot2", "vtable","tidyverse", "jtools", "e1071", "tseries", 
         "ggplot2", "plotly", "fRegression", "forecast") 

pack2<-c("lmtest", "fGarch", "vars", "FinTS", "moments", "rugarch", "sandwich", "rmgarch",
         "urca", "xts") 
#install.packages(pack2)


lapply(pack1, require, character.only = TRUE) 
lapply(pack2, require, character.only = TRUE) 

Data<- read_excel("C:\\Users\\Basma\\Documents\\Econometrics\\oil project\\Data.xlsx", 
                      col_names = TRUE, col_types =c('date', 'numeric', 'numeric', 'numeric', 'numeric'))  # in the sheet " Monthly". Identify the first column as "date"

Data<-data.frame(Data)

###################### Data visualization and pre-processing ###################### 

attach(Data) 
View (Data) 
cnames = colnames(Data)

###########
##NB:
## before any kind of estimation visualize the data, plot acf pacf..
## before doing anything, check if the series is stationary or not, with ADF 
## log here is the natural logarithm, in case you want to switch to level 
###########

#plot the series levels, just to visualize the data before doing any kind of estimation
par(mfrow=c(2,2))
plot(date,production)
plot(date,demand)
plot(date,price)
plot(date,inventories)

#here we wanted to plot the series level, but just for the first year
par(mfrow=c(2,2))
plot(date[1:12],production[1:12])
plot(date[1:12],demand[1:12])
plot(date[1:12],price[1:12])
plot(date[1:12],inventories[1:12])

#here we do the log transformation of the series except for the Demand series (only variable not to transform to log , because it contains negative values)
# we apply the 1st Diff on all the series
LR_Production <- diff (log(production)) 
Demand <- diff(demand)
LR_Price <- diff(log(price))
LR_Inventories <- diff(log(inventories))

#plot the series after applying log and 1st diff transformation
par(mfrow=c(2,2))
plot(date[2:428],LR_Production)
plot(date[2:428],Demand)
plot(date[2:428],LR_Price)
plot(date[2:428],LR_Inventories)

#check for heteroscedasticity
ArchTest(Demand,12)
ArchTest(LR_Inventories,12)
ArchTest(LR_Price,12)
ArchTest(LR_Production,12)

#plot the acf of the log diff series
par(mfrow=c(2,2))
acf(LR_Production)
acf(Demand)
acf(LR_Price)
acf(LR_Inventories)   # we notice very clear structural seasonality for inventories series

#check if the series are stationary, with ADF test
adf.test(LR_Production)
adf.test(Demand)
adf.test(LR_Price)
adf.test(LR_Inventories)

#p-value smaller that alpha = 0.01 so we are 99% confident to REJECT the H0=nonstationarity
###### we clearly see heteroscedasticity in the plots so no need to do the white or arch test, because the heteroscedasticity is visually pronounced.

df_LInventories <- ts(LR_Inventories,start=c(1974,02),end=c(2009,08),frequency=12)

fit.inventories <- tslm(df_LInventories ~ trend + season)
summary(fit.inventories)
#seasonality in inventories


df_prices <- ts(LR_Price,start=c(1974,02),end=c(2009,08),frequency=12)
fit.prices <- tslm(df_prices ~ trend + season)
summary(fit.prices)
# no seasonality in prices


df_demand <- ts(Demand,start=c(1974,02),end=c(2009,08),frequency=12)
fit.demand <- tslm(df_demand ~ trend + season)
summary(fit.demand)
# no pronounced seasonality in demand


df_production <- ts(LR_Production,start=c(1974,02),end=c(2009,08),frequency=12)
fit.production <- tslm(df_production ~ trend + season)
summary(fit.production)
# seasonality in production

#check for seasonality per quarter
#df_production_Q <- ts(production,start=c(1974,01),end=c(2009,08),frequency=12)
#production_Q <- aggregate(df_production_Q, nfrequency = 4)
#LDiff_production_Q <- diff (log(production_Q)) 
#LDiff_production_Q

#fit.production_Q <- tslm(LDiff_production_Q ~ trend + season)
#summary(fit.production_Q)
#seasonality in quarter 3 and 4
# and according to literature: "Based on the above price analysis of crude oil, 
#a projection of it suggests that crude oil prices generally peak between September and October. 
#Than prices start to make lower highs between October and November. "
#Finally, they hit the bottom by December, before they start rising again."

#~~~~~~~~~~~~~~~~~~~~~~~ VAR Model after using Dummies ###############
VARData <- data.frame(fit.production$fitted.values,Demand/100,LR_Price,fit.inventories$fitted.values)
VARselect(VARData, lag.max=5)
#construct the VAR and choose up to the max number of lag that I want R to check, estimate 5 models here, the 1st one with 1 lag, the second one with 2 lags BUT  be parsimonious don't choose a biig number of lags, keep it low, pointless from an economic pt of view 
# Schwarz (SC)  and Hannan (HQ) are more severe
VAR_Model <- VAR(VARData, p = 5)
summary(VAR_Model)

causality(VAR_Model, cause = "fit.production.fitted.values")
#production granger causes other variables

causality(VAR_Model, cause = "Demand.100")
#demand granger cause other variables

causality(VAR_Model, cause = "fit.inventories.fitted.values")
#inventories granger causes other variables

causality(VAR_Model, cause = "LR_Price")
#Price does NOT granger causes other variables
####################################

ir = irf(VAR_Model ,n.ahead = 20)
plot (ir)

vd = fevd (VAR_Model ,n.ahead = 20)
plot (vd)


#~~~~~~~~~~~~~~~~~~~~~~~ Change order of VAR ~~~~~~~~~~~~~~~~~~~~~~~ 
VARData <- data.frame(fit.production$fitted.values,fit.inventories$fitted.values,Demand/100,LR_Price)
VARselect(VARData, lag.max=5)
#construct the VAR and choose up to the max number of lag that I want R to check, estimate 5 models here, the 1st one with 1 lag, the second one with 2 lags BUT  be parsimonious don't choose a biig number of lags, keep it low, pointless from an economic pt of view 
# Schwarz (SC)  and Hannan (HQ) are more severe
VAR_Model <- VAR(VARData, p = 5)
summary(VAR_Model)

ir = irf(VAR_Model ,n.ahead = 20)
plot (ir)

vd = fevd (VAR_Model ,n.ahead = 20)
plot (vd)
