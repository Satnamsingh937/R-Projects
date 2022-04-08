
pack<-c("car","readxl","ggplot2", "vtable","tidyverse", "jtools", "e1071", "tseries", 
        "ggplot2", "plotly", "fRegression", "forecast", 
        "lmtest", "fGarch", "vars", "FinTS", "moments", "rugarch", "sandwich", "rmgarch",
        "urca", "xts") 
install.packages(pack)
lapply(pack, require, character.only = TRUE) 


# 1. 
df_st_Data <- read.csv(file = "C:\\Users\\ssatn\\Documents\\02 - Cours\\04 - NEOMA\\Econometrics and Time Series Applied to Finance\\Group Project\\Stocks_AC.csv") 

#1.1.
cnames = colnames(df_st_Data)
cnames[2]
df_GM_F = data.frame(df_st_Data$Date, df_st_Data$GM_AC, df_st_Data$F_AC)

plot(df_st_Data$Date, df_st_Data$GM_AC, df_st_Data$F_AC, xlab ="Excess Return on Market Portfolio",
     ylab ="Excess Return on Fund",
     xlim=c(5,100), ylim=c(0,30),pch = 200, cex =1, col="black")


#1.2.
T_F = nrow(df_st_Data$F_AC)
T_GM = nrow(df_st_Data$GM_AC)


#1.5
summary(df_st_Data)