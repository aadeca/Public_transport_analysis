
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", "lmtest", "slidify",
                  "olsrr", "moments","whitestrap","ggplot2","DataCombine","car", "dplyr", "tseries" )
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

getwd()
setwd("C:/Users/Amora 2/Desktop")

#Import data
date_CB<-read.csv(file="transit_total_CB.csv", row.names=1)
View(date_CB)
date_CB_log<-log(date_CB)
View(date_CB_log)

#Correlation matrix
cor_matrix <- cor(date_CB_log[, c("avg_fuel_price", "Total_VRH", "Total_VRM", "working_age_pop", "unemp_rate", "nr_accidents", "personal_income")])
View(as.matrix(cor_matrix))

#Regression model
model <- lm(Total_UPT~ avg_fuel_price + Total_VRH + unemp_rate + working_age_pop + nr_accidents + personal_income, data = date_CB_log)
summary(model)

#Heteroscedasticity test
white_test(model) #heteroscedasticity

#Correction with WLS 
model_WLS1 <- lm(Total_UPT~ avg_fuel_price + Total_VRH + unemp_rate + working_age_pop + nr_accidents + personal_income, 
                 data = date_CB_log, weights = 1/avg_fuel_price)

date_CB_log %<>% mutate(UPTstar = Total_UPT/sqrt(avg_fuel_price),
                         avg_price_star = avg_fuel_price/sqrt(avg_fuel_price),
                         VRHstar = Total_VRH/sqrt(avg_fuel_price), 
                         URstar = unemp_rate/sqrt(avg_fuel_price),
                         popstar = working_age_pop/sqrt(avg_fuel_price),
                         accstar = nr_accidents/sqrt(avg_fuel_price),
                         venitstar = personal_income/sqrt(avg_fuel_price),
                         constantstar = 1/sqrt(avg_fuel_price)
)

model_WLS2 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar, date_CB_log) 

white_test(model_WLS2) #homoscedasticity


#Autocorrelation test
dwtest(model_WLS2) #autocorrelation


#New dataset
CB_data <- data.frame(date_CB_log, resid_mod1=model_WLS2$residuals)
# Create lag1 variable 
CB_data_1 <- slide(CB_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
CB_data_2 <- na.omit(CB_data_1) # eliminate NA values
#New model with lag1 variable
model2 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar + lag1, data = CB_data_2)

dwtest(model2) #non-autocorrelation


#Normality test
jarque.bera.test(model_WLS2$residuals) #residuals are not normally distributed
#Cook distance
ols_plot_cooksd_bar(model_WLS2)
CB_cook <- date_CB_log[-c(99,100,101,107,108,110,111,120,121), ]
model3 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar, CB_cook)
jarque.bera.test(model3$residuals) #normally distributed
summary(model3)

#Import data
date_CR<-read.csv(file="transit_total_CR.csv", row.names=1)
View(date_CR)
date_CR_log<-log(date_CR)
View(date_CR_log)

#Correlation matrix
cor_matrix <- cor(date_CR_log[, c("avg_fuel_price", "Total_VRH", "Total_VRM", "working_age_pop", "unemp_rate", "nr_accidents", "personal_income")])
View(as.matrix(cor_matrix))

#Regression model
model <- lm(Total_UPT~ avg_fuel_price + Total_VRH + unemp_rate + working_age_pop + nr_accidents + personal_income, data = date_CR_log)
summary(model)

#Heteroscedasticity test
white_test(model) # heteroscedasticity
# Correction with WLS 
model_WLS1 <- lm(Total_UPT~ avg_fuel_price + Total_VRH + unemp_rate + working_age_pop + nr_accidents + personal_income, 
                 data = date_CR_log, weights = 1/avg_fuel_price)

date_CR_log %<>% mutate(UPTstar = Total_UPT/sqrt(avg_fuel_price),
                        avg_price_star = avg_fuel_price/sqrt(avg_fuel_price),
                        VRHstar = Total_VRH/sqrt(avg_fuel_price), 
                        URstar = unemp_rate/sqrt(avg_fuel_price),
                        popstar = working_age_pop/sqrt(avg_fuel_price),
                        accstar = nr_accidents/sqrt(avg_fuel_price),
                        venitstar = personal_income/sqrt(avg_fuel_price),
                        constantstar = 1/sqrt(avg_fuel_price)
)

model_WLS2 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar, date_CR_log) 

white_test(model_WLS2) # homoscedasticity

#Autocorrelation test
dwtest(model_WLS2) #autocorrelation

#New dataset
CR_data <- data.frame(date_CR_log, resid_mod1=model_WLS2$residuals)
# Create lag1 variable
CR_data_1 <- slide(CR_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
CR_data_2 <- na.omit(CR_data_1) # eliminate NA values
#New model with lag 1 variable
model2 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar + lag1, data = CR_data_2)

dwtest(model2) #residuals are not autocorrelated

#Normality test
jarque.bera.test(model_WLS2$residuals) #residuals are not normally distributed
#Cook distance
ols_plot_cooksd_bar(model_WLS2)
CR_cook <- date_CR_log[-c(11,98,99,100,102,103,107,108,109,110,111,120, 121), ]
model3 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar, CR_cook)
jarque.bera.test(model3$residuals) #normally distributed
summary(model3)

#Import data
date_LR<-read.csv(file="transit_total_LR.csv", row.names=1)
View(date_LR)
date_LR_log<-log(date_LR)
View(date_LR_log)

#Correlation matrix
cor_matrix <- cor(date_LR_log[, c("avg_fuel_price", "Total_VRH", "Total_VRM", "working_age_pop", "unemp_rate", "nr_accidents", "personal_income")])
View(as.matrix(cor_matrix))

#Regression model
model <- lm(Total_UPT~ avg_fuel_price + Total_VRH + unemp_rate + working_age_pop + nr_accidents + personal_income, data = date_LR_log)
summary(model)

#Heteroscedasticity test
white_test(model) # heteroscedasticity
#Correction with WLS
model_WLS1 <- lm(Total_UPT~ avg_fuel_price + Total_VRH + unemp_rate + working_age_pop + nr_accidents + personal_income, 
                 data = date_LR_log, weights = 1/avg_fuel_price)

date_LR_log %<>% mutate(UPTstar = Total_UPT/sqrt(avg_fuel_price),
                        avg_price_star = avg_fuel_price/sqrt(avg_fuel_price),
                        VRHstar = Total_VRH/sqrt(avg_fuel_price), 
                        URstar = unemp_rate/sqrt(avg_fuel_price),
                        popstar = working_age_pop/sqrt(avg_fuel_price),
                        accstar = nr_accidents/sqrt(avg_fuel_price),
                        venitstar = personal_income/sqrt(avg_fuel_price),
                        constantstar = 1/sqrt(avg_fuel_price)
)

model_WLS2 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar, date_LR_log) 

white_test(model_WLS2) #homoscedasticity

#Autocorrelation test
dwtest(model_WLS2) #autocorrelation

#New dataset
LR_data <- data.frame(date_LR_log, resid_mod1=model_WLS2$residuals)
# Create lag1 variable
LR_data_1 <- slide(LR_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
LR_data_2 <- na.omit(LR_data_1) # eliminate NA values
#New model with lag1 variable
model2 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar + lag1, data = LR_data_2)

dwtest(model2) #non-autocorrelation

#Normality test
jarque.bera.test(model_WLS2$residuals) #residuals are not normally distributed
#Cook distance
ols_plot_cooksd_bar(model_WLS2)
LR_cook <- date_LR_log[-c(79,86,100,101,107,108,109,110,111,120,121), ]
model3 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar, LR_cook)
jarque.bera.test(model3$residuals) #residuals are not normally distributed
summary(model3)

#Import data
date_HR<-read.csv(file="transit_total_HR.csv", row.names=1)
View(date_HR)
date_HR_log<-log(date_HR)
View(date_HR_log)

#Correlation matrix
cor_matrix <- cor(date_HR_log[, c("avg_fuel_price", "Total_VRH", "Total_VRM", "working_age_pop", "unemp_rate", "nr_accidents", "personal_income")])
View(as.matrix(cor_matrix))

#Regression model
model <- lm(Total_UPT~ avg_fuel_price + Total_VRH + unemp_rate + working_age_pop + nr_accidents + personal_income, data = date_HR_log)
summary(model)

#Heteroscedasticity test
white_test(model) # heteroscedasticity
#Correction with WLS
model_WLS1 <- lm(Total_UPT~ avg_fuel_price + Total_VRH + unemp_rate + working_age_pop + nr_accidents + personal_income, 
                 data = date_HR_log, weights = 1/avg_fuel_price)

date_HR_log %<>% mutate(UPTstar = Total_UPT/sqrt(avg_fuel_price),
                        avg_price_star = avg_fuel_price/sqrt(avg_fuel_price),
                        VRHstar = Total_VRH/sqrt(avg_fuel_price), 
                        URstar = unemp_rate/sqrt(avg_fuel_price),
                        popstar = working_age_pop/sqrt(avg_fuel_price),
                        accstar = nr_accidents/sqrt(avg_fuel_price),
                        venitstar = personal_income/sqrt(avg_fuel_price),
                        constantstar = 1/sqrt(avg_fuel_price)
)

model_WLS2 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar, date_HR_log) 

white_test(model_WLS2) #homoscedasticity

#Autocorrelation test
dwtest(model_WLS2) #autocorrelation

#New dataset
HR_data <- data.frame(date_HR_log, resid_mod1=model_WLS2$residuals)
#Create lag1 variable
HR_data_1 <- slide(HR_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
HR_data_2 <- na.omit(HR_data_1) # eliminate NA values
#New model with lag1 variable
model2 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar + lag1, data = HR_data_2)

dwtest(model2) #non-autocorrelation

#Normality test
jarque.bera.test(model_WLS2$residuals) #residuals are not normally distributed
#Cook distance
ols_plot_cooksd_bar(model_WLS2)
HR_cook <- date_HR_log[-c(98,99,100,101,102,103,108,109,110,111,114,120,121,123), ]
model3 <- lm(UPTstar ~ 0 + constantstar+ avg_price_star + VRHstar + URstar + popstar + accstar + venitstar, HR_cook)
jarque.bera.test(model3$residuals) #normally distributed
summary(model3)

