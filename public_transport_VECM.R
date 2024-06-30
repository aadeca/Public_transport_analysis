
PackageNames <- c("urca","vars","mFilter","tseries","forecast","tidyverse","stargazer",
                  "Metrics","tsDyn","dynlm","aTSA","coin" )
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

getwd()
setwd("C:/Users/Amora 2/Desktop/LICENTA/DATE")

#Import data
date_total<-read.csv("transit_total.csv", row.names=1)
View(date_total)
date_total_log<-log(date_total)

#Creating time series variables
carburant <- ts(date_total_log$avg_fuel_price, start = c(2012, 1), frequency = 12)
calatorii <- ts(date_total_log$Total_UPT, start = c(2012, 1), frequency = 12)
ore_parcurse <- ts(date_total_log$Total_VRH, start = c(2012, 1), frequency = 12)
mile_parcurse <- ts(date_total_log$Total_VRM, start = c(2012, 1), frequency = 12)
rata_somaj <- ts(date_total_log$unemp_rate, start = c(2012, 1), frequency = 12)
accidente <- ts(date_total_log$nr_accidents, start = c(2012, 1), frequency = 12)
venit_personal <- ts(date_total_log$personal_income, start = c(2012, 1), frequency = 12)

#Creating the dataframe
date_vecm <-cbind(carburant, calatorii, ore_parcurse, rata_somaj, accidente, venit_personal)
View(date_vecm)


#Graphs for seasonality 
ggsubseriesplot(carburant) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru pretul carburantului") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #seasonality 

ggsubseriesplot(calatorii) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru numarul de calatorii") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #seasonality 

ggsubseriesplot(ore_parcurse) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru numarul de ore parcurse") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #seasonality 

ggsubseriesplot(rata_somaj) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru rata somajului") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #seasonality 

ggsubseriesplot(accidente) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru nr de accidente") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #seasonality 

ggsubseriesplot(venit_personal) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru venitul personal") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #seasonality 


#Seasonal adjustment
carburant_seas_adj <- carburant %>% stl(s.window='periodic') %>% seasadj()
ggsubseriesplot(carburant_seas_adj) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru pretul carburantului") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #no seasonality

calatorii_seas_adj <- calatorii %>% stl(s.window='periodic') %>% seasadj()
ggsubseriesplot(calatorii_seas_adj) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru calatorii") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #no seasonality

ore_parcurse_seas_adj <- ore_parcurse %>% stl(s.window='periodic') %>% seasadj()
ggsubseriesplot(ore_parcurse_seas_adj) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru nr de ore parcurse") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #no seasonality

rata_somaj_seas_adj <- rata_somaj %>% stl(s.window='periodic') %>% seasadj()
ggsubseriesplot(rata_somaj_seas_adj) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru rata somajului") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #no seasonality

accidente_seas_adj <- accidente %>% stl(s.window='periodic') %>% seasadj()
ggsubseriesplot(accidente_seas_adj) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru nr de accidente") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #no seasonality

venit_personal_seas_adj <- venit_personal %>% stl(s.window='periodic') %>% seasadj()
ggsubseriesplot(venit_personal_seas_adj) +
  ylab("%") +
  ggtitle("Graficul sezonier pentru venitul personal") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() #no seasonality

#Creating a dataframe with adjusted series
date_adj<-cbind(carburant_seas_adj, calatorii_seas_adj, ore_parcurse_seas_adj, rata_somaj_seas_adj, accidente_seas_adj, venit_personal_seas_adj)


#Correlograms
ggtsdisplay(carburant_seas_adj)
ggtsdisplay(calatorii_seas_adj)
ggtsdisplay(ore_parcurse_seas_adj)
ggtsdisplay(rata_somaj_seas_adj)
ggtsdisplay(accidente_seas_adj)
ggtsdisplay(venit_personal_seas_adj)


# ADF test
# Trend and intercept
adf <- ur.df(carburant_seas_adj, type = "trend", selectlags = "AIC")
summary(adf) 
#Intercept
adf <- ur.df(carburant_seas_adj, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(carburant_seas_adj, type = "none", selectlags = "AIC")
summary(adf)

#Differentiating series
carburant_diff<-diff(carburant_seas_adj)
#Correlogram after differentiating
ggtsdisplay(carburant_diff)

#ADF
# Trend and intercept
adf <- ur.df(calatorii_seas_adj, type = "trend", selectlags = "AIC")
summary(adf)  
#Intercept
adf <- ur.df(calatorii_seas_adj, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(calatorii_seas_adj, type = "none", selectlags = "AIC")
summary(adf)

#Differentiating series
calatorii_diff<-diff(calatorii_seas_adj)
#Correlogram after differentiating
ggtsdisplay(calatorii_diff)

#ADF
# Trend and intercept
adf <- ur.df(ore_parcurse_seas_adj, type = "trend", selectlags = "AIC")
summary(adf) 
#Intercept
adf <- ur.df(ore_parcurse_seas_adj, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(ore_parcurse_seas_adj, type = "none", selectlags = "AIC")
summary(adf)

#Differentiating series
ore_parcurse_diff<-diff(ore_parcurse_seas_adj)
#Correlogram after differentiating
ggtsdisplay(ore_parcurse_diff)

#ADF
# Trend and intercept
adf <- ur.df(rata_somaj_seas_adj, type = "trend", selectlags = "AIC")
summary(adf)
#Intercept
adf <- ur.df(rata_somaj_seas_adj, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(rata_somaj_seas_adj, type = "none", selectlags = "AIC")
summary(adf)

#Differentiating series
rata_somaj_diff<-diff(rata_somaj_seas_adj)
#Correlogram after differentiating
ggtsdisplay(rata_somaj_diff)

#ADF
# Trend and intercept
adf <- ur.df(accidente_seas_adj, type = "trend", selectlags = "AIC")
summary(adf) 
#Intercept
adf <- ur.df(accidente_seas_adj, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(accidente_seas_adj, type = "none", selectlags = "AIC")
summary(adf)

#Differentiating series
accidente_diff<-diff(accidente_seas_adj)
#Correlogram after differentiating
ggtsdisplay(accidente_diff)

#ADF
# Trend and intercept
adf <- ur.df(venit_personal_seas_adj, type = "trend", selectlags = "AIC")
summary(adf) 
#Intercept
adf <- ur.df(venit_personal_seas_adj, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(venit_personal_seas_adj, type = "none", selectlags = "AIC")
summary(adf)

#Differentiating series
venit_personal_diff<-diff(venit_personal_seas_adj)
#Correlogram after differentiating
ggtsdisplay(venit_personal_diff)

#ADF test 
# Trend and intercept
adf <- ur.df(carburant_diff, type = "trend", selectlags = "AIC")
summary(adf) #stationarity
# Intercept
adf <- ur.df(carburant_diff, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(carburant_diff, type = "none", selectlags = "AIC")
summary(adf)

# Trend and intercept
adf <- ur.df(calatorii_diff, type = "trend", selectlags = "AIC")
summary(adf) #stationarity 
# Intercept
adf <- ur.df(calatorii_diff, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(calatorii_diff, type = "none", selectlags = "AIC")
summary(adf)

# Trend and intercept
adf <- ur.df(ore_parcurse_diff, type = "trend", selectlags = "AIC")
summary(adf) #stationarity  
# Intercept
adf <- ur.df(ore_parcurse_diff, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(ore_parcurse_diff, type = "none", selectlags = "AIC")
summary(adf)

# Trend and intercept
adf <- ur.df(rata_somaj_diff, type = "trend", selectlags = "AIC")
summary(adf) #stationarity
# Intercept
adf <- ur.df(rata_somaj_diff, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(rata_somaj_diff, type = "none", selectlags = "AIC")
summary(adf)

# Trend and intercept
adf <- ur.df(accidente_diff, type = "trend", selectlags = "AIC")
summary(adf) #stationarity
# Intercept
adf <- ur.df(accidente_diff, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(accidente_diff, type = "none", selectlags = "AIC")
summary(adf)

# Trend and intercept
adf <- ur.df(venit_personal_diff, type = "trend", selectlags = "AIC")
summary(adf) #stationarity 
# Intercept
adf <- ur.df(venit_personal_diff, type = "drift", selectlags = "AIC")
summary(adf)
#Determinist elements
adf <- ur.df(venit_personal_diff, type = "none", selectlags = "AIC")
summary(adf)

#Cointegration test
coint.test(y = calatorii,X = carburant, d = 1) #cointegrated series
coint.test(y = calatorii,X = ore_parcurse ,d = 1) #cointegrated series
coint.test(y = calatorii,X =rata_somaj ,d = 1) #cointegrated series
coint.test(y = calatorii,X = accidente ,d = 1) #cointegrated series
coint.test(y = calatorii,X = venit_personal, d = 1) #cointegrated series

#Optimal lag selection
lagselect <- VARselect(date_adj, lag.max = 10, type = 'const')
lagselect$selection
# 2 lags according to HQ and FPE 

#Johansen test - Trace method
ctest1 <- ca.jo(date_adj, type = 'trace', ecdet = 'const', K=2)
summary(ctest1) 
#Johansen test - Eigen values method
ctest2 <- ca.jo(date_adj, type = 'eigen', ecdet = 'const',K=2)
summary(ctest2)


# VECM model 
Model2 <- VECM(date_adj,
               lag = 1, 
               r=1, 
               estim = ('2OLS'),
               LRinclude = 'const')
summary(Model2)

# Residual diagnostics

#Transform VECM object in VAR object
ModelVAR <- vec2var(ctest1, r = 1)
summary(ModelVAR)

# Autocorrelation
Serial <- serial.test(ModelVAR, lags.pt = 5, type = 'PT.asymptotic')
Serial #autocorrelation

# Heteroscedasticity
Arch <- vars::arch.test(ModelVAR, lags.multi = 15, multivariate.only = TRUE)
Arch #residuals are not heteroscedastics

# Normality test
Norm <- normality.test(ModelVAR, multivariate.only = TRUE)
Norm # residuals are not normally distributed

#Granger causality
modelVar2 <- VAR(date_adj, p = 1, type = 'const', season = NULL, exog = NULL)
Granger <- causality(modelVar2, cause = 'carburant_seas_adj')
Granger
Granger <- causality(modelVar2, cause = 'ore_parcurse_seas_adj')
Granger
Granger <- causality(modelVar2, cause = 'rata_somaj_seas_adj')
Granger
Granger <- causality(modelVar2, cause = 'accidente_seas_adj')
Granger
Granger <- causality(modelVar2, cause = 'venit_personal_seas_adj')
Granger

#Impulse - response function
carburantirf <- irf(ModelVAR, impulse = 'carburant_seas_adj', response = 'calatorii_seas_adj', n.ahead= 12, boot = TRUE)
dev.new()
plot(carburantirf, ylab = 'carburant', main = 'Funcția de răspuns la impuls pentru numărul de călătorii și prețul carburantului')

oreirf <- irf(ModelVAR, impulse = 'ore_parcurse_seas_adj', response = 'calatorii_seas_adj', n.ahead= 12, boot = TRUE)
dev.new()
plot(oreirf, ylab = 'ore parcurse', main = 'Funcția de răspuns la impuls pentru numărul de călătorii și numărul de ore parcurse')

ratasomajirf <- irf(ModelVAR, impulse = 'rata_somaj_seas_adj', response = 'calatorii_seas_adj', n.ahead= 12, boot = TRUE)
dev.new()
plot(ratasomajirf, ylab = 'rata_somaj', main = 'Funcția de răspuns la impuls pentru numărul de călătorii și rata somajului')

accidenteirf <- irf(ModelVAR, impulse = 'accidente_seas_adj', response = 'calatorii_seas_adj', n.ahead= 12, boot = TRUE)
dev.new()
plot(accidenteirf, ylab = 'accidente', main = 'Funcția de răspuns la impuls pentru numărul de călătorii și numărul de accidente')

venitirf <- irf(ModelVAR, impulse = 'venit_personal_seas_adj', response = 'calatorii_seas_adj', n.ahead= 12, boot = TRUE)
dev.new()
plot(venitirf, ylab = 'venit_personal', main = 'Funcția de răspuns la impuls pentru numărul de călătorii și venitul personal')

# Variance decomposition
FEVD2 <- fevd(ModelVAR,n.ahead=12)
dev.new()
plot(FEVD2, main="Descompunerea varianței", legend=FALSE) 
View(FEVD2$calatorii_seas_adj)







