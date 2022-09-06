#------------------------------------------------------------------------ #
#     Fixed Income Securities
#     NOVA IMS
#     Group Project: Carlos Cardoso | 20211220   Carlota Reis | 20211208   
#                   Guilherme Miranda | 20210420    Mariana Garcia | 20210838 
#------------------------------------------------------------------------ #


rm(list = ls())
library(pacman)
p_load(tidyverse)
library(lubridate)
library(taRifx)
options(scipen = 999)


#Group Project

#Exercise 1

coupon_rate <- 0.0425
freq <- 2
fv <- 1000

#Issue date = 31/07/2015
#Pays coupons semi-annually - 15/01 and 15/07
#settlement date = trade date + 1 = 27/10/2015
#Last coupon on 15/07/2015

#a)
time_since_last_coupon <- 1+31+30+26
time_between_coupons <- 1+31+30+31+30+31+14
AI = (time_since_last_coupon/time_between_coupons)*(coupon_rate/freq) * fv


#b) and c)

#gbm function
gbm_vec <- function(nsim, t, mu, sigma, S0, dt = 1/360) {
  
  epsilon <- matrix(rnorm(t*nsim), ncol = nsim, nrow = t)
  
  gbm <- exp((mu - sigma^2 / 2) * dt + sigma * epsilon * sqrt(dt))
  gbm <- apply(rbind(rep(S0, nsim), gbm), 2, cumprod)
  
  return(gbm)
}


#Initial data
final_date<-as.Date("2025-07-15")
origin_date<-as.Date("2015-10-27")
first_coupon<-origin_date+80
settlement_date<-as.Date("2015-10-27")
real_coupon<-0.0425/2
face_value <- 1000

#Simulating 1000 scenarios for the CPI Index using given gbm estimate
nsim <- 1000
t <-julian(final_date,origin_date)
mu <- 0.03513
sigma <- 0.022439
S0 <- 237.46721
base_index<-237.14365

gbm <- gbm_vec(nsim, t, mu, sigma, S0)

#Creating a DataFrame with the gbm results
gbm_df <- as.data.frame(gbm) %>%
  mutate(ix = 1:nrow(gbm)) %>%
  pivot_longer(-ix, names_to = 'sim', values_to = 'CPI Index')

#Plotting gbm results
gbm_df %>%
  ggplot(aes(x=ix, y=`CPI Index`, color=sim)) +
  geom_line() +
  theme(legend.position = 'none')



#Creating a DataFrame containing the t when coupons are paid

date_df <- as.data.frame(c(settlement_date,seq.Date(first_coupon, as.Date("2025-10-27"), by = "6 month")))

time_step_df<-data.frame()
for(i in 2:nrow(date_df)){
  result = date_df[i,] - date_df[i-1,]
  time_step_df<-rbind(time_step_df,result)
}

time_step_df<-cumsum(time_step_df)

#get 1000 values of the CPI index (simulated values) for each t
coupon_df<-gbm_df[is.element(gbm_df$ix, time_step_df$X80),] 
cash_flows <- data.frame(coupon_df)

#create inflation index for each simulated value of the CPI-Index
cash_flows$index<-cash_flows$CPI.Index/base_index

#Plotting inflation rate curve (with simulated values for the CPI Index)

inf.curv_df <- as.data.frame((gbm/base_index)-1) %>%
  mutate(ix = 1:nrow(gbm/base_index)) %>%
  pivot_longer(-ix, names_to = 'sim', values_to = 'Daily Inflation Rate')

#Plotting gbm results
inf.curv_df %>%
  ggplot(aes(x=ix, y=`Daily Inflation Rate`, color=sim)) +
  geom_line() +
  theme(legend.position = 'none')


#add the nominal coupon value to each t

cash_flows$nominal_coupon <- cash_flows$index*real_coupon*face_value
cash_flows[19000:20000,5] = cash_flows[19000:20000,5] + face_value


#Estimate NSS

#convert t into year fraction
cash_flows$theta<-cash_flows$ix/360

# Nelson-Siegel Svensson method
p_load(dplyr)
nelson_siegel_calculate <- function(theta, tau1, tau2, beta0, beta1, beta2,beta3){
  beta0 + beta1*(1-exp(-theta/tau1))/(theta/tau1) + beta2*((1-exp(-theta/tau1))/(theta/tau1) - exp(-theta/tau1))+beta3*((1-exp(-theta/tau2))/(theta/tau2) - exp(-theta/tau2))
}

theta <- cash_flows$theta
tau1 <- 5
tau2 <- 0.5
beta0 <- 0.059
beta1 <- -0.016
beta2 <- -0.005
beta3 <- 0.01

#NSS ylds to Dataframe
ns_data <-
  data.frame(maturity=1:20000) %>% 
  mutate(ns_yield=nelson_siegel_calculate(theta,tau1,tau2,beta0,beta1,beta2,beta3))

#Add yld of each t to our cashflow DataFrame
cash_flows$nss_yield<-ns_data$ns_yield

#Add the discount factors of each t and each sim to our DataFrame
cash_flows$disc_factor<-1/((1+cash_flows$nss_yield)^cash_flows$theta)

#Add the discounted cashflows of each sim to our DataFrame
cash_flows$disc_cf<-cash_flows$nominal_coupon*cash_flows$disc_factor

#sum discounted cashflows for each simulation (get one price for each simulation)
tapply(cash_flows$disc_cf, cash_flows$sim, sum)
res.by <- by(cash_flows$disc_cf, coupon_df$sim, sum)
price_df<-as.data.frame(res.by)

bond_fairvalue <- mean(price_df$value)


#d)

#price distribution (one price for each simulation)

data.frame(price = price_df$value) %>%
  ggplot(aes(x = price)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  geom_density() + 
  ggtitle('Price distribution')

#Using the graph, we can see that we have a simulated price far from the others ('outlier')
#Removing that price to get a better price distribution

price_df_filtered <-price_df[order(price_df$value),]
price_df_filtered <- price_df_filtered[-nrow(price_df_filtered),]

data.frame(price = price_df_filtered$value) %>%
  ggplot(aes(x = price)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.1) +
  geom_density() + 
  ggtitle('Price distribution')


#Risk Measures

bond_price = bond_fairvalue

times = seq.int(from=1, 20000, by=1)


disc_conv = (1+cash_flows$nss_yield)^(-(times+2))
dur = (times*cash_flows$disc_cf/freq)
conv = (((times*(times+1)*cash_flows$disc_cf)*disc_conv)/bond_price)/(freq^2)


out = cbind(Mat=times, CF=cash_flows$disc_cf, PVCF=cash_flows$disc_cf, Duration=dur, Convexity=conv)
out = rbind(out, c(NA,sum(cash_flows$disc_cf), bond_price, sum(dur), sum(conv)))


CashFlow_Matrix=out


Duration=mean(dur)/bond_fairvalue
Mod_Dur = sum(dur/(1+cash_flows$nss_yield))
Dollar_Dur = Mod_Dur*bond_price
BPV = Dollar_Dur/10000
Convexity = sum(conv)
Dollar_convexity = Convexity*bond_price


Risk_Measures <- list('Duration'=Duration,'Mod.Duration'=Mod_Dur,
                      'Dollar Duration'=Dollar_Dur,'BPV'=BPV,
                      'Convexity' = Convexity,'Dollar Convexity'=Dollar_convexity)


  
#Exercise 2

#a) Bootstrap



get_zero_coupon <- function(coupons=c(1.50,1.75,2.00,2.25,2.5,2.75,3,3.25,3.50,3.75),
                            BondPrices=c(96.6,93.71,91.56,90.24,89.74,90.04,91.09,92.82,95.19,98.14),
                            nominal_value=100){
  
  #We assume both coupons and BondPrices vectors are arranged to 1 year increasing maturity.
  price_matrix <- matrix(0,nrow=length(coupons),ncol=length(coupons))
  
  #Assign the coupons for each year
  for(i in 1:length(coupons)){
    price_matrix[i,1:i] <- coupons[i]
  }
  
  #Add the maturity nominal value
  diag(price_matrix) <- diag(price_matrix) + nominal_value
  
  #Solve the system of equations to get B(0,t)
  zero_coupon_prices <- solve(price_matrix, BondPrices)
  
  #Get zero coupon yields R(0,t)
  zero_coupon_yields <- (1/zero_coupon_prices)^(1/1:length(coupons))-1
  
  return(list(B0t=zero_coupon_prices, R0t=zero_coupon_yields))
}

get_zero_coupon()

#b)
#i) 3.65 year spot interest rate using linear interpolation method


spot.df<-as.data.frame(as.list(get_zero_coupon()))

R03 <- spot.df[3,2]
R04 <- spot.df[4,2]

R03p65<-((4-3.65)*R03+(3.65-3)*R04)/(4-3)
R03p65


#ii) 3.65 year spot interest rate using linear cubic method

A_mat <- t(matrix(1:4, nrow=4, ncol=4, byrow=TRUE)^(3:0))
abcd_vec <- solve(A_mat, c(0.0175,0.02,0.0225, 0.0250))

t_val <- 3.65
sum(abcd_vec*((3.65)^(3:0)))



#c) Fair value Bond 11 using the yield curve in a)


Bond_df <- as.data.frame(get_zero_coupon()$R0t[1:8])
Bond_df$coupon<-4
Bond_df[8,2]<- Bond_df[8,2]+100
Bond_df$ttm<-(1:8)
names(Bond_df)[1]<-"ylds"
Bond_df$disc<- 1/((1+Bond_df$ylds)^Bond_df$ttm)
Bond_df$disc_cf<-Bond_df$coupon*Bond_df$disc
fair_value11<-sum(Bond_df$disc_cf)

#Since fair value < dirty price --> trading strategy would be to sell



#d)

price11<- -98.10

cf_vector<-Bond_df$coupon
cf_vector<-c(price11,cf_vector)

bval <- function(y, cf_vector, t=seq(along = cf_vector)-1)
  sum(cf_vector / (1 + y)^t)

ytm <- function(cf_vector) {
  uniroot(bval, c(0, 1), cf_vector = cf_vector)$root
}

ytm11<-ytm(cf_vector)




#Exercise 3


fv <- 100
initial_date <-as.Date('2012-02-09')


#Estimating the yield curve using NSS model and Risk Measures of coefficients
NSS_spot <- function(theta, tau1, tau2, beta0, beta1, beta2, beta3=0){
  beta0 + beta1*(1-exp(-theta/tau1))/(theta/tau1) + 
    beta2*((1-exp(-theta/tau1))/(theta/tau1) - exp(-theta/tau1)) + 
    beta3*((1-exp(-theta/tau2))/(theta/tau2) - exp(-theta/tau2))
}

NSS_Sens <- function(beta0=0.059, beta1=-0.016, beta2=-0.005, beta3=0.01, tau1=5, tau2=0.5, 
                     par=100, coupon_rate, maturity){
  
  ct <- par*rep(coupon_rate,maturity)
  ct[length(ct)] <- ct[length(ct)]+par
  theta_i <- 1:maturity 
  
  r0t <- 0
  for(i in 1:maturity){
    r0t[i] <- NSS_spot(theta=i,tau1=tau1,tau2=tau2,beta0=beta0,beta1=beta1,beta2=beta2,beta3=beta3)}
  
  # dollar-durations
  beta0_sens <- -sum(ct*theta_i*exp(-theta_i*r0t))
  beta1_sens <- -sum(ct*theta_i*(1-exp(-theta_i/tau1))/(theta_i/tau1)*exp(-theta_i*r0t))
  beta2_sens <- -sum(ct*theta_i*((1-exp(-theta_i/tau1))/(theta_i/tau1) - exp(-theta_i/tau1))*exp(-theta_i*r0t))
  beta3_sens <- -sum(ct*theta_i*((1-exp(-theta_i/tau2))/(theta_i/tau2) - exp(-theta_i/tau2))*exp(-theta_i*r0t))
  
  # parametric durations
  B0 <- sum(ct*exp(-theta_i*r0t))
  D0 <- -(1/B0)*beta0_sens
  D1 <- -(1/B0)*beta1_sens
  D2 <- -(1/B0)*beta2_sens
  D3 <- -(1/B0)*beta3_sens
  
  return(list(B0=B0, Sens=c(Beta0=beta0_sens, Beta1=beta1_sens, Beta2=beta2_sens, Beta3=beta3_sens),
              Duration=c(Beta0=D0, Beta1=D1, Beta2=D2, Beta3=D3)))
}




#a)
#Computing coeficient Risk Measures for Target Portfolio
RM_dollar <- (matrix(NA, nrow = 13, ncol=4, dimnames = list(bonds_3=c(1:13), 
                                                     c('level','slope','curvature1','curvature2'))))

RM_dur <- (matrix(NA, nrow = 13, ncol=4, dimnames = list(bonds_3=c(1:13), 
                                                            c('level','slope','curvature1','curvature2'))))

Price_before <- (matrix(NA, nrow = 13, ncol=1, dimnames = list(bonds_3=c(1:13), 
                                                               c('bond_price'))))

coupon_rate<-c(0.04, 0.075,0.04,0.07,0.0575,0.055,0.04,0.0475,0.045,0.05,0.045,0.04,0.05)

maturity_bonds<- c((as.Date('2015-12-01')),(as.Date('2016-12-04')),(as.Date('2017-12-06')),(as.Date('2018-12-10')),(as.Date('2019-12-03')),(as.Date('2020-12-09')),(as.Date('2022-12-06')),(as.Date('2025-12-03')),(as.Date('2030-12-03')),(as.Date('2035-12-04')),(as.Date('2040-12-04')),(as.Date('2041-12-01')),(as.Date('2042-12-07')))


for (i in 1:13){
  coupon_dates <- data.frame(c(initial_date,rev(seq.Date(as.Date(maturity_bonds[i]), initial_date, by = "-12 months"))))
  time_step_df<-data.frame()
  for(j in 2:nrow(coupon_dates)){
    result = coupon_dates[j,] - coupon_dates[j-1,]
    time_step_df<-rbind(time_step_df,result)
  }
  
  
  time_step_df<-cumsum(time_step_df)/360
  
  
  
  RM_dollar[i,] <- NSS_Sens(coupon_rate=coupon_rate[i],maturity=time_step_df[nrow(time_step_df),1])$Sens
  RM_dur[i,] <- NSS_Sens(coupon_rate=coupon_rate[i],maturity=time_step_df[nrow(time_step_df),1])$Duration
  Price_before[i,] <- NSS_Sens(coupon_rate=coupon_rate[i],maturity=time_step_df[nrow(time_step_df),1])$B0}



#b)
#Computing coeficient Risk Measures for Hedging Portfolio

RM_dollar_2 <- (matrix(NA, nrow = 5, ncol=4, dimnames = list(bonds_3=c(1:5), 
                                                     c('level','slope','curvature1','curvature2'))))

RM_dur_2 <- (matrix(NA, nrow = 5, ncol=4, dimnames = list(bonds_3=c(1:5), 
                                                             c('level','slope','curvature1','curvature2'))))

                                                            

coupon_rate_2<-c(0.045,0.05,0.06,0.06,0.065)



maturity_bonds_2<- c((as.Date('2016-04-12')),(as.Date('2022-12-28')),(as.Date('2025-05-06')),(as.Date('2030-10-10')),(as.Date('2041-10-10')))




for (i in 1:5){
  coupon_dates_2 <- data.frame(c(initial_date,rev(seq.Date(as.Date(maturity_bonds_2[i]), initial_date, by = "-12 months"))))
  time_step_df_2<-data.frame()
  for(j in 2:nrow(coupon_dates_2)){
    result_2 = coupon_dates_2[j,] - coupon_dates_2[j-1,]
    time_step_df_2<-rbind(time_step_df_2,result_2)
  }
  
  
  time_step_df_2<-cumsum(time_step_df_2)/360
  
  
  
  RM_dollar_2[i,] <- NSS_Sens(coupon_rate=coupon_rate_2[i],maturity=time_step_df_2[nrow(time_step_df_2),1])$Sens

  RM_dur_2[i,] <- NSS_Sens(coupon_rate=coupon_rate_2[i],maturity=time_step_df_2[nrow(time_step_df_2),1])$Duration}



#c)

#Hedging Portfolio
maturities <- (maturity_bonds_2 - initial_date)/360

bonds <- matrix(c(coupon_rate_2*fv, maturities), nrow = 5, ncol=2, dimnames = list(1:5, c('coupon','maturity')))

IRRm <- matrix(NA, nrow=5, ncol = 9, 
               dimnames = list(bond=c(1:5), c('B0','S0','S1','S2','S3','D0','D1','D2','D3')))

for (i in 1:5){
  IRRm[i,1] <- NSS_Sens(coupon_rate=bonds[i,1]/100, maturity=bonds[i,2])$B0
  IRRm[i,2:5] <- NSS_Sens(coupon_rate=bonds[i,1]/100, maturity=bonds[i,2])$Sens  
  IRRm[i,6:9] <- NSS_Sens(coupon_rate=bonds[i,1]/100,maturity=bonds[i,2])$Duration
}




qts <-c(10000,250000,50000,100000,10000,200000,15000,10000,30000,75000,100000,10000,10000)
# Target portfolio
P0 <- Price_before
P0 <-  sum(P0*qts)
# Hedging portfolio

a <- rbind(-t(IRRm[,2:5]), B0=IRRm[,1])

RM_dollar_2_mean <- colMeans(RM_dollar_2)
b <- c(RM_dollar_2_mean,P0=P0)

# weights
w <- solve(a,b);

Cost <- sum(w*IRRm[,1])
Cost



#d)
#Considering new NSS parameters

beta0_new <- 0.065
beta1_new <- -0.01
beta2_new <- 0.001
beta3_new <- 0.02
tau1_new <- 5
tau2_new <-0.5

par_value = fv*qts
maturities_target <- (maturity_bonds - initial_date)/360


#i) e ii)

P0_new <- NSS_Sens(beta0=beta0_new, beta1 = beta1_new,beta2 = beta2_new,beta3 = beta3_new,
                   tau1 = tau1_new, tau2 = tau2_new, coupon_rate =coupon_rate, maturity=maturities_target, par=par_value)$B0
B0_new <- matrix(NA, nrow = 5, ncol=1, dimnames = list(1:5, c('B0*')))

for (i in 1:5){
  B0_new[i] <- NSS_Sens(beta0=beta0_new, beta1 = beta1_new,beta2 = beta2_new,beta3 = beta3_new,
                        tau1 = tau1_new, tau2 = tau2_new, coupon_rate=bonds[i,1]/100, maturity=bonds[i,2])$B0}


#Impact on target portfolio 
PL <- sum((B0_new-IRRm[,1])*w) + (P0_new-P0)


# Hedging scenario given the yield shift
PL/P0*100

# No-hedging scenario given the yield shift
((P0_new-P0)/P0)*100