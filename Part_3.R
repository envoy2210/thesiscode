# CONTENT : interaction of finance and ownership in R&D. Expansion for lag of R&D by GMM. test for certain
# characteristics


#I. data description
# 1.1 Packages in use:
library(plm)
library(tseries)
library(lmtest)
library(readxl)
library(lme4)
library(stargazer)
library(mfx)
#library(Hmisc) this is reason for the error in reshaping

#1.2 Data transformation
setwd("E:/Truyen/Pro Schiavo's paper/data sub")
load("E:/Truyen/Pro Schiavo's paper/data sub/data2_m.Rda")
data3m <- reshape(data2_m,
                  varying = list(c(5:8), c(9:12), c(14:17), c(18:21), c(23:26), c(27:30), c(31:34),
                                 c(35:38), c(39:42), c(43:46), c(47:50), c(55:58)),
                  v.names = c("Asset","ROA","emp","age","proma","RD","exp","leverage","otherloan","year", "holder","fixed.asset"),
                  times = 1:4,
                  direction = "long")
#data3m <- reshape(data2_m,
#                   varying = list(c("Asset2015","Asset2014","Asset2013","Asset2012"), 
#                                    c("ROA2015","ROA2014","ROA2013","ROA2012"), 
#                                    c("emp2015","emp2014","emp2013","emp2012"),
#                                    c("age2015","age2014","age2013","age2012"), 
#                                    c("proma2015", "proma2014","proma2013","proma2012"),
#                                    c("RD2015","RD2014","RD2013","RD2012"),
#                                    c("exp2015","exp2014","exp2013","exp2012"),
#                                    c("leverage2015","leverage2014","leverage2013","leverage2012"),
#                                    c("otherloan2015","otherloan2014","otherloan2013","otherloan2012"),
#                                    c("year2015","year2014","year2013","year2012")),
#                  v.names = c("Asset","ROA","emp","age","proma","RD","exp","leverage","otherloan","year"),
#                  times = 1:4,
#                  direction = "long")
# in case the previous code is wrong, you can use this "long version"
attach(data3m)
data3m <- data3m[order(taxcode),]
detach(data3m)
head(data3m)

#rcorr(as.matrix(data3m))
#rcorr(data3m, type="pearson")

#1.3 Heterogeneity across years
data3m$RDp <- 0
data3m$RDp[data3m$RD > 0] <- 1
library(gplots)
plotmeans(RDp ~ year, main = "probability of R&D over years", data = data3m)
plotmeans(RD ~ year, main = "RD amount over years", data = data3m, subset = data3m$RD >0)
detach("package:gplots")
# probability of R&D investment varies among years while the R&D ratio seems to be very stable


#IV: Interaction of Ownership and Finance:
#4.1 Probability of R&D
m5_1 <- glm(RDp ~  log(emp)+ log(age) + holder+ geo1 + geo2 + ROA + proma + leverage + I(leverage^2)+ 
              otherloan + (1 | year) + holder*leverage,  family = binomial, data = data3m)
summary(m5_1)
me_m5_1 <- logitmfx(m5_1, data= data3m, atmean = TRUE)
me_m5_1

# this one proves to be irrelevant between 2 variables
m5_2 <- glm(RDp ~  log(emp)+ log(age) + holder+ geo1 + geo2 + ROA + proma + leverage + I(leverage^2)+ 
              otherloan + (1 | year) + holder*leverage + holder*I(leverage^2),  family = binomial, data = data3m)
summary(m5_2)
me_m5_2 <- logitmfx(m5_2, data= data3m, atmean = TRUE)
me_m5_2
# this one does show a relationship!!!!!!!
m5_3 <- glm(RDp ~  log(emp)+ log(age) + holder+ geo1 + geo2 + ROA + proma + leverage + I(leverage^2)+ 
              otherloan + (1 | year) + holder*proma + holder*leverage + holder*I(leverage^2),  family = binomial, data = data3m)
summary(m5_3)
me_m5_3 <- logitmfx(m5_3, data= data3m, atmean = TRUE)
me_m5_3
# profit margin continues proving to be irrelevant in R&D activities
#m5_4 <- glm(RDp ~  log(emp)+ log(age) + shareholder+ geo + ROA + proma + leverage + I(leverage^2)+ 
#              otherloan + (1 | year) + shareholder*proma + shareholder*leverage + shareholder*I(leverage^2),  family = binomial, data = data3m)
#summary(m5_4)
# number of shareholders are irrelevant

#4.2 R&D amount
m6_2_probit <- glm(RDp ~ log(emp)+ log(age) + holder + geo1 + geo2 + ROA + proma + leverage + I(leverage^2) + 
                     otherloan + holder*leverage + holder*I(leverage^2), family = binomial(link="probit"), data = data3m)
data3m$IMR2 <- dnorm(m6_2_probit$linear.predictors)/pnorm(m6_2_probit$linear.predictors)

m6_2 <- lm(RD ~ log(emp)+ log(age) + holder + ROA + proma + leverage + I(leverage^2) + 
             otherloan + holder*leverage + holder*I(leverage^2) + IMR2, index = c("taxcode", "year")
           ,data = subset(data3m,data3m$RD >0, model = "within"))
summary(m6_2)
fixef(m6_2) # this one does not yeild result
# interactive variables are signficiant but holder now not.
stargazer(m5_1, m5_2, m5_3, m6_2, type="text",
          dep.var.labels=c("R&D probability","R&D amount"),
          covariate.labels=c("Log of employees number","Log of firm age","concentration of shareholding","Geographical dummy","Return over asset", "Profit margin", "firm's leverage","firms' leverage squared", "leverage of other sources", "year fixed effect","interaction of concentration and profit margin","interaction of concentration and leverage","interaction of concentration and leverage squared"), out="models.txt")


m6_2_probit <- glm(RDp ~ log(emp)+ log(age) + holder + geo1 + geo2 + ROA + proma + poly(fixed.asset,2) + 
                     otherloan + poly(fixed.asset,2)*holder, family = binomial(link="probit"), data = data3m)
data3m$IMR2 <- dnorm(m6_2_probit$linear.predictors)/pnorm(m6_2_probit$linear.predictors)

m6_2 <- lm(RD ~ log(emp)+ log(age) + holder + geo1 + geo2 + ROA + proma + poly(fixed.asset,2) + 
             otherloan + poly(fixed.asset,2)*holder + IMR2, index = c("taxcode", "year")
           ,data = subset(data3m,data3m$RD >0, model = "within"))
summary(m6_2)

# V. MOdel expansion with GMM for the lag of RD. Proof of the long-term commitment nature of R&D activity


