# I. Ownership and R&D

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
                                 c(35:38), c(39:42), c(43:46), c(47:50)),
                  v.names = c("Asset","ROA","emp","age","proma","RD","exp","leverage","otherloan","year", "holder"),
                  times = 1:4,
                  direction = "long")

attach(data3m)
data3m <- data3m[order(taxcode),]
detach(data3m)
head(data3m)

#1.3 Heterogeneity across years
data3m$RDp <- 0
data3m$RDp[data3m$RD > 0] <- 1
library(gplots)
plotmeans(RDp ~ year, main = "probability of R&D over years", data = data3m)
plotmeans(RD ~ year, main = "RD amount over years", data = data3m, subset = data3m$RD >0)
detach("package:gplots")
# probability of R&D investment varies among years while the R&D ratio seems to be very stable

stargazer(data3m, type = "text", title="Descriptive statistics", digits=1, out="data_description.txt")

---------------------------------------------------------------------------------------------------------
#II. Ownership and R&D
#2.1 Probability of R&D investment
m1_shareholder <- glm(RDp ~  log(emp)+ log(age) + shareholder+ geo1 + geo2 + ROA + (1 | year),  family = binomial, data = data3m)
summary(m1_shareholder) # shareholder and geo are irrelevant in firms' probability of R&D
me_m1_shareholder <- logitmfx(m1_shareholder, data= data3m, atmean = TRUE)
me_m1_shareholder

m1_holder <- glm(RDp ~ log(emp)+ log(age) + holder +geo1 + geo2 + ROA + (1 | year), family = binomial(link="logit"), data = data3m)
summary(m1_holder)
me_m1_holder <- logitmfx(m1_holder, data= data3m, atmean = TRUE)
me_m1_holder

#model explanation: RDp :             dummy variable of probability of R&D investment
#                   RD:               R&D ratio (R&D amount/asset)
#                   me_m1_shareholder:marginal effect of variables in the m1_shareholder logit regression
#                   me_m1_holder     :marginal effect of variables in the m1_holder logit regression
#                   log(emp):         logarithm of number of employees
#                   log(age):         logarithm of age
#                   shareholder:      number of shareholders
#                   holder:           concentration of shareholder (how much share the top owner has)
#                   geo1:             dummy varialbe 1 if firms's headquater is in the North 
#                                     Italy, 0 if firm is in the South
#                   ROA:              firm's return on assets
#                   year:             fixed effect factors to capture the hidden factor that may happen 
#                                     in surveyed year
#                   lag(RD, -1):      lag of R&D ratio last year  

stargazer(m1_shareholder, m1_holder, type="text",
          dep.var.labels=c("R&D probability"),
          covariate.labels=c("Log of employees number","Log of firm age","number of shareholder", 
                             "concentration of shareholding","NOrthen geographical dummy", 
                             "Central geographical dummy","Return over asset", "year fixed effect"), 
                            out="m1_coefficients.txt")

#stargazer(me_m1_shareholder, me_m1_holder, type = "text", title="Marginal effects", digits=1, out="Marginal_effects.txt")
# Prediction the hit ratio
RDpfac   <- cut(data3m$RDp, breaks=c(-Inf, median(data3m$RDp), Inf), labels=c("0", "1"))
RDphat    <- fitted(m1_holder)
thresh  <- 0.5  # threshold for dichotomizing according to predicted probability
RDphatFac <- cut(RDphat, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
cTab    <- table(RDpfac, RDphatFac)    # contingency table
addmargins(cTab)                   # marginal sums


sum(diag(cTab)) / sum(cTab)

#2.2 R&D investment
#fixed effect and random effects
m1_holder_probit <- glm(RDp ~ log(emp)+ log(age) + holder+ geo1 + geo2 + ROA + (1 | year), family = binomial(link="probit"), data = data3m)
summary(m1_holder_probit)
data3m$IMR <- dnorm(m1_holder_probit$linear.predictors)/pnorm(m1_holder_probit$linear.predictors)
m2_fixed <- plm(RD ~ log(emp) + log(age) + holder + geo1 + geo2 + ROA + IMR, index = c("taxcode", "year"), 
                data = subset(data3m,data3m$RD >0, model = "within"))
summary(m2_fixed)
m2_random <- plm(RD ~ log(emp) + log(age) + holder + geo1 + geo2 + ROA + IMR, index = c("taxcode", "year"), 
                data = subset(data3m,data3m$RD >0, model = "random"))
summary(m2_random)

#pooled OLS
m2_pooled <- lm(RD ~ log(emp) + log(age) + holder + geo1 + geo2 + ROA + IMR, data = subset(data3m,data3m$RD >0))
summary(m2_pooled)

#with year dummy
m2_yeardummy <- lm(RD ~ log(emp) + log(age) + holder + geo1 + geo2 + ROA + IMR + factor(year) - 1, 
                   data = subset(data3m,data3m$RD >0))
summary(m2_yeardummy)

stargazer(m2_fixed,m2_random, m2_pooled, m2_yeardummy, type="text",
          dep.var.labels=c("R&D asset ratio"),
          covariate.labels=c("Log of employees number","Log of firm age",
                             "concentration of share","Northern dummy","Central dummy", 
                             "Return over asset", "Inverse Mills ratio"), out="m2.txt")


#2.3 Test of data characteristics:
# pFtest(m2_fixed,m2_yeardummy)
# fixed versus time dummy regression 
phtest(m2_fixed, m2_random) 
# Hausman test between fixed and random effect models but does not yield any result
pbgtest(m2_fixed) 
# Breusch-Godfrey test for serial correlation. Result: no serial correlation in the dataset
adf.test(data3m$RD, k=2) 
# Dickey- FUller test for stochastic trend. Result: pvalue = 0.01 there is no root variable. 
# The data set is stationary. The test show the lag of 1 year ago is significant but take 2 years it is not
# so I just pick the last year lag in fixed effect regression
bptest(m2_fixed) 
#Breusch-Pagan test for heteroskedascity p-value nearly zero there is heteroskedascity
coeftest(m2_fixed, vcovHC)
coeftest(m2_fixed, vcovHC(m2_fixed, method = "arellano")) 
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), function(x) sqrt(diag(vcovHC(m2_fixed, type = x)))))
cor.test(data3m$shareholder, data3m$holder, method = "pearson")
# highly significant correlation between 2 variables, we should not include them simultenously though
# two variable show different results at the probability of R&D regression


