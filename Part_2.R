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
                  v.names = c("Asset","ROA","emp","age","proma","RD","exp","leverage","otherloan","year", "holder",
                              "fixed.asset"),
                  times = 1:4,
                  direction = "long")
data3m$RDp <- 0
data3m$RDp[data3m$RD > 0] <- 1

#III: Finance and R&D
m3_holder <- glm(RDp ~ proma + leverage + I(leverage^2)+ otherloan + geo1 + geo2 + (1 | year),  family = binomial, data = data3m)
summary(m3_holder)
me_m3_holder <- logitmfx(m3_holder, data= data3m, atmean = TRUE)
me_m3_holder

data4m <- subset(data3m, data3m$RDp == 1)

m3_holder_probit <- glm(RDp ~ proma + leverage + I(leverage^2)+ otherloan + geo1 + geo2 + (1 | year),  family = binomial(link = "probit"), data = data4m)
summary(m3_holder_probit)
data4m$IMR2 <- dnorm(m3_holder_probit$linear.predictors)/pnorm(m3_holder_probit$linear.predictors)
m4_fixed <- plm(RD ~ proma + leverage + I(leverage^2)+ otherloan + IMR2, index = c("taxcode","year"),
                data = data4m, model = "within")
summary(m4_fixed)
#model explanation: me_m3_holder : marginal effects of included variables in the logit regression
#                   RDp :        dummy variable of probability of R&D investment
#                   RD:          R&D ratio (R&D amount/ asset)
#                   proma:       profit margin ratio (profit/revenue)
#                   leverage:    firm's leverage (debt/asset)
#                   otherloan:   other funds ratio ((subsidiary + due to shareholder + bond)/asset)
#                   holder:      concentration of shareholder (how much share the top owner has)
#                   geo:         dummy varialbe 1 if firms's headquater is in the North and Central 
#                                Italy, 0 if firm is in the South
#                   IMR2 :       Reversed Mills ratio to solve selection bias
#                   year:        fixed effect factors to capture the hidden factor that may happen in
#                                surveyed year





# pending varialbe: bank relationship and bank herfindahl index by region (bank competition) in 
# collecting period
cor.test(data3m$leverage, data3m$otherloan) 
# suspected relationship since 2 variables are positively correlated and bear signficant relationship
# simulteneity problem between R&D and profit margin and leverage

# note: the 



# Test for simulteneity.
data4m <- subset(data3m, data3m$RDp == 1)
data4m$res_m4_fixed <- residuals(m4_fixed)
data4m$res_m4_fixed <- resid(m4_fixed)
m4_endo <- lm(leverage ~ res_m4_fixed + IMR2, data=data4m)
summary(m4_endo) # simulteneity existence

log <- lm(fixed.asset ~ leverage, data = data4m)
summary(log)

# Alternative regression, use fixed.effect asset as instrumental variable for leverage
m3_holder_alt <- glm(RDp ~ proma + poly(fixed.asset,2) + otherloan + geo1 + geo2 + (1 | year),  family = binomial, data = data3m)
summary(m3_holder_alt)
me_m3_holder_alt <- logitmfx(m3_holder_alt, data= data3m, atmean = TRUE)
me_m3_holder_alt









