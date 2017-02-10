############### THE NEW CODE #############
##########################################

#I. data description
# 1.1 Packages in use:
library(plm)
library(tseries)
library(lmtest)
library(readxl)
library(lme4)
library(stargazer)
#library(Hmisc) this is reason for the error in reshaping

#1.2 Data transformation
setwd("E:/Truyen/Pro Schiavo's paper/data sub")
data2_m <- read_excel("~/data2_m.xlsx")
data3m <- reshape(data2_m,
                  varying = list(c(5:8), c(9:12), c(14:17), c(18:21), c(24:27), c(28:31), c(32:35),
                                 c(36:39), c(40:43), c(44:47), c(48:51)),
                  v.names = c("Asset","ROA","emp","age","proma","RD","exp","leverage","otherloan","year", "share"),
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


---------------------------------------------------------------------------------------------------------

#II. Ownership and R&D
#2.1 Probability of R&D investment
m1_shareholder <- glm(RDp ~  log(emp)+ log(age) + shareholder+ geo + ROA + (1 | year),  family = binomial, data = data3m)
summary(m1_shareholder) # shareholder and geo are irrelevant in firms' probability of R&D
coef(m1_shareholder)
#confint(m1_shareholder) 
  #model explanation: RDp :        dummy variable of probability of R&D investment
  #                   RD:          R&D ratio (R&D amount/asset)
  #                   log(emp):    logarithm of number of employees
  #                   log(age):    logarithm of age
  #                   shareholder: number of shareholders
  #                   holder:      concentration of shareholder (how much share the top owner has)
  #                   geo:         dummy varialbe 1 if firms's headquater is in the North and Central 
  #                                Italy, 0 if firm is in the South
  #                   ROA:         firm's return on assets
  #                   year:        fixed effect factors to capture the hidden factor that may happen in
  #                                surveyed year
  #                   lag(RD, -1): lag of R&D amount last year           

m1_holder <- glm(RDp ~ log(emp)+ log(age) + holder+ geo + ROA + (1 | year) + (1 |taxcode), family = binomial(link="logit"), data = data3m)
summary(m1_holder)
  stargazer(m1_shareholder, m1_holder, type="text",
          dep.var.labels=c("R&D probability"),
          covariate.labels=c("Log of employees number","Log of firm age","number of shareholder", "concentration of shareholding","Geographical dummy","Return over asset", "year fixed effect"), out="m1.txt")

require(MASS)
exp(coefficients(summary((m1_holder))))
(ctable <- exp(coef(summary(m1_holder))))
anova(m1_holder, test="Chisq")
# Prediction the hit ratio
RDpfac   <- cut(data3m$RDp, breaks=c(-Inf, median(data3m$RDp), Inf), labels=c("0", "1"))
m1_holder <- glmer(RDp ~ log(emp)+ log(age) + holder+ geo + ROA + (1 | year),  family = binomial, data = data3m)
RDphat    <- fitted(m1_holder)
thresh  <- 0.5  # threshold for dichotomizing according to predicted probability
RDphatFac <- cut(Yhat, breaks=c(-Inf, thresh, Inf), labels=c("0", "1"))
cTab    <- table(RDpfac, RDphatFac)    # contingency table
addmargins(cTab)                   # marginal sums


sum(diag(cTab)) / sum(cTab) #0.8070262

      
#m1_institution <- glm(RDp ~ log(emp) + log(age) + institution + geo, data= data3m, family = "binomial")
#summary(m1_institution)
    # Potential problem: multicolineality between holder and institution
    # Note: due to existence of O employees cases, the emp is plused one in this research
    # Note: I choose log(emp) over log(revenue) in representing firm sizeas the later is related to proma 
    # (profit margin)
    # Number of shareholder are irrelevant in firm's R&D probability whilst holder (concentration of 
    # share) does
    # 3 pending variables : institutional shareholder (solving data problem), 
    # group (whether firms belong to a group), 



#2.2 R&D investment
#pooled OLS
m2_pooled <- lm(RD ~ lag(RD,-1) + log(emp) + log(age) + holder + geo, data = subset(data3m,data3m$RD >0))
summary(m2_pooled)

#with year dummy
m2_yeardummy <- lm(RD ~ log(emp) + log(age) + holder + geo + factor(year) - 1, 
                   data = subset(data3m,data3m$RD >0))
summary(m2_yeardummy)

#fixed effect and random effects
m2_fixed <- plm(RD ~ lag(RD, -1) + log(emp) + log(age) + share + geo + ROA, index = c("taxcode", "year"), 
         data = subset(data3m,data3m$RD >0, model = "within"))
   # geo's coefficient is negative but insignificant since only the North occupies 60% of national R&D
   # investment (ISTAT), geo receives values of one if firms works in North and Central Italy.
summary(m2_fixed)
fixef(m2_fixed)

m2_random <- plm(RD ~ lag(RD,-1) + log(emp) + log(age) + holder + geo + ROA, index = c("year"), 
                             data = subset(data3m,data3m$RD >0, model = "random"))
summary(m2_random)
yhat <- m2_fixed$fitted
plot(data3m$holder, data3m$RD, xlab="top holder", ylab="R&D probability")
abline(lm(data3m$RD ~ data3m$holder), lwd=3, col="red")

stargazer(m2_pooled, m2_yeardummy,m2_fixed,m2_random, type="text",
          dep.var.labels=c("R&D asset ratio"),
          covariate.labels=c("Lag of R&D ratio","Log of employees number","Log of firm age","concentration of share","Geographical dummy","Return over asset"), out="m2.txt")


#2.3 Test of data characteristics:
# pFtest(m2_fixed,m2_yeardummy)
# fixed versus time dummy regression 
phtest(m2_fixed, m2_random) 
# Hausman test between fixed and random effect models but does not yield any result
pbgtest(m6) 
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
---------------------------------------------------------------------------------------------------------

#III: Finance and R&D
m3_holder <- glmer(RDp ~ proma + leverage + I(leverage^2)+ otherloan + geo + (1 | year),  family = binomial, data = data3m)
summary(m3_holder)
#model explanation: RDp :        dummy variable of probability of R&D investment
#                   RD:          R&D ratio (R&D amount/ asset)
#                   proma:       profit margin ratio (profit/revenue)
#                   leverage:    firm's leverage (debt/asset)
#                   otherloan:   other funds ratio ((subsidiary + due to shareholder + bond)/asset)
#                   holder:      concentration of shareholder (how much share the top owner has)
#                   geo:         dummy varialbe 1 if firms's headquater is in the North and Central 
#                                Italy, 0 if firm is in the South
#                   year:        fixed effect factors to capture the hidden factor that may happen in
#                                surveyed year
                              

m4_fixed <- lm(RD ~ proma + leverage + I(leverage^2)+ otherloan + geo, index = c("year"),
         data = subset(data3m,data3m$RD>0, model = "within"))
summary(m4_fixed)
m4_pooled <- lm(RD ~ proma + leverage + I(leverage^2)+ otherloan + geo, index = c("year"),
             data = subset(data3m,data3m$RD>0))
summary(m4_pooled)

# pending varialbe: bank relationship and bank herfindahl index by region (bank competition) in 
# collecting period
cor.test(data3m$leverage, data3m$otherloan) 
# suspected relationship since 2 variables are positively correlated and bear signficant relationship

--------------------------------------------------------------------------------------------------------

#IV: Interaction of Ownership and Finance:
#4.1 Probability of R&D
m5_1 <- glm(RDp ~  log(emp)+ log(age) + holder+ geo + ROA + proma + leverage + I(leverage^2)+ 
                   otherloan + (1 | year) + holder*leverage,  family = binomial, data = data3m)
summary(m5_1)
# this one proves to be irrelevant between 2 variables
m5_2 <- glm(RDp ~  log(emp)+ log(age) + holder+ geo + ROA + proma + leverage + I(leverage^2)+ 
              otherloan + (1 | year) + holder*leverage + holder*I(leverage^2),  family = binomial, data = data3m)
summary(m5_2)
# this one does show a relationship!!!!!!!
m5_3 <- glm(RDp ~  log(emp)+ log(age) + holder+ geo + ROA + proma + leverage + I(leverage^2)+ 
              otherloan + (1 | year) + holder*proma + holder*leverage + holder*I(leverage^2),  family = binomial, data = data3m)
summary(m5_3)
# profit margin continues proving to be irrelevant in R&D activities
#m5_4 <- glm(RDp ~  log(emp)+ log(age) + shareholder+ geo + ROA + proma + leverage + I(leverage^2)+ 
#              otherloan + (1 | year) + shareholder*proma + shareholder*leverage + shareholder*I(leverage^2),  family = binomial, data = data3m)
#summary(m5_4)
# number of shareholders are irrelevant

#4.2 R&D amount
m6_2 <- lm(RD ~ log(emp)+ log(age) + holder+ geo + ROA + proma + leverage + I(leverage^2) + 
            otherloan + (1 | year) + holder*leverage + holder*I(leverage^2)
            ,data = subset(data3m,data3m$RD >0, model = "within"))
summary(m6_2)
fixef(m6_2) # this one does not yeild result
# interactive variables are signficiant but holder now not.
stargazer(m5_1, m5_2, m5_3, m6_2, type="text",
          dep.var.labels=c("R&D probability","R&D amount"),
          covariate.labels=c("Log of employees number","Log of firm age","concentration of shareholding","Geographical dummy","Return over asset", "Profit margin", "firm's leverage","firms' leverage squared", "leverage of other sources", "year fixed effect","interaction of concentration and profit margin","interaction of concentration and leverage","interaction of concentration and leverage squared"), out="models.txt")
