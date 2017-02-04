############### THE NEW CODE #############
##########################################

#II. data description
setwd("E:/Truyen/Pro Schiavo's paper/data sub")
data2_m <- read_excel("~/data2_m.xlsx")
data3m <- reshape(data2_m,
                   varying = list(c("Asset2015","Asset2014","Asset2013","Asset2012"), 
                                    c("ROA2015","ROA2014","ROA2013","ROA2012"), 
                                    c("emp2015","emp2014","emp2013","emp2012"),
                                    c("age2015","age2014","age2013","age2012"), 
                                    c("proma2015", "proma2014","proma2013","proma2012"),
                                    c("RD2015","RD2014","RD2013","RD2012"),
                                    c("exp2015","exp2014","exp2013","exp2012"),
                                    c("leverage2015","leverage2014","leverage2013","leverage2012"),
                                    c("otherloan2015","otherloan2014","otherloan2013","otherloan2012"),
                                    c("year2015","year2014","year2013","year2012")),
                  v.names = c("Asset","ROA","emp","age","proma","RD","exp","leverage","otherloan","year"),
                  times = 1:4,
                  direction = "long")
attach(data3m)
data3m <- data3m[order(taxcode),]
detach(data3m)
head(data3m)

# Heterogeneity across years
data3m$RDp <- 0
data3m$RDp[data3m$RD > 0] <- 1
library(gplots)
plotmeans(RDp ~ year, main = "probability of R&D over years", data = data3m)
plotmeans(RD ~ year, main = "RD amount over years", data = data3m, subset = data3m$RD >0)
detach("package:gplots")
    # probability of R&D investment varies among years
#III. Ownership and R&D
#1. Probability of R&D investment

m1 <- glm(RDp ~ log(emp) + log(age) + shareholder + geo, data= data3m, family = "binomial")
summary(m1)

m2 <- glm(RDp ~ log(emp) + log(age) + holder + geo, data= data3m, family = "binomial")
summary(m2)
    #Number of shareholder are irrelevant in firm's R&D probability whilst holder (concentration of share)
    #does
m3 <- glm(RDp ~ log(emp) + log(age) + institution + geo, data= data3m, family = "binomial")
summary(m3)
    # Problem: multicolineality between holder and institution
  # Note: due to existence of O employees cases, the emp is plused one in this research
#pooled OLS
m4 <- lm(RD ~ log(emp) + age + shareholder + geo, data = data3m[which(RD >0])
summary(m4)

#with year dummy
m5 <- lm(RD ~ log(emp) + age + shareholder + geo + factor(year) - 1, data = data3m)
summary(m3)

#fixed effect
m6 <- glmer(RD ~ log(emp) + age + shareholder + geo, index = c("year"), data = data3m, model = "within")
summary(m6)
fixef(m6)
pFtest(m6,m2)

#IV: Finance and R&D
m7 <- glm(RDp ~ proma + leverage + otherloan + geo, data= data3m, family = "binomial")
summary(m7)

m8 <- lm(RD ~ log(emp) + age + shareholder + geo, index = c("year"), data = data3m, model = "within")
summary(m8)
cor(data3m$leverage, data3m$otherloan)


#V: Interaction of Ownership and Finance:
m7<- glm(RDp ~ log(emp) + age + holder)
