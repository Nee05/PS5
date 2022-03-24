library(haven)
data = read_dta("data_ps5.dta")
View(data)
#checking for missing values
sum(is.na(data))
#3.1.a
#we need to create lowage*post variable - lwpost 
data$lwpost<-data$lowwage*data$post
#for this outcome varibale is ln_avwage
did1<-lm(ln_avwage~lowwage+post+lwpost, data = data)
summary(did1)
#3.1.b outcome var is net_pcm
did2<-lm(net_pcm~lowwage+post+lwpost, data = data)
summary(did2)
stargazer::stargazer(did1)
stargazer(did2)
#3.3
# we need the time varibale - the year in which treatment has begun in our case 1999
data$time = ifelse(data$year >= 1999, 1, 0)
did3<-lm(ln_avwage~lwpost+time+regno, data = data)
summary(did3)
install.packages("plm")
library(plm)
install.packages("lmtest")
library(lmtest)
install.packages("sos")
library(sos)
findFunction("coeftest")
trial <- plm(ln_avwage ~ lwpost+year+regno, 
                    data = data,
                    index = c("regno", "year"), 
                    model = "within")
coeftest(trial, vcov =vcovHC(trial, type = "sss", cluster = "group"))
trial2 <- plm(net_pcm ~ lwpost+time+regno, 
             data = data,
             index = c("regno", "year"), 
             model = "within")

coeftest(trial2, vcov =vcovHC(trial2, type = "sss", cluster = "group"))
install.packages("lfe")
library(lfe)

#3.4
data$indtrends<-data$sic2*data$year
View(data)
#ind specific time trend
trial3 <- plm(ln_avwage ~ lwpost+regno+indtrends, 
             data = data,
             index = c("regno", "indtrends"), 
             model = "within")
summary(trial3)
drop(data$trends)
coeftest(trial3, vcov =vcovHC(trial3, type = "sss", cluster = "group"))
#firm specific time trend
class(data$regno)
data$new_regno<-as.numeric(data$regno)
View(data)
data$firmtrend<-data$new_regno*data$year
trial4 <- plm(ln_avwage ~ lwpost+regno+firmtrend, 
              data = data,
              index = c("regno", "firmtrend"), 
              model = "within")
summary(trial4)
coeftest(trial4, vcov =vcovHC(trial4, type = "sss", cluster = "group"))
