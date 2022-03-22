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
stargazer::stargazer(did1,did2, type = "text")
#3.3
# we need the time varibale - the year in which treatment has begun in our case 1999
data$time = ifelse(data$year >= 1999, 1, 0)
did3<-lm(ln_avwage~lwpost+time+regno, data = data)
summary(did3)
install.packages("plm")
library(plm)
trial <- plm(ln_avwage ~ lwpost+time+regno, 
                    data = data,
                    index = c("regno", "year"), 
                    model = "within")
summary(trial)
trial2 <- plm(net_pcm ~ lwpost+time+regno, 
             data = data,
             index = c("regno", "year"), 
             model = "within")
summary(trial2)
