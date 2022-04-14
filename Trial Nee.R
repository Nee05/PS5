library(haven)
library(stargazer)
library(plm)
library(lmtest)
library(sandwich)
library(lfe)
library(sos)

data = read_dta("data_ps5.dta")
View(data)

#checking for missing values
table(is.na(data))

#3.1.a
#we need to create lowage*post variable - lwpost 
data$lwpost<-data$lowwage*data$post
#for this outcome varibale is ln_avwage
did1A<-lm(ln_avwage~lowwage, data = data)
did1B<-lm(ln_avwage~post, data = data)
did1C<-lm(ln_avwage~lowwage+post+lwpost, data = data)
summary(did1A)
summary(did1B)
summary(did1C)

#3.1.b outcome var is net_pcm
did2A<-lm(net_pcm~lowwage, data = data)
did2B<-lm(net_pcm~post, data = data)
did2C<-lm(net_pcm~lowwage+post+lwpost, data = data)
summary(did2A)
summary(did2B)
summary(did2C)
stargazer(did1C, did2C, type="text")
stargazer(did1C, did2C, type="latex")

#3.2

SE1 = coeftest(did1C, vcov. = vcovCL, cluster = ~regno )
SE2 = coeftest(did2C, vcov. = vcovCL, cluster = ~regno )

Pre_tr1 = sqrt(SE1[1,2]^2+SE1[2,2]^2)
Pre_tr_t1 =(SE1[1,1]+SE1[2,1])/Pre_tr1
Post_c1 = sqrt(SE1[1,2]^2+SE1[3,2]^2)
Post_c_t1 =(SE1[1,1]+SE1[3,1])/Post_c1

Post_tr1 =  sqrt(SE1[1,2]^2+SE1[2,2]^2+SE1[3,2]^2+SE1[4,2]^2)
Post_tr_t1 =(SE1[1,1]+SE1[2,1]+SE1[3,1]+SE1[4,1])/Post_tr1

diff_G1 = sqrt(Post_c1^2+Post_tr1^2)
diff_P1 = sqrt(Pre_tr1^2+Post_tr1^2)

Pre_tr2 = sqrt(SE2[1,2]^2+SE2[2,2]^2)
Pre_tr_t2 =(SE2[1,1]+SE2[2,1])/Pre_tr2
Post_c2 = sqrt(SE2[1,2]^2+SE2[3,2]^2)
Post_c_t2 =(SE2[1,1]+SE2[3,1])/Post_c2

Post_tr2 =  sqrt(SE2[1,2]^2+SE2[2,2]^2+SE2[3,2]^2+SE2[4,2]^2)
Post_tr_t2 =(SE2[1,1]+SE2[2,1]+SE2[3,1]-SE2[4,1])/Post_tr2

diff_G2 = sqrt(Post_c2^2+Post_tr2^2)
diff_P2 = sqrt(Pre_tr2^2+Post_tr2^2)

#3.3
# we need the time varibale - the year in which treatment has begun in our case 1999

did3<-lm(ln_avwage~lwpost+as.factor(year)+regno, data = data)
Clust_se_did3 = coeftest(did3, vcov. = vcovCL, cluster = ~regno )

stargazer(did3, Clust_se_did3, keep = c("lwpost"), type = "text")
stargazer(did3, Clust_se_did3, keep = c("lwpost"), type = "latex")

did3A<-lm(net_pcm~lwpost+as.factor(year)+regno, data = data)
Clust_se_did3A = coeftest(did3A, vcov. = vcovCL, cluster = ~regno )

stargazer(did3A, Clust_se_did3A, keep = c("lwpost"), type = "text")
stargazer(did3A, Clust_se_did3A, keep = c("lwpost"), type = "latex")


#3.4

#ind specific time trend
trial3 <- lm(ln_avwage~lwpost+year*as.factor(sic2), data = data)
Clust_se_trial3 = coeftest(trial3, vcov. = vcovCL, cluster = ~regno )

stargazer(trial3, Clust_se_trial3, keep = c("lwpost"), type = "text")
stargazer(trial3, Clust_se_trial3, keep = c("lwpost"), type = "latex")

trial3A <- lm(net_pcm~lwpost+year*as.factor(sic2), data = data)
Clust_se_trial3A = coeftest(trial3A, vcov. = vcovCL, cluster = ~regno )

stargazer(trial3A, Clust_se_trial3A, keep = c("lwpost"), type = "text")
stargazer(trial3A, Clust_se_trial3A, keep = c("lwpost"), type = "latex")


#firm specific time trend
trial4 <- lm(ln_avwage~lwpost+year*regno, data = data)
Clust_se_trial4=coeftest(trial4, vcov. = vcovCL, cluster = ~regno )

stargazer(trial4, Clust_se_trial4, keep = c("lwpost"), type = "text")
stargazer(trial4, Clust_se_trial4, keep = c("lwpost"), type = "latex")

trial4A <- lm(net_pcm~lwpost+year*regno, data = data)
Clust_se_trial4A=coeftest(trial4A, vcov. = vcovCL, cluster = ~regno )

stargazer(trial4A, Clust_se_trial4A, keep = c("lwpost"), type = "text")
stargazer(trial4A, Clust_se_trial4A, keep = c("lwpost"), type = "latex")

