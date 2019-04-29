setwd("~/Google Drive/Uni/Winter 2018/STA302/A2")
dat <- read.table("Census.txt", sep = "" ,header=T)

head(dat)

LIFE <- dat$LIFE
MALE <- dat$MALE
BIRTH <- dat$BIRTH
DIVO <- dat$DIVO
BEDS <- dat$BEDS
EDUC <- dat$EDUC
INCO <- dat$INCO
# 
y <- LIFE
# 
# 
# one <-c(1)
# 
# MALE <- append(one, MALE)
# BIRTH <- append(one, BIRTH)
# DIVO <- append(one, DIVO)
# BEDS <- append(one, BEDS)
# EDUC <- append(one, EDUC)
# INCO <- append(one, INCO)


#X_column <- matrix(MALE, nrow = 1, ncol = 52)
# X_column <- append(MALE, BIRTH)
# X_column <- append(X_column, DIVO)
# X_column <- append(X_column, BEDS)
# X_column <- append(X_column, DIVO)
# X_column <- append(X_column, EDUC)
# X_column <- append(X_column, INCO)

# X_column <- rbind(X_column, matrix(BIRTH, nrow = 1, ncol= 52))
# X_column <- rbind(X_column, DIVO)  
# X_column <- rbind(X_column, BEDS)  
# X_column <- rbind(X_column, DIVO)  
# X_column <- rbind(X_column, EDUC)  
# X_column <- rbind(X_column, INCO)  


x = as.matrix(dat)
x <-x[1:51, 2:7]
x<- cbind(1, x)
mode(x) = 'numeric'




n <- 51
p <-7

(X<-x)
Xt <- t(X)

XtX <- Xt %*% X
XtXinv <-solve(XtX)

Xty <- Xt %*% y

(bhat <- XtXinv%*%Xty)

#a
y_hat <- X %*% bhat
e_hat = y - y_hat

#c
RSS <- (t(e_hat) %*% e_hat)
sigma_sq_hat = RSS / (n)

s_sq = RSS / (n - p - 1)

#d
s <-c(s_sq^(1/2))

se_bs = diag(s * (XtXinv)^(1/2))

y_bar = c(sum(y_hat)/ n)

SST <- sum((LIFE - y_bar)^(2))
R_sq <- 1 - (RSS/SST)
# Explains the proportion of variability explained by the regression, which is 46.849%

#PART2
#a
MLR <- lm(formula = LIFE~ MALE + BIRTH + DIVO + BEDS + EDUC + INCO, data=dat) 
summary(MLR)

#b
#H_0: B0 = B1 = B2 = B3 = B4 = B5 = B6
#H_A: Bi != 0 [any i in range 0 to 6]
SSReg = SST-RSS
F_val = (SSReg/(p)/(RSS/(n-p-1)))
F_crt = qf(.95, df1=p, df2=n-p-1) 

#F_val > F_crit we fail to accept the Null hypothesis.


#C
#(0) REJECT
#H_0: B0 = 0
#H_A: B0 != 0
#t_value0 = 16.448 
t_crit = qt(c(.005, .995), df=n-p-1)

#(i)MALE ACCEPT
#H_0: B1 = 0
#H_A: B1 != 0
#t_value1 =2.670
t_crit = qt(c(.005, .995), df=n-p-1)

#(ii)BIRTH REJECT
#H_0: B2 = 0
#H_A: B2 != 0
#t_value2 =4.400
t_crit = qt(c(.005, .995), df=n-p-1)


#(iii)DIVO ACCEPT
#H_0: B3 = 0
#H_A: B3 != 0
#t_value3 =2.658
t_crit = qt(c(.005, .995), df=n-p-1)


#(iv)BEDS REJECT
#H_0: B4 = 0
#H_A: B4 != 0
#t_value4 =3.409
t_crit = qt(c(.005, .995), df=n-p-1)


#(v)EDUC ACCEPT
#H_0: B5 = 0
#H_A: B5 != 0
#t_value5 = 2.133
t_crit = qt(c(.005, .995), df=n-p-1)



#(vi)INCO ACCEPT
#H_0: B6 = 0
#H_A: B6 != 0
#t_value6 =0.786
t_crit = qt(c(.005, .995), df=n-p-1)

#Yes, the results indicate that variables  MALE, DIVO, EDUC and INCO should be reomoved.

MLR_reduced <- lm(formula = LIFE~ BIRTH + BEDS, data=dat) 
summary(MLR_reduced)
#eqn: y_red = 79.12 + 

#e

MLR_red_MALE_INCO <- lm(formula = LIFE~ BIRTH + BEDS + DIVO + EDUC, data=dat) 
anova(MLR, MLR_red_MALE_INCO)
#H_0: B1 = B6 = 0
#H_A: B1, B6 both not zero.
#F_val = 3.5642
#F_Crit = 3.028492
#F_Crit =  qf(.99, df1=p, df2=n-2) 
#F_val > F_crit we fail to accept the Null hypothesis

#f

MLR_MALE <- lm(LIFE ~ MALE, data=dat) 
anova(MLR, MLR_MALE)
#F_val = 7.0963


#g 
#H_0: Bi = 0 [i = 1, 2]
#H_A: B1, B2 both not zero
MLR_B0 = lm(LIFE ~ 1, data=dat) 
MLR_MALE_BIRTH <- lm(LIFE ~ MALE + BIRTH, data=dat) 
anova(MLR_B0, MLR_MALE_BIRTH)
#F_val = 8.14
#We fail to accept the H_0 and hence x1 and x2 are significant/useful in predicting the response




#i
#1
MLR_B0 = lm(LIFE ~ 1, data=dat) 
MLR_MALE
MLR_BIRTH <- lm(LIFE ~ BIRTH, data=dat) 
MLR_DIVO <- lm(LIFE ~ DIVO, data=dat) 
MLR_INCO <- lm(LIFE ~ INCO, data=dat) 
MLR_MALE_BIRTH
MLR_MALE_DIVO <- lm(LIFE~ MALE + DIVO, data=dat) 
MLR_MALE_INCO <- lm(LIFE~ MALE + INCO, data=dat) 
MLR_BIRTH_DIVO <- lm(LIFE ~ BIRTH + DIVO, data=dat) 
MLR_BIRTH_INCO <- lm(LIFE ~ BIRTH + INCO, data=dat) 
MLR_DIVO_INCO <- lm(LIFE ~ DIVO + INCO, data=dat) 
MLR_MALE_BIRTH_DIVO <- lm(LIFE~ MALE + BIRTH + DIVO, data=dat) 
MLR_MALE_BIRTH_INCO <- lm(LIFE~ MALE + BIRTH + INCO, data=dat) 
MLR_BIRTH_DIVO_INCO <- lm(LIFE ~ BIRTH + DIVO + INCO, data=dat) 
MLR_MALE_DIVO_INCO <- lm(LIFE ~ MALE + DIVO + INCO, data=dat) 
MLR_mbdi <- lm(LIFE ~ MALE + BIRTH + DIVO + INCO, data=dat) 

#AIC
aic_b0 <- AIC(MLR_B0) 
aic_m <- AIC(MLR_MALE) 
aic_d <- AIC(MLR_DIVO)
aic_i <- AIC(MLR_INCO)
aic_b <- AIC(MLR_BIRTH)
aic_mb <- AIC(MLR_MALE_BIRTH)
aic_md <- AIC(MLR_MALE_DIVO)
aic_mi <- AIC(MLR_MALE_INCO)
aic_bd <- AIC(MLR_BIRTH_DIVO)
aic_bi <- AIC(MLR_BIRTH_INCO)
aic_di <- AIC(MLR_DIVO_INCO)
aic_mbd <- AIC(MLR_MALE_BIRTH_DIVO)
aic_mbi <- AIC(MLR_MALE_BIRTH_INCO)
aic_bdi <- AIC(MLR_BIRTH_DIVO_INCO)
aic_mdi <- AIC(MLR_MALE_DIVO_INCO)
aic_mbdi <- AIC(MLR_mbdi)

# lowest AIC is with model MALE BIRTH DIVO
#2
null<- MLR_B0
full<- MLR_mbdi

forwdAIC=step(null, scope=list(lower=null, upper=full), direction="forward")
#Best Model according to forwardAIC is LIFE ~ BIRTH + MALE + DIVO

#3
backAIC=step(full, direction="backward", data=dat)
#Best Model according to backwardAIC is LIFE ~ BIRTH + MALE + DIVO

#pART 3
#(a)
std_res <- rstandard(MLR)
plot(MLR$fitted.values, std_res, ylab="Standardized Residuals", xlab="Fitted Values", main="Standardized Residuals")
abline(0, 0)
abline(2, 0, col=c("red"))
abline(-2, 0, col=c("red"))
#Yes, there are 3 points with residuals greater than |2| and may be influential.

#b
lev = hat(model.matrix(MLR))
plot(lev)
abline(0.5, 0, col=c("red"))
dat[lev >0.5,]
# 3 points have lev > 0.5. Idices 1, 8 and 34.

#c
cook = cooks.distance(MLR)

plot(cook)
dat[cook > 1,]
#not all points are the same 

#d
plot(BEDS, std_res, ylab="Standardized Residuals", xlab="BEDS", main="Standardized Residuals")
abline(0, 0)
abline(2, 0, col=c("red"))
abline(-2, 0, col=c("red"))
points(BEDS[8], std_res[8] , col='red', pch = 19)
#There are 3 points that are outliers based on std_res + the outlier red point corresponding to DC 

#e
dat2 <- subset(dat, STATE != 'DC')

LIFE2 <- dat2$LIFE
MALE2 <- dat2$MALE
BIRTH2 <- dat2$BIRTH
DIVO2 <- dat2$DIVO
BEDS2 <- dat2$BEDS
EDUC2 <- dat2$EDUC
INCO2 <- dat2$INCO

MLR_NoDC <- lm(formula = LIFE2 ~ MALE2 + BIRTH2 + DIVO2 + BEDS2 + EDUC2 + INCO2, data=dat2)
std_res_NoDC <- rstandard(MLR_NoDC)
plot(MLR_NoDC$fitted.values, std_res_NoDC, ylab="Standardized Residuals", xlab="Fitted Values", main="Standardized Residuals")
abline(0, 0)
abline(2, 0, col=c("red"))
abline(-2, 0, col=c("red"))



#Yes there is a noticeable change in the Standardised residuals. The fitted line has changed.

#f
#MALE
plot(MALE2, std_res_NoDC, ylab="Standardized Residuals", xlab="MALE", main="Standardized Residuals")
abline(0, 0)
abline(2, 0, col=c("red"))
abline(-2, 0, col=c("red"))
points(MALE[44], std_res_NoDC[44] , col='red', pch = 19)

#BEDS
plot(BIRTH2, std_res_NoDC, ylab="Standardized Residuals", xlab="BIRTH", main="Standardized Residuals")
abline(0, 0)
abline(2, 0, col=c("red"))
abline(-2, 0, col=c("red"))
points(BIRTH2[44], std_res_NoDC[44] , col='red', pch = 19)

#DIVO
plot(DIVO2, std_res_NoDC, ylab="Standardized Residuals", xlab="DIVO", main="Standardized Residuals")
abline(0, 0)
abline(2, 0, col=c("red"))
abline(-2, 0, col=c("red"))
points(DIVO[44], std_res_NoDC[44] , col='red', pch = 19)

#BEDS
plot(BEDS2, std_res_NoDC, ylab="Standardized Residuals", xlab="BEDS", main="Standardized Residuals")
abline(0, 0)
abline(2, 0, col=c("red"))
abline(-2, 0, col=c("red"))
points(BEDS2[44], std_res_NoDC[44] , col='red', pch = 19)

#EDUC
plot(EDUC2, std_res_NoDC, ylab="Standardized Residuals", xlab="BEDS", main="Standardized Residuals")
abline(0, 0)
abline(2, 0, col=c("red"))
abline(-2, 0, col=c("red"))
points(EDUC2[44], std_res_NoDC[44] , col='red', pch = 19)

#INCO
plot(INCO2, std_res_NoDC, ylab="Standardized Residuals", xlab="BEDS", main="Standardized Residuals")
abline(0, 0)
abline(2, 0, col=c("red"))
abline(-2, 0, col=c("red"))
points(INCO2[44], std_res_NoDC[44] , col='red', pch = 19)

#ALL Data corresponding to this STATE is an outlier


dat3 <- subset(dat2, STATE != 'UT')

LIFE3 <- dat3$LIFE
MALE3 <- dat3$MALE
BIRTH3 <- dat3$BIRTH
DIVO3 <- dat3$DIVO
BEDS3 <- dat3$BEDS
EDUC3 <- dat3$EDUC
INCO3 <- dat3$INCO

MLR_NoDC_NoUT <- lm(formula = LIFE3 ~ MALE3+ BIRTH3 + DIVO3 + BEDS3 + EDUC3 + INCO3, data=dat3)

#R_squared is higher without UT data. Therefore this model explains the proportion of variability explained by the regression, better.






