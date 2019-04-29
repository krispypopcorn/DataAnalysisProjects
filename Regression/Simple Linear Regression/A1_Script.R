setwd("~/Google Drive/Uni/Winter 2018/STA302/A1")
echo=TRUE;
Dat=read.table("GPA.txt", header=F) 
# change column names 
names(Dat)<-c("GPA","ACT")
# print the six first observations head(Dat)
Dat <- Dat[, c("ACT", "GPA")]
head(Dat)
n = nrow(Dat)
#a
gpa <- Dat[,2]

act <- Dat[,1]

mu_hat_act <- mean(act)
mu_hat_gpa <- mean(gpa)

var_gpa <- var(gpa)
var_act <- var(act)
#-----------

#b
cccc <- cor(gpa, act)
#weak positive

#c
x <- act
y <- gpa


y_mean <- mu_hat_gpa
x_mean <- mu_hat_act

plot(y~x, data=Dat, col="red")
abline(v=x_mean, h=y_mean, lty=2)

fit.Gpa <- lm(GPA ~ ACT, data=Dat)
summary(fit.Gpa)
str(summary(fit.Gpa))

coef(fit.Gpa) #b0 and b1

plot(y~x, data=Dat, col="red", pch=19, xlab="ACT", ylab="GPA", xlim=c(0,40))
abline(coef(fit.Gpa), col="orange")
abline(v=x_mean, h=y_mean, lty=2)
abline(h=20, lty=2)
title('Scatter PLot and the Line of the best fit for ACT and GPA data')
#   (Intercept)         ACT 
# 2.11404929  0.03882713 
#   b0            b1

#e
# For every unit change in the GPA score there is an increase in the ACT score by 0.3882713.
#
#

#f
b0 = 2.11404929
b1 = 0.03882713 

RSS <-sum(resid(fit.Gpa)^2)
s_square = RSS / (n-2)
s = (s_square)^(0.5)
std_error_B1 = ((s_square)/(sum((x - x_mean)^2)))^(1/2)

#H_0: B1 = 0
#H_A: B1 =/= 0
t_value = b1 / std_error_B1

#t-stat =  2.920
#t-value = 1.980	

#since t-value < t-stat, we reject the H_0 
#then b1 is not 0 and there is a linear relationship btwn ACT scores and GPA

#(i)
anova(fit.Gpa)

#(j)
#MSE = RSS/(n-2)
mse <- 0.3883  

#(k)
#f-stat = 9.2402
#f(.05, 1, 118) = 3.9201
##REject null

gpa <- data.frame(ACT=28)
CI_gpa <- predict(fit.Gpa, gpa, interval="confidence", level = 0.95, se.fit = TRUE)
# 3.061384 3.341033

PI_gpa_20 <- predict(fit.Gpa, gpa, interval="prediction", level = 0.95, se.fit = TRUE)
