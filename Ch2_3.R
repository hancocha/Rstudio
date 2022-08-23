# Using linear regression model to capture nonlinear relationship

attach(c1)

plot(x,y)
abline(lm(y~x))

plot(x,log(y))
abline(lm(log(y)~x))

summary(lm(y~x))
summary(lm(log(y)~x))

attach(d1)

plot(x,y)
abline(lm(y~x))

plot(log(x),y)
abline(lm(y~log(x)))

summary(lm(y~x))
summary(lm(y~log(x)))

# Simulation: Unbiased and variance
# You will not be asked to replicate simulation code
# Unbiased part
set.seed(100)
II = 10000
N = 30
beta0 = 1
beta1 = 2
beta0mat = matrix(0,II,1)
beta1mat = matrix(0,II,1)
for(i in 1:II){
  x = rnorm(N,0,2)
  e = rnorm(N,0,1)
  y = beta0 + beta1*x + e
  beta0mat[i]=summary(lm(y~x))$coefficients[1,1]
  beta1mat[i]=summary(lm(y~x))$coefficients[2,1]
}

mean(beta0mat)
mean(beta1mat)

# Variance
set.seed(100)
II = 10000
N = 30
beta0 = 1
beta1 = 2
beta0mat = matrix(0,II,1)
beta1mat = matrix(0,II,1)
varbeta0mat = matrix(0,II,1)
varbeta1mat = matrix(0,II,1)
for(i in 1:II){
  x = rnorm(N,0,2)
  e = rnorm(N,0,1)
  y = beta0 + beta1*x + e
  beta0mat[i]=summary(lm(y~x))$coefficients[1,1]
  beta1mat[i]=summary(lm(y~x))$coefficients[2,1]
  varbeta0mat[i]=summary(lm(y~x))$coefficients[1,2]
  varbeta1mat[i]=summary(lm(y~x))$coefficients[2,2]
}

sqrt(var(beta0mat))
mean(varbeta0mat)
sqrt(var(beta1mat))
mean(varbeta1mat)

hist(beta0mat,100)
hist(beta1mat,100)

# Multiple Regression Example

attach(gpa1)

summary(lm(colGPA~hsGPA+ACT+alcohol))

# Partial out formula
# For beta1, the slope coefficent for hsGPA

a = lm(hsGPA~ACT+alcohol)
res_a = a$residuals
summary(lm(colGPA~res_a))

# For beta2, the slope coefficent for ACT

b = lm(ACT~hsGPA+alcohol)
res_b = b$residuals
summary(lm(colGPA~res_b))

# For beta3, the slope coefficent for alcohol

c = lm(alcohol~ACT+hsGPA)
res_c = c$residuals
summary(lm(colGPA~res_c))

# R square vs. Adjusted R square

attach(gpa1)

summary(lm(colGPA~hsGPA+ACT+alcohol))

set.seed(111)
x = rnorm(length(colGPA),0,1)

summary(lm(colGPA~hsGPA+ACT+alcohol+x))

# Correlation square and R-square

a = lm(colGPA~hsGPA+ACT+alcohol)

summary(a)

yfit = a$fitted.values

cor(colGPA,yfit)

cor(colGPA,yfit)^2

# Omitted Variable Bias

attach(hprice2)

summary(lm(lprice~dist))

cor(dist,nox)

summary(lm(lprice~dist+nox))

summary(lm(nox~dist))

# Practice Questions
# question 1
attach(meap93)
# part a

summary(lm(math10~lexpend+lnchprg))

# part b

mean(lexpend)
plot(lexpend,math10)

mean(lnchprg)
plot(lnchprg,math10)

# part c

summary(lm(math10~lexpend))

# part d

summary(lm(lnchprg~lexpend))

# question 2
attach(wage1)
# part a

summary(lm(lwage~educ+exper+tenure))

# part c

age = educ+exper+6+rnorm(length(educ),0,0.1)

summary(lm(lwage~educ+exper+tenure+age))

