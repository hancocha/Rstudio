# Chapter 4 hypothesis testing

# Example question 1

attach(gpa1)
# part a:

a<-lm(colGPA~hsGPA+ACT+skipped+alcohol)


# part b: testing beta_2 = 0 vs beta_2 =/= 0
# We can answer this question using either one of the three methods: t statistics, confidence interval, and p-value
# We can calculate t stat as,

# we find that the absolute value of test statistics is smaller than critical value, so we fail to reject the null hypothesis.

# Confidence interval
CIbeta2 = c(summary(a)$coefficients[3,1]-qt(0.975,141-4-1)*summary(a)$coefficients[3,2],summary(a)$coefficients[3,1]+qt(0.975,141-4-1)*summary(a)$coefficients[3,2])
# since 0 is contained in confidence interval, we fail to reject the null hypothesis

# part c: testing beta_4=1 vs beta_4 =/= 1

# We can calculate t stat as,
tstatbeta4 = (summary(a)$coefficients[5,1]-1)/summary(a)$coefficients[5,2]
# We can use t-table to find critical value or ask R to find for us
qt(0.975,141-4-1) # finding critical value for t statistics when alpha=0.05 and two-sided test
# This is a two-sided test, so we can test it by comparing the absolute value of t statistics with critical value
(abs(tstatbeta4)>qt(0.975,141-4-1))
# we find that the absolute value of test statistics is larger than critical value, so we reject the null hypothesis.

# Example question 2
attach(vote1)

summary(a<-lm(voteA~lexpendA+lexpendB))

linearHypothesis(a,c("lexpendA=-lexpendB"))

# Example question 3
attach(wage2)

summary(a<-lm(wage~educ+exper+meduc+feduc))

linearHypothesis(a,c("educ=exper"))

linearHypothesis(a,c("meduc=feduc"))

# Example question 4
attach(hprice1)

summary(a <- lm(price~assess))
tstatbeta1 = (summary(a)$coefficients[2,1]-1)/summary(a)$coefficients[2,2]
qt(0.95,88-1-1)

tstatbeta1>qt(0.95,88-1-1)

linearHypothesis(a,c("(Intercept) = 0","assess=1"))

# Example question 5
attach(ceosal2)

summary(a <- lm(lsalary~lsales+lmktval+profmarg+ceoten+comten))

linearHypothesis(a,c("ceoten=0","comten=0"))

# Example question 6
attach(return1)

summary(lm(return~dkr+eps+netinc+salary))

