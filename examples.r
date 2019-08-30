#Section 1.3, Table 1.3 APHASIA Bar Plot
APHASIA<-read.table("C:/Users/APHASIA.txt")
counts <- table(APHASIA)
barplot(counts, main="Frequency",
        xlab="Type")


#Example 1.12

xdata <- c(229, 255, 280, 203, 229)
n<- length(xdata)
xbar <- mean(xdata)
s <- sd(xdata)
se_xbar <- s/sqrt(n)
# 95% Conf Int for mu
c(xbar-qt(0.975,4)*se_xbar, xbar+qt(0.975,n-1)*se_xbar)


#Example 1.15 H0: mu=1 vs H1: mu  > 1
xdata<- c(0.5, 0.9, 4.5, 3.4, 1.0,
2.7, 1.1, 1.9, 0.0, 0.0,
4.2, 2.1, 0.0, 2.0, 3.4,
3.4, 2.5, 0.9, 5.1, 2.4)
n<- length(xdata)
xbar <- mean(xdata)
s <- sd(xdata)
se_xbar <- s/sqrt(n)
mu0<- 1
#  test stat
t<- (xbar-mu0)/se_xbar
# pvalue
1-pt(t,n-1)


# class example lm
x=c(1,2,3,4,5)
y=c(1,1,2,2,4)
fit = lm(y ~ x)
summary(fit)
y_hat = -0.1+0.7*x
plot(x,y, ylim=c(0,5))
lines(x,y_hat)
sigma(fit)

#Example 1
x=c(1,2,3,4,5)
y=c(2.5 , 5.3,  6.6, 9.4,  10.5)
fit1 = lm(y ~ x)
summary(fit1)
plot(x,y, ylim=c(0,11))
lines(x,fit1$fitted.values)
sigma(fit)


DataTamp <-read.table("C:/TAMPALMS.txt", header=TRUE)
plot(DataTamp$Market_Val,DataTamp$Sale_Price)
fit2<- lm(DataTamp$Sale_Price ~ DataTamp$Market_Val)
summary(fit2)
lines(DataTamp$Market_Val,fit2$fitted.values,)
