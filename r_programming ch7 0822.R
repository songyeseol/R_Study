#ex1
x<- c(8.30,9.50,9.60,8.75,8.40,9.10,8.15,8.80)
t.test(x)

#예제 7.2
x<- c(8.30,9.50,9.60,8.75,8.40,9.10,8.15,8.80)
z<- (mean(x)-8.5)/(1/sqrt(8))
pz<- pnorm(z,0,1)
pvalue <- 2*(1-pz)
pvalue

#Using R 패키지 이용
library(UsingR)
mean(x)
simple.z.test(x,1.0,0.95)

#예7.3
x1<- c(1.1,2.3,4.3,2.2,5.3)
x2<- c(2.3,4.3,3.5)
t.test(x1,x2, var.equal = T, alternative = 'two.sided')
t.test(x1,x2, var.equal = F)
t.test(x1,x2, var.equal = T, alt = 'greater')
t.test(x1,x2, var.equal = T, alt - 'less')

#예7.4
dd = read.table('G:/r programming/method.txt', header=T)
t.test(x~method, var.equal =T, data=dd)
t.test(x~method, var.equal =F,data=dd)
var.test(x~method, data=dd)

#예7.5
x1<- c(1.1,2.3,4.3,2.2,5.3)
x2<- c(2.3,4.3,3.5)
var.test(x1,x2)

#예7.6
# 짝지어진 집단에 대한 t검정 (시험성적 변화)
pre<- c(77,56,64,60,58,72,67,78,67,79)
post<- c(99,80,78,65,59,67,65,85,74,80)
t.test(post,pre,paired = T) 

#예7.7
#일집단 비율안에서..
prop.test(x=110, n=150, p=0.85,alternative = 'two.sided')

p0<- 0.85; n<-150; x<- 110
sd<- sqrt(p0*(1-p0)/n)
z<- (x/n -p0)/sd
pvalue = 2*(pnorm(z))
pvalue
prop.test(x=110, n=150, p=0.85,alternative = 'less')

#예7.8
#이집단 비율안에서
phat<- c(100/300, 170/400)
n<- c(300,400)
prop.test(n*phat, n, alt = 'two.sided')
