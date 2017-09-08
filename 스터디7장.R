# 7.1

x = c(8.30, 9.50, 9.60, 8.75, 8.40, 9.10, 8.15, 8.80 )
t.test(x, mu=8.5)

t.test(x, mu=8.5, alt='greater')

t.test(x, mu=8.5, alt='less')

# 7.2

z = (mean(x)-8.5)/(1/sqrt(8))
pz = pnorm(z, 0, 1)
z
pz
pvalue=2*(1-pz)
pvalue

library(UsingR)
simple.z.test(x, 1.0, conf.level=0.95)

# 7.3

x1 = c(1.1,2.3,4.3,2.2,5.3)
x2 = c(2.3,4.3,3.5)

t.test(x1, x2, var.equal = T, alt = 'two.sided')

t.test(x1, x2, var.equal = F, conf.level = 0.95)

t.test(x1, x2, var.equal = T, alt = 'greater')

t.test(x1, x2, var.equal = T, alt = 'less')

# 7.4

dd = read.table('C:/Users/User/method.txt', header = T)
t.test(x~method, var.equal=T, data=dd)
dd$x
dd$method

var.test(x~method, data=dd)

# 7.5

var.test(x1, x2)

# 7.6 

pre = c(77,56,64,60,58,72,67,78,67,79)
post = c(99,80,78,65,59,67,65,85,74,80)
t.test(post, pre, paired=T)

# 7.7 

prop.test(x=110, n=150, p=0.85, alt='two.sided')

p0 = 0.85; n = 150 ; x = 110
sd = sqrt(p0*(1-p0)/n)
z = (x/n - p0)/sd
pvalue = 2*(pnorm(z))
pvalue

prop.test(x=110, n=150, p=0.85, alt='less')

# 7.8

phat = c( 100/300, 170/400)
phat
n = c(300,400)
phat*n

prop.test(c(100, 170), n, alt='two.sided')


############################ 연습문제

#1a)

time = c(159, 280, 101, 121, 224, 222, 379, 179, 250, 170)

# h0: mu <= 225 ; h1: mu > 225

#1b) 

mean(time)
median(time)
var(time)
sd(time)

#1c) 

t.test(time, alt ='greater', conf.level=0.95)

#1d)

t.test(time, mu=225, alt = 'greater', conf.level=0.95)

confint(time)
#1e) 

t.test(time, mu=225, alt ='greater', conf.level=0.90)

#1f)

y = log(time)

#1g) 

mean(y)
var(y)

#2a)

light = c(25,16,44,62,36,58,38)
mean(light)

#2b) 

var(light)

#2c) 

sd(light)

#2d) 

boxplot(light)

#2e) 

boxplot(light, col='green',horizontal=T)

#3a)

ma =c(501,502,495,498,499,506)
mb = c(508,510,503,504,500,504,505)

var.test(ma, mb, conf.level=0.95)

#3b) 

t.test(ma, mb, var.equal=TRUE)

#3c)

mean(ma)
median(ma)
var(ma)
sd(ma)

mean(mb)
median(mb)
var(mb)
sd(mb)

#4a)

before = c(55,60,70,75,66,78,80,83,88,73)
after = c(54,55,64,73,61,70,76,65,78,72)
diff = before-after

mean(before); mean(after); mean(diff)
var(before); var(after); var(diff)
sd(before); sd(after); sd(diff)

t.test(before, after, paired=T)

#5a)

x1 =c(51,27,37,42,27,43,41,38,36,26,29,35)
x2 =c(36,20,22,36,18,32,22,21,23,31,20,30)

mean(x1) ; var(x1) ; sd(x1)
mean(x2) ; var(x2) ; sd(x2)

#5b)

t.test(x1, mu=30, conf.level=0.95)

#5c) 

t.test(x2, mu=25, conf.level=0.95)

#6a)

prop.test(x = 120, n = 300, p=0.50, alt='two.sided')

#6b)

prop.test(x = 120, n = 300, p=0.50, alt='less')

#7)

phat = c(500/3500, 200/2800)
n = c(3500, 2800)
prop.test(phat*n, n, alt='two.sided')

#8) 

prop.test(x=50, n=200, p=0.2, alt='two.sided')

#9)

phat = c(3000/5500, 2000/3000)
n = c(5500, 3000)
prop.test(phat*n, n, alt='less')  #b가 뒤에 있어서 less로 들어간것 

phat1 = c(2000/3000, 3000/5500)
n1 = c(3000,5500)
prop.test(phat1*n1, n1, alt='greater')

#10a) 

c = c(14,15,16,13,12,17,15,13,16,13)
d = c(8,11,9,8,10,11,7,9,6,8,7,10)

mean(c) ; sd(c)

#10b)

mean(d) ; sd(d)

#10c)

var.test(c, d) #분산 같음

t.test(c, d, var.equal=T)

#11)

g1 = c(67,79,57,66,71,78)
g2 = c(42,61,64,76,45,58)

var.test(g1, g2) #분산같음

t.test(g1, g2, var.equal=T)

#12) 

before = c(2.1,5.0,1.4,4.6,3.0,4.3,3.2)
after = c(1.9,0.5,2.8,3.1,2.7,1.8)

var(after) ; var(before)
var.test(before, after)

t.test(before, after, var.equal=T)

#13)

a = c(90,88,78,65,78,60,89,73)
b = c(80,78,75,67,73,62,79,70)

var.test(a,b)
var(a);var(b)

t.test(a,b, paired=T)

#!4)

e = c(90,88,78,65,78,60,89,73)
f = c(80,78,75,69,73,62,79,70)

var.test(e,f)

t.test(e,f,paired=T)
