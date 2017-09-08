#예제 4.1

pnorm(-1, mean = 0, sd=1, lower.tail = TRUE)
x<- seq(-5,5, length =200)
y= dnorm(x,mean =0, sd=1)
plot(x,y,type='l',lwd=2, col='darkred')

x<-seq(-5,-1,length =100)
y= dnorm(x,mean=0, sd=1)
polygon(c(-5,x,-1),c(0,y,0),col='lightsalmon')

#4.2
pnorm(-1, mean = 0, sd=1, lower.tail = FALSE)
x=seq(-5,5,length=200)
y=dnorm(x,mean=0, sd=1)
plot(x,y,type='l',lwd =2, col='red')

x<-seq(-1,5,length=100)
y=dnorm(x,mean=0,sd=1)
polygon(c(-1,x,5), c(0,y,0),col='lightgray')


#4.3
rnorm(1, mean=100, sd=16)

#4.4
rnorm(5, mean=280, sd=100)


par(mfrow=c(2,2))
pnorm(270, mean=280, sd=10)    #p(x<=270) 인 확률 구하기

qnorm(0.10, mean=280, sd=10, lower.tail = TRUE)

qnorm(0.10, mean = 280, sd=10, lower.tail = FALSE)

qnorm(0.90, mean = 280, sd=10, lower.tail = FALSE)

# 4.5
qnorm(0,05,0,1)
qnorm(0,95,0,1)
dnorm(0,0,1)

x = rnorm(100)
hist(x, probability=TRUE, col = gray(.7), main = 'normal mu0, sigma=1')
curve(dnorm(x), add=T)  #커브곡선 추가

#4.6
par(mfrow=c(2,2))
x<- rt(1000,df=5); y<- dt(x,df=5)       #t분포의 난수
  plot(x,y,sub = 't-dist')

x<- rnorm(1000,0,1) ; y<-dnorm(x,0,1)                  #표준정규분포의 난수
  plot(x,y)

x<- rexp(1000,rate = 1); y<- dexp(x,rate = 1) #지수분포의 난수
  plot(x,y)

x<- rpois(1000, lambda=3); y<- dpois(X,lambda = 3)
  plot(x,y)
rnorm(n, mean = 0, sd=1)

#4.7
#a
bin <- dbinom(0:5, size = 10, prob = 0.3)
bin
names(bin)= 0:5  #함수에 이름붙이
bin

#b
x<- c(1,6,8)
pbinom(x, size = 10, prob = 0.3)   #각값까지의 누적확률

#c
cump<- c(0.2,0.5,0.8)
qbinom(cump, size = 10, prob = 0.3)   #0.2,0.5,0.8 의 누적확률 갖는 확률변수값

#d
rbinom(5, size = 10, prob = 0.3)   #이항분포 B(10,0.3)을 따르는 5개의 난수

#4.8 포아송분포
#b
ppois(1:4, lambda=3)
qpois(0.2, lambda=3)
rpois(10, 3)
ppois(3,3)
1-ppois(3,3)
ppois(5, 3) - ppois(2,3) #P(3<= X <= 5)확률
ppois(5, lambda=3)- ppois(2,lambda=3)

#P(X>5) = 1 - P(X<5)
1-ppois(5,3, lower.tail = T)
ppois(5,3,lower.tail = F)

#h
plot(table(rpois(100,3)), type = 'h', col = 'salmon', lwd = 10) #평균 3인 포아송분포를 따르는 난수100개에 대해 막대그래프




#4.3 정규성 검정
par(mfrow = c(2,3))   
x<-seq(6.5, 13.5, by=0.01)
for (i in 1:5){
  y<- rnorm(50, mean = 10, sd=1)
  hist(y, probability = TRUE, xlim= c(6.5, 13.5), ylim = c(0,0.5))
  
  lines(x, dnorm(x,10,1))
}

x<- rnorm(10)
x
shapiro.test(x) 

#107


par(mfrow=c(2,2))
x <- rnorm(50)
  qqnorm(x, sub = 'normal')
  qqline(x)  #liner
  
x <- runif(50, min=0, max=1)
  qqnorm(x, sub= 'Uniform')
  qqline(x)
  
x <- rexp(50, rate=1)
  qqnorm(x, sub = 'Exponential')
  qqline(x)
x <- rpois(50, lambda = 3)
  qqnorm(x, sub='Poisson')
  qqline(x)


#P.108
m=100; n=5; p = 0.5
  try.n=c(5,10,15,30)
  par(mfrow=c(2,2))
  for (i in 1:4){
    n=try.n[i]
  res=rbinom(m,n,p)
  hist(res, prob=TRUE)
  curve(dnorm(res, n*p, sqrt(n*p*(1-p))), add=TRUE)
  }
  
  #110
m=50
mx= rep(0,m)
n.value=c(5,10,15,30,50)
plot(0,0,type ='n', xlim = c(0,1), ylim = c(0,10),ylab = 'density',
       xlab = 'mx', main = 'uniform mean to normal')
for(k in 1:length(n.value)) {
  n=n.value[k]
  for(i in 1:m){
  mx[i] = mean(runif(n,0,1))    
  }
  lines(density(mx),lty=k, col=k)
}          




plot(0,0, type='n', xlim = c(0,3), ylim = c(0,3.7), ylab = 'density',
     xlab='mx', main = 'exponential to normal')
for(k in 1:length(n.value)){
  n=n.value[k]
  for (i in 1:m){
  mx[i]= mean(rexp(n,rate=1))
  }
lines(density(mx),lty=k, col=k)
  }




#####---Chapter04 연습문제-----------------------------------------------------------########(p.112)
#1.
rand<- rnorm(20,3,5)
rand
mean(rand); sd(rand); hist(rand)

#2
rand_p<- rpois(10,3)
rand_p
mean(rand_p); sd(rand_p); hist(rand_p)

#3
1-qnorm(0.4,10,3)
1-pnorm(12,10,3)
(pnorm(11.5,10,3)-0.5)*2

#4  ##########
qnorm(0.45,0,1)
qnorm(0.495,0,1)


#5
1-pbinom(2,size =5, prob=1/6)

#6
pbinom(5020, size= 10000, prob = 0.5) - pbinom(4980, size=10000, prob = 0.5)

#7
1-pt(2.5,10)
pt(-2.5, 10)
2* pt(1.8, 10)
1 - pt(2.5,10)
pt(-2.5,10)
pt(1.5,10)-pt(-1,10)


#8
1- ppois(2,3)
ppois(3,3)
ppois(7,3)-ppois(1,3)
ppois(4,3)

#9
d<- c(1.5, 2.2, 0.9, 1.3, 2.0, 1.2, 2.5, 2.7, 1.8, 2.3)
hist(d)
qqnorm(d)
qqline(d)
shapiro.test(d)  #p-value 보기
mean(d);var(d);sd(d)

#10
student<- c(182, 167, 166, 159, 178, 176, 169, 163, 166, 181,
            171, 182, 172, 186, 171, 166, 170, 168, 154, 173,
            174, 166, 160, 162, 161, 179, 147, 162, 170, 166,
            165, 178, 171, 169, 183, 149, 168, 177, 170, 163)
mean(student); var(student); sd(student)
hist(student)

line(student, dnorm(student, mean(student), sd(student))
qqnorm(student); qqline(student)

#11
uniform_1<- rnorm(30,0,2)
uniform_2<- rnorm(50,0,2)
mu<- mean(uniform_2)
for(i )
hist(mu)
qqnorm(mu)
qqline(mu)

y<- rpois(30,3)
y
hist(rpois(50,3))
y1<- rpois(50,3)
qqnorm(y1)
qqline(y1)
