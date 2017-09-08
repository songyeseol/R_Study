#p.116 #5.1
#분할표
res <- c('y','n','y','y','y','n','n','y','y','y')
table(res)


###범주형 데이터(막대그래프, 파이, 점그림)
#막대그래프
barplot(table(res), xlab = 'response', ylab= 'frequency')
barplot(table(res), xlab = 'response', ylab ='frequency', horiz = T) #수직으로 되는

#파이
pie(table(res), main = 'response')

#Simple Pie Chart
slices<- c(10,12,4,16,8)
lb <- c('US','UK','Austrailia','Germany','France')
pie(slices, labels = lb, main = 'Pie Chart of Countries')

#Pie Chart with Percentages
slices<- c(10,12,4,16,8)
lb <- c('US','UK','Austrailia','Germany','France')
pct<- round(slices/sum(slices)*100)     #해당 슬라이스를 전체합의 퍼센트로 바꿔줌
lb <- paste(lb, pct, '%')  #기존에 있던 lb에 pct를 붙여줌    #####paste 함수
pie(slices, labels = lb, main = 'Pie Chart of Countries')

#3D Exploded Pie Chart
library(plotrix)
slices<- c(10,12,4,16,8)
lb <- c('US','UK','Austrailia','Germany','France')
pie3D(slices, labels = lb, exoplode = 0.1, main = 'Pie Chart of Countries')#

#점그림
dotchart(table(res), main = 'dotchart')
sales = c(50,39,47,45)
names(sales) = c('Amy','John','Jack','Lisa')
dotchart(sales, xlab = 'Amount of Sales', main = 'dotchart')

###숫자형 데이터
# 줄기잎그림
x<- c(48,86,34,98,67,78,56,45,85,75,64,75,75,75,58,45,83,74)
stem(x)
stem(x,scale = 2) 

#상자그림
boxplot(x, main = 'Box Plot', sub = 'Basketball game scores')
boxplot(x, horizontal = T, main = 'Box Plot', sub = 'Basketball game scores') #

#히스토그램
hist(x)
hist(x, prob =TRUE, main = 'Histogram of scores with density line',sub = 'basket ball game scores')
lines(density(x))   #확률밀도함수 추정선 추가

#중심경향 측도 (p.122)
x<- c(45,86,34,98,67,78,56,45,85,75,64,75,75,75,58,45,83,74)
mean(x); median(x); var(x); sd(x); IQR(x); range(x);
quantile(x) #사분위수
summary(x)

#표준화 점수 ###########?
y<- c(2,5,7,9,3)
scale(y)
yr<- round(scale(y), digits =2)  #scale(y)값을 반올림 2까지한거.
yr
mean(yr)
sd(yr)

#신뢰구간
library(plotrix)
meany = rep(0,10)  #0을 10개씩 벡터로 만듦
std = rep(0,10)

for(i in 1:10){
  y<- rnorm(30)
  meany[i] = mean(y)           #meany에 ynorm 채워넣음
  std[i] = sd(y)/sqrt(30)       #std에 sd/sqrt(n) 채워넣음
}
par(mfrow=c(1,2))

#신뢰구간 플롯
plotCI(x,y=NULL,uiw,liw=uiw,ui=NULL,li=NULL,err="y", sfrac=0.01,gap=0,slty=par("lty"),add=FALSE,scol=NULL,pt.bg=par("bg"))


#1:10 을 가로에/ meany(정규분포)를 세로,
plotCI(1:10, meany, std, main = 'confidence intervals')
abline(h=0)

plotCI(meany,1:10, 2*std, pch =21, err = 'x', main = 'confidence intervals')
abline(v=0)

plotCI(1:10, meany, 2*std, lwd =2, col ='red',scol ='blue') #lwd는 선의 굵기



#########----연습문제-------------------------------###########
#1.
par(mfrow=c(1,1))

data("InsectSprays")
attach(InsectSprays)

InsectSprays

table(InsectSprays)
barplot(InsectSprays$spray)

pie(table(InsectSprays))

mean(count)

sum(count[spray=="A"])
sum(count[spray=="B"])
sum(count[spray=="C"])
sum(count[spray=="D"])
sum(count[spray=="E"])
sum(count[spray=="F"])



InsectSprays$count

sum_s = rep(0,6)

sum_s[i] = sum(InsectSprays$spray)  



#2
river<- c(735,320,325,392,524,450,1459,135,465,600,330,336,280,315,870,
          906,202,329,290,1000,600,505,1450,840,1243,890,350,407,286,280)

mean(river); median(river); var(river); sd(river); quantile(river)
length(river); 
quantile(river, c(0.15, 0.45, 0.8))
hist(river)
boxplot(river)


#3
bulb<- c(25,16,44,62,36,58,38)
mean(bulb); var(bulb); sd(bulb)

boxplot(bulb, main = 'Lifecycle of bulbs', sub = 'how long it lasts', col = 'lightpink')
stem(bulb)

#4
rr<- rep(0,10)

for (k in 1:10){
  rr[k] = mean(rnorm(20,0,1))
}
rr

rs<- rep(0,10)
for (k in 1:10){
  rs[k] = sd(rnorm(20,0,1))
}
rs

plotCI(x,y=NULL,uiw,liw=uiw,ui=NULL,li=NULL,err="y", sfrac=0.01,gap=0,slty=par("lty"),add=FALSE,scol=NULL,pt.bg=par("bg"))
library(plotrix)
meany = rep(0,10)  #0을 10개씩 벡터로 만듦
std = rep(0,10)
library(plotrix)
m=10;meany=rep(0,m);std=rep(0,m);n=20
for (i in 1:m){
  y=runif(n,min=0,max=1)
  meany[i]=mean(y)
  std[i]=sd(y)/sqrt(n)
}
meany
std
plotCI(1:m,meany,2*std)
abline(h=0.5)


#1:10 을 가로에/ meany(정규분포)를 세로,
plotCI(1:1-, 0, 1, main = 'confidence intervals')
abline(h=0)




#5
revenue<-c(19,21,15,23,24,15,15,15,16,29,
           18,32,20,23,24,24,25,25,25,25,
           25,25,25,36,26,28,30)
mean(revenue); median(revenue)
var(revenue); sd(revenue); range(revenue)
hist(revenue)

re<- round(revenue, digits=-1)
k1<-revenue[revenue<20]
k3<-revenue[revenue>=30]
k2<-revenue[revenue>=20 & revenue<30]
k4<-c(k1,k2,k3))
as.numeric()
dotchart(table(revenue),horizontal=F)


#6
impurity<-c(2.3, 2.4, 3.1, 2.2, 1.0, 2.3, 2.1, 1.1, 1.2, 0.9, 1.5, 1.1)
m6<- mean(impurity)
median(impurity)
var(impurity); sd(impurity); range(impurity)
library(plotrix)
ci<- c((m6-qnorm(0.975)*1.796371/sqrt(12)),(m6+qnorm(0.975)*1.796371/sqrt(12)))
ci
boxplot(impurity)
stem(impurity)

#7 ####

# 7
x7=matrix(c(60,30,10,40),nc=2)
rownames(x7)=c("흡연","비흡연")
colnames(x7)=c("주름있음","주름없음")
x7
pie(x7[1,],main="흡연")
pie(x7[2,],main="비흡연")

#7(a)
a = x[1,][1]/sum(x[1,])
a

#7(b)
b = x[2,][1]/sum(x[2,])
b

#7(c)
c = a-b
c

x
pie(x[[1]])
pie(x$주름있음)
pie(x$주름없음)
pie(x$흡연자)


with<- c(60,10)
without<-c(30,40)
d<-data.frame(with,without)


#8
TV<- c(5.7, 6.7, 6.8, 7.9, 10.6, 11.3, 9.8, 8.4, 8.3, 9.5,
       6.7, 6.9, 9.8, 8.8, 12.1, 10.2, 9.5, 9.4, 9.3, 5.9)
m8<- mean(TV)
var(TV); sd(TV); range(TV)
ci<- c((m8-qt(0.975,19)*sd(TV)/sqrt(20)),(m8+qt(0.975,19)*sd(TV)/sqrt(20)))
ci
boxplot(TV)
hist(TV)
x<- seq(5,13,0.01) 
lines(x, dnorm(x,mean(TV),sd(TV))) ##########


#9
cel<- c(20870, 39400, 65000, 45000, 35890, 29000, 56770,
        23000, 38550, 59800, 39880, 56780, 35220, 48990)

mean(cel)
var(cel); sd(cel); range(cel)
boxplot(cel);
hist(cel,probability = T)
  lines(density(cel))

#10##########
stu<- c(120,80)

80/200
120/200

#11
freshman<- c(5.6, 7.8, 6.5, 7.2, 6.9, 7.3, 5.8, 7.5, 8.2, 7.8)
m11<- mean(freshman)
var(freshman); sd(freshman); range(freshman)
ci<- c((m11-qnorm(0.975)*sd(freshman)/sqrt(10)),(m11+qnorm(0.975)*sd(freshman)/sqrt(10)))
ci

boxplot(freshman)
