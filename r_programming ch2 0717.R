###############################################################
####Chapter2 예제문제 #########################################
###############################################################

#p.18
x<-1:5
x [x>= 3]<-20 #x가 3보다 크거나 같을때 / 20 대입 ####

sqrt(2) ;sin(pi);exp(1)
log(10, base = 10)
log(10, 2) #base= 넣는거 생략가능
log(10,3)
log(10)               #base defalt 값은 exp(1)
log(2)
log(2, exp(1))
log(2, 10)

abs(-3)               #절대값
factorial(5)          #n!
choose(5,2)           #순열조합

#p.20 2.2
a = c()
a #NULL 값 리턴

#p.21 2.4
k_score = c(97,80,67,88,99,77,78,76,67,97,97)
k_score_2 = c(88,89,90,79,93,81,84,97,81,91,86)
score = c(k_score, k_score_2)
score

#p.23
c(T,T,F,F,T,T,F)
c
x <- -3:3
x #-3,-2,-1,0,1,2,3

w = x<0 # w는 x<0을 성립하는 x의 값들 (true or false)
sum(w)


#p.24
fact <- c(1,2,3)

fact1= as.factor(fact) #범주형 factor로 변환
fact1

fact2 = as.logical(fact) #true or false
fact2
exp.fact = factor(c('exp1','exp2'))
fact3 = as.numeric(exp.fact) #숫자형 데이터 객
fact3

#p.24 2.5반올림
a = c(-2.456,3.6789, 5.2344567)
ceiling(a)                    #올림
floor(a)                      #내림
trunc(a)                      #버림
round(a)                      #반올림 
aa = round(a ,digits = 2)     #밑두자리까지 반올림


#p.25 2.5기초연산
k_score = c(97,80,67,88,99,77,78,76,67,97,97)
sum(k_score) ; mean(k_score) ; max(k_score) ; min(k_score) ; range(k_score)
var(k_score) ; sd(k_score) ; median(k_score) ;length(k_score)
rank(k_score)             #점수 순위 (값이 클수록 낮음) ####how works? --> 홀수는 .0/ 짝수는 .5

x <- cbind(x1 =3, x2= c(4:1,2:8))
dimnames(x)[[1]] = letters[3:13] #행,열 이름 지정 (1은 행, 2는 열)
dimnames(x)[[2]] = letters[1:2]
x

apply(x,1,mean) #행의 평균
apply(x,2, mean) #열의 평균

#p.27
library(MASS)
data(cabbages)  #저장되어있는 데이터 불러오기
attach(cabbages) 
cabbages

cabbages.mean = aggregate(HeadWt, list(Cult), FUN = 'mean')
names(cabbages.mean) = c("Cult", "Mean of HeadWt")        #Cult 별로 mean
cabbages.mean

cabbages.mean = aggregate(HeadWt, list(Cult,Date), FUN = 'mean')
names(cabbages.mean) = c("Cult", "Date","Mean of HeadWt")                #Cult, Date 별로 mean

cabbages1 = cabbages[(cabbages$Date == "d16"),]                          #Date 가 d16일때만 데이터 불러오
cabbages2 = cabbages[(cabbages$Cult == "c52"),]                          #Cult 가 c52일때만 데이터 추출
cabbages3 = cabbages[(cabbages$VitC >50),]
cabbages4 = cabbages[(cabbages$Date == "d21")&(cabbages$VitC> 70),]

cabbages4

#p.28
ida = c(10,5,3,4,8)               #벡터
idb = c(16,7,2,10,5)  
da = cbind(ida,idb)               #행렬
da
a_s = sort(da)                    #행렬 자체에 sort 함수를 넣으면, 모든 element를 sort하는것
a_s1 = sort(ida)
a_s2 = da[order(ida),]            #ida자체에 sort를 하면서 나머지 짝도 같이 올수있게
a_s2
rank(a_s2)                        #순위값
order(a_s2)                       #순서부여

#p.29
1:10
seq(1:10)
rev(1:10)                         #역으로
seq(1,10,2)                       #1:10에서 2칸씩
rep(2,10)                         #2를 10번 반복
rep(1:3,4)                        #1:3을 4번반복
rep(c(1:3),4)                     #위와같음
rep(c(4,3,1),2)                   #c 2번반복

#p.31 2.7 데이터벡터
x = c(100,120,130,143,150,166,172,122,139)
x[1]
x[1:3]
x[c(2,4,7)] #벡터안 인덱스값 반환

names(x) = letters[1:9]  #벡터의 각 열 인덱이름 추가
names(x) = seq(1:9)

xm1 = x[-3]              #벡터에서 3번째 값 제거
xm2 = x[-c(1,5)]         #벡터에서 c벡터안의 인덱스 해당값 제거
xm3 = x[x!=150]          #x값에서 150이 아닐때
xm4 = x[x>140]
xm4



#p.33 2.8벡터의 행렬표현
a = matrix(1:6,nrow =3)       #행렬 만드는데, 1:6까지 숫자를 집어넢고 행이 3이여야.(nrow갯수 원소값 갯수의 약수가 아니면 불가능)
b = matrix(1:8, ncol = 2)     #열이 2


#p.34 
x = c(1,4,2,5,3) ; y = c(6,2,6,1,0)

x+y
xy = t(x)%*% y   ##########전치행렬 표시 어떻게?
xx = x%*% t(x)
x %*% t(x)  #행렬의 곱은 %*%

x*y ;x*x #원소의 곱

temp = c(x,y)              #행렬을 한 벡터로 옮기기 (sort, order 함수도 행렬->벡터로 바꾸는거 가능)
temp1 = cbind(x,y)                 #벡터2개 -> 행렬화 (c는 세로로, r은 가로)
rbind(x,y)
temp1
t(temp1)                       #전치행렬 (transposed matrix)
dim(temp1)                     # 몇개의 행과 열

library(MASS)                  #역행렬 (inverse matrix)
ginv(temp1)                    ######ginv는 no need to be square / but solve needs square ####
solve(temp1)

temp2 <- cbind(c(3,5),c(9,1))

#p.37 -2.8.3 고유값&고유벡터
s <- cbind(c(34.74,15.66),c(15.66,378.6))
s
eigen(s)                  #eigen함수는 squred만 됨. $values = 고유값 & $vectors =각 고유값이 갖는 고유벡터
lambda = eigen(s) ########이해불가
attributes(lambda)
lambda$values[[1]]
lambda$values[[2]]
lambda$vectors[,1]
lambda$vectors[,2]

#p.38 2.9배열열
g = array(1:24, c(4,3,2)) #3차원배열 4->행, 3->열, 2-> 차원

g[1, , ] #첫번째 행  3x2
g[, , 1] #1차원 함수4x3
g[, 1, ] #첫번쨰 열 4x2

array(1:120, c(5,4,3,2))

#p.39 2.10반복문
for (i in 1:4) print(i)

start = 100; end = 200
isum = 0
for (i in start:end) isum = isum +i
print(isum)



#p.40 예제 2.8 ####이해안됨
x<- c(5,6,7,8)
n<- length(x)
xx = rep(0,n)
for( j in 1:n){ xx[j] = x[j]^2
(xx[j])}
print(xx)

#2.9 문자변수의 루핑
transport <- c('bus','subway','car','bike')
for (i in transport)
  print(i)

x= cbind(x1 =3, x2 = 4, x3 = 4)
dimnames(x)[[2]] = letters[1:3]


s = 0
start<-5 ; end<-11
for (i in start:end) s = s+i^2
print(s)


# 2.10 While 을 이용한 루핑
n = 0
sum.sofar = 0
while(sum.sofar <=100) {
  n= n+1
  sum.sofar = sum.sofar +n
}
print(n) ;print(sum.sofar)

#p.41 repeat() 을 이용한 루핑
n = 0
sum.sofar = 0
repeat{r}
print(n) ; print(sum.sofar)


#p.41 조건문
x <- c(1,-2,-3,4,5)
pos = rep(0,5)          #pos 자리를 0으로 미리 채워줘야함(파이썬에서 empty 미리 넣는거 처럼)
for (i in 1:5){
  if( x[i]>0) pos[i] = 1
  else pos[i] = 0
}
print(pos)

#p.42 결측값(missing value)
  x <-c(1,6,2,NA)
is.na(x)                # 결측값 포함 여부 확인 가능 true or false --> 전부 false 출력
x [x>2]                 #x에서 x>2인값만 선택해서 벡터만듦
mean(x) 
mean(x, na.rm = TRUE)    #na.rm = TRUE 옵션 -> 결측값 제외한 데이터로 함수값 구함
xx = na.omit(x)
attr(,"na.action")        #########d이해안됨
attr(,"class")            


r = rank(x)              #NA자리에는 랭킹의 마지막인 4가나옴.
r = rank(x, na.last = TRUE) ; r = rank(x, na.last = FALSE) 
# na.last =TRUE일때는 NA의 랭킹값을 마지막으로 주고, FALSE일떄는 1로 준다. 

#p.43 2.13데이터 프레임
data("OrchardSprays")
OrchardSprays
read.table("OrchardSprays") 

attributes(OrchardSprays)  #프레임의 구성요소를 알 수 있음

OrchardSprays$decrease  #decrease열 다나옴
OrchardSprays$decrease[2] #decrease열의 2번째 인덱스 나옴
OrchardSprays [[1]]   #1열 다나옴
OrchardSprays [[2]]   #2열 다나옴
OrchardSprays [1,]
OrchardSprays [,1]
a <-OrchardSprays$treatment #해당열을 변수 a로 지정


#p.47 2.16데이터 합치기
authors<- data.frame(
  surname = c('Tukey','Vena','Tier','RIp','Mac'),
  nationality = c('US','Aust','US','UK','Aust'))

books<- data.frame(
  name = (c('Tukey','Vena','Tier','RIp','Mac')),
  year = c(1976,1995,1996,1998,2000))
authors
books
d = merge(authors, books, by.x='surname', by.y = 'name')
d


#p.48
authors<- data.frame(
  surname = c('Tukey','Vena','TIer','RIp','Mac'),
  nationality = c('US','Aust','US','UK','Aust'))

books2<- data.frame(
  name = c('Tukey','Vena','TIer','RIp','Mac','kim'),
  year = c(1976,1995,1996,1998,2000,2008)  )
authors
books2
d = merge(authors, books2, by.x='name', by.y = 'surname', all =TRUE) 
                              #결측값 있을경우 all=T 안쓰면 결측값 제외됨
d



#p.50 2.17객체관리
x<-1
mode(x) ; length(x)
a <- c(1,2,3)
a
mode(a) ; length(a)
b <- factor(7:9)
b                  #결과값에 levels: 1 2 3 뜸 #####factor 이해 잘 안됨

x<- 1:4 ; n<- 10 ; y<- 2:4
data.frame(x,n)

L1<- list(x,y) ; L2<- list(A=x, B=y) ###2개 차이점 이해
attribute(L1)
L2
matrix(data = 1:4, 2,2)

ts(1:10, start=2001)   #######What is Time Series

x<-3 ; y<-2.5 ; z<-1
exp1<- expression(x/(y+exp(z)))     #함수지정하는 느낌?
exp1                     
eval(exp1)


#################################################
###############연습문제##########################
#################################################
#1
1+2*(3+4)
1+1/2+1/3+1/4+1/5+1/6
sqrt((4+3)*(2+5))
((1+2)/(4+5))^3
122+(12*23)
factorial(10)
5^2+6^2+7^2+8^2+9^2+10^2+11^2
sin(80)+cos(40)
log(24,10) + log(10, base = exp(1) )
x=15; y=3; sqrt(  (3*(x^2) + 2*(y^3)/((x+y)*(x-y) )))
cos(pi/3) 



#2
x <- c(2,3,5,7,9,10)
x2 <- x^2
s=0
for (i in c)
  s = s+i^2
print(s)
x <- x-2
max(x)
min(x)
x_up<-x[x>5] 
length(x)
t(x)%*%x
x%*%t(x)
xc<-cbind(x,x2) 
xr<-rbind(x,x2)            
xc
xr

#################################################

#3
A<- cbind(c(1,-1,4),c(-1,1,3),c(4,3,2))
B<- cbind(c(3,-2,4),c(-2,1,0),c(4,0,5))
x<- rbind(1,-2,4)
y<- rbind(3,2,1)

A+B ; t(A) ; (t(x)%*%A)%*%y ; t(x)%*%x ; t(x)%*%A%*%x
t(x)*y ; t(A)%*%A ; A%*%B ; t(y)%*%B ; x%*%t(x)
x+y ; x-y ; t(x-y) ; x%*%t(y) ; A-B
t(A) + t(B) ; t(A+B) ; 3*x ; (t(x)%*%y)^2 ; B%*%A

library(MASS)
ginv(A)


#################################################

#4

#a
rep("a",10)
#b
rep(c(1:5),3)
#c
seq(1,100,by=2 )


#################################################

#5
library(UsingR)
data(primes)

length(primes)
x<- primes[primes<201] ; length(x)
y<- primes[primes>= 1000]; length(y)
pp<- primes[500<primes&primes<1000]
pp

#################################################

#6
a1 <- c(86,71,77,68,91,72)
a2 <- c(88,77,76,64,96,72)

#a

zz
write.table('brother.txt','where', sep=",", row.names =FALSE)
write.table('brother_e.xls',sep=",", row.names =FALSE)
write.table('brother_e.csv',sep=",", row.names =FALSE)

#d
mean(elder); mean(younger)
sd(elder); sd(younger)

#################################################

#7
#a
x<- c(-4.123, -3.556, 1.634, 2.213, 3.875)
x
#b
y<-trunc(x, digits =2)
y
#c 
x-
#d
z<- round(x, digits = 2)
z
#e
ceiling(x)



#################################################

#8번문제
korean <- data.frame(
  surname1= I(c('kim', 'lee', 'park', 'oh', 'yang', 'min','jung')),
  kscore = c(93,84,87,95,96,77,82))
  
english <- data.frame(
  surname2 = I(c('kim', 'lee', 'park', 'oh', 'yang', 'min','jung', 'choi')),
  escore = c(90,95,88,75,79,87,90,90))
korean
english


d <- merge(korean, english, by.x='surname1', by.y ='surname2',all=TRUE) 
d

#################################################
#9-(a)
names1<-c('kim','lee','park','oh','anh','min','jung','moon')
x1<-c(93,84,87,95,98,77,82,92)
names2<- c('kang','yun','park','cho','yang','min','jung','choi')
x2<-c(90,95,88,75,79,87,90,91)

x_t <-cbind(c(names1, names2), c(x1,x2))
x_t
a_s = x_t[order(x_t[,1]),decreasing=TRUE]
a_s
a_s1 = x_t[order(x_t[,1]),decreasing=FALSE]
a_s1
a_s = x_t[order(x_t[,1],decreasing=TRUE),]
a_s
a_s_1 = x_t[order(x_t[,1],decreasing=FALSE),]
a_s_1
a_s2 = x_t[order(x_t[,2],decreasing=TRUE),] 
a_s2
a_s3 = x_t[order(x_t[,2],decreasing=FALSE),]
a_s3

#################################################
#10
celcius<-c
for (c in 20:30) print((9/5)*c +32)


#############################################


