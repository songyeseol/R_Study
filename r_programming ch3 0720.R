###############################################################
####Chapter3 예제문제 #########################################
###############################################################



#p.61
par (mfrow = c(1,1)) #여러 그래프를 동시에 비교 :par() ---> given 2 by 1 layout of plots
x<-(0:20)*pi/10
y<-cos(x)
ysin <-sin(x)
plot(x,y)
plot(x,ysin)
return

#p.61
x<-(0:20)*pi/10
y<-cos(x); ysin <-sin(x); ysin2 = sin(x)^2

par(mfrow =c(1,2))
  yy = cbind(y,ysin,ysin2)
  matplot(x,yy,type ="pll", pch ="*" )
  plot(x,y)
    lines(x,ysin,type ="p", pch = "*")
    lines(x,ysin2)
    return
    
    
#p.62 3.2
par(mfrow = c(2,2))          #한페이지에 2x2개 나오게 하는함수
plot(x,y,type='p')           #point
plot(x,y,type='l')          #line
plot(x,y, 'b')              #both 
plot(x,y,'p',pch=19, col='orange')     #pch는 점의 모양

#p.64
plot(c(0,7),c(1,7), type = "n", ylab=" ", xlab=" ",axes = FALSE, 
     main = "line type")

for(i in 1:6){
  lines(c(0,7), c(i,i), lty=i )
  axis(2, at=i, labels = paste("lty=", i), las = i)
  }

op = par(mfrow=c(2,2))
plot(x,y, type="b", main="cosine graph", sub="type=b")      #point type
plot(x,y, type="o", las =1, bty = "u", sub = "type =o")     #line type
plot(x,y, type = "h", bty="7", sub="type=h")
plot(x,y, type="s", bty ="n", sub="type=s")                 #step line   
par(op)



#65 abline()-----> 그래프에 수직,평행,대각선 그어주
plot(x,y)
abline(a,b)
abline(h=y)                 #y -> horizontal line으로 그어줌
abline(v=x)                 #x -> vertical line으로 그어줌
abline(lm.obj)              #        

lines(x,y)
abline(x,y)

#p.66
data(cars)
attach(cars)
mean(speed)
mean(dist)

par(mfrow=c(2,2))
plot(speed,dist,pch=1)
  abline(v=15.4)     #15.4인 곳에 vertical line 그어줌
plot(speed,dist,pch=2)
  abline(h=42.98)
plot(speed,dist, pch =3)
  abline(-17,4)
plot(speed,dist,pch = 8)
  abline(v=15.4); abline(h=42.98)

#p.67 예3.2
library(readr)
pr <- read_csv("C:/Users/user/Desktop/primates.csv")
 attach(pr)
win.graph()

pr
# graph 1
plot(x=bodywt, y=brainwt, pch="*", xlab="Bodyweight",
     ylab ="Brain wight", xlim=c(0,250), ylim=c(0,1400), sub = "primates")

# graph 2
plot(x=bodywt, y=brainwt, pch="*", xlab="Bodyweight",
     ylab ="Brain wight", xlim=c(0,250), ylim=c(0,1400), sub = "primates")

plot(x=bodywt, y=brainwt, type='n', xlab="Bodyweight", ylab ="Brain weight")
  points(bodywt[bodywt >=400], brainwt[brainwt<400], pch=3)
  points(bodywt[bodywt <400], brainwt[brainwt<400], pch=1)
  legend(120,1000, c("brain weight<=400", "brain weight >400"), pch=c(3,1))

  
par(mfrow=c(2,2))

# graph 3
plot(bodywt, brainwt, xlim=c(0,300))
  text(x=bodywt, y = brainwt, labels = animal)

# graph 4
plot(bodywt, brainwt, xlim = x(0,300))
  text(x=bodywt, y = brainwt, labels = animal, adj=0)  
  

#########3.3 그래프 좌표축에 수식쓰기
#p.69
p = (1:100)/100
plot(p,sqrt(p*(1-p)),ylab = expression(sqrt(p(1-p))),type ='b')
title('Standard deviation')
plot(p,p*p, ylab = expression(p*p), type = 'l', las =1)
    
#########3.4 다자원 데이터에 대한 다양한 표현
# p.70
library(aplpack)
data("longley")
longley
faces(longley)

stars(longley)


#########3.5 lattice package
win.graph()
plot(x,y)
library(lattice)
show.settings()

#p.73 ex3.3

data(quakes)
quakes
quakes[1:3,]

library(lattice)

mini = min(quakes$depth) ; maxi = max(quakes$depth)
r = ceiling( (maxi-mini)/8) 
r
inf = seq(mini,maxi,r)         
inf
quakes$depth.cat = factor(floor((quakes$depth-mini)/r),
                          labels = paste(inf, inf+r, sep="-"))
quakes[1,]

par(mfrow=c(2,2))
# graph 1
xyplot(lat ~long | depth.cat, data = quakes, main = "Fiji earthquakes data")

# graph 2
cloud(mag~lat*long, data = quakes, sub="magnitude with longitude and lattitude")

# graph 3 
splom(quakes[,1:4])  #scatterplot matrix

# graph 4
bwplot(mag~depth.cat, data=quakes, main = "box drawings")

# drawing histogram
par(mfrow=c(1,1))
hist(quakes$mag)
hist(quakes$mag, probability = T, main = "histogram with density line")
lines(density(quakes$mag))                         ###확률밀도함수 추정선


###p.75 예 3.4
data(Orange)
Orange
#1.
library(lattice)
xyplot( circumference~age |Tree, data=Orange, main='Orange Trees')

histogram(~circumference |Tree, data = Orange, main = 'Orange Trees')

bwplot(circumference ~Tree, data = Orange, main = 'Orange Trees')

bwplot(age~Tree, data = Orange, main = 'Orange Trees')

#p.77


data("iris")
iris[1,]

library(lattice)
#1. 산점도 행렬 (scatterplot matrix)
splom(iris[1:4])  

#2.구름 삼차원 산점도(cloud 3d scatterplot)
cloud(Sepal.Length~ Petal.Length*Petal.Width, data=iris, 
      main = "cloud plot", sub = "iris data")

#3. 평행선 그림(parallel plot)
parallel(~iris[1:4]|Species, data=iris)

#4. 모자이크 (Mosaic plot)
a = rbind(c(50,30), c(50,70))
colnames(a) = c('males', 'females')
rownames(a) = c('survive', 'no survive')
mosaicplot(a, col=2:3)

data("HairEyeColor")
mosaicplot(HairEyeColor, shade =T)




##########3.6 3d graph
#p.80
x1<- seq(-3,3, length = 50)
x2<- seq(-4,4, length = 60)
f = function(x1,x2){x1^2+x2^2+x1*x2}
y = outer(x1,x2, FUN=f)
persp(x1,x2,y)
contour(x1,x2,y)

#p.81 ex 3.5   (bivariate normal dist.)
mu1<-0
mu2<-0
s11<-10
s12<-15
s22<-10
rho<-0.5
x1<-seq(-10,10,length=41)
x2<-x1

f<-function(x1,x2){
  term1<- 1/(2*pi*sqrt(s11*s22*(1-rho^2)))
  term2<- -1/(2*(1-rho^2))
  term3<- (x1-mu1)^2/s11
  term4<- (x2-mu2)^2/s22
  term5<- 2*rho*((x1-mu1)*(x2-mu2))/(sqrt(s11)*sqrt(s22))
  term1*exp(term2*(term3+term4-term5))
}
z<-outer(x1,x2,f) #calculating the density values

#Graph 1 
persp(x1,x2,z, main = "Two demansional Normal Dist.=")

  
#Graph 2 
contour(x1,x2,z, xlab = "x1", ylab = "x2", main="contour plot")

#Graph3
win.graph()
persp(x1,x2,z,
      main = "Two dimensional Normal Dist.",
      sub = expression(italic(f)~(bold(x)) 
        == frac(1,2~pi~sqrt(sigma[11]~sigma[22]~(1-rho^2)))~phantom(0)^bold(.)
        ~exp~bgroup("{", list(-frac(1,2(1-rho^2)),
         bgroup("[", frac((x[1]~-~mu[1])^2, sigma[11])
                ~-~2~rho~frac(x[1]~-~mu[1])^2, sigma[11]))
                ~ frac(x[2])~-~mu[2],sqrt(sigma[22]))~+~
                  frac((x[2]~-~mu[2])^2, sigma[22]),"]")),"}") ),

      col = "lightgreen", theta = 30, phi = 20, r = 50, d=0.1, expand = 0.5,|
      theta =90,| phi = 180, shade = 0.75, ticktype = "detailed", ntick = 5)


mtext(expression(list(mu[1]==0, mu[2]==0, sigma[11]==10,sigma[22]==10, sigma[12]==15,
                      rho==0.5)), side=3)

#########3.7도형그리기
#p.83 사각형
x<- c(1,-1,-1,1,1); y<- c(1,1,-1,-1,1)
plot(x,y,type='l', axes=F, xlab =" ", ylab=" ")
title("Rectangle")

#p.84 원
z<- seq(0,2*pi,length =500)
x<- sin(z); y<-cos(z)
par(pin<-c(5,5))
plot(x,y, type='l', axes=F, xlab =" ", ylab = " ")
title('circle')

#나선
z<- seq(30*pi, 5*pi, length=1000)
x<- sin(z)/(0.1*z) ; y<-cos(z)/(0.1*z)
par(pin = c(5,5))
plot(x,y, type='l', axes = F, xlab = " ", ylab =" ")
title ("Spiral")

#########3.8 그래프 축표시 & 색활용
#p.85
BOTTOM<-1; LEFT<-2; TOP<-3; RIGHT<-4

x<-1:10 ; y<-1:10
par(col.lab="orange", col.main = "gray1")
plot(x,y,col=0:10,pch=CIRCLE<-16, main="Chart Title", axes = FALSE, 
     xlab = "X axis", ylab = "Y axis")



axis(BOTTOM, col="red", col.axis = "red")
axis(LEFT, col = 'green', col.axis = 'green', at= 2*0:5, labels = paste(20*0.5), las =1)
axis(TOP, col = 'blue',col.axis='blue')

#########3.9 모형적합선 그리기
#p.86

library(SemiPar)
    data(fossil)
    attach(fossil)
    
    fit<-spm(strontium.ratio~f(age))
    plot(fit)
    
op<-par(bg = 'white')
par (bg = 'honeydew')
plot(fit, ylim = range(strontium.ratio),col = 'green',
     lwd=5, shade.col ='mediumpurple1', rug.col = 'blue')
points(age,strontium.ratio,col='orange',pch = 16)
par(op)


##########3.10, 지도그리기
#p.87
library(mapdata)
map('world', fill=TRUE, col ="honey dew")
map('world', region=c('USA'))
m = map('world', region =c('South Korea', 'North Korea'))

library(mapproj)
map.grid(m)



###############################################################
####Chapter3 연습문제 #########################################
###############################################################


#-----1번-----------------------------------------------------#
par (mfrow = c(1,1))

y<- x^2 ; x<- seq(-1,1,length = 100)
#b
plot(x,y,pch = 8, xlim = c(-1,1), ylim = c(-1,1)) 
plot(x,x^2,type = 'l',col='red')

#-----2번-----------------------------------------------------#
x<-seq(0,5,0.1)
y<-x; y2<- x^2; ylog<- log(x); ysq<- sqrt(x)
#a
par (mfrow = c(1,1))
plot(x,y); plot(x,y2); 
plot(x,ylog); plot(x,ysq)

#b
yy<- cbind(y,y2,ylog,ysq)        #모든 그래프를 yy변수에 cbind를 이용해 합친다
matplot(x,yy,type = 'l')         #matplot을 이용해 yy에 있는 함수의 그래프를 그린다.  

#c
plot(x,y, type = 'p')
  lines(x,y2,type='l')                #lines를 이용하면 그래프 위에 추가!
  lines(x,ylog, type='b')
  lines(x,ysq, type = 'b')

#-----3번-----------------------------------------------------#

x <- seq(-pi,pi,0.01)
  
plot(x,sin(x), type='l', col="tomato")
  lines(x,cos(x), col="skyblue3")
  lines(x,y,col="seagreen") # tan
  
  
  y <- tan(x)
  thres <- 1                    ##이 방법 써줌으로써 탄젠트의 일직선 지워버림
  y[y>thres] = NA
  y[y<-thres] = NA
  
  lines(x,y,col="seagreen") # tan
  
#-----4번-----------------------------------------------------#
  
library(UsingR)
data(primes)
histogram(primes, col = 'seagreen')

#-----5번-----------------------------------------------------#
#a
x<- seq(-3,3,0.1)
fx<-(1/sqrt(2*pi)*exp(1))^(-1/2*(x^2))
plot(x,fx,type = 'l')


x = seq(-3,3, 0.1)      ##표준정규분ㅍ  
y = dnorm(x)
plot(x, y, type='l')

#b
fx<- exp(-abs(x))
plot(x,fx, type = 'l')

#-----6번-----------------------------------------------------####이해불가
t<- c(0.5,1,2)
x<- seq(0,5,0.01)
fx<- t*exp(1)^(-t*x)
matplot(x,fx,type='l',col='lightpink')

#-----7번-----------------------------------------------------#

j<-c(1:6)
pj<- 1/6
plot(j,pj)            #'x' and 'y' lengths differ

x<-1:6
y=c(1/6,1/6,1/6,1/6,1/6,1/6)
plot(x,y)



#-----8번----------------------------------------------------#
#a
par (mfrow = c(2,2))

x<- seq(-2,2,length=30); y<- seq(-3,3,length=30)
f=function(x,y){x^2 + y^2 + x*(y-3)}

z<-outer(x,y,FUN = f)
persp(x,y,z,col = 'wheat1')

#b
x<- seq(-5,5,length = 50); y<- seq(-5,5,length =50)
f = function(x,y){1-exp(-1*(1/(x^2+y^2)))}
z<-outer(x,y,FUN = f)
persp(x,y,z, col = 'lightskyblue1')


#c
x<- seq(0,16, length = 50); x=y
f = function(x,y){x*sin(y)}
z<-outer(x,y, f)
persp(x,y,z, col = 'lightskyblue3')

x = seq(0, 16, 0.1)
x =y 
f = function(x, y){
  x*sin(y)
}
z = outer(x, y, f)
persp(x, y, z)

##########

plot(x,y, type = 'p')
lines(x,y2,type='l')                #lines를 이용하면 그래프 위에 추가!
lines(x,ylog, type='b')
lines(x,ysq, type = 'b')


#-----9번----------------------------------------------------#
data(crime)
crime
#a
splom(crime)
#b
library(aplpack)
faces(crime)
#c
stars(crime) ##별그림이 안뜸
#d
mean(crime$Arson); sd(crime$Arson) ####what is Arson?


#-----10번----------------------------------------------------#

data("OrchardSprays")

#a
xyplot(decrease~colpos|treatment, data=OrchardSprays)         #산점도

#b
mean(OrchardSprays$decrease); var(OrchardSprays$decrease) #평균,분산

#c
boxplot(OrchardSprays$decrease)                           #상자그림

#d
xyplot(colpos~decrease, data = OrchardSprays, sub = 'OrchardSprays')
.
#11
x<- seq(0,2*pi,length = 100)
z1<- sin(3*x) + sin(4*x)
z2<- sin(3*x) + sin(6*x)
z3<- sin(5*x) + sin(8*x)
z4<- sin(4*x) + sin(7*x)

plot(x,z1, type = 'l', col = 'tomato',lwd=2)
  lines(x,z2, col = 'seagreen',lwd=2)   
  lines(x,z3, col = 'blue',lwd=2)
  lines(x,z4, col = 'orange',lwd=2)


#12

stretch_a<-c(46,54,48,50,44,42,52)
distance_a<-c(183,217,189,208,178,150,249)
stretch_b<-c(25,45,35,40,55,60)
distance_b<- c(71,196,127,187,249,291)
s1<- cbind(stretch_a, distance_a)
s2<- cbind(stretch_b,distance_b)

plot(s1,type = 'p',pch = 'a',col = 'blue')
  lines(s2,type='p',pch='b',col = 'red')


  
# graph 4
plot(stretch_a, distance_a, xlim = c(0,300))
text(x=bodywt, y = brainwt, labels = animal, adj=0)  


a = data.frame(stretch=c(46, 54, 48, 50, 44, 42, 52), 
               distance= c(183, 217, 189, 208, 178, 150, 249))
b = data.frame(stretch=c(25, 45, 35, 40, 55, 60),
               distance=c(71, 196, 127, 187, 249, 291))

plot(a, type='p', pch='a')
  lines(b, type='p', pch='b')
  
  ss=read.csv(file="C:/Users/User/Desktop/band.csv")
  attach(ss)
  win.graph()
  plot(stretch,distance,type='n')
  text(x=stretch,y=distance, labels=class)

