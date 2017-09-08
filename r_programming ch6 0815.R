p.132
x<- matrix(c(54,3,7,12), nc =2) ###nc는 colon갯수
rbind(c(54,7),c(3,12))
cbind(c(54,3),c(7,12))

rownames(x) = c('p.buckled','p.unbuckled')  #이름만들기
colnames(x) = c('c.buckled','c.unblckled')
x

margin.table(x,1) #r의 합
margin.table(x,2) #c의 합
rowSums(x) #r의 합
colSums(x) #c의합

addmargins(x) #matrix + sum 도 보여줌
prop.table(x)  #조건부확률

par(mfrow = c(1,2))
barplot(x, main = 'child seatbelt usage')
barplot(x, main = 'child seatbelt usage', legend.text=T) #위에 항목 이름 붙이기
barplot(x, main = 'child seatbelt usage', beside = T) #히스토그램 옆으로 그리기

#예제6.1 (p.134)
nico<- read.csv('G:/r programming/nicotin.csv')
attach(nico)
y<- table(nicotin,stopsmoke) #table하면 갯수
prop.table(y)
detach(nico)

#예제 6.2 (p.136)
blood<- read.table('G:/r programming/blood.txt')
attach(blood)
blood
blood[, 2:3]
rowMeans(blood[, 2:3]) #blood의 2,3 열의 가로 평균 구하기
colMeans(blood[, 2:3]) #blood의 2,3 열의 세로 평균 구하기

names(blood)<- c('id','machine','expert')
blood
cor(machine,expert)
plot(machine,expert, main = 'blood pressure measurement')
par(mfrow=c(1,2))
boxplot(machine,expert, names = c('mach','exp'))
plot(density(machine), ylim =c(0,0.04), main = 'density plots')
lines(density(exp), lyt=2)
#p.138
cor(machine, expert)
cor(machine, expert, method = 'spearman')
cor.test(machine,expert)


#예제 6.3 (p.139)
x = machine; y = expert
zones = matrix(c(2,0,1,3), ncol =2, byrow =T) #[변수지정]matrix형성
zones
layout(zones,widths = c(2/3,1/3), heights=c(1/3,2/3)) 
#layout 함수 -> 형성한 매트릭스의 너비, 높이 결정해줌
xhist = hist(x,plot = F) ;yhist = hist(y, plot=F) #[변수지정] 2개의hist만들기
top = max(c(xhist$counts, yhist$counts)) #[변수지정] 각 값의 최댓값 = 그래프의 top
xrange = c(min(x),max(x)); yrange = c(min(y), max(y)) #[변수지정]그래프의 최소,최댓값
#####그래프 3개 그리기
par(mar = c(6,6,1,1))  #그래프 플롯창에 마진을 ()만큼 남기
plot(x,y, xlim= xrange, ylim = yrange) #plot그리기
par(mar = c(0,6,1,1))
barplot(xhist$counts, axes = F, ylim = c(0, top), space =0, col = 'seagreen')
title('Scatterplot with marginal histograms')

par(mar = c(6,0,1,1))
barplot(yhist$counts, axes=F, xlim = c(0,top), space =0, horiz =T, col ='salmon')
detach(blood)



#예6.4 (p.140)
kid <- read.table('G:/r programming/kid.height.txt',header = T)
attach(kid)
kid
cor(height, weight)
plot(height, weight, pch = as.character(gender))
detach(kid)


#예 6.5 (p.141)
par(mfrow=c(1,1))

data(trees)
attach(trees)
head(trees)
N = nrow(trees)
with(trees, {
  symbols(Height, Volume, circles = Girth/16, inches = F, bg=1:N,main = 'bubble plot')
  palette('default')
})
detach(trees)


#####---Chapter06 연습문제-------------------------------------------------########(p.143)
###############-----------------------------------------------------------###############


#####---문제1-----------------------------------------------------------########
frac <- read.csv('G:/r programming/fracture.csv',header = T)
attach(frac)
frac
#a
table(gender); table(fracture)
#b

mean(frac[frac$gender==1,]$blood)
mean(frac[frac$gender==2,]$blood)

#c
mean(frac[frac$fracture==1,]$blood)
mean(frac[frac$fracture==2,]$blood)

#d
cor(frac$age,frac$blood, method = 'pearson')
cor(frac$age,frac$blood, method = 'spearman')

#f
par(mfrow=c(1,1))
boxplot(frac$fracture,frac$blood) #골절 종류별로 blood 상자그림
boxplot(frac$gender,frac$blood) #성별로 blood상자그림

#h
plot(frac$age, frac$blood,pch = as.character(frac$fracture))
#i
plot(frac$age, frac$blood,pch = as.character(frac$gender))


detach(frac)

#####---문제2-----------------------------------------------------------########
table(frac)
#비율표



#####---문제3-----------------------------------------------------------########

x<- c(147,158,131,142,180); y<- c(122,128,125,123,115)
bowling<-rbind(x,y)
colnames(bowling)<- c(1:5)
bowling
cor(x,y,method = 'pearson')
cor(x,y,method = 'spearman')
boxplot(x,y)

#####---문제4-----------------------------------------------------------########
yes<- c(40,30,35,20)
no<- c(20,30,45,40)
event<- rbind(yes,no)
colnames(event)<- as.factor(1:4)
event
plot(table(event))
#####---문제5-----------------------------------------------------------########

data_diabetes<- read.csv("G:/r programming/diabetes.csv")
attach(data_diabetes)
#y1= 상대무게; y2=공복시 혈당; x1 = glucose 한계량 ; x2 = 인슐린반응성 ; x3 = 인슐린 저항성
#a
cor(Y1,Y2)
cor(X1,Y2)
#c
cor.test(X1,Y2) #pvalue = 0.94 유의x
#d
length(Y2[Y2>=90])
length(Y2[Y2<90])
#e
plot(Y1,Y2)
plot(X1,Y2)
#g
cor.test(X1,X2*X3*Y) #####
