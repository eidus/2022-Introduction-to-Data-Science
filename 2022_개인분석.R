#사용한 패키지
install.packages('ggplot2')
install.packages('ggrepel')
install.packages('dpylr')
install.packages('scatterplot3d')
install.packages('plotly')
library(ggplot2)
library(ggrepel)
library(dpylr)
library(scatterplot3d)
library(plotly)

#현재 디렉토리 확인
getwd()
#다르면 지정해주기
setwd("C:/Users/rladb/Desktop/R데이터사이언스/2021_Rstudio")
#다시 확인
getwd()
#데이터 불러오기
us<-read.csv("지역별합친데이터.csv", header=TRUE,fileEncoding = "euc-kr")
#변수별별 통계와 표준편차
summary(us$계)
summary(us$X1인당.개인소득)
sd(us$X1인당.개인소득)
sd(us$계)
#boxplot
ggplot(data = us)+geom_boxplot(aes(계))
ggplot(data = us)+geom_boxplot(aes(X1인당.개인소득))


#복지사 수 합
us[,"복지사합"]<-us$사회복지사..명.+us$요양보호사..명.+us$재활치료사..명.
#코드가 진행됨에 따라 분류함.1은 서울과 경기도, 2는 제주, 인천, 대구, 강원도. 0은 나머지 지역들

us$시도분류<-c('1','0','0','0','0','0','0','0','1','0','0','0','0','0','0','0','0')
factor(us$시도분류)

#상관관계
#1인 개인소득 & 복지시설 수
cor(us$X1인당.개인소득,us$계)
cor.test(us$X1인당.개인소득,us$계)


#scatter plot 산점도
#전국 
ggplot(data = us , aes(x = 계, y = X1인당.개인소득,colour=시도분류))+geom_point()+geom_text_repel(aes(label = 시도별 ))
#1은 서울과 경기도. 0은 나머지 지역들
us$시도분류<-c('1','0','0','0','0','0','0','0','1','0','0','0','0','0','0','0','0')
factor(us$시도분류)
#서울과 경기도 filter
us%>% filter(계>10000|X1인당.개인소득>23000)
#그 외 지역 filter 후 
us%>% filter(계<10000&X1인당.개인소득<23000)%>%ggplot(aes(x = 계, y = X1인당.개인소득))+geom_point()+geom_text_repel(aes(label = 시도별 ))
#추세선 추가
us%>% filter(계<10000&X1인당.개인소득<23000)%>%ggplot(aes(x = 계, y = X1인당.개인소득))+geom_point()+geom_text_repel(aes(label = 시도별 ))+geom_smooth()
#nocap에 저장
nocap <-us%>% filter(계<10000&X1인당.개인소득<23000)
#소득대비 시설
nocap$계/nocap$X1인당.개인소득
#소득대비 시설비율 평균
mean(nocap$계/nocap$X1인당.개인소득)
#서울과 경기 제외한 지역의 산점도
us%>% filter(계<10000&X1인당.개인소득<23000)%>%ggplot(aes(x = 계, y = X1인당.개인소득, color=))+geom_point()+geom_text_repel(aes(label = 시도별))
#서울과 경기를 제외한 지역의 상관관계 
cor(nocap$X1인당.개인소득,nocap$계)
#상관관계 유의성 평가
cor.test(nocap$X1인당.개인소득,nocap$계)

#낮은 지역 뽑아내기
#소득과 시설의 각 평균
mean(nocap$X1인당.개인소득)
mean(nocap$계)
#방법 1
nocap%>%filter((X1인당.개인소득 < mean(nocap$X1인당.개인소득))&(계< mean(nocap$계)))%>%select(시도별)
#방법 2
mean(nocap$계/(nocap$X1인당.개인소득/10))
nocap%>%filter(nocap$계/(nocap$X1인당.개인소득/10)< mean(nocap$계/(nocap$X1인당.개인소득/10)))%>%select(시도별)
#방법3
nocap%>%filter(nocap$계/(nocap$X1인당.개인소득/10)< 1)%>%select(시도별,계,X1인당.개인소득)


#nocap 시도분류
nocap$시도분류<-c('1','0','0','1','1','1','1','0','1','1','1','1','1','1','0')
#그래프 다시 그리기- 경기서룽ㄹ 제외 
ggplot(data = nocap , aes(x = 계, y = X1인당.개인소득,colour=시도분류))+geom_point()+geom_text_repel(aes(label = 시도별 ))
#그래프 다시그리기- 전국
#코드가 진행됨에 따라 분류함.1은 서울과 경기도, 2는 제주, 인천, 대구, 강원도. 0은 나머지 지역들
us$시도분류<-c('1','0','2','2','0','0','0','0','1','2','0','0','0','0','0','0','2')
ggplot(data = us , aes(x = 계, y = X1인당.개인소득,colour=시도분류))+geom_point()+geom_text_repel(aes(label = 시도별 ))


#인구수 - 시설 - 소득 +
nocap%>%filter((X1인당.개인소득 >= mean(nocap$X1인당.개인소득))&(계< mean(nocap$계))&(X65세.이상.인구< mean(X65세.이상.인구)))%>%select(시도별, 소득격차)
#인구수 - 시설 - 소득 -
nocap%>%filter((X1인당.개인소득 < mean(nocap$X1인당.개인소득))&(계< mean(nocap$계))&(X65세.이상.인구< mean(X65세.이상.인구)))%>%select(시도별, 소득격차)
#인구수 + 시설 - 소득 -
nocap%>%filter((X1인당.개인소득 < mean(nocap$X1인당.개인소득))&(계< mean(nocap$계))&(X65세.이상.인구>= mean(X65세.이상.인구)))%>%select(시도별, 소득격차)
us[3:4,c(1,9,10)]
#인구수 + 시설 - 소득 +
nocap%>%filter((X1인당.개인소득 >= mean(nocap$X1인당.개인소득))&(계< mean(nocap$계))&(X65세.이상.인구>= mean(X65세.이상.인구)))%>%select(시도별, 소득격차)

#3차원 산점도
p<- plot_ly(us, x = us$X1인당.개인소득, y = us$X65세.이상.인구, z = us$계, color = us$시도분류, text = ~paste('시도:',us$시도별,'<br>1인당개인소득:',us$X1인당.개인소득,'<br>노인인구수:',us$X65세.이상.인구,'<br>복지시설 수:',us$계))%>% add_markers()%>% layout(scene = list(xaxis = list(title = '1인당 개인소득'), yaxis = list(title = '복지시설 수'),zaxis = list(title = '노인 인구')))
p


#panel.cor : 상관관계 함수, 온라인에서 퍼옴
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr)) #on.exit() par()함수 인자가 있으면 실해
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y)) #상관계수 절대값
  txt <- format(c(r, 0.123456789), digits=digits)[1] #상관계수 자릿수 지정
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt) #상관계수 크기에 비례하게 글자지정
}
#산점도 행렬 
pairs(us[,c(3,7,8,11,31)],upper.panel = panel.cor ,main="노인복지현황", pch = 21, bg = c("blue","red","green3")[unclass(us$시도분류)])
pairs(nocap[,c(3,7,8,11,31)],upper.panel = panel.cor ,main="노인복지현황", pch = 21, bg = c("blue","red","green3")[unclass(nocap$시도분류)])

