
path = "C:\\Users\\admin\\Desktop\\korea_gg_real_estate_Anomaly_Detection\\apart\\input"

path1 = paste(path, "/2016/아파트.csv", sep='')
path2 = paste(path, "/2016/단독다가구.csv", sep='')
path3 = paste(path, "/2016/연립다세대.csv", sep='')
path4 = paste(path, "/2016/오피스텔.csv", sep='')

path5 = paste(path, "/2017/아파트.csv", sep='')
path6 = paste(path, "/2017/단독다가구.csv", sep='')
path7 = paste(path, "/2017/연립다세대.csv", sep='')
path8 = paste(path, "/2017/오피스텔.csv", sep='')

path9 = paste(path, "/2018/아파트.csv", sep='')
path10 = paste(path, "/2018/단독다가구.csv", sep='')
path11 = paste(path, "/2018/연립다세대.csv", sep='')
path12 = paste(path, "/2018/오피스텔.csv", sep='')

path13 = paste(path, "/2019/아파트.csv", sep='')
path14 = paste(path, "/2019/단독다가구.csv", sep='')
path15 = paste(path, "/2019/연립다세대.csv", sep='')
path16 = paste(path, "/2019/오피스텔.csv", sep='')

path_shp = paste(path, "/TL_SCCO_SIG.shp", sep='')
  
apt1 <- read.csv(path1, header=T, stringsAsFactors=TRUE)
apt2 <- read.csv(path2, header=T, stringsAsFactors=TRUE)
apt3 <- read.csv(path3, header=T, stringsAsFactors=TRUE)
apt4 <- read.csv(path4, header=T, stringsAsFactors=TRUE)

apt5 <- read.csv(path5, header=T, stringsAsFactors=TRUE)
apt6 <- read.csv(path6, header=T, stringsAsFactors=TRUE)
apt7 <- read.csv(path7, header=T, stringsAsFactors=TRUE)
apt8 <- read.csv(path8, header=T, stringsAsFactors=TRUE)

apt9 <- read.csv(path9, header=T, stringsAsFactors=TRUE)
apt10 <- read.csv(path10, header=T, stringsAsFactors=TRUE)
apt11 <- read.csv(path11, header=T, stringsAsFactors=TRUE)
apt12 <- read.csv(path12, header=T, stringsAsFactors=TRUE)

apt13 <- read.csv(path13, header=T, stringsAsFactors=TRUE)
apt14 <- read.csv(path14, header=T, stringsAsFactors=TRUE)
apt15 <- read.csv(path15, header=T, stringsAsFactors=TRUE)
apt16 <- read.csv(path16, header=T, stringsAsFactors=TRUE)

str(apt1); str(apt5); str(apt9); str(apt13);
str(apt2); str(apt6); str(apt10); str(apt14);
str(apt3); str(apt7); str(apt11); str(apt15);
str(apt4); str(apt8); str(apt12); str(apt16);
head(apt1)

apt1 <- subset(apt1, select= -c(번지,본번,부번,단지명,도로명))
apt2 <- subset(apt2, select= -c(번지,도로조건,도로명)); apt2['층'] <- 1; apt2 <- apt2[,c(1,3,2,4:7, 9, 8)]
apt3 <- subset(apt3, select= -c(번지,본번,부번,건물명,도로명))
apt4 <- subset(apt4, select= -c(번지,본번,부번,단지명,도로명))

apt5 <- subset(apt5, select= -c(번지,본번,부번,단지명,도로명))
apt6 <- subset(apt6, select= -c(번지,도로조건,도로명)); apt6['층'] <- 1; apt6 <- apt6[,c(1,3,2,4:7, 9, 8)]
apt7 <- subset(apt7, select= -c(번지,본번,부번,건물명,도로명))
apt8 <- subset(apt8, select= -c(번지,본번,부번,단지명,도로명))

apt9 <- subset(apt9, select= -c(번지,본번,부번,단지명,도로명))
apt10 <- subset(apt10, select= -c(번지,도로조건,도로명)); apt10['층'] <- 1; apt10 <- apt10[,c(1,3,2,4:7, 9, 8)]
apt11 <- subset(apt11, select= -c(번지,본번,부번,건물명,도로명))
apt12 <- subset(apt12, select= -c(번지,본번,부번,단지명,도로명))

apt13 <- subset(apt13, select= -c(번지,본번,부번,단지명,도로명))
apt14 <- subset(apt14, select= -c(번지,도로조건,도로명)); apt14['층'] <- 1; apt14 <- apt14[,c(1,3,2,4:7, 9, 8)]
apt15 <- subset(apt15, select= -c(번지,본번,부번,건물명,도로명))
apt16 <- subset(apt16, select= -c(번지,본번,부번,단지명,도로명))


apt1['aptType'] <- '아파트';     apt5['aptType'] <- '아파트';     apt9['aptType'] <- '아파트';      apt13['aptType'] <- '아파트'
apt2['aptType'] <- '단독다가구'; apt6['aptType'] <- '단독다가구'; apt10['aptType'] <- '단독다가구'; apt14['aptType'] <- '단독다가구'
apt3['aptType'] <- '연립다세대'; apt7['aptType'] <- '연립다세대'; apt11['aptType'] <- '연립다세대'; apt15['aptType'] <- '연립다세대'
apt4['aptType'] <- '오피스텔';   apt8['aptType'] <- '오피스텔';   apt12['aptType'] <- '오피스텔';   apt16['aptType'] <- '오피스텔'

str(apt1); str(apt5); str(apt9); str(apt13);
str(apt2); str(apt6); str(apt10); str(apt14);
str(apt3); str(apt7); str(apt11); str(apt15);
str(apt4); str(apt8); str(apt12); str(apt16);

a <- c('city','priceType','area', 'yyyymm', 'day', 'deposit','price', 'floor', 'build', 'aptType')
colnames(apt1) <- a; colnames(apt2) <- a; colnames(apt3) <- a; colnames(apt4) <- a;
colnames(apt5) <- a; colnames(apt6) <- a; colnames(apt7) <- a; colnames(apt8) <- a;
colnames(apt9) <- a; colnames(apt10) <- a; colnames(apt11) <- a; colnames(apt12) <- a;
colnames(apt13) <- a; colnames(apt14) <- a; colnames(apt15) <- a; colnames(apt16) <- a;

apt0 <- rbind(apt1, apt2, apt3, apt4, apt5, apt6, apt7, apt8, apt9, apt10, apt11, apt12, apt13, apt14, apt15, apt16)
str(apt0)
head(apt0)

apt0$yyyy <- substr(apt0$yyyymm, 1, 4)
apt0$mm <- substr(apt0$yyyymm, 5, 6)
apt0$city <- as.character(apt0$city)
apt0$si <- data.frame(do.call('rbind', strsplit(apt0$city, split=' ', fixed=T)))[,2]
apt0$deposit <- as.numeric(gsub(",", "", apt0$deposit))
apt0$price <- as.numeric(gsub(",", "", apt0$price))
apt0$floor <- as.numeric(gsub(",", "", apt0$floor))
apt0$day <- as.character(apt0$day)
apt <- apt0[,-1]
head(apt)
str(apt)

# 데이터가 너무 방대하여 10000개만 비복원추출############
set.seed(42)

all <- apt[sample(nrow(apt), 5000), ]
write.csv(apt, "C:/Users/admin/Desktop/korea_gg_real_estate_Anomaly_Detection/all.csv")

train <- apt[sample(nrow(apt), 4000) , ]
test <- apt[sample(nrow(apt), 1000) , ]

str(all)
str(train)
str(test)


saveRDS(all, file="C:/Users/admin/Desktop/korea_gg_real_estate_Anomaly_Detection/all.RDS")
saveRDS(train,file="C:/Users/admin/Desktop/korea_gg_real_estate_Anomaly_Detection/train.RDS" )
saveRDS(test,file="C:/Users/admin/Desktop/korea_gg_real_estate_Anomaly_Detection/test.RDS" )

#읽어오기#########################################################



path = "C:/Users/admin/Desktop/korea_gg_real_estate_Anomaly_Detection/"
all <- readRDS(file=paste(path, "all.RDS", sep=''))
train <- readRDS(file=paste(path, "train.RDS", sep=''))
test <- readRDS(file=paste(path, "test.RDS", sep=''))

str(all)

md_all <- all[,c(1,2,5,6,9,12)]
md_train <- train[,c(1,2,5,6,9,12)]
md_test <- test[,c(1,2,5,6,9,12)]

# 모델링 ##########################################################
head(md_train, 5)
head(md_test, 5)

# 회귀 결정트리 ##########################################################
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

decision_tree <- rpart(price ~ ., data=md_train, method="anova") #회귀트리
summary(decision_tree)
ptree<-prune(decision_tree, cp=0.028)
rpart.plot(ptree)

decision_tree <- rpart(deposit ~ ., data=md_train, method="anova") #회귀트리
summary(decision_tree)
ptree<-prune(decision_tree, cp=0.1)
rpart.plot(ptree)

# 랜덤포레스트 ##########################################################
install.packages("randomForest")
# install.packages("forecast")
library(randomForest)
# library(forecast)

############################################ 추가

md_train$aptType <- as.factor(md_train$aptType)
md_train$si <- as.factor(md_train$si)
md_test$aptType <- as.factor(md_test$aptType)
md_test$si <- as.factor(md_test$si)

############################################ 원본

result.rf <- randomForest(price~., data=md_train, ntree=100, mtry=4, na.action=na.omit, importancve=TRUE)
#traing cases는 100, 사용변수는 4, 결측치가 나면 에러가 나므로 결측치를 없애줌

result.rf
varImpPlot(result.rf)
round(importance(result.rf ), 2)
tmp <- round(importance(result.rf ), 2)
tmp[order(tmp),]

plot(result.rf)

y_pred = predict(result.rf, newdata=md_test)
y_pred

(mse = mean((md_test$price - y_pred)^2))
(rmse = sqrt(mse))
############################################ 추가

# https://tongtongsear.tistory.com/268

true <- md_test$price

length(y_pred)
length(true)

summary(y_pred)
summary(true)

cor(y_pred, true)
resi <- (y_pred - true)
plot(resi, main="Residual Distribution")

df = as.data.frame(cbind(true, y_pred))
df
e_lm <- lm(true~y_pred, data=df)
summary(e_lm)
p <- predict(e_lm, interval="prediction", level=0.95)
# confindence interval : 지정된 신뢰구간 내에서 예측치들의 평균값이 위치할 범위
# prediction interval : 지정된 신뢰구간 내에서 예측치들이 존재할 범위 ★

d <- as.data.frame(cbind(true, p))
head(d)
table(d$true > d$upr)
table(d$true < d$lwr)

plot(y_pred, true, main="Anomaly Detection Plot 1")

lines(y_pred, d[,2],lwd=2, col='blue')
lines(y_pred, d[,3],lwd=2, col='red')
lines(y_pred, d[,4],lwd=2, col='red')

head(d)
conf <- (true > d[,4] | true < d[,3])
cls <- ifelse(conf, 'abnormal', 'normal')
table(cls)
round(prop.table(table(cls)), 4)*100

############################################ 원본

result.rf2 <- randomForest(deposit~., data=md_train, ntree=100, mtry=4, na.action=na.omit )
#traing cases는 100, 사용변수는 4, 결측치가 나면 에러가 나므로 결측치를 없애줌
result.rf2
varImpPlot(result.rf2)
round(importance(result.rf2 ), 2)
tmp2 <- round(importance(result.rf2 ), 2)
tmp2[order(tmp2),]

plot(result.rf2)

y_pred2 = predict(result.rf2, newdata=md_test)
(mse = mean((md_test$deposit - y_pred2)^2))
(rmse = sqrt(mse))

y_pred2[2]
md_test$deposit[2]

plot(y_pred2, true2)

############################################ 추가

true2 <- md_test$deposit

length(y_pred2)
length(true2)

summary(y_pred2)
summary(true2)

cor(y_pred2, true2)
resi2 <- (y_pred2 - true2)
plot(resi2, main="Residual Distribution 2")

df = as.data.frame(cbind(true2, y_pred2))
df
e_lm2 <- lm(true2~y_pred2, data=df)
summary(e_lm2)
p2 <- predict(e_lm2, interval="prediction", level=0.95)
d2 <- as.data.frame(cbind(true, p2))
table(d2$true > d2$upr)
table(d2$true < d2$lwr)

plot(y_pred2, true2,  main="Anomaly Detection Plot 2")

lines(y_pred2, d2[,2],lwd=2, col='blue')
lines(y_pred2, d2[,3],lwd=2, col='red')
lines(y_pred2, d2[,4],lwd=2, col='red')

head(d)
conf2 <- (true > d2[,4] | true < d2[,3])
cls2 <- ifelse(conf, 'abnormal', 'normal')
table(cls2)
round(prop.table(table(cls2)), 4)*100

############################################ 원본

# EDA ##########################################################
df1 <- subset(all, priceType=='월세')
df2 <- subset(all, priceType=='전세')

tmp <- gsub(' ','',paste(paste(paste(paste(substr(df1$yyyymm, 1, 4), "-"), substr(df1$yyyymm, 5, 6)), '-'),df1$day))
df1$yyyymmdd <- as.Date(tmp)

tmp <- gsub(' ','',paste(paste(paste(paste(substr(df2$yyyymm, 1, 4), "-"), substr(df2$yyyymm, 5, 6)), '-'),df2$day))
df2$yyyymmdd <- as.Date(tmp)

str(df1)
str(df2)
str(df1)
str(df2)

dim(df1)
dim(df2)


# R Graphics를 이용한 데이터 분포 확인 ==========================
par(mfcol=c(2,2))

# 전체 
hist(all$deposit, main = "보증금의 분포", xlab="보증금(만원)");
boxplot(all$deposit, horizontal=TRUE);
# summary(all$deposit)
hist(all$price , main = "월·전세가격의 분포", xlab="월·전세가격(만원)");
boxplot(all$price, horizontal=TRUE);
# summary(all$price)

hist(all$area, main = "면적의 분포", xlab="면적(m²)");
boxplot(all$area, horizontal=TRUE);
# summary(all$area)
hist(all$floor, main = "층의 분포", xlab="층");
boxplot(all$floor, horizontal=TRUE);
# summary(apt$floor)


#월세
hist(df1$deposit, main = "보증금의 분포", xlab="보증금(만원)");
boxplot(df1$deposit, horizontal=TRUE);
# summary(df1$deposit)
hist(df1$price, main = "월세가격의 분포", xlab="월세가격(만원)");
boxplot(df1$price, horizontal=TRUE);
# summary(df1$price)

hist(df1$area, main = "면적의 분포", xlab="면적(m²)");
boxplot(df1$area, horizontal=TRUE);
# summary(df1$area)
hist(df1$floor, main = "층의 분포", xlab="층");
boxplot(df1$floor, horizontal=TRUE)
# summary(df1$floor)


#전세
hist(df2$area, main = "면적의 분포", xlab="면적(m²)");
boxplot(df2$area, horizontal=TRUE);
# summary(df2$area)
hist(df2$floor, main = "층의 분포", xlab="층");
boxplot(df2$floor, horizontal=TRUE);
# summary(df2$floor)

par(mfrow=c(2,1))

hist(df2$deposit, main = "전세가격의 분포", xlab="전세가격(만원)");
boxplot(df2$deposit, horizontal=TRUE);
#summary(df2$deposit)
#summary(df2$price)

par(mfcol=c(1,1))

table(all$priceType); round(prop.table(table(all$priceType)),3)
table(all$aptType); round(prop.table(table(all$aptType)),3)
table(all$yyyy); round(prop.table(table(all$yyyy)),3)
table(all$mm); round(prop.table(table(all$mm)),3)
table(as.factor(all$day)); round(prop.table(table(as.factor(all$day))),3)


# ggplot 시각화 패키지 로딩 ======================================
install.packages(c("ggplot2", "RColorBrewer", "ggcorrplot", "ggExtra"))
library(ggplot2)
library(RColorBrewer)
library(ggcorrplot)
library(ggExtra)


# 시계열 그래프
str(df1)
str(df2)

sub_s1 <- aggregate(df2$deposit,
                    by = list(YYYYMMDD = df2$yyyymmdd),
                    FUN = median)
ggplot(sub_s1, aes(x=YYYYMMDD, y=x)) + geom_line() + geom_smooth() +
  labs(title = "시계열 : 2016~2019년간 경기도의 전세 가격", x="월", y="가격(만 원)")


sub_s2 <- aggregate(df1$price,
                    by = list(YYYYMMDD = df1$yyyymmdd),
                    FUN = median, na.rm=TRUE)
ggplot(sub_s2, aes(x=YYYYMMDD, y=x)) + geom_line() + geom_smooth() +
  labs(title = "시계열 : 2016~2019년간 경기도의 월세 가격", x="월", y="가격(만 원)")


sub_s3 <- aggregate(df1$deposit,
                    by = list(YYYYMMDD = df1$yyyymmdd),
                    FUN = median, na.rm=TRUE)
ggplot(sub_s3, aes(x=YYYYMMDD, y=x)) + geom_line() + geom_smooth() + 
  labs(title = "시계열 : 2016~2019년간 경기도의 월세 보증금 가격", x="월", y="가격(만 원)")



# 해당 도시와 거래날짜 및 건축일자에 관련된 가격 : 박스플롯, 타일그래프

head(all)
str(all)

# 전체
sub1 <- aggregate(all$deposit,
                  by = list(YYYY = as.character(all$yyyy),
                            Si = as.character(all$si),
                            AptType = as.character(all$aptType)),
                  FUN = median, na.rm=TRUE)

# tapply(sub1$x, list(sub1$YYYY, sub1$AptType), median)

ggplot(sub1, aes(AptType, x)) + geom_boxplot(aes(fill=AptType)) + scale_fill_brewer(palette="Dark2") +
  labs(title = "Boxplot : 아파트유형에 따른 가격 수준", x="유형", y="금액(만 단위)")
ggplot(sub1, aes(YYYY, x)) + geom_boxplot(aes(fill=YYYY)) + scale_fill_brewer(palette="Dark2") +
  labs(title = "Boxplot : 연도에 따른 가격 수준", x="연도", y="금액(만 단위)")
ggplot(sub1, aes(Si, x)) + geom_boxplot(aes(fill=Si)) +
  labs(title = "Boxplot : 도시에 따른 가격 수준", x="도시", y="금액(만 단위)") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1)) 
ggplot(sub1, aes(x=AptType, y=Si)) + geom_tile(aes(fill=x)) + facet_wrap(~YYYY) + 
  scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "타일차트 : 각 연도간 도시의 아파트유형에 따른 가격(중앙값 기준, 만 단위)", x="유형", y="도시") +
  theme(text = element_text(size=9))
  


sub1_1 <- aggregate(all$deposit,
                  by = list(Build = as.numeric(all$build),
                            YYYY = as.character(all$yyyy),
                            Si = as.character(all$si)),
                  FUN = median, na.rm=TRUE)

# tapply(sub1_1$x, list(sub1_1$Build, sub1_1$YYYY ,sub1_1$Si), median)

ggplot(sub1_1, aes(x=as.factor(Build), y=Si)) + geom_tile(aes(fill=x)) + facet_wrap(~YYYY) + 
  scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "타일차트 : 2016~2019년간 건축연도에 따른 도시의 전세 및 월세 보증금 가격", x='가격은 중앙값 기준, 만 단위', y="도시") +
  theme(text = element_text(size=7), axis.text.x = element_text(angle=90, hjust=1))




sub1_2 <- aggregate(all$deposit,
                  by = list(YYYY = as.character(all$yyyy),
                            Si = as.character(all$si),
                            PriceType = as.character(all$priceType)),
                  FUN = median, na.rm=TRUE)

# tapply(sub1_2$x, list(sub1_2$YYYY, sub1_2$PriceType), median)

ggplot(sub1_2, aes(x=PriceType, y=Si)) + geom_tile(aes(fill=x)) + facet_wrap(~YYYY) + 
  scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "타일차트 : 각 연도간 도시의 전·월세 유형에 따른 전세 및 월세 보증금 가격(중앙값 기준, 만 단위)", x="유형", y="도시") +
  theme(text = element_text(size=9))



# 월세

sub2 <- aggregate(df1$price,
                  by = list(YYYY = as.character(df1$yyyy),
                            Si = as.character(df1$si),
                            AptType = as.character(df1$aptType)),
                  FUN = median, na.rm=TRUE)

# tapply(sub2$x, list(sub2$YYYY, sub2$Si), median)

ggplot(sub2, aes(YYYY, x)) + geom_boxplot(aes(fill=YYYY)) + scale_fill_brewer(palette="Dark2") +
  labs(title = "Boxplot : 연도에 따른 월세 수준", x="연도", y="금액(만 단위)")
ggplot(sub2, aes(AptType, x)) + geom_boxplot(aes(fill=AptType)) +
  labs(title = "Boxplot : 아파트유형에 따른 월세 수준", x="도시", y="금액(만 단위)") 
ggplot(sub2, aes(Si, x)) + geom_boxplot(aes(fill=Si)) +
  labs(title = "Boxplot : 도시에 따른 월세 수준", x="도시", y="금액(만 단위)") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1))
ggplot(sub2, aes(x=AptType, y=Si)) + geom_tile(aes(fill=x)) + facet_wrap(~YYYY) + 
  scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) + 
  labs(title = "타일차트 : 각 연도간 도시의 월세 가격(중앙값 기준, 만 단위)", x="아파트유형", y="도시")


# 전세

sub3 <- aggregate(df2$deposit,
                  by = list(YYYY = as.character(df2$yyyy),
                            Si = as.character(df2$si),
                            AptType = as.character(df2$aptType)),
                  FUN = median, na.rm=TRUE)

# tapply(sub3$x, list(sub3$YYYY, sub3$Si), median)

ggplot(sub3, aes(YYYY, x)) + geom_boxplot(aes(fill=YYYY)) + scale_fill_brewer(palette="Dark2") +
  labs(title = "Boxplot : 연도에 따른 전세 수준", x="연도", y="금액(만 단위)")
ggplot(sub3, aes(AptType, x)) + geom_boxplot(aes(fill=AptType)) +
  labs(title = "Boxplot : 아파트유형에 따른 전세 수준", x="도시", y="금액(만 단위)") 
ggplot(sub3, aes(Si, x)) + geom_boxplot(aes(fill=Si)) +
  labs(title = "Boxplot : 도시에 따른 전세 수준", x="도시", y="금액(만 단위)") +
  theme(text = element_text(size=15), axis.text.x = element_text(angle=90, hjust=1))
ggplot(sub3, aes(x=AptType, y=Si)) + geom_tile(aes(fill=x)) + facet_wrap(~YYYY) + 
  scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) + 
  labs(title = "타일차트 : 각 연도간 도시의 전세 가격(중앙값 기준, 만 단위)", x="아파트유형", y="도시")


str(apt)




# 산점도 : 코드 실행에 시간이 2분 정도 걸림
str(df1)

par(mfrow=c(1,1))

# 산점도 : 보증금과 월세 간의 관계
g <- ggplot(df1, aes(x=deposit, y=price, color=as.factor(mm))) + geom_point() + geom_smooth() + 
  labs(title = "산점도 : 2015~2018년간 보증금에 따른 월세 가격의 월 변화", x='보증금 가격(중앙값 기준, 만 단위)', y="가격(중앙값 기준, 만 단위)")
ggMarginal(g, groupColor=TRUE, groupFill=TRUE)



# 산점도 : 층 또는 면적과 월세/월세보증금/전세 간의 관계

#월세
g <- ggplot(df1, aes(x=floor, y=price, color=as.factor(mm))) + geom_point() + geom_smooth() +
  labs(title = "산점도 : 2016~2019년간 층에 따른 월세 가격의 월 변화", x='층', y="가격(중앙값 기준, 만 단위)")
ggMarginal(g, groupColor=TRUE, groupFill=TRUE)

g <- ggplot(df1, aes(x=area, y=price, color=as.factor(mm))) + geom_point() + geom_smooth() +
  labs(title = "산점도 : 2016~2019년간 면적에 따른 월세 가격의 월 변화", x='면적', y="가격(중앙값 기준, 만 단위)")
ggMarginal(g, groupColor=TRUE, groupFill=TRUE)

#월세보증금
g <- ggplot(df1, aes(x=floor, y=deposit, color=as.factor(mm))) + geom_point() + geom_smooth() +
  labs(title = "산점도 : 2016~2019년간 층에 따른 월세 보증금의 월 변화", x='층', y="가격(중앙값 기준, 만 단위)")
ggMarginal(g, groupColor=TRUE, groupFill=TRUE)

g <- ggplot(df1, aes(x=area, y=deposit, color=as.factor(mm))) + geom_point() + geom_smooth() +
  labs(title = "산점도 : 2016~2019년간 면적에 따른 월세 보증금의 월 변화", x='면적', y="가격(중앙값 기준, 만 단위)")
ggMarginal(g, groupColor=TRUE, groupFill=TRUE)

#전세
g <- ggplot(df2, aes(x=floor, y=deposit, color=as.factor(mm))) + geom_point() + geom_smooth() +
  labs(title = "산점도 : 2016~2019년간 층에 따른 전세 가격의 월 변화", x='층', y="가격(중앙값 기준, 만 단위)")
ggMarginal(g, groupColor=TRUE, groupFill=TRUE)

g <- ggplot(df2, aes(x=area, y=deposit, color=as.factor(mm))) + geom_point() + geom_smooth() +
  labs(title = "산점도 : 2016~2019년간 면적에 따른 전세 가격의 월 변화", x='면적', y="가격(중앙값 기준, 만 단위)")
ggMarginal(g, groupColor=TRUE, groupFill=TRUE)


# 월세보증금과 월세와의 관계
g <- ggplot(df1, aes(x=deposit, y=price, color=as.factor(priceType))) + geom_point() + geom_smooth() +
  labs(title = "산점도 : 2016~2019년간 가격유형에 따른 변화", x='보증금', y="가격(중앙값 기준, 만 단위)")
ggMarginal(g, groupColor=TRUE, groupFill=TRUE)

g <- ggplot(df1, aes(x=deposit, y=price, color=as.factor(aptType))) + geom_point() + geom_smooth() +
  labs(title = "산점도 : 2016~2019년간 아파트유형에 따른 변화", x='보증금', y="가격(중앙값 기준, 만 단위)")
ggMarginal(g, groupColor=TRUE, groupFill=TRUE)



# 상관관계그래프 : 층, 면적, 보증금(전세가격), 월세 가격 간의 관계
df1_naomit <- na.omit(df1)
df2_naomit <- na.omit(df2)

corr <- round(cor(subset(df1_naomit, select=c(deposit, price, area, floor))), 3)
ggcorrplot(corr, hc.order=TRUE, type="lower", lab=TRUE, lab_size=3,
           method="circle", color=c("tomato2", "white", "springgreen3"), title="Correlation : 월세 데이터의 상관관계")

corr <- round(cor(subset(df2_naomit, select=c(deposit, area, floor))), 3)
ggcorrplot(corr, hc.order=TRUE, type="lower", lab=TRUE, lab_size=3,
           method="circle", color=c("tomato2", "white", "springgreen3"), title="Correlation : 전세 데이터의 상관관계")




# 지도

######코드#######
getwd()
install.packages(c("sp","purrr","dplyr"))
library(sp)
library(sf)
library(purrr)
library(dplyr)

korea_sig <-st_read(path_shp)
korea_sig $ SIG_KOR_NM <-iconv (korea_sig$SIG_KOR_NM, localeToCharset(), "UTF-8")
str(korea_sig)

df <- korea_sig[,c(1,3)]
head(df$geometry)

unique(sub1$Si)
unique(korea_sig$SIG_KOR_NM)

df[df$SIG_KOR_NM == '고양시덕양구', 2] = '고양덕양구'
df[df$SIG_KOR_NM == '고양시일산동구', 2] = '고양일산동구'
df[df$SIG_KOR_NM == '고양시일산서구', 2] = '고양일산서구'

df[df$SIG_KOR_NM == '성남시 분당구', 2] = '성남분당구'
df[df$SIG_KOR_NM == '성남시 수정구', 2] = '성남수정구'
df[df$SIG_KOR_NM == '성남시 중원구', 2] = '성남중원구'

df[df$SIG_KOR_NM == '수원시 권선구', 2] = '수원권선구'
df[df$SIG_KOR_NM == '수원시 영통구', 2] = '수원영통구'
df[df$SIG_KOR_NM == '수원시 장안구', 2] = '수원장안구'
df[df$SIG_KOR_NM == '수원시 팔달구', 2] = '수원팔달구'

df[df$SIG_KOR_NM == '안산시단원구', 2] = '안산단원구'
df[df$SIG_KOR_NM == '안산시상록구', 2] = '안산상록구'

df[df$SIG_KOR_NM == '안양시동안구', 2] = '안양동안구'
df[df$SIG_KOR_NM == '안양시만안구', 2] = '안양만안구'

df[df$SIG_KOR_NM == '용인시기흥구', 2] = '용인기흥구'
df[df$SIG_KOR_NM == '용인시수지구', 2] = '용인수지구'
df[df$SIG_KOR_NM == '용인시처인구', 2] = '용인처인구'

colnames(df) <- c("SIG_CD", "Si", "geometry")
str(df)
unique(df$Si)


# 월세 보증금
s_2016 <- subset(df1, yyyy=='2016')
s_2017 <- subset(df1, yyyy=='2017')
s_2018 <- subset(df1, yyyy=='2018')
s_2019 <- subset(df1, yyyy=='2019')

str(s_2016)

subgeo2016_de <- aggregate(s_2016$deposit,
                        by = list(Si = as.character(s_2016$si)),
                        FUN = median, na.rm=TRUE)
subgeo2017_de <- aggregate(s_2017$deposit,
                        by = list(Si = as.character(s_2017$si)),
                        FUN = median, na.rm=TRUE)
subgeo2018_de <- aggregate(s_2018$deposit,
                        by = list(Si = as.character(s_2018$si)),
                        FUN = median, na.rm=TRUE)
subgeo2019_de <- aggregate(s_2019$deposit,
                        by = list(Si = as.character(s_2019$si)),
                        FUN = median, na.rm=TRUE)

subgeo2016_de <- merge(x=subgeo2016_de, y = df, by='Si')
subgeo2017_de <- merge(x=subgeo2017_de, y = df, by='Si')
subgeo2018_de <- merge(x=subgeo2018_de, y = df, by='Si')
subgeo2019_de <- merge(x=subgeo2019_de, y = df, by='Si')


subgeo2016_de <- subgeo2016_de[,c(3,1,4,2)]
subgeo2016_de <- subgeo2016_de[order(subgeo2016_de$SIG_CD) ,]

subgeo2017_de <- subgeo2017_de[,c(3,1,4,2)]
subgeo2017_de <- subgeo2017_de[order(subgeo2017_de$SIG_CD) ,]

subgeo2018_de <- subgeo2018_de[,c(3,1,4,2)]
subgeo2018_de <- subgeo2018_de[order(subgeo2018_de$SIG_CD) ,]

subgeo2019_de <- subgeo2019_de[,c(3,1,4,2)]
subgeo2019_de <- subgeo2019_de[order(subgeo2019_de$SIG_CD) ,]


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index=subgeo2016_de$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2016년 각 시의 월세 보증금 중앙 값")


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2017_de$x)  -> tdf
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2017년 각 시의 월세 보증금 중앙 값") 


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2018_de$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2018년 각 시의 월세 보증금 중앙 값") 


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2019_de$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2019년 각 시의 월세 보증금 중앙 값")  



#### 월세

subgeo2016_pr <- aggregate(s_2016$price,
                           by = list(Si = as.character(s_2016$si)),
                           FUN = median, na.rm=TRUE)
subgeo2017_pr <- aggregate(s_2017$price,
                           by = list(Si = as.character(s_2017$si)),
                           FUN = median, na.rm=TRUE)
subgeo2018_pr <- aggregate(s_2018$price,
                           by = list(Si = as.character(s_2018$si)),
                           FUN = median, na.rm=TRUE)
subgeo2019_pr <- aggregate(s_2019$price,
                           by = list(Si = as.character(s_2019$si)),
                           FUN = median, na.rm=TRUE)

subgeo2016_pr <- merge(x=subgeo2016_pr, y = df, by='Si')
subgeo2017_pr <- merge(x=subgeo2017_pr, y = df, by='Si')
subgeo2018_pr <- merge(x=subgeo2018_pr, y = df, by='Si')
subgeo2019_pr <- merge(x=subgeo2019_pr, y = df, by='Si')


subgeo2016_pr <- subgeo2016_pr[,c(3,1,4,2)]
subgeo2016_pr <- subgeo2016_pr[order(subgeo2016_pr$SIG_CD) ,]

subgeo2017_pr <- subgeo2017_pr[,c(3,1,4,2)]
subgeo2017_pr <- subgeo2017_pr[order(subgeo2017_pr$SIG_CD) ,]

subgeo2018_pr <- subgeo2018_pr[,c(3,1,4,2)]
subgeo2018_pr <- subgeo2018_pr[order(subgeo2018_pr$SIG_CD) ,]

subgeo2019_pr <- subgeo2019_pr[,c(3,1,4,2)]
subgeo2019_pr <- subgeo2019_pr[order(subgeo2019_pr$SIG_CD) ,]


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2016_pr$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2016년 각 시의 월세가격 중앙 값") 


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2017_pr$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2017년 각 시의 월세가격 중앙 값")


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2018_pr$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2018년 각 시의 월세가격 중앙 값")


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2019_pr$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2019년 각 시의 월세가격 중앙 값")



#### 전세

s_2016 <- subset(df2, yyyy=='2016')
s_2017 <- subset(df2, yyyy=='2017')
s_2018 <- subset(df2, yyyy=='2018')
s_2019 <- subset(df2, yyyy=='2019')

subgeo2016_de2 <- aggregate(s_2016$deposit,
                           by = list(Si = as.character(s_2016$si)),
                           FUN = median, na.rm=TRUE)
subgeo2017_de2 <- aggregate(s_2017$deposit,
                           by = list(Si = as.character(s_2017$si)),
                           FUN = median, na.rm=TRUE)
subgeo2018_de2 <- aggregate(s_2018$deposit,
                           by = list(Si = as.character(s_2018$si)),
                           FUN = median, na.rm=TRUE)
subgeo2019_de2 <- aggregate(s_2019$deposit,
                           by = list(Si = as.character(s_2019$si)),
                           FUN = median, na.rm=TRUE)

subgeo2016_de2 <- merge(x=subgeo2016_de2, y = df, by='Si')
subgeo2017_de2 <- merge(x=subgeo2017_de2, y = df, by='Si')
subgeo2018_de2 <- merge(x=subgeo2018_de2, y = df, by='Si')
subgeo2019_de2 <- merge(x=subgeo2019_de2, y = df, by='Si')


subgeo2016_de2 <- subgeo2016_de2[,c(3,1,4,2)]
subgeo2016_de2 <- subgeo2016_de2[order(subgeo2016_de2$SIG_CD) ,]

subgeo2017_de2 <- subgeo2017_de2[,c(3,1,4,2)]
subgeo2017_de2 <- subgeo2017_de2[order(subgeo2017_de2$SIG_CD) ,]

subgeo2018_de2 <- subgeo2018_de2[,c(3,1,4,2)]
subgeo2018_de2 <- subgeo2018_de2[order(subgeo2018_de2$SIG_CD) ,]

subgeo2019_de2 <- subgeo2019_de2[,c(3,1,4,2)]
subgeo2019_de2 <- subgeo2019_de2[order(subgeo2019_de2$SIG_CD) ,]


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2016_de2$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd"))+
  labs(title = "2016년 각 시의 전세가격 중앙 값")


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2017_de2$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2017년 각 시의 전세가격 중앙 값")


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2018_de2$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2018년 각 시의 전세가격 중앙 값") 


df %>% 
  filter(substr(SIG_CD, start = 1, stop = 2) == "41") %>% 
  mutate(index = subgeo2019_de2$x)  -> tdf 
ggplot(tdf) + 
  geom_sf(aes(fill = index)) + scale_fill_gradientn(colors=brewer.pal(n=9, name="YlOrRd")) +
  labs(title = "2019년 각 시의 전세가격 중앙 값")


# Code End

