library(readr)
library(dplyr)
library(reshape2)
library(psych)

setwd("C:/Users/Seri/Desktop/dpdp")

main_store <- read.table(file = 'main_store4.csv', header = TRUE, sep = ',')
sinchon_store <- read.table(file = 'sinchon_store4.csv', header = TRUE, sep = ',')
cheonho_store <- read.table(file = 'cheonho_store4.csv', header = TRUE, sep = ',')
trade_store <- read.table(file = 'trade_store4.csv', header = TRUE, sep = ',')
department <- read.table(file = 'department1.csv', header = TRUE, sep = ',')
department <- read.table(file = 'department.csv', header = TRUE, sep = ',')
department

#  ID 별 환불횟수 컬럼 만들기
department$re_countone <-ifelse(department$refund>0, 1, 0)
recount <- dcast(department, custid ~ . ,value.var="re_countone", sum)
recount
# 환불횟수 컬럼 cheonho_store에 붙이기
department <- merge.data.frame(department, recount,by= 'custid', all=TRUE)
names(department)[22]<-c('re_count')

department <- department[ ,-21] #re_countone컬럼삭제

#  ID 별 구매횟수 컬럼 만들기
department$a_count <-ifelse(department$amount>0, 1, 0)
acount <- dcast(department, custid ~ . ,value.var="a_count", sum)
acount
# 구매횟수 컬럼 department에 붙이기
department <- merge.data.frame(department, acount,by= 'custid', all=TRUE)
names(department)[23]<-c('amount_count')
department<- department[ ,-22] #re_countone컬럼삭제

department$satisfaction <- department$amount_count - department$re_count
department$satis_cl <- ifelse(department$satisfaction>=-1&department$satisfaction<24,1,
                              ifelse(department$satisfaction>=24&department$satisfaction<46,2,
                                     ifelse(department$satisfaction>=46&department$satisfaction<83,3,
                                            ifelse(department$satisfaction>=83&department$satisfaction<=527,4,NA))))

department[is.na(department)]<-1
cor(department$amount,department$satis_cl)
summary(department)
department[is.na(department)] <-1

###################################################################################

department$sum_amount <- sum(dpartment$amount)

dep <- table(department$custid,department$satis_cl)
depart %>% group
dep$sum_amount <- sum(department$amount)

##########################################사용자ID와 만족도 컬럼 추출 및 특성파악

#지점통합 만족도와 매출 상관도
dept_cor <- select(department,amount_count,re_count,satis_cl)
dept_corr<-select(department,amount,satis_cl)

# 결측치 0값 처리
summary(dept_cor)
dept_cor[is.na(dept_cor)] <- 1
dept_cor %>% is.na %>% sum
cor(dept_cor)
plot(dept_cor)


#####################################본점

main <- select(main_store,custid,satis_cl)
# 결측치 0값 처리
main[is.na(main)] <- 1
main %>% is.na %>% sum
summary(main$satis_cl)

#본점 빈도분석
main_freq <- table(main$satis_cl)
main_freq
barplot(main_freq, main="본점 만족도 그래프", xlab="만족도",ylab="순위" )
barplot(main_freq-120000, main="본점 만족도 그래프", xlab="만족도",ylab="순위" )



#####################################신촌점

sinchon <- select(sinchon_store,custid,satis_cl)
# 결측치 0값 처리
sinchon[is.na(sinchon)] <- 1
sinchon %>% is.na %>% sum
summary(sinchon$satis_cl)

#신촌점 빈도분석
sinchon_freq <- table(sinchon$satis_cl)
sinchon_freq 
barplot(sinchon_freq, main="신촌점 만족도 그래프", xlab="만족도",ylab="순위" )
barplot(sinchon_freq-90000, main="신촌점 만족도 그래프", xlab="만족도",ylab="순위" )

#####################################무역점

trade <- select(trade_store,custid, satis_cl)
# 결측치 0값 처리
trade[is.na(sinchon)] <- 1
trade %>% is.na %>% sum
summary(trade$satis_cl)

#무역점 빈도분석
trade_freq <-table(trade_store$satis_cl)
trade_freq 
barplot(trade_freq, main="무역점 만족도 그래프", xlab="만족도",ylab="순위" )
barplot(trade_freq-110000, main="무역점 만족도 그래프", xlab="만족도",ylab="순위" )

#####################################천호점

cheonho <- select(cheonho_store,custid, satis_cl)
# 결측치 0값 처리
cheonho[is.na(cheonho)] <- 1
cheonho %>% is.na %>% sum
summary(cheonho$satis_cl)

#천호점 빈도분석
cheonho_freq <- table(cheonho_store$satis_cl)
cheonho_freq 
barplot(cheonho_freq, main="천호점 만족도 그래프", xlab="만족도",ylab="순위" )
barplot(cheonho_freq-80000, main="천호점 만족도 그래프", xlab="만족도",ylab="순위" )
