library(readr)
library(dplyr)
library(reshape2)
library(psych)
library(gridExtra)
install.packages('gridExtra')


setwd("C:/Users/Seri/Desktop/dpdp")

main_store <- read.table(file = 'main_store4.csv', header = TRUE, sep = ',')
sinchon_store <- read.table(file = 'sinchon_store4.csv', header = TRUE, sep = ',')
cheonho_store <- read.table(file = 'cheonho_store4.csv', header = TRUE, sep = ',')
trade_store <- read.table(file = 'trade_store4.csv', header = TRUE, sep = ',')
department <- read.table(file = 'department4.csv', header = TRUE, sep = ',')
department
write.csv(trade_store, file="department4.csv", row.names = FALSE)


###################################################################################

satisf <- department %>% group_by(custid) %>% summarise(total_amount = sum(amount),satisfaction = mean(satisfaction))
satisf[is.na(satisf)]<-1
cor(satisf$total_amount,satisf$satisfaction)
plot(total_amount ~ satisfaction, data = satisf,
     xlab = "매출액", 
     ylab = "만족도")


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
main_store
main <- select(main_store,custid,amount,satisfaction,satis_cl)
main <- main %>% group_by(custid) %>% summarise(total_amount = sum(amount),satisfaction = mean(satisfaction))
# 결측치 0값 처리
main[is.na(main)] <- 1
main %>% is.na %>% sum
summary(main)

#본점 빈도분석
main_freq <- table(main$satis_cl)
length(main_freq)
barplot(main_freq, main="본점 만족도 그래프", xlab="만족도",ylab="순위" )
barplot(main_freq-120000, main="본점 만족도 그래프", xlab="만족도",ylab="순위" )

# 상관도 분석
cor(main$satisfaction,main$total_amount)

#####################################신촌점

sinchon <- select(sinchon_store,custid,amount,satisfaction,satis_cl)
sinchon <- sinchon %>% group_by(custid) %>% summarise(total_amount = sum(amount),satisfaction = mean(satisfaction))
# 결측치 0값 처리
sinchon[is.na(sinchon)] <- 1
sinchon %>% is.na %>% sum
summary(sinchon)

#신촌점 빈도분석
sinchon_freq <- table(sinchon$satis_cl)
sinchon_freq 
barplot(sinchon_freq, main="신촌점 만족도 그래프", xlab="만족도",ylab="순위" )
barplot(sinchon_freq-90000, main="신촌점 만족도 그래프", xlab="만족도",ylab="순위" )

#상관도 분석
cor(sinchon$satisfaction,sinchon$total_amount)

#####################################무역점

trade <- select(trade_store,custid,amount,satisfaction,satis_cl)
trade <- trade %>% group_by(custid) %>% summarise(total_amount = sum(amount),satisfaction = mean(satisfaction))
# 결측치 0값 처리
trade[is.na(sinchon)] <- 1
trade %>% is.na %>% sum
summary(trade)

#무역점 빈도분석
trade_freq <-table(trade_store$satis_cl)
trade_freq 
barplot(trade_freq, main="무역점 만족도 그래프", xlab="만족도",ylab="순위" )
barplot(trade_freq-110000, main="무역점 만족도 그래프", xlab="만족도",ylab="순위" )

#본점 상관도 분석
cor(trade$satisfaction,trade$total_amount)

#####################################천호점
cheonho_store
cheonho <- select(cheonho_store,custid,amount,satisfaction,satis_cl)
cheonho <- cheonho %>% group_by(custid) %>% summarise(total_amount = sum(amount),satisfaction = mean(satisfaction))
# 결측치 0값 처리
cheonho[is.na(cheonho)] <- 1
cheonho %>% is.na %>% sum
summary(cheonho)

#천호점 빈도분석
cheonho_freq <- table(cheonho_store$satis_cl)
cheonho_freq 
barplot(cheonho_freq, main="천호점 만족도 그래프", xlab="만족도",ylab="순위" )
barplot(cheonho_freq-80000, main="천호점 만족도 그래프", xlab="만족도",ylab="순위" )

#상관도 분석
cor(cheonho$satisfaction,cheonho$total_amount)



##############################################3

#브랜드 개수
main <- select(main_store,custid,brand,satisfaction,satis_cl)
main <- main %>% group_by(custid) %>% summarise(brand_count = length(brand),satisfaction = mean(satisfaction))


main_freq <- table(main$satis_cl)
main_freq
main_brand_freq <- table(main$brand)
length(main_brand_freq)/25

#본점 최상위 만족도 비율
124724/(121090+126178+124510+124724)*100
#신촌점 최상위 만족도 비율
104878/(99397+102719+106852+104878)*100
#무역점 최상위 만족도 비율
120792/(119220+117433+120759+120792)*100
#천호점 최상위 만족도 비율
86527/(84087+82751+84569+86527)*100

#본점 3, 4점 만족도 비율
(124510+124724)/(121090+126178+124510+124724)*100
#신촌점 3, 4점 만족도 비율
(106852+104878)/(99397+102719+106852+104878)*100
#무역점 3, 4점 만족도 비율
(120759+120792)/(119220+117433+120759+120792)*100
#천호점 3, 4점 만족도 비율
(84569+86527)/(84087+82751+84569+86527)*100

# 브랜드개수 대비 최상위 만족도(4점) 
(889/25.12) 
(1008/25.34) 
(1045/25.25) 
(1101/25.60) 

grid.arrange(a,b,c,d, nrow=, ncol=)


