library(readr)
library(dplyr)
library(reshape2)
install.packages('reshape2')

########### 지점별 만족도 변수 넣기
#파일 불러오기

setwd("C:/Users/Seri/Desktop/departmentData")

###########################################본점
main_store <- read.table(file = 'main_store.csv', 
                          header = TRUE, sep = ',')
dim(main_store)
main_store
#  ID 별 환불횟수 컬럼 만들기
main_store$re_countone <-ifelse(main_store$refund>0, 1, 0)
recount <- dcast(main_store, custid ~ . ,value.var="re_countone", sum)
recount
# 환불횟수 컬럼 main_store에 붙이기
main_store <- merge.data.frame(main_store, recount,by= 'custid', all=TRUE)
names(main_store)[22]<-c('re_count')

main_store <- main_store[ ,-21] 

#  ID 별 구매횟수 컬럼 만들기
main_store$amount_count <-ifelse(main_store$amount>0, 1, 0)
acount <- dcast(main_store, custid ~ . ,value.var="amount_count", sum)
acount
# 구매횟수 컬럼 main_store에 붙이기
main_store <- merge.data.frame(main_store, acount,by= 'custid', all=TRUE)
names(main_store)[23]<-c('amount_count')
main_store<- main_store[ ,-22] #re_countone컬럼삭제

main_store$satisfaction <- main_store$amount_count - main_store$re_count
main_store<- main_store[ ,-22] # 구매횟수 컬럼 삭제
main_store<- main_store[ ,-21] # 환불횟수 컬럼 삭제


#########################################신촌점

sinchon_store <- read.table(file = 'sinchon_store.csv', 
                         header = TRUE, sep = ',')
dim(sinchon_store)
sinchon_store

#  ID 별 환불횟수 컬럼 만들기
sinchon_store$re_countone <-ifelse(sinchon_store$refund>0, 1, 0)
recount <- dcast(sinchon_store, custid ~ . ,value.var="re_countone", sum)
recount
# 환불횟수 컬럼 sinchon_store에 붙이기
sinchon_store <- merge.data.frame(sinchon_store, recount,by= 'custid', all=TRUE)
names(sinchon_store)[22]<-c('re_count')

sinchon_store <- sinchon_store[ ,-21] #re_countone컬럼삭제

#  ID 별 구매횟수 컬럼 만들기
sinchon_store$amount_count <-ifelse(sinchon_store$amount>0, 1, 0)
acount <- dcast(sinchon_store, custid ~ . ,value.var="amount_count", sum)
acount
# 구매횟수 컬럼 sinchon_store에 붙이기
sinchon_store <- merge.data.frame(sinchon_store, acount,by= 'custid', all=TRUE)
names(sinchon_store)[23]<-c('amount_count')
sinchon_store<- sinchon_store[ ,-22] #re_countone컬럼삭제

sinchon_store$satisfaction <- sinchon_store$amount_count - sinchon_store$re_count
sinchon_store
sinchon_store<- main_store[ ,-22] # 구매횟수 컬럼 삭제
sinchon_store<- main_store[ ,-21] # 환불횟수 컬럼 삭제

##########################################무역점

trade_store <- read.table(file = 'trade_store.csv', 
                         header = TRUE, sep = ',')
dim(trade_store)
trade_store
#  ID 별 환불횟수 컬럼 만들기
trade_store$re_countone <-ifelse(trade_store$refund>0, 1, 0)
recount <- dcast(trade_store, custid ~ . ,value.var="re_countone", sum)
recount
# 환불횟수 컬럼 trade_store에 붙이기
trade_store <- merge.data.frame(trade_store, recount,by= 'custid', all=TRUE)
names(trade_store)[22]<-c('re_count')

trade_store <- trade_store[ ,-21] #re_countone컬럼삭제

#  ID 별 구매횟수 컬럼 만들기
trade_store$amount_count <-ifelse(trade_store$amount>0, 1, 0)
acount <- dcast(trade_store, custid ~ . ,value.var="amount_count", sum)
acount
# 구매횟수 컬럼 trade_store에 붙이기
trade_store <- merge.data.frame(trade_store, acount,by= 'custid', all=TRUE)
names(trade_store)[23]<-c('amount_count')
trade_store<- trade_store[ ,-22] #re_countone컬럼삭제

trade_store$satisfaction <- trade_store$amount_count - trade_store$re_count
trade_store
trade_store<- main_store[ ,-22] # 구매횟수 컬럼 삭제
trade_store<- main_store[ ,-21] # 환불횟수 컬럼 삭제


#########################################천호점
cheonho_store <- read.table(file = 'cheonho_store.csv', 
                            header = TRUE, sep = ',')
dim(cheonho_store)
cheonho_store

#  ID 별 환불횟수 컬럼 만들기
cheonho_store$re_countone <-ifelse(cheonho_store$refund>0, 1, 0)
recount <- dcast(cheonho_store, custid ~ . ,value.var="re_countone", sum)
recount
# 환불횟수 컬럼 cheonho_store에 붙이기
cheonho_store <- merge.data.frame(cheonho_store, recount,by= 'custid', all=TRUE)
names(cheonho_store)[22]<-c('re_count')

cheonho_store <- cheonho_store[ ,-21] #re_countone컬럼삭제

#  ID 별 구매횟수 컬럼 만들기
cheonho_store$amount_count <-ifelse(cheonho_store$amount>0, 1, 0)
acount <- dcast(cheonho_store, custid ~ . ,value.var="amount_count", sum)
acount
# 구매횟수 컬럼 cheonho_store에 붙이기
cheonho_store <- merge.data.frame(cheonho_store, acount,by= 'custid', all=TRUE)
names(cheonho_store)[23]<-c('amount_count')
cheonho_store<- cheonho_store[ ,-22] #re_countone컬럼삭제

cheonho_store$satisfaction <- cheonho_store$amount_count - cheonho_store$re_count
cheonho_store
cheonho_store<- main_store[ ,-22] # 구매횟수 컬럼 삭제
cheonho_store<- main_store[ ,-21] # 환불횟수 컬럼 삭제

write.csv(main_store, file="main_store.csv", row.names = FALSE)
write.csv(sinchon_store, file="sinchon_store.csv", row.names = FALSE)
write.csv(trade_store, file="trade_store.csv", row.names = FALSE)
write.csv(cheonho_store, file="cheonho_store.csv", row.names = FALSE)
