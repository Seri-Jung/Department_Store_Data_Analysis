library(readr)
library(dplyr)
#install.packages(c('psych', 'Hmisc', 'skimr'))
#install.packages('dplyr')

setwd("C:/Users/Seri/Desktop/department_data")
#C:/Users/Seri/Desktop/department_data

############################################################################
#데이터 합치기
x_test <- read.table(file = 'x_test.csv', 
                 header = TRUE, sep = ',')

x_train <- read.table(file = 'x_train.csv', 
                     header = TRUE, sep = ',')

xxx <- rbind(x_test, x_train)
yyy <- arrange(xxx,custid)

dim(xxx)
View(xxx)


y_test <- read.table(file = 'y_test.csv', 
                      header = TRUE, sep = ',')

y_train <- read.table(file = 'y_train.csv', 
              header = TRUE, sep = ',')

yyy <- rbind(y_test, y_train)
yyy <- arrange(yyy,custid)
dim(yyy)
View(yyy)

department <- merge(xxx, yyy, by= 'custid', all=TRUE)
head(department)
dim(department)
View(department)
write.csv(department, file="department_merge.csv", row.names = FALSE)
str(department)
summary(department)

length(unique(department$product))
length(unique(department$brand))
length(unique(department$corner))
length(unique(department$pc))
length(unique(department$part))
department$part

########### 기술통계분석 패키지 ###############################################

library(psych)
# 각 변수 컬럼별 여러가지 기술통계량을 세부적으로 비교해줌.
psych::describe(department)

# 명목척도, 서열척도, 등간척도, 비율척도
library(Hmisc)
# Hmisc의 describe : 각 변수 컬럼별 단위로 기술통계량을 상세하게 제공함.
Hmisc::describe(department)

library(skimr)
# 각 변수를 측정척도별로 분리해 기술통계량과 히스토그램을 나타내줌.
skimr::describe(department)

###############################################################################

############범주형 변수컬럼 서브데이터셋 추출

# 범주형 변수컬럼명 파악
ctg_names <- c('custid','date_time','store','product','brand','corner','pc','part','imported','amount','discount','installment','gender')

# 번주형 변수컬럼 데이터셋 추출
ctg <- department[ctg_names]
head(ctg)

# 내부구조 조회
str(ctg)

# 기본 기술통계량 파악
summary(ctg)

################################################################################

# 할인액 
department$discount <- ifelse(department$discount< 0 , 0, department$discount)
department

# 할인율 = 할인액/구매액 * 100
department <- department %>% mutate((discount/amount)*100)
department
names(department) <- c('custid','date_time','store','product','brand','corner','pc','part','imported','amount','discount','installment','gender','dis_rate')
department$dis_rate <- ifelse(department$discount< 0, 0, department$dis_rate)

#브랜드 개수
main_freq <- table(main_store$brand)

