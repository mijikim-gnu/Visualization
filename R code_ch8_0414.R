################################################################################
# 의학통계학(2021학년도 1학기)
# 8. 데이터 수집 및 정리(실습용 R 코드)
# 2021-04-14
################################################################################


################################################################################
# R basics rules, R data structure
################################################################################

# 명령어 실행: Ctrl+Enter 또는 Run 클릭

# 대소문자를 구별함
a <- c(1, 2, 3, 4, 5)
A <- c(6, 8, 10)

# 변수명에 “-”이나 “ (space)”를 포함할 수 없음
install.packages("moonBook")
library(moonBook)
data(acs)

# 큰 따옴표(“ ”) 안의 내용은 문자로 인식함
number <- c(1, 2, 3)
character <- c("1", "2", "3")
str(number)
str(character)

# 패키지를 한 번 설치하면 다시 설치할 필요 없이 “library” 또는 “require” 함수로 불러오면 됨
library(moonBook)
require(moonBook)
# 또는 우측 plot 창에서 "packages"에서 library 클릭

# 함수에 대해 궁금하다면 “?함수이름“ 또는 “help(함수이름)”으로 검색이 가능함
?hist
hist(acs$age)

# “=“ 또는 “<-”으로 데이터를 입력(할당)할 수 있음
a <- c(1, 2, 3, 4, 5)
b = c(6, 7, 8, 9, 10)

# 경로에 있는 “＼”는 “\\”나 “/”로 바꿔야 함
setwd("C:\Users\user\Desktop\(1학기)의학통계학\statistics")
setwd("C:\\Users\\user\\Desktop\\(1학기)의학통계학\\statistics")
setwd("C:/Users/user/Desktop/(1학기)의학통계학/statistics")

# “#” 뒤에 오는 것은 모두 실행하지 않음
# 주석

# 명령어는 한 줄에 한 개씩 써야 함, 같은 줄 안에서 구분하고 싶다면 “;”로 구분함
a <- c(1, 2, 3, 4, 5);A <- c(6, 8, 10)

# ()는 함수에 사용, []은 행렬의 위치를 지정하거나 조건문을 쓸 때 사용함
hist(acs$age)
acs$sex
acs[ ,2] # 특정 변수(sex) 선택하기
acs[2, ] # 특정 개체(두 번째 개체) 선택하기


################################################################################
# 데이터 정리
################################################################################

### 1) 작업폴더(working directory) 설정하기

# 현재 설정된 working directory 확인하기
getwd()

# Working directory 설정하기(본인의 컴퓨터에 있는 경로를 사용! \ -> \\ 또는 \ -> /로 수정)
setwd("C:\\Users\\user\\Desktop\\(1학기)의학통계학\\statistics")


### 2) 데이터 불러오기

# Global environment에 기존 data 삭제
rm(list=ls())

# read.csv 함수로 csv 파일 불러오기(본인의 컴퓨터에 있는 경로를 사용! \ -> \\ 또는 \ -> /로 수정)
df <- read.csv("C:\\Users\\user\\Desktop\\(1학기)의학통계학\\statistics\\df.csv")

# 상위 6개 row 보기
head(df)


### 3) 변수 속성 확인

# 변수 속성 확인
str(df)

# 변수 속성 변경(numeric, integer, character, factor, Date 등)
df$id <- as.character(df$id)
df$year <- as.factor(df$year)
df$sex <- as.factor(df$sex)
df$age <- as.numeric(df$age)
df$smk1 <- as.factor(df$smk1)
df$smk2 <- as.factor(df$smk2)
df$ht <- as.numeric(df$ht)
df$wt <- as.numeric(df$wt)
df$glu <- as.numeric(df$glu)
str(df)


### 4) 변수 결측값(missing value) 확인

# 전체적으로 결측값 없는지 확인
sum(is.na(df$id))
sum(is.na(df$year))
sum(is.na(df$sex))
sum(is.na(df$age))
sum(is.na(df$smk1))
sum(is.na(df$smk2))
sum(is.na(df$ht))
sum(is.na(df$wt))
sum(is.na(df$glu))

# 한 번에 결측값 확인할 수 있는 함수 만들기
na.count <- apply(df, 2, function(x) sum(is.na(x)))
na.count[na.count > 0]

# 변수별 결측값 위치 확인
install.packages("VIM") # VIM 패키지 설치 필요
library(VIM)
VIM::aggr(df, prop = F, numbers = T)


### 5) 변수 자료요약, 이상값(outlier) 확인, 새로운 변수 만들기

# 범주형 변수의 자료요약
table(df$year)

table(df$sex)

table(df$smk1)
sum(is.na(df$smk1))
df$smk1 <- ifelse(df$smk1 == 9, NA, df$smk1)
df$smk1 <- as.factor(df$smk1)
table(df$smk1)
plot(df$smk1)
summary(df$smk1)

table(df$smk2)
df$smk2 <- ifelse(df$smk2 == 9, NA, df$smk2)
df$smk2 <- as.factor(df$smk2)
table(df$smk2)
plot(df$smk2)
summary(df$smk2)

# 변수생성(smk: 현재흡연 여부) 1. 과거흡연, 비흡연, 2. 현재흡연(평생 담배 5갑(100개비) 이상 피웠고 현재 담배를 피우는 경우)
df$smk <- ifelse(df$smk1 == 1 | (df$smk1 == 2 & df$smk2 == 3) | df$smk1 == 3, 1,
                 ifelse((df$smk1 == 2) & (df$smk2 == 1 | df$smk2 == 2), 2, NA))
df$smk <- as.factor(df$smk)
table(df$smk)
plot(df$smk)
summary(df$smk)

# 연속형 변수의 자료요약
summary(df$age)
table(df$age)
hist(df$age)

summary(df$ht)
hist(df$ht)

summary(df$wt)
hist(df$wt)
plot(df$ht, df$wt)

# 변수생성(bmi) bmi=kg/m2
df$bmi <- df$wt / (df$ht/100)^2
df$bmi <- as.numeric(df$bmi)
hist(df$bmi)

# 변수생성(bmi_c: BMI 3개 범주) 1. 저체중(0<bmi<18.5), 2. 정상(18.5<=bmi<25), 3. 비만(bmi>=25)
df$bmi_c <- ifelse(df$bmi > 0 & df$bmi <18.5, 1,
                   ifelse(df$bmi >= 18.5 & df$bmi < 25, 2,
                          ifelse(df$bmi >= 25, 3, NA)))
df$bmi_c <- as.factor(df$bmi_c)


### 6) 대상자 선택

# 30세 이상 성인
df1 <- subset(df, df$age >= 30)

# 결측값 확인
na.count <- apply(df1, 2, function(x) sum(is.na(x)))
na.count[na.count > 0]
VIM::aggr(df1, prop = F, numbers = T)

# 결측값 없는 대상자만 선택
df2 <- df1[complete.cases(df1), ]
sum(is.na(df2))


### 7) 파일 저장하기(csv)

# csv 파일로 데이터 저장하기
write.csv(df2, "C:\\Users\\user\\Desktop\\(1학기)의학통계학\\statistics\\df2.csv", row.names = F)