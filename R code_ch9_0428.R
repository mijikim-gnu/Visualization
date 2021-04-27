################################################################################
# 의학통계학(2021학년도 1학기)
# 9. 두 집단 간 차이 비교(실습용 R 코드)
# 2021-04-28
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
df <- read.csv("C:\\Users\\user\\Desktop\\(1학기)의학통계학\\statistics\\df2.csv")

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
df$smk <- as.factor(df$smk)
df$bmi <- as.numeric(df$bmi)
df$bmi_c <- as.factor(df$bmi_c)
str(df)

### 4) 결측값 확인
sum(is.na(df))

### 5) 자료요약
table(df$sex)
round(prop.table(table(df$sex))*100, 1)
library(moonBook)
mytable(~sex, data=df)

# 정규성 검정
# density plot
hist(df$age, freq=F)
lines(density(df$age), col="red")

# Q-Q plot
qqnorm(df$age)
qqline(df$age, col="red")

# shapiro-wilk test
shapiro.test(df$age) # n=3~5000 Shapiro-Wilk normality test
nortest::ad.test(df$age) # n>=5000 Anderson-Darling normality test
summary(df$age)
sd(df$age)
mytable(~age, data=df, digits=2)

table(df$smk)
round(prop.table(table(df$smk))*100, 1)
mytable(~smk, data=df)

hist(df$bmi, freq=F)
lines(density(df$bmi), col="red")
qqnorm(df$bmi)
qqline(df$bmi, col="red")
nortest::ad.test(df$bmi)
summary(df$bmi)
sd(df$bmi)
mytable(~bmi, data=df, digits=2)

hist(df$glu, freq=F)
lines(density(df$glu), col="red")
qqnorm(df$glu)
qqline(df$glu, col="red")
nortest::ad.test(df$glu)
summary(df$glu)
sd(df$glu)
mytable(~glu, data=df, digits=2)

### 6) t-test
# age
# 정규성 검정(각 군별로 시행)
shapiro.test(df$age[df$sex==1]) # p<0.05이긴 하나 n 충분히 큼 -> 정규성 만족
shapiro.test(df$age[df$sex==2]) # p<0.05이긴 하나 n 충분히 큼 -> 정규성 만족
# 등분산성 검정(각 군별로 시행)
var.test(df$age~df$sex) # p=0.6772 -> 등분산성 만족
# t 검정
t.test(df$age~df$sex, var.equal=T)
?t.test
sd(df$age[df$sex==1]);sd(df$age[df$sex==2])
mytable(sex~age, data=df, digits=2)

# bmi
# 정규성 검정(각 군별로 시행)
shapiro.test(df$bmi[df$sex==1]) # p<0.05이긴 하나 n 충분히 큼 -> 정규성 만족
shapiro.test(df$bmi[df$sex==2]) # p<0.05이긴 하나 n 충분히 큼 -> 정규성 만족
# 등분산성 검정(각 군별로 시행)
var.test(df$bmi~df$sex) # p=9.819e-08 -> 등분산성 만족 X
# t 검정(자유도 수정)
t.test(df$bmi~df$sex, var.equal=F)
sd(df$bmi[df$sex==1]);sd(df$bmi[df$sex==2])
mytable(sex~bmi, data=df, digits=2)

# glu
# 정규성 검정(각 군별로 시행)
shapiro.test(df$glu[df$sex==1]) # p<0.05이긴 하나 n 충분히 큼 -> 정규성 만족
shapiro.test(df$glu[df$sex==2]) # p<0.05이긴 하나 n 충분히 큼 -> 정규성 만족
# 등분산성 검정(각 군별로 시행)
var.test(df$glu~df$sex) # p<2.2e-16 -> 등분산성 만족 X
# t 검정(자유도 수정)
t.test(df$glu~df$sex, var.equal=F)
sd(df$glu[df$sex==1]);sd(df$glu[df$sex==2])
mytable(sex~bmi, data=df, digits=2)

# 정규성 만족 X -> 비모수 검정(wilcoxon rank sum test)
wilcox.test(age~sex, data=df)

### 7) chi-squared test
tab <- table(df$smk, df$sex)
tab
install.packages("Rcmdr")
library(Rcmdr)
colPercents(tab)
mytable(sex~smk, data=df)      

chisq.test(tab, correct=F)
chisq.test(tab)$expected # 기대빈도가 5 이하인 셀이 20% 이상이면 fisher's exact test
fisher.test(tab)