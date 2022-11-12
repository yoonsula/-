rm(list=ls())
raw <- read.csv('/Users/chanhyeok/Desktop/찬혁/공모전/여행원자료.csv', encoding = 'euc-kr', header = T,
                na.strings = '#NULL!')
#해외여행자 제외
raw1 <- raw[raw$국내_여행여부==1,]

#필요한 변수들만 선택
mydata <- select(raw1,c(ID,SA1_1,D_TRA1_1_Q6,A1_1,A2,A4_1,A7,NA8_TOTAL,NA9,A11,A12,A13,DQ1,DQ5,DQ5AC,DQ6A,DQ7,BSEX,BAGE))

#결측값 확인
mydata[is.na(mydata$DQ5AC),c('DQ5','DQ5AC')]  #근로여부가 2인 사람(근로 안함)만 결측치 -> 0으로 대체
mydata[is.na(mydata$D_TRA1_1_Q6),] %>% nrow   #결측치 9996개
mydata1$DQ5 %>% table

#결측값 제거
mydata$DQ5AC <- ifelse(is.na(mydata$DQ5AC),0,mydata$DQ5AC)
mydata <- mydata[complete.cases(mydata$D_TRA1_1_Q6),]

#재범주화
mydata1 <- filter(mydata,D_TRA1_1_Q6<=3)
mydata1 <- filter(mydata1,A1_1<=3 | A1_1==6)

mydata1$D_TRA1_1_Q6 <- factor(mydata1$D_TRA1_1_Q6,
                              labels = c('hotel','motel','pension'))
mydata1$A1_1 <- factor(mydata1$A1_1,
                       labels = c('car','train','plane','bus'))

#집단 나누기
mydata1 <- mutate(mydata1, NA_TOTAL=NA8_TOTAL+NA9)
mydata1$NA_TOTAL %>% median       #median : 159750

cost_low <- filter(mydata1, NA_TOTAL<=median(mydata1$NA_TOTAL))
cost_high <- filter(mydata1, NA_TOTAL>median(mydata1$NA_TOTAL))

income_low <- filter(mydata1, DQ6A<=3)
income_middle <- filter(mydata1, DQ6A>3 & DQ6A<=5)
income_high <- filter(mydata1, DQ6A>5)

#분석
#지출집단
result_cl <- aov(A11~D_TRA1_1_Q6+A1_1, data=cost_low)
result_cl %>% summary
result_cl %>% TukeyHSD

result_ch <- aov(A11~D_TRA1_1_Q6+A1_1, data=cost_high)
result_ch %>% summary
result_ch %>% TukeyHSD


#소득집단
result_il <- aov(A11~D_TRA1_1_Q6+A1_1, data=income_low)
result_il %>% summary
result_il %>% TukeyHSD

result_im <- aov(A11~D_TRA1_1_Q6+A1_1, data=income_middle)
result_im %>% summary
result_im %>% TukeyHSD

result_ih <- aov(A11~D_TRA1_1_Q6+A1_1, data=income_high)
result_ih %>% summary
result_ih %>% TukeyHSD

