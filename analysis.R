##BLV reproduction model to estimate the economic loss##
#5 years model#
sim_d = 6*365 #2190
#Discarding first 1 year
sim_discard = 365

#0. Basics----
#0.1. Herd structure----
#Initial parity (IP), 頭数を記載。
IP0 = 50 #未経産牛頭数、例、ここは全体で119頭になるよう変えてよいです。
IP1 = 22 #例
IP2 = 17 #例
IP3 = 13 #例
IP4 = 8 #例
IP5 = 9 #例
#Initial parity status
IP <- c(rep(0, IP0), rep(1, IP1), rep(2, IP2), rep(3, IP3), rep(4, IP4), rep(5, IP5))

#New borne calves which remain in the herd in year 1 - 4
NB1 = 25
NB2 = 25
NB3 = 25
NB4 = 25

#0.2. Age in days of each parity----
#各産次の日齢の範囲を与える。外れ値は考慮しない。
ageIP0 = c(0, 750) #未経産牛の日齢範囲、ここは変えないでください。
ageIP1 = c(750, 1150) #ここは変えないでください。
ageIP2 = c(1100, 1550) #ここは変えないでください。
ageIP3 = c(1450, 1900) #ここは変えないでください。
ageIP4 = c(1850, 2350) #ここは変えないでください。
ageIP5 = c(2250, 2700) #ここは変えないでください。
ageIP <- c(ageIP0, ageIP1, ageIP2, ageIP3, ageIP4, ageIP5)
ageIPmat <- matrix(ageIP, nrow = 2)

#Birthday of new borne calves
DB_NB1 <- sort(sample(1:365, size = NB1, replace = TRUE))
DB_NB2 <- sort(sample(366:730, size = NB2, replace = TRUE))
DB_NB3 <- sort(sample(731:1095, size = NB3, replace = TRUE))
DB_NB4 <- sort(sample(1096:1460, size = NB4, replace = TRUE))

#0.3. Age in days of delivery at each parity----
#各産次の出産日を与える。外れ値は考慮しない。
#代替案として、正規分布から抽出しても良い。その場合、c()内は平均値と標準偏差。
age_del1 = c(750, 870) #初産日齢の範囲、ここは変えないでください。
age_del2 = c(1100, 1220) #ここは変えないでください。
age_del3 = c(1450, 1570) #ここは変えないでください。
age_del4 = c(1850, 2000) #ここは変えないでください。
age_del5 = c(2250, 2400) #ここは変えないでください。
age_del6 = c(2700, 2850) #ここは変えないでください。
age_del <- c(age_del1, age_del2, age_del3, age_del4, age_del5, age_del6)
age_delmat <- matrix(age_del, nrow = 2)

#0.4. BLV infection----
#Proportion of BLV infected cattle (I + L) in each parity
pBLV0 = 0.02 #20%の場合の例、データに基づき作成。必ず産次と共に上昇するように設定してください。
pBLV1 = 0.03 #例
pBLV2 = 0.15  #例
pBLV3 = 0.27 #例
pBLV4 = 0.29 #例
pBLV5 = 0.41  #例
pBLV <- c(pBLV0, pBLV1, pBLV2, pBLV3, pBLV4, pBLV5)

#Proportion of persistent leukocytosis (L) among BLV infected cows
#抵抗性遺伝子保有牛を考慮。データに基づき作成。
#必ずわずかでも産次と共に上昇するように設定してください。
pL_BLV0 = 0.15 #未経産のBLV感染牛に占めるL牛の割合（例: 20%の場合）
pL_BLV1 = 0.2 #例
pL_BLV2 = 0.26 #例
pL_BLV3 = 0.261 #例
pL_BLV4 = 0.262 #例
pL_BLV5 = 0.263 #例
pL_BLV <- c(pL_BLV0, pL_BLV1, pL_BLV2, pL_BLV3, pL_BLV4, pL_BLV5)

#0.5. Reproduction----
#0.5.1. Number of AIs----
nAI_mas0 = 3.0 #未経産牛で未経産乳房炎に罹患してた牛の平均AI回数、データセットにはないと思われる。
nAI_nomas0 = 2.25 #未経産牛で未経産乳房炎に罹患しいない牛の平均AI回数。データに基づき作成。
nAI_mas1 = 3.37 # 回数の平均値がPoison分布のパラメータ, lambda
nAI_nomas1 = 2.41#例
nAI_mas2 = 3.33#例
nAI_nomas2 = 2.51#例
nAI_mas3 = 3.84#例
nAI_nomas3 = 2.37#例
nAI_mas4 = 3.82#例
nAI_nomas4 = 2.82#例
nAI_mas5 = 3.23#例
nAI_nomas5 = 2.12#例
nAI_mas <- c(nAI_mas0, nAI_nomas0, nAI_mas1, nAI_nomas1, nAI_mas2, nAI_nomas2, nAI_mas3, nAI_nomas3,
             nAI_mas4, nAI_nomas4, nAI_mas5, nAI_nomas5)
nAI_masmat <- matrix(nAI_mas, nrow = 2)

#0.5.2. Number of AIs until removal from a herd----
nAI_remove = 6

#0.5.3. Waiting days before first AI after delivery----
day_wait_AI = 60

#0.5.4. Pregnancy period
preg_period = 283

#0.6. Mastitis----
#0.6.1. Probability of mastitis----
#データに基づき作成。
pmas_S1 = 0.18 #Probability of mastitis in the first delivery in a susceptible cow
pmas_I1 = 0.18 #例
pmas_L1 = 0.18 #例
pmas_S2 = 0.27 #例
pmas_I2 = 0.5 #例
pmas_L2 = 0.5 #例
pmas_S3 = 0.26 #例
pmas_I3 = 0.52 #例
pmas_L3 = 0.43 #例
pmas_S4 = 0.35 #例
pmas_I4 = 0.45 #例
pmas_L4 = 0.14 #例
pmas_S5 = 0.34 #例
pmas_I5 = 0.38 #例
pmas_L5 = 0.6 #例
pmas <- c(pmas_S1, pmas_I1, pmas_L1, pmas_S2, pmas_I2, pmas_L2, pmas_S3, pmas_I3, pmas_L3,
          pmas_S4, pmas_I4, pmas_L4, pmas_S5, pmas_I5, pmas_L5)
pmasmat <- matrix(pmas, nrow = 3)

#0.6.2. Day until mastitis after delivery----
days_mas_SI <- 99 
days_mas_L <- 31

#0.7. Milk yield----
#Milking day
milk_day = 305

#Milk yield per parity (liter)
#農水省などのデータに基づき作成。
yield_P1 = 9111 #例
yield_P2 = 10652 #例
yield_P3 = 11034 #例
yield_P4 = 10994 #例
yield_P5 = 10491 #例
yield_P6 = 9057 #例

#Milk price per liter
milk_price = 103 #データに基づき作成。

#0.8. Culling rate----
#Culling due to mastitis and parity。データに基づき作成。分からなければこのままで良い。
cull_mas1 = 0.0
cull_nomas1 = 0.0
cull_mas2 = 0.2
cull_nomas2 = 0.00
cull_mas3 = 0.3
cull_nomas3 = 0.1
cull_mas4 = 0.6
cull_nomas4 = 0.2
cull_mas5 = 0.8
cull_nomas5 = 0.7
cull_mas6 = 0.95
cull_nomas6 = 0.9

#0.Cattle price----
#データに基づき作成。
cattle_price = 1000000

#A. BLV infected farm----
#A1. Setting initial status----
#A1.1. Assignment of ID in initial herd----
BID_IP0 <- 1 : IP0
BID_IP1 <- IP0 + 1 : IP1
BID_IP2 <- IP0 + IP1 + 1 : IP2
BID_IP3 <- IP0 + IP1 + IP2 + 1 : IP3
BID_IP4 <- IP0 + IP1 + IP2 + IP3 + 1 : IP4
BID_IP5 <- IP0 + IP1 + IP2 + IP3 + IP4 + 1 : IP5
BID <- c(BID_IP0, BID_IP1, BID_IP2, BID_IP3, BID_IP4, BID_IP5)

#A1.2. Assignment of age in days at the simulation day 0----
#BIPage is the age in days at the simulation day 0
BIP0age <- sort(sample(ageIP0[1]:ageIP0[2], size = IP0, replace = TRUE))
BIP1age <- sort(sample(ageIP1[1]:ageIP1[2], size = IP1, replace = TRUE))
BIP2age <- sort(sample(ageIP2[1]:ageIP2[2], size = IP2, replace = TRUE))
BIP3age <- sort(sample(ageIP3[1]:ageIP3[2], size = IP3, replace = TRUE))
BIP4age <- sort(sample(ageIP4[1]:ageIP4[2], size = IP4, replace = TRUE))
BIP5age <- sort(sample(ageIP5[1]:ageIP5[2], size = IP5, replace = TRUE))
BIPage <- c(BIP0age, BIP1age, BIP2age, BIP3age, BIP4age, BIP5age)

#A1.3. Assignment of BLV status (SIL) at the simulation day 0----
#0: S, 1: I, 2:L
BIP0_BLV <- rbinom(IP0, 1, pBLV0)*(1+rbinom(IP0, 1, pL_BLV0))
BIP1_BLV <- rbinom(IP1, 1, pBLV1)*(1+rbinom(IP1, 1, pL_BLV1))
BIP2_BLV <- rbinom(IP2, 1, pBLV2)*(1+rbinom(IP2, 1, pL_BLV2))
BIP3_BLV <- rbinom(IP3, 1, pBLV3)*(1+rbinom(IP3, 1, pL_BLV3))
BIP4_BLV <- rbinom(IP4, 1, pBLV4)*(1+rbinom(IP4, 1, pL_BLV4))
BIP5_BLV <- rbinom(IP5, 1, pBLV5)*(1+rbinom(IP5, 1, pL_BLV5))
BIP_BLV <- c(BIP0_BLV, BIP1_BLV, BIP2_BLV, BIP3_BLV, BIP4_BLV, BIP5_BLV)

BNB1_BLV <- rbinom(NB1, 1, pBLV0)*(1+rbinom(NB1, 1, pL_BLV0)) 
BNB2_BLV <- rbinom(NB2, 1, pBLV0)*(1+rbinom(NB2, 1, pL_BLV0))
BNB3_BLV <- rbinom(NB3, 1, pBLV0)*(1+rbinom(NB3, 1, pL_BLV0))
BNB4_BLV <- rbinom(NB4, 1, pBLV0)*(1+rbinom(NB4, 1, pL_BLV0))

#A1.4. Delivery date----
#A1.4.1. Expected delivery date in age (day) in IP0 heifers----
#正規分布を使っても良い。その場合、平均をmu、標準偏差をsdとして
#BIP0_day_del <- rnorm(IPO, mu, sd)
BIP0_day_del1 <- sample(age_del1[1]:age_del1[2], size = IP0, replace = TRUE)

#A1.4.2. Previous delivery date in age (day)----
BIP1_day_del2 <- sample(age_del2[1]:age_del2[2], size = IP1, replace = TRUE)
BIP2_day_del3 <- sample(age_del3[1]:age_del3[2], size = IP2, replace = TRUE)
BIP3_day_del4 <- sample(age_del4[1]:age_del4[2], size = IP3, replace = TRUE)  
BIP4_day_del5 <- sample(age_del5[1]:age_del5[2], size = IP4, replace = TRUE)
BIP5_day_del6 <- sample(age_del6[1]:age_del6[2], size = IP5, replace = TRUE)

#A1.4.3. Expected delivery date in age (day) of new borne calves----
BNB1_day_del1 <- sample(age_del1[1]:age_del1[2], size = NB1, replace = TRUE)
BNB2_day_del1 <- sample(age_del1[1]:age_del1[2], size = NB2, replace = TRUE)
BNB3_day_del1 <- sample(age_del1[1]:age_del1[2], size = NB3, replace = TRUE)
BNB4_day_del1 <- sample(age_del1[1]:age_del1[2], size = NB4, replace = TRUE)

#A2. Simulation----
#A2.0. IP0 simulation----
#A2.0.1. First parity----
#Simulation day of first parity
BIP0_simd_del1 <- BIP0_day_del1 - BIP0age

#Mastitis at first parity
BIP0_mas1 <- numeric(IP0)
for (i in 1:IP0){
BIP0_mas1[i] <- rbinom(1, 1, pmasmat[1 + BIP0_BLV[i], 1])
}
BIP0_mas1

#First lactation period
BIP0_lac1_period <- ifelse(BIP0_mas1==1, ifelse(BIP0_BLV==2, days_mas_L, days_mas_SI), milk_day)

#First lactation milk yield
BIP0_lac1_yield <- ifelse(BIP0_mas1==1, ifelse(BIP0_BLV==2, yield_P1 * days_mas_L / milk_day, 
                                               yield_P1 * days_mas_SI / milk_day), yield_P1)

#First lactation end age
BIP0age_lac1e <- BIP0_day_del1 + BIP0_lac1_period 

#First lactation end simulation day
BIP0_simd_lac1e <- BIP0_simd_del1 + BIP0_lac1_period

#First lactation income #linear score 5 milk price not considered  ##per day?
BIP0_lac1_income <- ifelse(BIP0_simd_del1 > sim_d, 0,
                           ifelse(BIP0_simd_lac1e < sim_discard, 0,
                                  ifelse(BIP0_simd_lac1e > sim_d, 
                                         BIP0_lac1_yield * milk_price * (sim_d - BIP0_simd_del1)/BIP0_lac1_period,
                                      ifelse(BIP0_simd_del1 < sim_discard,
                                             BIP0_lac1_yield * milk_price *
                                               (BIP0_simd_lac1e - sim_discard)/BIP0_lac1_period,
                                             BIP0_lac1_yield * milk_price * BIP0_lac1_period / milk_day))))
                           
#AI for second pregnancy
BIP0_AI2 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_AI2[i] <- rpois(1, ifelse(BIP0_mas1[i]==1, nAI_masmat[1,1], nAI_masmat[2,1]))
}
BIP0_AI2

#Removal of cattle
BIP0_preg2_removed <- ifelse(BIP0_AI2 > nAI_remove-1, 1, 
                             ifelse(BIP0_mas1 == 1, rbinom(1, 1, cull_mas1), 
                                    rbinom(1, 1, cull_nomas1)))

#Second pregnancy age
BIP0age_preg2 <- BIP0_day_del1 + day_wait_AI + 21*BIP0_AI2

#Second pregnancy simulation day
BIP0_simd_preg2 <- BIP0_simd_del1 + day_wait_AI + 21*BIP0_AI2


#A2.0.2. Second parity----
#Second parity age
BIP0age_del2 <- BIP0age_preg2 + preg_period

#Second parity simulation day
BIP0_simd_del2 <- BIP0_simd_preg2 + preg_period 

#BLV status of IP0 at second pregnancy
BIP0_BLV_del2 <- numeric(IP0)
for (i in 1:IP0){
BIP0_BLV_del2[i] <- ifelse(BIP0_BLV[i] == 0, 
                           rbinom(1, 1, (pBLV1 - pBLV0)/(1 - pBLV0))*(1 + rbinom(1, 1, pL_BLV1)), 
                           ifelse(BIP0_BLV[i] == 1, 
                                  1 + rbinom(1, 1, (pL_BLV1 - pL_BLV0)/(1 - pL_BLV0)), 2))
}

#Mastitis at second parity
BIP0_mas2 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_mas2[i] <- rbinom(1, 1, pmasmat[1 + BIP0_BLV_del2[i], 2])
}
BIP0_mas2

#Second lactation period
BIP0_lac2_period <- ifelse(BIP0_mas2==1, ifelse(BIP0_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
BIP0_lac2_yield <- ifelse(BIP0_mas2==1, ifelse(BIP0_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
BIP0age_lac2e <- BIP0age_del2 + BIP0_lac2_period 

#Second lactation end simulation day
BIP0_simd_lac2e <- BIP0_simd_del2 + BIP0_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
BIP0_lac2_income <- ifelse(BIP0_simd_del2 > sim_d, 0,
                           ifelse(BIP0_simd_lac2e < sim_discard, 0,
                                  ifelse(BIP0_simd_lac2e > sim_d, 
                                         BIP0_lac2_yield * milk_price * (sim_d - BIP0_simd_del2)/BIP0_lac2_period *
                                           (1 - BIP0_preg2_removed),
                                         ifelse(BIP0_simd_del2 < sim_discard,
                                                BIP0_lac2_yield * milk_price *
                                                  (BIP0_simd_lac2e - sim_discard)/BIP0_lac2_period *
                                                  (1 - BIP0_preg2_removed),
                                                BIP0_lac2_yield * milk_price * BIP0_lac2_period / milk_day *
                                                  (1 - BIP0_preg2_removed)))))

#AI for third pregnancy
BIP0_AI3 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_AI3[i] <- rpois(1, ifelse(BIP0_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
BIP0_AI3

#Removal of cattle
BIP0_preg3_removed <- ifelse(BIP0_AI3 > nAI_remove-1, 1, 
                             ifelse(BIP0_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))
BIP0_preg3_removed_cumulative <- ifelse(BIP0_preg2_removed==1, 1, 
                                      ifelse(BIP0_AI3 > nAI_remove-1, 1, 
                                       ifelse(BIP0_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                                          rbinom(1, 1, cull_nomas2))))

#Third pregnancy age
BIP0age_preg3 <- BIP0age_del2 + day_wait_AI + 21*BIP0_AI3

#Third pregnancy simulation day
BIP0_simd_preg3 <- BIP0_simd_del2 + day_wait_AI + 21*BIP0_AI3


#A2.0.3. Third parity----
#Third parity age
BIP0age_del3 <- BIP0age_preg3 + preg_period

#Third parity simulation day
BIP0_simd_del3 <- BIP0_simd_preg3 + preg_period 

#BLV status of IP0 at third pregnancy
BIP0_BLV_del3 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_BLV_del3[i] <- ifelse(BIP0_BLV_del2[i] == 0, 
                             rbinom(1, 1, (pBLV2 - pBLV1)/(1 - pBLV1))*(1 + rbinom(1, 1, pL_BLV2)), 
                             ifelse(BIP0_BLV_del2[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV2 - pL_BLV1)/(1 - pL_BLV1)), 2))
}
BIP0_BLV_del3

#Mastitis at third parity
BIP0_mas3 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_mas3[i] <- rbinom(1, 1, pmasmat[1 + BIP0_BLV_del3[i], 3])
}
BIP0_mas3

#Third lactation period
BIP0_lac3_period <- ifelse(BIP0_mas3==1, ifelse(BIP0_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
BIP0_lac3_yield <- ifelse(BIP0_mas3==1, ifelse(BIP0_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
BIP0age_lac3e <- BIP0age_del3 + BIP0_lac3_period 

#Third lactation end simulation day
BIP0_simd_lac3e <- BIP0_simd_del3 + BIP0_lac3_period

#Third lactation income #linear score 5 milk price not considered
BIP0_lac3_income <- ifelse(BIP0_simd_del3 > sim_d, 0,
                           ifelse(BIP0_simd_lac3e < sim_discard, 0,
                                  ifelse(BIP0_simd_lac3e > sim_d, 
                                         BIP0_lac3_yield * milk_price * (sim_d - BIP0_simd_del3)/BIP0_lac3_period *
                                           (1 - BIP0_preg3_removed_cumulative),
                                         ifelse(BIP0_simd_del3 < sim_discard,
                                                BIP0_lac3_yield * milk_price *
                                                  (BIP0_simd_lac3e - sim_discard)/BIP0_lac3_period *
                                                  (1 - BIP0_preg3_removed_cumulative),
                                                BIP0_lac3_yield * milk_price * BIP0_lac3_period / milk_day *
                                                  (1 - BIP0_preg3_removed_cumulative)))))

#AI for fourth pregnancy
BIP0_AI4 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_AI4[i] <- rpois(1, ifelse(BIP0_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
BIP0_AI4

#Removal of cattle
BIP0_preg4_removed <- ifelse(BIP0_AI4 > nAI_remove-1, 1, 
                             ifelse(BIP0_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
BIP0_preg4_removed_cumulative <- ifelse(BIP0_preg3_removed_cumulative==1, 1, 
                                        ifelse(BIP0_AI4 > nAI_remove-1, 1, 
                                               ifelse(BIP0_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
BIP0age_preg4 <- BIP0age_del3 + day_wait_AI + 21*BIP0_AI4

#Fourth pregnancy simulation day
BIP0_simd_preg4 <- BIP0_simd_del3 + day_wait_AI + 21*BIP0_AI4


#A2.0.4. Fourth parity----
#Fourth parity age
BIP0age_del4 <- BIP0age_preg4 + preg_period

#Fourth parity simulation day
BIP0_simd_del4 <- BIP0_simd_preg4 + preg_period 

#BLV status of IP0 at fourth pregnancy
BIP0_BLV_del4 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_BLV_del4[i] <- ifelse(BIP0_BLV_del3[i] == 0, 
                             rbinom(1, 1, (pBLV3 - pBLV2)/(1 - pBLV2))*(1 + rbinom(1, 1, pL_BLV3)), 
                             ifelse(BIP0_BLV_del3[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV3 - pL_BLV2)/(1 - pL_BLV2)), 2))
}
BIP0_BLV_del4

#Mastitis at fourth parity
BIP0_mas4 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_mas4[i] <- rbinom(1, 1, pmasmat[1 + BIP0_BLV_del4[i], 4])
}
BIP0_mas4

#Fourth lactation period
BIP0_lac4_period <- ifelse(BIP0_mas4==1, ifelse(BIP0_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
BIP0_lac4_yield <- ifelse(BIP0_mas4==1, ifelse(BIP0_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
BIP0age_lac4e <- BIP0age_del4 + BIP0_lac4_period 

#Fourth lactation end simulation day
BIP0_simd_lac4e <- BIP0_simd_del4 + BIP0_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
BIP0_lac4_income <- ifelse(BIP0_simd_del4 > sim_d, 0,
                           ifelse(BIP0_simd_lac4e < sim_discard, 0,
                                  ifelse(BIP0_simd_lac4e > sim_d, 
                                         BIP0_lac4_yield * milk_price * (sim_d - BIP0_simd_del4)/BIP0_lac4_period *
                                           (1 - BIP0_preg4_removed_cumulative),
                                         ifelse(BIP0_simd_del4 < sim_discard,
                                                BIP0_lac4_yield * milk_price *
                                                  (BIP0_simd_lac4e - sim_discard)/BIP0_lac4_period *
                                                  (1 - BIP0_preg4_removed_cumulative),
                                                BIP0_lac4_yield * milk_price * BIP0_lac4_period / milk_day *
                                                  (1 - BIP0_preg4_removed_cumulative)))))


#AI for fifth pregnancy
BIP0_AI5 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_AI5[i] <- rpois(1, ifelse(BIP0_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
BIP0_AI5

#Removal of cattle
BIP0_preg5_removed <- ifelse(BIP0_AI5 > nAI_remove-1, 1, 
                             ifelse(BIP0_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
BIP0_preg5_removed_cumulative <- ifelse(BIP0_preg4_removed_cumulative==1, 1, 
                                        ifelse(BIP0_AI5 > nAI_remove-1, 1, 
                                               ifelse(BIP0_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
BIP0age_preg5 <- BIP0age_del4 + day_wait_AI + 21*BIP0_AI5

#Fifth pregnancy simulation day
BIP0_simd_preg5 <- BIP0_simd_del4 + day_wait_AI + 21*BIP0_AI5


#A2.0.5. Fifth parity----
#Fifth parity age
BIP0age_del5 <- BIP0age_preg5 + preg_period

#Fifth parity simulation day
BIP0_simd_del5 <- BIP0_simd_preg5 + preg_period 

#BLV status of IP0 at fifth pregnancy
BIP0_BLV_del5 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_BLV_del5[i] <- ifelse(BIP0_BLV_del4[i] == 0, 
                             rbinom(1, 1, (pBLV4 - pBLV3)/(1 - pBLV3))*(1 + rbinom(1, 1, pL_BLV4)), 
                             ifelse(BIP0_BLV_del4[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV4 - pL_BLV3)/(1 - pL_BLV3)), 2))
}
BIP0_BLV_del5

#Mastitis at fifth parity
BIP0_mas5 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_mas5[i] <- rbinom(1, 1, pmasmat[1 + BIP0_BLV_del5[i], 5])
}
BIP0_mas5

#Fifth lactation period
BIP0_lac5_period <- ifelse(BIP0_mas5==1, ifelse(BIP0_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
BIP0_lac5_yield <- ifelse(BIP0_mas5==1, ifelse(BIP0_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
BIP0age_lac5e <- BIP0age_del5 + BIP0_lac5_period 

#Fifth lactation end simulation day
BIP0_simd_lac5e <- BIP0_simd_del5 + BIP0_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
BIP0_lac5_income <- ifelse(BIP0_simd_del5 > sim_d, 0,
                           ifelse(BIP0_simd_lac5e < sim_discard, 0,
                                  ifelse(BIP0_simd_lac5e > sim_d, 
                                         BIP0_lac5_yield * milk_price * (sim_d - BIP0_simd_del5)/BIP0_lac5_period *
                                           (1 - BIP0_preg5_removed_cumulative),
                                         ifelse(BIP0_simd_del5 < sim_discard,
                                                BIP0_lac5_yield * milk_price *
                                                  (BIP0_simd_lac5e - sim_discard)/BIP0_lac5_period *
                                                  (1 - BIP0_preg5_removed_cumulative),
                                                BIP0_lac5_yield * milk_price * BIP0_lac5_period / milk_day *
                                                  (1 - BIP0_preg5_removed_cumulative)))))

#AI for sixth pregnancy
BIP0_AI6 <- numeric(IP0)
for (i in 1:IP0){
  BIP0_AI6[i] <- rpois(1, ifelse(BIP0_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
BIP0_AI6

#Removal of cattle
BIP0_preg6_removed <- ifelse(BIP0_AI6 > nAI_remove-1, 1, 
                             ifelse(BIP0_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
BIP0_preg6_removed_cumulative <- ifelse(BIP0_preg5_removed_cumulative==1, 1, 
                                        ifelse(BIP0_AI6 > nAI_remove-1, 1, 
                                               ifelse(BIP0_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
BIP0age_preg6 <- BIP0age_del5 + day_wait_AI + 21*BIP0_AI6

#Sixth pregnancy simulation day
BIP0_simd_preg6 <- BIP0_simd_del5 + day_wait_AI + 21*BIP0_AI6


#A2.1. IP1 simulation----
#A2.1.2. Second parity----
#Simulation day of second parity
BIP1_simd_del2 <- BIP1_day_del2 - BIP1age

#BLV status of IP1 at second pregnancy
BIP1_BLV_del2 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_BLV_del2[i] <- ifelse(BIP1_BLV[i] == 0, 
                             rbinom(1, 1, (pBLV1 - pBLV0)/(1 - pBLV0))*(1 + rbinom(1, 1, pL_BLV1)), 
                             ifelse(BIP1_BLV[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV1 - pL_BLV0)/(1 - pL_BLV0)), 2))
}
BIP1_BLV_del2

#Mastitis at second parity
BIP1_mas2 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_mas2[i] <- rbinom(1, 1, pmasmat[1 + BIP1_BLV_del2[i], 2])
}
BIP1_mas2

#Second lactation period
BIP1_lac2_period <- ifelse(BIP1_mas2==1, ifelse(BIP1_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
BIP1_lac2_yield <- ifelse(BIP1_mas2==1, ifelse(BIP1_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
BIP1age_lac2e <- BIP1_day_del2 + BIP1_lac2_period 

#Second lactation end simulation day
BIP1_simd_lac2e <- BIP1_simd_del2 + BIP1_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
BIP1_lac2_income <- ifelse(BIP1_simd_del2 > sim_d, 0,
                           ifelse(BIP1_simd_lac2e < sim_discard, 0,
                                  ifelse(BIP1_simd_lac2e > sim_d, 
                                         BIP1_lac2_yield * milk_price * (sim_d - BIP1_simd_del2)/BIP1_lac2_period,
                                         ifelse(BIP1_simd_del2 < sim_discard,
                                                BIP1_lac2_yield * milk_price *
                                                  (BIP1_simd_lac2e - sim_discard)/BIP1_lac2_period,
                                                BIP1_lac2_yield * milk_price * BIP1_lac2_period / milk_day))))

#AI for third pregnancy
BIP1_AI3 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_AI3[i] <- rpois(1, ifelse(BIP1_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
BIP1_AI3

#Removal of cattle
BIP1_preg3_removed <- ifelse(BIP1_AI3 > nAI_remove-1, 1, 
                             ifelse(BIP1_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))

#Third pregnancy age
BIP1age_preg3 <- BIP1_day_del2 + day_wait_AI + 21*BIP1_AI3

#Third pregnancy simulation day
BIP1_simd_preg3 <- BIP1_simd_del2 + day_wait_AI + 21*BIP1_AI3


#A2.1.3. Third parity----
#Third parity age
BIP1age_del3 <- BIP1age_preg3 + preg_period

#Third parity simulation day
BIP1_simd_del3 <- BIP1_simd_preg3 + preg_period 

#BLV status of IP1 at third pregnancy
BIP1_BLV_del3 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_BLV_del3[i] <- ifelse(BIP1_BLV_del2[i] == 0, 
                             rbinom(1, 1, (pBLV2 - pBLV1)/(1 - pBLV1))*(1 + rbinom(1, 1, pL_BLV2)), 
                             ifelse(BIP0_BLV_del2[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV2 - pL_BLV1)/(1 - pL_BLV1)), 2))
}
BIP1_BLV_del3

#Mastitis at third parity
BIP1_mas3 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_mas3[i] <- rbinom(1, 1, pmasmat[1 + BIP1_BLV_del3[i], 3])
}
BIP1_mas3

#Third lactation period
BIP1_lac3_period <- ifelse(BIP1_mas3==1, ifelse(BIP1_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
BIP1_lac3_yield <- ifelse(BIP1_mas3==1, ifelse(BIP1_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
BIP1age_lac3e <- BIP1age_del3 + BIP1_lac3_period 

#Third lactation end simulation day
BIP1_simd_lac3e <- BIP1_simd_del3 + BIP1_lac3_period

#Third lactation income #linear score 5 milk price not considered
BIP1_lac3_income <- ifelse(BIP1_simd_del3 > sim_d, 0,
                           ifelse(BIP1_simd_lac3e < sim_discard, 0,
                                  ifelse(BIP1_simd_lac3e > sim_d, 
                                         BIP1_lac3_yield * milk_price * (sim_d - BIP1_simd_del3)/BIP1_lac3_period *
                                           (1 - BIP1_preg3_removed),
                                         ifelse(BIP1_simd_del3 < sim_discard,
                                                BIP1_lac3_yield * milk_price *
                                                  (BIP1_simd_lac3e - sim_discard)/BIP1_lac3_period *
                                                  (1 - BIP1_preg3_removed),
                                                BIP1_lac3_yield * milk_price * BIP1_lac3_period / milk_day *
                                                  (1 - BIP1_preg3_removed)))))

#AI for fourth pregnancy
BIP1_AI4 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_AI4[i] <- rpois(1, ifelse(BIP1_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
BIP1_AI4

#Removal of cattle
BIP1_preg4_removed <- ifelse(BIP1_AI4 > nAI_remove-1, 1, 
                             ifelse(BIP1_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
BIP1_preg4_removed_cumulative <- ifelse(BIP1_preg3_removed==1, 1, 
                                        ifelse(BIP1_AI4 > nAI_remove-1, 1, 
                                               ifelse(BIP1_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
BIP1age_preg4 <- BIP1age_del3 + day_wait_AI + 21*BIP1_AI4

#Fourth pregnancy simulation day
BIP1_simd_preg4 <- BIP1_simd_del3 + day_wait_AI + 21*BIP1_AI4


#A2.1.4. Fourth parity----
#Fourth parity age
BIP1age_del4 <- BIP1age_preg4 + preg_period

#Fourth parity simulation day
BIP1_simd_del4 <- BIP1_simd_preg4 + preg_period 

#BLV status of IP1 at fourth pregnancy
BIP1_BLV_del4 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_BLV_del4[i] <- ifelse(BIP1_BLV_del3[i] == 0, 
                             rbinom(1, 1, (pBLV3 - pBLV2)/(1 - pBLV2))*(1 + rbinom(1, 1, pL_BLV3)), 
                             ifelse(BIP1_BLV_del3[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV3 - pL_BLV2)/(1 - pL_BLV2)), 2))
}
BIP1_BLV_del4

#Mastitis at fourth parity
BIP1_mas4 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_mas4[i] <- rbinom(1, 1, pmasmat[1 + BIP1_BLV_del4[i], 4])
}
BIP1_mas4

#Fourth lactation period
BIP1_lac4_period <- ifelse(BIP1_mas4==1, ifelse(BIP1_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
BIP1_lac4_yield <- ifelse(BIP1_mas4==1, ifelse(BIP1_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
BIP1age_lac4e <- BIP1age_del4 + BIP1_lac4_period 

#Fourth lactation end simulation day
BIP1_simd_lac4e <- BIP1_simd_del4 + BIP1_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
BIP1_lac4_income <- ifelse(BIP1_simd_del4 > sim_d, 0,
                           ifelse(BIP1_simd_lac4e < sim_discard, 0,
                                  ifelse(BIP1_simd_lac4e > sim_d, 
                                         BIP1_lac4_yield * milk_price * (sim_d - BIP1_simd_del4)/BIP1_lac4_period *
                                           (1 - BIP1_preg4_removed_cumulative),
                                         ifelse(BIP1_simd_del4 < sim_discard,
                                                BIP1_lac4_yield * milk_price *
                                                  (BIP1_simd_lac4e - sim_discard)/BIP1_lac4_period *
                                                  (1 - BIP1_preg4_removed_cumulative),
                                                BIP1_lac4_yield * milk_price * BIP1_lac4_period / milk_day *
                                                  (1 - BIP1_preg4_removed_cumulative)))))

#AI for fifth pregnancy
BIP1_AI5 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_AI5[i] <- rpois(1, ifelse(BIP1_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
BIP1_AI5

#Removal of cattle
BIP1_preg5_removed <- ifelse(BIP1_AI5 > nAI_remove-1, 1, 
                             ifelse(BIP1_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
BIP1_preg5_removed_cumulative <- ifelse(BIP1_preg4_removed_cumulative==1, 1, 
                                        ifelse(BIP1_AI5 > nAI_remove-1, 1, 
                                               ifelse(BIP1_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
BIP1age_preg5 <- BIP1age_del4 + day_wait_AI + 21*BIP1_AI5

#Fifth pregnancy simulation day
BIP1_simd_preg5 <- BIP1_simd_del4 + day_wait_AI + 21*BIP1_AI5


#A2.1.5. Fifth parity----
#Fifth parity age
BIP1age_del5 <- BIP1age_preg5 + preg_period

#Fifth parity simulation day
BIP1_simd_del5 <- BIP1_simd_preg5 + preg_period 

#BLV status of IP1 at fifth pregnancy
BIP1_BLV_del5 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_BLV_del5[i] <- ifelse(BIP1_BLV_del4[i] == 0, 
                             rbinom(1, 1, (pBLV4 - pBLV3)/(1 - pBLV3))*(1 + rbinom(1, 1, pL_BLV4)), 
                             ifelse(BIP1_BLV_del4[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV4 - pL_BLV3)/(1 - pL_BLV3)), 2))
}
BIP1_BLV_del5

#Mastitis at fifth parity
BIP1_mas5 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_mas5[i] <- rbinom(1, 1, pmasmat[1 + BIP1_BLV_del5[i], 5])
}
BIP1_mas5

#Fifth lactation period
BIP1_lac5_period <- ifelse(BIP1_mas5==1, ifelse(BIP1_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
BIP1_lac5_yield <- ifelse(BIP1_mas5==1, ifelse(BIP1_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
BIP1age_lac5e <- BIP1age_del5 + BIP1_lac5_period 

#Fifth lactation end simulation day
BIP1_simd_lac5e <- BIP1_simd_del5 + BIP1_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
BIP1_lac5_income <- ifelse(BIP1_simd_del5 > sim_d, 0,
                           ifelse(BIP1_simd_lac5e < sim_discard, 0,
                                  ifelse(BIP1_simd_lac5e > sim_d, 
                                         BIP1_lac5_yield * milk_price * (sim_d - BIP1_simd_del5)/BIP1_lac5_period *
                                           (1 - BIP1_preg5_removed_cumulative),
                                         ifelse(BIP1_simd_del5 < sim_discard,
                                                BIP1_lac5_yield * milk_price *
                                                  (BIP1_simd_lac5e - sim_discard)/BIP1_lac5_period *
                                                  (1 - BIP1_preg5_removed_cumulative),
                                                BIP1_lac5_yield * milk_price * BIP1_lac5_period / milk_day *
                                                  (1 - BIP1_preg5_removed_cumulative)))))

#AI for sixth pregnancy
BIP1_AI6 <- numeric(IP1)
for (i in 1:IP1){
  BIP1_AI6[i] <- rpois(1, ifelse(BIP1_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
BIP1_AI6

#Removal of cattle
BIP1_preg6_removed <- ifelse(BIP1_AI6 > nAI_remove-1, 1, 
                             ifelse(BIP1_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
BIP1_preg6_removed_cumulative <- ifelse(BIP1_preg5_removed_cumulative==1, 1, 
                                        ifelse(BIP1_AI6 > nAI_remove-1, 1, 
                                               ifelse(BIP1_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
BIP1age_preg6 <- BIP1age_del5 + day_wait_AI + 21*BIP1_AI6

#Sixth pregnancy simulation day
BIP1_simd_preg6 <- BIP1_simd_del5 + day_wait_AI + 21*BIP1_AI6


#A2.2. IP2 simulation----
#A2.2.3. Third parity----
#Simulation day of third parity
BIP2_simd_del3 <- BIP2_day_del3 - BIP2age

#BLV status of IP2 at third pregnancy
BIP2_BLV_del3 <- numeric(IP2)
for (i in 1:IP2){
  BIP2_BLV_del3[i] <- ifelse(BIP2_BLV[i] == 0, 
                             rbinom(1, 1, (pBLV2 - pBLV1)/(1 - pBLV1))*(1 + rbinom(1, 1, pL_BLV2)), 
                             ifelse(BIP2_BLV[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV2 - pL_BLV1)/(1 - pL_BLV1)), 2))
}
BIP2_BLV_del3

#Mastitis at third parity
BIP2_mas3 <- numeric(IP2)
for (i in 1:IP2){
  BIP2_mas3[i] <- rbinom(1, 1, pmasmat[1 + BIP2_BLV_del3[i], 3])
}
BIP2_mas3

#Third lactation period
BIP2_lac3_period <- ifelse(BIP2_mas3==1, ifelse(BIP2_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
BIP2_lac3_yield <- ifelse(BIP2_mas3==1, ifelse(BIP2_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
BIP2age_lac3e <- BIP2_day_del3 + BIP2_lac3_period 

#Third lactation end simulation day
BIP2_simd_lac3e <- BIP2_simd_del3 + BIP2_lac3_period

#Third lactation income #linear score 5 milk price not considered
BIP2_lac3_income <- ifelse(BIP2_simd_del3 > sim_d, 0,
                           ifelse(BIP2_simd_lac3e < sim_discard, 0,
                                  ifelse(BIP2_simd_lac3e > sim_d, 
                                         BIP2_lac3_yield * milk_price * (sim_d - BIP2_simd_del3)/BIP2_lac3_period,
                                         ifelse(BIP2_simd_del3 < sim_discard,
                                                BIP2_lac3_yield * milk_price *
                                                  (BIP2_simd_lac3e - sim_discard)/BIP2_lac3_period,
                                                BIP2_lac3_yield * milk_price * BIP2_lac3_period / milk_day))))

#AI for fourth pregnancy
BIP2_AI4 <- numeric(IP2)
for (i in 1:IP2){
  BIP2_AI4[i] <- rpois(1, ifelse(BIP2_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
BIP2_AI4

#Removal of cattle
BIP2_preg4_removed <- ifelse(BIP2_AI4 > nAI_remove-1, 1, 
                             ifelse(BIP2_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))

#Fourth pregnancy age
BIP2age_preg4 <- BIP2_day_del3 + day_wait_AI + 21*BIP2_AI4

#Fourth pregnancy simulation day
BIP2_simd_preg4 <- BIP2_simd_del3 + day_wait_AI + 21*BIP2_AI4


#A2.2.4. Fourth parity----
#Fourth parity age
BIP2age_del4 <- BIP2age_preg4 + preg_period

#Fourth parity simulation day
BIP2_simd_del4 <- BIP2_simd_preg4 + preg_period 

#BLV status of IP1 at fourth pregnancy
BIP2_BLV_del4 <- numeric(IP2)
for (i in 1:IP2){
  BIP2_BLV_del4[i] <- ifelse(BIP2_BLV_del3[i] == 0, 
                             rbinom(1, 1, (pBLV3 - pBLV2)/(1 - pBLV2))*(1 + rbinom(1, 1, pL_BLV3)), 
                             ifelse(BIP2_BLV_del3[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV3 - pL_BLV2)/(1 - pL_BLV2)), 2))
}
BIP2_BLV_del4

#Mastitis at fourth parity
BIP2_mas4 <- numeric(IP2)
for (i in 1:IP2){
  BIP2_mas4[i] <- rbinom(1, 1, pmasmat[1 + BIP2_BLV_del4[i], 4])
}
BIP2_mas4

#Fourth lactation period
BIP2_lac4_period <- ifelse(BIP2_mas4==1, ifelse(BIP2_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
BIP2_lac4_yield <- ifelse(BIP2_mas4==1, ifelse(BIP2_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
BIP2age_lac4e <- BIP2age_del4 + BIP2_lac4_period 

#Fourth lactation end simulation day
BIP2_simd_lac4e <- BIP2_simd_del4 + BIP2_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
BIP2_lac4_income <- ifelse(BIP2_simd_del4 > sim_d, 0,
                           ifelse(BIP2_simd_lac4e < sim_discard, 0,
                                  ifelse(BIP2_simd_lac4e > sim_d, 
                                         BIP2_lac4_yield * milk_price * (sim_d - BIP2_simd_del4)/BIP2_lac4_period *
                                           (1 - BIP2_preg4_removed),
                                         ifelse(BIP2_simd_del4 < sim_discard,
                                                BIP2_lac4_yield * milk_price *
                                                  (BIP2_simd_lac4e - sim_discard)/BIP2_lac4_period *
                                                  (1 - BIP2_preg4_removed),
                                                BIP2_lac4_yield * milk_price * BIP2_lac4_period / milk_day *
                                                  (1 - BIP2_preg4_removed)))))

#AI for fifth pregnancy
BIP2_AI5 <- numeric(IP2)
for (i in 1:IP2){
  BIP2_AI5[i] <- rpois(1, ifelse(BIP2_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
BIP2_AI5

#Removal of cattle
BIP2_preg5_removed <- ifelse(BIP2_AI5 > nAI_remove-1, 1, 
                             ifelse(BIP2_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
BIP2_preg5_removed_cumulative <- ifelse(BIP2_preg4_removed==1, 1, 
                                        ifelse(BIP2_AI5 > nAI_remove-1, 1, 
                                               ifelse(BIP2_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
BIP2age_preg5 <- BIP2age_del4 + day_wait_AI + 21*BIP2_AI5

#Fifth pregnancy simulation day
BIP2_simd_preg5 <- BIP2_simd_del4 + day_wait_AI + 21*BIP2_AI5


#A2.2.5. Fifth parity----
#Fifth parity age
BIP2age_del5 <- BIP2age_preg5 + preg_period

#Fifth parity simulation day
BIP2_simd_del5 <- BIP2_simd_preg5 + preg_period 

#BLV status of IP2 at fifth pregnancy
BIP2_BLV_del5 <- numeric(IP2)
for (i in 1:IP2){
  BIP2_BLV_del5[i] <- ifelse(BIP2_BLV_del4[i] == 0, 
                             rbinom(1, 1, (pBLV4 - pBLV3)/(1 - pBLV3))*(1 + rbinom(1, 1, pL_BLV4)), 
                             ifelse(BIP2_BLV_del4[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV4 - pL_BLV3)/(1 - pL_BLV3)), 2))
}
BIP2_BLV_del5

#Mastitis at fifth parity
BIP2_mas5 <- numeric(IP2)
for (i in 1:IP2){
  BIP2_mas5[i] <- rbinom(1, 1, pmasmat[1 + BIP2_BLV_del5[i], 5])
}
BIP2_mas5

#Fifth lactation period
BIP2_lac5_period <- ifelse(BIP2_mas5==1, ifelse(BIP2_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
BIP2_lac5_yield <- ifelse(BIP2_mas5==1, ifelse(BIP2_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
BIP2age_lac5e <- BIP2age_del5 + BIP2_lac5_period 

#Fifth lactation end simulation day
BIP2_simd_lac5e <- BIP2_simd_del5 + BIP2_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
BIP2_lac5_income <- ifelse(BIP2_simd_del5 > sim_d, 0,
                           ifelse(BIP2_simd_lac5e < sim_discard, 0,
                                  ifelse(BIP2_simd_lac5e > sim_d, 
                                         BIP2_lac5_yield * milk_price * (sim_d - BIP2_simd_del5)/BIP2_lac5_period *
                                           (1 - BIP2_preg5_removed_cumulative),
                                         ifelse(BIP2_simd_del5 < sim_discard,
                                                BIP2_lac5_yield * milk_price *
                                                  (BIP2_simd_lac5e - sim_discard)/BIP2_lac5_period *
                                                  (1 - BIP2_preg5_removed_cumulative),
                                                BIP2_lac5_yield * milk_price * BIP2_lac5_period / milk_day *
                                                  (1 - BIP2_preg5_removed_cumulative)))))

#AI for sixth pregnancy
BIP2_AI6 <- numeric(IP2)
for (i in 1:IP2){
  BIP2_AI6[i] <- rpois(1, ifelse(BIP2_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
BIP2_AI6

#Removal of cattle
BIP2_preg6_removed <- ifelse(BIP2_AI6 > nAI_remove-1, 1, 
                             ifelse(BIP2_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
BIP2_preg6_removed_cumulative <- ifelse(BIP2_preg5_removed_cumulative==1, 1, 
                                        ifelse(BIP2_AI6 > nAI_remove-1, 1, 
                                               ifelse(BIP2_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
BIP2age_preg6 <- BIP2age_del5 + day_wait_AI + 21*BIP2_AI6

#Sixth pregnancy simulation day
BIP2_simd_preg6 <- BIP2_simd_del5 + day_wait_AI + 21*BIP2_AI6


#A2.3. IP3 simulation----
#A2.3.4. Fourth parity----
#Simulation day of fourth parity
BIP3_simd_del4 <- BIP3_day_del4 - BIP3age

#BLV status of IP3 at fourth pregnancy
BIP3_BLV_del4 <- numeric(IP3)
for (i in 1:IP3){
  BIP3_BLV_del4[i] <- ifelse(BIP3_BLV[i] == 0, 
                             rbinom(1, 1, (pBLV3 - pBLV2)/(1 - pBLV2))*(1 + rbinom(1, 1, pL_BLV3)), 
                             ifelse(BIP3_BLV[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV3 - pL_BLV2)/(1 - pL_BLV2)), 2))
}
BIP3_BLV_del4

#Mastitis at fourth parity
BIP3_mas4 <- numeric(IP3)
for (i in 1:IP3){
  BIP3_mas4[i] <- rbinom(1, 1, pmasmat[1 + BIP3_BLV_del4[i], 4])
}
BIP3_mas4

#Fourth lactation period
BIP3_lac4_period <- ifelse(BIP3_mas4==1, ifelse(BIP3_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
BIP3_lac4_yield <- ifelse(BIP3_mas4==1, ifelse(BIP3_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
BIP3age_lac4e <- BIP3_day_del4 + BIP3_lac4_period 

#Fourth lactation end simulation day
BIP3_simd_lac4e <- BIP3_simd_del4 + BIP3_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
BIP3_lac4_income <- ifelse(BIP3_simd_del4 > sim_d, 0,
                           ifelse(BIP3_simd_lac4e < sim_discard, 0,
                                  ifelse(BIP3_simd_lac4e > sim_d, 
                                         BIP3_lac4_yield * milk_price * (sim_d - BIP3_simd_del4)/BIP3_lac4_period,
                                         ifelse(BIP3_simd_del4 < sim_discard,
                                                BIP3_lac4_yield * milk_price *
                                                  (BIP3_simd_lac4e - sim_discard)/BIP3_lac4_period,
                                                BIP3_lac4_yield * milk_price * BIP3_lac4_period / milk_day))))

#AI for fifth pregnancy
BIP3_AI5 <- numeric(IP3)
for (i in 1:IP3){
  BIP3_AI5[i] <- rpois(1, ifelse(BIP3_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
BIP3_AI5

#Removal of cattle
BIP3_preg5_removed <- ifelse(BIP3_AI5 > nAI_remove-1, 1, 
                             ifelse(BIP3_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))

#Fifth pregnancy age
BIP3age_preg5 <- BIP3_day_del4 + day_wait_AI + 21*BIP3_AI5

#Fifth pregnancy simulation day
BIP3_simd_preg5 <- BIP3_simd_del4 + day_wait_AI + 21*BIP3_AI5


#A2.3.5. Fifth parity----
#Fifth parity age
BIP3age_del5 <- BIP3age_preg5 + preg_period

#Fifth parity simulation day
BIP3_simd_del5 <- BIP3_simd_preg5 + preg_period 

#BLV status of IP3 at fifth pregnancy
BIP3_BLV_del5 <- numeric(IP3)
for (i in 1:IP3){
  BIP3_BLV_del5[i] <- ifelse(BIP3_BLV_del4[i] == 0, 
                             rbinom(1, 1, (pBLV4 - pBLV3)/(1 - pBLV3))*(1 + rbinom(1, 1, pL_BLV4)), 
                             ifelse(BIP3_BLV_del4[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV4 - pL_BLV3)/(1 - pL_BLV3)), 2))
}
BIP3_BLV_del5

#Mastitis at fifth parity
BIP3_mas5 <- numeric(IP3)
for (i in 1:IP3){
  BIP3_mas5[i] <- rbinom(1, 1, pmasmat[1 + BIP3_BLV_del5[i], 5])
}
BIP3_mas5

#Fifth lactation period
BIP3_lac5_period <- ifelse(BIP3_mas5==1, ifelse(BIP3_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
BIP3_lac5_yield <- ifelse(BIP3_mas5==1, ifelse(BIP3_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
BIP3age_lac5e <- BIP3age_del5 + BIP3_lac5_period 

#Fifth lactation end simulation day
BIP3_simd_lac5e <- BIP3_simd_del5 + BIP3_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
BIP3_lac5_income <- ifelse(BIP3_simd_del5 > sim_d, 0,
                           ifelse(BIP3_simd_lac5e < sim_discard, 0,
                                  ifelse(BIP3_simd_lac5e > sim_d, 
                                         BIP3_lac5_yield * milk_price * (sim_d - BIP3_simd_del5)/BIP3_lac5_period * 
                                           (1 - BIP3_preg5_removed),
                                         ifelse(BIP3_simd_del5 < sim_discard,
                                                BIP3_lac5_yield * milk_price *
                                                  (BIP3_simd_lac5e - sim_discard)/BIP3_lac5_period * 
                                                  (1 - BIP3_preg5_removed),
                                                BIP3_lac5_yield * milk_price * BIP3_lac5_period / milk_day) * 
                                           (1 - BIP3_preg5_removed))))

#AI for sixth pregnancy
BIP3_AI6 <- numeric(IP3)
for (i in 1:IP3){
  BIP3_AI6[i] <- rpois(1, ifelse(BIP3_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
BIP3_AI6

#Removal of cattle
BIP3_preg6_removed <- ifelse(BIP3_AI6 > nAI_remove-1, 1, 
                             ifelse(BIP3_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
BIP3_preg6_removed_cumulative <- ifelse(BIP3_preg5_removed==1, 1, 
                                        ifelse(BIP3_AI6 > nAI_remove-1, 1, 
                                               ifelse(BIP3_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
BIP3age_preg6 <- BIP3age_del5 + day_wait_AI + 21*BIP3_AI6

#Sixth pregnancy simulation day
BIP3_simd_preg6 <- BIP3_simd_del5 + day_wait_AI + 21*BIP3_AI6


#A2.4. IP4 simulation----
#A2.4.5. Fifth parity----
#Simulation day of fifth parity
BIP4_simd_del5 <- BIP4_day_del5 - BIP4age

#BLV status of IP4 at fifth pregnancy
BIP4_BLV_del5 <- numeric(IP4)
for (i in 1:IP4){
  BIP4_BLV_del5[i] <- ifelse(BIP4_BLV[i] == 0, 
                             rbinom(1, 1, (pBLV4 - pBLV3)/(1 - pBLV3))*(1 + rbinom(1, 1, pL_BLV4)), 
                             ifelse(BIP4_BLV[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV4 - pL_BLV3)/(1 - pL_BLV3)), 2))
}
BIP4_BLV_del5

#Mastitis at fifth parity
BIP4_mas5 <- numeric(IP4)
for (i in 1:IP4){
  BIP4_mas5[i] <- rbinom(1, 1, pmasmat[1 + BIP4_BLV_del5[i], 5])
}
BIP4_mas5

#Fifth lactation period
BIP4_lac5_period <- ifelse(BIP4_mas5==1, ifelse(BIP4_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
BIP4_lac5_yield <- ifelse(BIP4_mas5==1, ifelse(BIP4_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
BIP4age_lac5e <- BIP4_day_del5 + BIP4_lac5_period 

#Fifth lactation end simulation day
BIP4_simd_lac5e <- BIP4_simd_del5 + BIP4_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
BIP4_lac5_income <- ifelse(BIP4_simd_del5 > sim_d, 0,
                           ifelse(BIP4_simd_lac5e < sim_discard, 0,
                                  ifelse(BIP4_simd_lac5e > sim_d, 
                                         BIP4_lac5_yield * milk_price * (sim_d - BIP4_simd_del5)/BIP4_lac5_period,
                                         ifelse(BIP4_simd_del5 < sim_discard,
                                                BIP4_lac5_yield * milk_price *
                                                  (BIP4_simd_lac5e - sim_discard)/BIP4_lac5_period,
                                                BIP4_lac5_yield * milk_price * BIP4_lac5_period / milk_day))))

#AI for sixth pregnancy
BIP4_AI6 <- numeric(IP4)
for (i in 1:IP4){
  BIP4_AI6[i] <- rpois(1, ifelse(BIP4_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
BIP4_AI6

#Removal of cattle
BIP4_preg6_removed <- ifelse(BIP4_AI6 > nAI_remove-1, 1, 
                             ifelse(BIP4_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))

#Sixth pregnancy age
BIP4age_preg6 <- BIP4_day_del5 + day_wait_AI + 21*BIP4_AI6

#Sixth pregnancy simulation day
BIP4_simd_preg6 <- BIP4_simd_del5 + day_wait_AI + 21*BIP4_AI6


#A2.5. IP5 simulation----
#This model assumes that after milking fifth parity, cattle are culled. 
#By the end of 365 days discarding time, cattle would not be in the herd. Therefore parity five is not simulated.


#A2.6. New borne cows simulation----
#A2.6.1. Borne in year 1----
#A2.6.1.1. First parity----
#Simulation day of first parity
BNB1_simd_del1 <- DB_NB1 + BNB1_day_del1

#Mastitis at first parity
BNB1_mas1 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_mas1[i] <- rbinom(1, 1, pmasmat[1 + BNB1_BLV[i], 1])
}
BNB1_mas1

#First lactation period
BNB1_lac1_period <- ifelse(BNB1_mas1==1, ifelse(BNB1_BLV==2, days_mas_L, days_mas_SI), milk_day)

#First lactation milk yield
BNB1_lac1_yield <- ifelse(BNB1_mas1==1, ifelse(BNB1_BLV==2, yield_P1 * days_mas_L / milk_day, 
                                               yield_P1 * days_mas_SI / milk_day), yield_P1)

#First lactation end age
BNB1age_lac1e <- BNB1_day_del1 + BNB1_lac1_period 

#First lactation end simulation day
BNB1_simd_lac1e <- BNB1_simd_del1 + BNB1_lac1_period

#First lactation income #linear score 5 milk price not considered
BNB1_lac1_income <- ifelse(BNB1_simd_del1 > sim_d, 0,
                           ifelse(BNB1_simd_lac1e < sim_discard, 0,
                                  ifelse(BNB1_simd_lac1e > sim_d, 
                                         BNB1_lac1_yield * milk_price * (sim_d - BNB1_simd_del1)/BNB1_lac1_period,
                                         ifelse(BNB1_simd_del1 < sim_discard,
                                                BNB1_lac1_yield * milk_price *
                                                  (BNB1_simd_lac1e - sim_discard)/BNB1_lac1_period,
                                                BNB1_lac1_yield * milk_price * BNB1_lac1_period / milk_day))))

#AI for second pregnancy
BNB1_AI2 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_AI2[i] <- rpois(1, ifelse(BNB1_mas1[i]==1, nAI_masmat[1,1], nAI_masmat[2,1]))
}
BNB1_AI2

#Removal of cattle
BNB1_preg2_removed <- ifelse(BNB1_AI2 > nAI_remove-1, 1, 
                             ifelse(BNB1_mas1 == 1, rbinom(1, 1, cull_mas1), 
                                    rbinom(1, 1, cull_nomas1)))

#Second pregnancy age
BNB1age_preg2 <- BNB1_day_del1 + day_wait_AI + 21*BNB1_AI2

#Second pregnancy simulation day
BNB1_simd_preg2 <- BNB1_simd_del1 + day_wait_AI + 21*BNB1_AI2


#A2.6.1.2. Second parity----
#Second parity age
BNB1age_del2 <- BNB1age_preg2 + preg_period

#Second parity simulation day
BNB1_simd_del2 <- BNB1_simd_preg2 + preg_period 

#BLV status of NB1 at second pregnancy
BNB1_BLV_del2 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_BLV_del2[i] <- ifelse(BNB1_BLV[i] == 0, 
                             rbinom(1, 1, (pBLV1 - pBLV0)/(1 - pBLV0))*(1 + rbinom(1, 1, pL_BLV1)), 
                             ifelse(BNB1_BLV[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV1 - pL_BLV0)/(1 - pL_BLV0)), 2))
}

#Mastitis at second parity
BNB1_mas2 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_mas2[i] <- rbinom(1, 1, pmasmat[1 + BNB1_BLV_del2[i], 2])
}
BNB1_mas2

#Second lactation period
BNB1_lac2_period <- ifelse(BNB1_mas2==1, ifelse(BNB1_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
BNB1_lac2_yield <- ifelse(BNB1_mas2==1, ifelse(BNB1_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
BNB1age_lac2e <- BNB1age_del2 + BNB1_lac2_period 

#Second lactation end simulation day
BNB1_simd_lac2e <- BNB1_simd_del2 + BNB1_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
BNB1_lac2_income <- ifelse(BNB1_simd_del2 > sim_d, 0,
                           ifelse(BNB1_simd_lac2e < sim_discard, 0,
                                  ifelse(BNB1_simd_lac2e > sim_d, 
                                         BNB1_lac2_yield * milk_price * (sim_d - BNB1_simd_del2)/BNB1_lac2_period *
                                           (1 - BNB1_preg2_removed),
                                         ifelse(BNB1_simd_del2 < sim_discard,
                                                BNB1_lac2_yield * milk_price *
                                                  (BNB1_simd_lac2e - sim_discard)/BNB1_lac2_period *
                                                  (1 - BNB1_preg2_removed),
                                                BNB1_lac2_yield * milk_price * BNB1_lac2_period / milk_day *
                                                  (1 - BNB1_preg2_removed)))))

#AI for third pregnancy
BNB1_AI3 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_AI3[i] <- rpois(1, ifelse(BNB1_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
BNB1_AI3

#Removal of cattle
BNB1_preg3_removed <- ifelse(BNB1_AI3 > nAI_remove-1, 1, 
                             ifelse(BNB1_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))
BNB1_preg3_removed_cumulative <- ifelse(BNB1_preg2_removed==1, 1, 
                                        ifelse(BNB1_AI3 > nAI_remove-1, 1, 
                                               ifelse(BNB1_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                                      rbinom(1, 1, cull_nomas2))))

#Third pregnancy age
BNB1age_preg3 <- BNB1age_del2 + day_wait_AI + 21*BNB1_AI3

#Third pregnancy simulation day
BNB1_simd_preg3 <- BNB1_simd_del2 + day_wait_AI + 21*BNB1_AI3


#A2.6.1.3. Third parity----
#Third parity age
BNB1age_del3 <- BNB1age_preg3 + preg_period

#Third parity simulation day
BNB1_simd_del3 <- BNB1_simd_preg3 + preg_period 

#BLV status of NB1 at third pregnancy
BNB1_BLV_del3 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_BLV_del3[i] <- ifelse(BNB1_BLV_del2[i] == 0, 
                             rbinom(1, 1, (pBLV2 - pBLV1)/(1 - pBLV1))*(1 + rbinom(1, 1, pL_BLV2)), 
                             ifelse(BNB1_BLV_del2[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV2 - pL_BLV1)/(1 - pL_BLV1)), 2))
}
BNB1_BLV_del3

#Mastitis at third parity
BNB1_mas3 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_mas3[i] <- rbinom(1, 1, pmasmat[1 + BNB1_BLV_del3[i], 3])
}
BNB1_mas3

#Third lactation period
BNB1_lac3_period <- ifelse(BNB1_mas3==1, ifelse(BNB1_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
BNB1_lac3_yield <- ifelse(BNB1_mas3==1, ifelse(BNB1_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
BNB1age_lac3e <- BNB1age_del3 + BNB1_lac3_period 

#Third lactation end simulation day
BNB1_simd_lac3e <- BNB1_simd_del3 + BNB1_lac3_period

#Third lactation income #linear score 5 milk price not considered
BNB1_lac3_income <- ifelse(BNB1_simd_del3 > sim_d, 0,
                           ifelse(BNB1_simd_lac3e < sim_discard, 0,
                                  ifelse(BNB1_simd_lac3e > sim_d, 
                                         BNB1_lac3_yield * milk_price * (sim_d - BNB1_simd_del3)/BNB1_lac3_period *
                                           (1 - BNB1_preg3_removed_cumulative),
                                         ifelse(BNB1_simd_del3 < sim_discard,
                                                BNB1_lac3_yield * milk_price *
                                                  (BNB1_simd_lac3e - sim_discard)/BNB1_lac3_period *
                                                  (1 - BNB1_preg3_removed_cumulative),
                                                BNB1_lac3_yield * milk_price * BNB1_lac3_period / milk_day *
                                                  (1 - BNB1_preg3_removed_cumulative)))))

#AI for fourth pregnancy
BNB1_AI4 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_AI4[i] <- rpois(1, ifelse(BNB1_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
BNB1_AI4

#Removal of cattle
BNB1_preg4_removed <- ifelse(BNB1_AI4 > nAI_remove-1, 1, 
                             ifelse(BNB1_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
BNB1_preg4_removed_cumulative <- ifelse(BNB1_preg3_removed_cumulative==1, 1, 
                                        ifelse(BNB1_AI4 > nAI_remove-1, 1, 
                                               ifelse(BNB1_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
BNB1age_preg4 <- BNB1age_del3 + day_wait_AI + 21*BNB1_AI4

#Fourth pregnancy simulation day
BNB1_simd_preg4 <- BNB1_simd_del3 + day_wait_AI + 21*BNB1_AI4


#A2.6.1.4. Fourth parity----
#Fourth parity age
BNB1age_del4 <- BNB1age_preg4 + preg_period

#Fourth parity simulation day
BNB1_simd_del4 <- BNB1_simd_preg4 + preg_period 

#BLV status of NB1 at fourth pregnancy
BNB1_BLV_del4 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_BLV_del4[i] <- ifelse(BNB1_BLV_del3[i] == 0, 
                             rbinom(1, 1, (pBLV3 - pBLV2)/(1 - pBLV2))*(1 + rbinom(1, 1, pL_BLV3)), 
                             ifelse(BNB1_BLV_del3[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV3 - pL_BLV2)/(1 - pL_BLV2)), 2))
}
BNB1_BLV_del4

#Mastitis at fourth parity
BNB1_mas4 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_mas4[i] <- rbinom(1, 1, pmasmat[1 + BNB1_BLV_del4[i], 4])
}
BNB1_mas4

#Fourth lactation period
BNB1_lac4_period <- ifelse(BNB1_mas4==1, ifelse(BNB1_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
BNB1_lac4_yield <- ifelse(BNB1_mas4==1, ifelse(BNB1_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
BNB1age_lac4e <- BNB1age_del4 + BNB1_lac4_period 

#Fourth lactation end simulation day
BNB1_simd_lac4e <- BNB1_simd_del4 + BNB1_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
BNB1_lac4_income <- ifelse(BNB1_simd_del4 > sim_d, 0,
                           ifelse(BNB1_simd_lac4e < sim_discard, 0,
                                  ifelse(BNB1_simd_lac4e > sim_d, 
                                         BNB1_lac4_yield * milk_price * (sim_d - BNB1_simd_del4)/BNB1_lac4_period *
                                           (1 - BNB1_preg4_removed_cumulative),
                                         ifelse(BNB1_simd_del4 < sim_discard,
                                                BNB1_lac4_yield * milk_price *
                                                  (BNB1_simd_lac4e - sim_discard)/BNB1_lac4_period *
                                                  (1 - BNB1_preg4_removed_cumulative),
                                                BNB1_lac4_yield * milk_price * BNB1_lac4_period / milk_day *
                                                  (1 - BNB1_preg4_removed_cumulative)))))


#AI for fifth pregnancy
BNB1_AI5 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_AI5[i] <- rpois(1, ifelse(BNB1_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
BNB1_AI5

#Removal of cattle
BNB1_preg5_removed <- ifelse(BNB1_AI5 > nAI_remove-1, 1, 
                             ifelse(BNB1_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
BNB1_preg5_removed_cumulative <- ifelse(BNB1_preg4_removed_cumulative==1, 1, 
                                        ifelse(BNB1_AI5 > nAI_remove-1, 1, 
                                               ifelse(BNB1_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
BNB1age_preg5 <- BNB1age_del4 + day_wait_AI + 21*BNB1_AI5

#Fifth pregnancy simulation day
BNB1_simd_preg5 <- BNB1_simd_del4 + day_wait_AI + 21*BNB1_AI5


#A2.6.1.5. Fifth parity----
#Fifth parity age
BNB1age_del5 <- BNB1age_preg5 + preg_period

#Fifth parity simulation day
BNB1_simd_del5 <- BNB1_simd_preg5 + preg_period 

#BLV status of NB1 at fifth pregnancy
BNB1_BLV_del5 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_BLV_del5[i] <- ifelse(BNB1_BLV_del4[i] == 0, 
                             rbinom(1, 1, (pBLV4 - pBLV3)/(1 - pBLV3))*(1 + rbinom(1, 1, pL_BLV4)), 
                             ifelse(BNB1_BLV_del4[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV4 - pL_BLV3)/(1 - pL_BLV3)), 2))
}
BNB1_BLV_del5

#Mastitis at fifth parity
BNB1_mas5 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_mas5[i] <- rbinom(1, 1, pmasmat[1 + BNB1_BLV_del5[i], 5])
}
BNB1_mas5

#Fifth lactation period
BNB1_lac5_period <- ifelse(BNB1_mas5==1, ifelse(BNB1_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
BNB1_lac5_yield <- ifelse(BNB1_mas5==1, ifelse(BNB1_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
BNB1age_lac5e <- BNB1age_del5 + BNB1_lac5_period 

#Fifth lactation end simulation day
BNB1_simd_lac5e <- BNB1_simd_del5 + BNB1_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
BNB1_lac5_income <- ifelse(BNB1_simd_del5 > sim_d, 0,
                           ifelse(BNB1_simd_lac5e < sim_discard, 0,
                                  ifelse(BNB1_simd_lac5e > sim_d, 
                                         BNB1_lac5_yield * milk_price * (sim_d - BNB1_simd_del5)/BNB1_lac5_period *
                                           (1 - BNB1_preg5_removed_cumulative),
                                         ifelse(BNB1_simd_del5 < sim_discard,
                                                BNB1_lac5_yield * milk_price *
                                                  (BNB1_simd_lac5e - sim_discard)/BNB1_lac5_period *
                                                  (1 - BNB1_preg5_removed_cumulative),
                                                BNB1_lac5_yield * milk_price * BNB1_lac5_period / milk_day *
                                                  (1 - BNB1_preg5_removed_cumulative)))))

#AI for sixth pregnancy
BNB1_AI6 <- numeric(NB1)
for (i in 1:NB1){
  BNB1_AI6[i] <- rpois(1, ifelse(BNB1_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
BNB1_AI6

#Removal of cattle
BNB1_preg6_removed <- ifelse(BNB1_AI6 > nAI_remove-1, 1, 
                             ifelse(BNB1_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
BNB1_preg6_removed_cumulative <- ifelse(BNB1_preg5_removed_cumulative==1, 1, 
                                        ifelse(BNB1_AI6 > nAI_remove-1, 1, 
                                               ifelse(BNB1_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
BNB1age_preg6 <- BNB1age_del5 + day_wait_AI + 21*BNB1_AI6

#Sixth pregnancy simulation day
BNB1_simd_preg6 <- BNB1_simd_del5 + day_wait_AI + 21*BNB1_AI6


#A2.6.2. Borne in year 2----
#A2.6.2.1. First parity----
#Simulation day of first parity
BNB2_simd_del1 <- DB_NB2 + BNB2_day_del1

#Mastitis at first parity
BNB2_mas1 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_mas1[i] <- rbinom(1, 1, pmasmat[1 + BNB2_BLV[i], 1])
}
BNB2_mas1

#First lactation period
BNB2_lac1_period <- ifelse(BNB2_mas1==1, ifelse(BNB2_BLV==2, days_mas_L, days_mas_SI), milk_day)

#First lactation milk yield
BNB2_lac1_yield <- ifelse(BNB2_mas1==1, ifelse(BNB2_BLV==2, yield_P1 * days_mas_L / milk_day, 
                                               yield_P1 * days_mas_SI / milk_day), yield_P1)

#First lactation end age
BNB2age_lac1e <- BNB2_day_del1 + BNB2_lac1_period 

#First lactation end simulation day
BNB2_simd_lac1e <- BNB2_simd_del1 + BNB2_lac1_period

#First lactation income #linear score 5 milk price not considered
BNB2_lac1_income <- ifelse(BNB2_simd_del1 > sim_d, 0,
                           ifelse(BNB2_simd_lac1e < sim_discard, 0,
                                  ifelse(BNB2_simd_lac1e > sim_d, 
                                         BNB2_lac1_yield * milk_price * (sim_d - BNB2_simd_del1)/BNB2_lac1_period,
                                         ifelse(BNB2_simd_del1 < sim_discard,
                                                BNB2_lac1_yield * milk_price *
                                                  (BNB2_simd_lac1e - sim_discard)/BNB2_lac1_period,
                                                BNB2_lac1_yield * milk_price * BNB2_lac1_period / milk_day))))

#AI for second pregnancy
BNB2_AI2 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_AI2[i] <- rpois(1, ifelse(BNB2_mas1[i]==1, nAI_masmat[1,1], nAI_masmat[2,1]))
}
BNB2_AI2

#Removal of cattle
BNB2_preg2_removed <- ifelse(BNB2_AI2 > nAI_remove-1, 1, 
                             ifelse(BNB2_mas1 == 1, rbinom(1, 1, cull_mas1), 
                                    rbinom(1, 1, cull_nomas1)))

#Second pregnancy age
BNB2age_preg2 <- BNB2_day_del1 + day_wait_AI + 21*BNB2_AI2

#Second pregnancy simulation day
BNB2_simd_preg2 <- BNB2_simd_del1 + day_wait_AI + 21*BNB2_AI2


#A2.6.2.2. Second parity----
#Second parity age
BNB2age_del2 <- BNB2age_preg2 + preg_period

#Second parity simulation day
BNB2_simd_del2 <- BNB2_simd_preg2 + preg_period 

#BLV status of NB1 at second pregnancy
BNB2_BLV_del2 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_BLV_del2[i] <- ifelse(BNB2_BLV[i] == 0, 
                             rbinom(1, 1, (pBLV1 - pBLV0)/(1 - pBLV0))*(1 + rbinom(1, 1, pL_BLV1)), 
                             ifelse(BNB2_BLV[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV1 - pL_BLV0)/(1 - pL_BLV0)), 2))
}

#Mastitis at second parity
BNB2_mas2 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_mas2[i] <- rbinom(1, 1, pmasmat[1 + BNB2_BLV_del2[i], 2])
}
BNB2_mas2

#Second lactation period
BNB2_lac2_period <- ifelse(BNB2_mas2==1, ifelse(BNB2_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
BNB2_lac2_yield <- ifelse(BNB2_mas2==1, ifelse(BNB2_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
BNB2age_lac2e <- BNB2age_del2 + BNB2_lac2_period 

#Second lactation end simulation day
BNB2_simd_lac2e <- BNB2_simd_del2 + BNB2_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
BNB2_lac2_income <- ifelse(BNB2_simd_del2 > sim_d, 0,
                           ifelse(BNB2_simd_lac2e < sim_discard, 0,
                                  ifelse(BNB2_simd_lac2e > sim_d, 
                                         BNB2_lac2_yield * milk_price * (sim_d - BNB2_simd_del2)/BNB2_lac2_period *
                                           (1 - BNB2_preg2_removed),
                                         ifelse(BNB2_simd_del2 < sim_discard,
                                                BNB2_lac2_yield * milk_price *
                                                  (BNB2_simd_lac2e - sim_discard)/BNB2_lac2_period *
                                                  (1 - BNB2_preg2_removed),
                                                BNB2_lac2_yield * milk_price * BNB2_lac2_period / milk_day *
                                                  (1 - BNB2_preg2_removed)))))

#AI for third pregnancy
BNB2_AI3 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_AI3[i] <- rpois(1, ifelse(BNB2_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
BNB2_AI3

#Removal of cattle
BNB2_preg3_removed <- ifelse(BNB2_AI3 > nAI_remove-1, 1, 
                             ifelse(BNB2_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))
BNB2_preg3_removed_cumulative <- ifelse(BNB2_preg2_removed==1, 1, 
                                        ifelse(BNB2_AI3 > nAI_remove-1, 1, 
                                               ifelse(BNB2_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                                      rbinom(1, 1, cull_nomas2))))

#Third pregnancy age
BNB2age_preg3 <- BNB2age_del2 + day_wait_AI + 21*BNB2_AI3

#Third pregnancy simulation day
BNB2_simd_preg3 <- BNB2_simd_del2 + day_wait_AI + 21*BNB2_AI3


#A2.6.2.3. Third parity----
#Third parity age
BNB2age_del3 <- BNB2age_preg3 + preg_period

#Third parity simulation day
BNB2_simd_del3 <- BNB2_simd_preg3 + preg_period 

#BLV status of NB1 at third pregnancy
BNB2_BLV_del3 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_BLV_del3[i] <- ifelse(BNB2_BLV_del2[i] == 0, 
                             rbinom(1, 1, (pBLV2 - pBLV1)/(1 - pBLV1))*(1 + rbinom(1, 1, pL_BLV2)), 
                             ifelse(BNB2_BLV_del2[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV2 - pL_BLV1)/(1 - pL_BLV1)), 2))
}
BNB2_BLV_del3

#Mastitis at third parity
BNB2_mas3 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_mas3[i] <- rbinom(1, 1, pmasmat[1 + BNB2_BLV_del3[i], 3])
}
BNB2_mas3

#Third lactation period
BNB2_lac3_period <- ifelse(BNB2_mas3==1, ifelse(BNB2_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
BNB2_lac3_yield <- ifelse(BNB2_mas3==1, ifelse(BNB2_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
BNB2age_lac3e <- BNB2age_del3 + BNB2_lac3_period 

#Third lactation end simulation day
BNB2_simd_lac3e <- BNB2_simd_del3 + BNB2_lac3_period

#Third lactation income #linear score 5 milk price not considered
BNB2_lac3_income <- ifelse(BNB2_simd_del3 > sim_d, 0,
                           ifelse(BNB2_simd_lac3e < sim_discard, 0,
                                  ifelse(BNB2_simd_lac3e > sim_d, 
                                         BNB2_lac3_yield * milk_price * (sim_d - BNB2_simd_del3)/BNB2_lac3_period *
                                           (1 - BNB2_preg3_removed_cumulative),
                                         ifelse(BNB2_simd_del3 < sim_discard,
                                                BNB2_lac3_yield * milk_price *
                                                  (BNB2_simd_lac3e - sim_discard)/BNB2_lac3_period *
                                                  (1 - BNB2_preg3_removed_cumulative),
                                                BNB2_lac3_yield * milk_price * BNB2_lac3_period / milk_day *
                                                  (1 - BNB2_preg3_removed_cumulative)))))

#AI for fourth pregnancy
BNB2_AI4 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_AI4[i] <- rpois(1, ifelse(BNB2_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
BNB2_AI4

#Removal of cattle
BNB2_preg4_removed <- ifelse(BNB2_AI4 > nAI_remove-1, 1, 
                             ifelse(BNB2_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
BNB2_preg4_removed_cumulative <- ifelse(BNB2_preg3_removed_cumulative==1, 1, 
                                        ifelse(BNB2_AI4 > nAI_remove-1, 1, 
                                               ifelse(BNB2_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
BNB2age_preg4 <- BNB2age_del3 + day_wait_AI + 21*BNB2_AI4

#Fourth pregnancy simulation day
BNB2_simd_preg4 <- BNB2_simd_del3 + day_wait_AI + 21*BNB2_AI4


#A2.6.2.4. Fourth parity----
#Fourth parity age
BNB2age_del4 <- BNB2age_preg4 + preg_period

#Fourth parity simulation day
BNB2_simd_del4 <- BNB2_simd_preg4 + preg_period 

#BLV status of NB1 at fourth pregnancy
BNB2_BLV_del4 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_BLV_del4[i] <- ifelse(BNB2_BLV_del3[i] == 0, 
                             rbinom(1, 1, (pBLV3 - pBLV2)/(1 - pBLV2))*(1 + rbinom(1, 1, pL_BLV3)), 
                             ifelse(BNB2_BLV_del3[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV3 - pL_BLV2)/(1 - pL_BLV2)), 2))
}
BNB2_BLV_del4

#Mastitis at fourth parity
BNB2_mas4 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_mas4[i] <- rbinom(1, 1, pmasmat[1 + BNB2_BLV_del4[i], 4])
}
BNB2_mas4

#Fourth lactation period
BNB2_lac4_period <- ifelse(BNB2_mas4==1, ifelse(BNB2_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
BNB2_lac4_yield <- ifelse(BNB2_mas4==1, ifelse(BNB2_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
BNB2age_lac4e <- BNB2age_del4 + BNB2_lac4_period 

#Fourth lactation end simulation day
BNB2_simd_lac4e <- BNB2_simd_del4 + BNB2_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
BNB2_lac4_income <- ifelse(BNB2_simd_del4 > sim_d, 0,
                           ifelse(BNB2_simd_lac4e < sim_discard, 0,
                                  ifelse(BNB2_simd_lac4e > sim_d, 
                                         BNB2_lac4_yield * milk_price * (sim_d - BNB2_simd_del4)/BNB2_lac4_period *
                                           (1 - BNB2_preg4_removed_cumulative),
                                         ifelse(BNB2_simd_del4 < sim_discard,
                                                BNB2_lac4_yield * milk_price *
                                                  (BNB2_simd_lac4e - sim_discard)/BNB2_lac4_period *
                                                  (1 - BNB2_preg4_removed_cumulative),
                                                BNB2_lac4_yield * milk_price * BNB2_lac4_period / milk_day *
                                                  (1 - BNB2_preg4_removed_cumulative)))))


#AI for fifth pregnancy
BNB2_AI5 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_AI5[i] <- rpois(1, ifelse(BNB2_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
BNB2_AI5

#Removal of cattle
BNB2_preg5_removed <- ifelse(BNB2_AI5 > nAI_remove-1, 1, 
                             ifelse(BNB2_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
BNB2_preg5_removed_cumulative <- ifelse(BNB2_preg4_removed_cumulative==1, 1, 
                                        ifelse(BNB2_AI5 > nAI_remove-1, 1, 
                                               ifelse(BNB2_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
BNB2age_preg5 <- BNB2age_del4 + day_wait_AI + 21*BNB2_AI5

#Fifth pregnancy simulation day
BNB2_simd_preg5 <- BNB2_simd_del4 + day_wait_AI + 21*BNB2_AI5


#A2.6.2.5. Fifth parity----
#Fifth parity age
BNB2age_del5 <- BNB2age_preg5 + preg_period

#Fifth parity simulation day
BNB2_simd_del5 <- BNB2_simd_preg5 + preg_period 

#BLV status of IP0 at fifth pregnancy
BNB2_BLV_del5 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_BLV_del5[i] <- ifelse(BNB2_BLV_del4[i] == 0, 
                             rbinom(1, 1, (pBLV4 - pBLV3)/(1 - pBLV3))*(1 + rbinom(1, 1, pL_BLV4)), 
                             ifelse(BNB2_BLV_del4[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV4 - pL_BLV3)/(1 - pL_BLV3)), 2))
}
BNB2_BLV_del5

#Mastitis at fifth parity
BNB2_mas5 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_mas5[i] <- rbinom(1, 1, pmasmat[1 + BNB2_BLV_del5[i], 5])
}
BNB2_mas5

#Fifth lactation period
BNB2_lac5_period <- ifelse(BNB2_mas5==1, ifelse(BNB2_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
BNB2_lac5_yield <- ifelse(BNB2_mas5==1, ifelse(BNB2_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
BNB2age_lac5e <- BNB2age_del5 + BNB2_lac5_period 

#Fifth lactation end simulation day
BNB2_simd_lac5e <- BNB2_simd_del5 + BNB2_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
BNB2_lac5_income <- ifelse(BNB2_simd_del5 > sim_d, 0,
                           ifelse(BNB2_simd_lac5e < sim_discard, 0,
                                  ifelse(BNB2_simd_lac5e > sim_d, 
                                         BNB2_lac5_yield * milk_price * (sim_d - BNB2_simd_del5)/BNB2_lac5_period *
                                           (1 - BNB2_preg5_removed_cumulative),
                                         ifelse(BNB2_simd_del5 < sim_discard,
                                                BNB2_lac5_yield * milk_price *
                                                  (BNB2_simd_lac5e - sim_discard)/BNB2_lac5_period *
                                                  (1 - BNB2_preg5_removed_cumulative),
                                                BNB2_lac5_yield * milk_price * BNB2_lac5_period / milk_day *
                                                  (1 - BNB2_preg5_removed_cumulative)))))

#AI for sixth pregnancy
BNB2_AI6 <- numeric(NB2)
for (i in 1:NB2){
  BNB2_AI6[i] <- rpois(1, ifelse(BNB2_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
BNB2_AI6

#Removal of cattle
BNB2_preg6_removed <- ifelse(BNB2_AI6 > nAI_remove-1, 1, 
                             ifelse(BNB2_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
BNB2_preg6_removed_cumulative <- ifelse(BNB2_preg5_removed_cumulative==1, 1, 
                                        ifelse(BNB2_AI6 > nAI_remove-1, 1, 
                                               ifelse(BNB2_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
BNB2age_preg6 <- BNB2age_del5 + day_wait_AI + 21*BNB2_AI6

#Sixth pregnancy simulation day
BNB2_simd_preg6 <- BNB2_simd_del5 + day_wait_AI + 21*BNB2_AI6


#A2.6.3. Borne in year 3----
#A2.6.3.1. First parity----
#Simulation day of first parity
BNB3_simd_del1 <- DB_NB3 + BNB3_day_del1

#Mastitis at first parity
BNB3_mas1 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_mas1[i] <- rbinom(1, 1, pmasmat[1 + BNB3_BLV[i], 1])
}
BNB3_mas1

#First lactation period
BNB3_lac1_period <- ifelse(BNB3_mas1==1, ifelse(BNB3_BLV==2, days_mas_L, days_mas_SI), milk_day)

#First lactation milk yield
BNB3_lac1_yield <- ifelse(BNB3_mas1==1, ifelse(BNB3_BLV==2, yield_P1 * days_mas_L / milk_day, 
                                               yield_P1 * days_mas_SI / milk_day), yield_P1)

#First lactation end age
BNB3age_lac1e <- BNB3_day_del1 + BNB3_lac1_period 

#First lactation end simulation day
BNB3_simd_lac1e <- BNB3_simd_del1 + BNB3_lac1_period

#First lactation income #linear score 5 milk price not considered
BNB3_lac1_income <- ifelse(BNB3_simd_del1 > sim_d, 0,
                           ifelse(BNB3_simd_lac1e < sim_discard, 0,
                                  ifelse(BNB3_simd_lac1e > sim_d, 
                                         BNB3_lac1_yield * milk_price * (sim_d - BNB3_simd_del1)/BNB3_lac1_period,
                                         ifelse(BNB3_simd_del1 < sim_discard,
                                                BNB3_lac1_yield * milk_price *
                                                  (BNB3_simd_lac1e - sim_discard)/BNB3_lac1_period,
                                                BNB3_lac1_yield * milk_price * BNB3_lac1_period / milk_day))))

#AI for second pregnancy
BNB3_AI2 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_AI2[i] <- rpois(1, ifelse(BNB3_mas1[i]==1, nAI_masmat[1,1], nAI_masmat[2,1]))
}
BNB3_AI2

#Removal of cattle
BNB3_preg2_removed <- ifelse(BNB3_AI2 > nAI_remove-1, 1, 
                             ifelse(BNB3_mas1 == 1, rbinom(1, 1, cull_mas1), 
                                    rbinom(1, 1, cull_nomas1)))

#Second pregnancy age
BNB3age_preg2 <- BNB3_day_del1 + day_wait_AI + 21*BNB3_AI2

#Second pregnancy simulation day
BNB3_simd_preg2 <- BNB3_simd_del1 + day_wait_AI + 21*BNB3_AI2


#A2.6.3.2. Second parity----
#Second parity age
BNB3age_del2 <- BNB3age_preg2 + preg_period

#Second parity simulation day
BNB3_simd_del2 <- BNB3_simd_preg2 + preg_period 

#BLV status of NB1 at second pregnancy
BNB3_BLV_del2 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_BLV_del2[i] <- ifelse(BNB3_BLV[i] == 0, 
                             rbinom(1, 1, (pBLV1 - pBLV0)/(1 - pBLV0))*(1 + rbinom(1, 1, pL_BLV1)), 
                             ifelse(BNB3_BLV[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV1 - pL_BLV0)/(1 - pL_BLV0)), 2))
}

#Mastitis at second parity
BNB3_mas2 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_mas2[i] <- rbinom(1, 1, pmasmat[1 + BNB3_BLV_del2[i], 2])
}
BNB3_mas2

#Second lactation period
BNB3_lac2_period <- ifelse(BNB3_mas2==1, ifelse(BNB3_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
BNB3_lac2_yield <- ifelse(BNB3_mas2==1, ifelse(BNB3_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
BNB3age_lac2e <- BNB3age_del2 + BNB3_lac2_period 

#Second lactation end simulation day
BNB3_simd_lac2e <- BNB3_simd_del2 + BNB3_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
BNB3_lac2_income <- ifelse(BNB3_simd_del2 > sim_d, 0,
                           ifelse(BNB3_simd_lac2e < sim_discard, 0,
                                  ifelse(BNB3_simd_lac2e > sim_d, 
                                         BNB3_lac2_yield * milk_price * (sim_d - BNB3_simd_del2)/BNB3_lac2_period *
                                           (1 - BNB3_preg2_removed),
                                         ifelse(BNB3_simd_del2 < sim_discard,
                                                BNB3_lac2_yield * milk_price *
                                                  (BNB3_simd_lac2e - sim_discard)/BNB3_lac2_period *
                                                  (1 - BNB3_preg2_removed),
                                                BNB3_lac2_yield * milk_price * BNB3_lac2_period / milk_day *
                                                  (1 - BNB3_preg2_removed)))))

#AI for third pregnancy
BNB3_AI3 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_AI3[i] <- rpois(1, ifelse(BNB3_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
BNB3_AI3

#Removal of cattle
BNB3_preg3_removed <- ifelse(BNB3_AI3 > nAI_remove-1, 1, 
                             ifelse(BNB3_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))
BNB3_preg3_removed_cumulative <- ifelse(BNB3_preg2_removed==1, 1, 
                                        ifelse(BNB3_AI3 > nAI_remove-1, 1, 
                                               ifelse(BNB3_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                                      rbinom(1, 1, cull_nomas2))))

#Third pregnancy age
BNB3age_preg3 <- BNB3age_del2 + day_wait_AI + 21*BNB3_AI3

#Third pregnancy simulation day
BNB3_simd_preg3 <- BNB3_simd_del2 + day_wait_AI + 21*BNB3_AI3


#A2.6.3.3. Third parity----
#Third parity age
BNB3age_del3 <- BNB3age_preg3 + preg_period

#Third parity simulation day
BNB3_simd_del3 <- BNB3_simd_preg3 + preg_period 

#BLV status of NB1 at third pregnancy
BNB3_BLV_del3 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_BLV_del3[i] <- ifelse(BNB3_BLV_del2[i] == 0, 
                             rbinom(1, 1, (pBLV2 - pBLV1)/(1 - pBLV1))*(1 + rbinom(1, 1, pL_BLV2)), 
                             ifelse(BNB3_BLV_del2[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV2 - pL_BLV1)/(1 - pL_BLV1)), 2))
}
BNB3_BLV_del3

#Mastitis at third parity
BNB3_mas3 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_mas3[i] <- rbinom(1, 1, pmasmat[1 + BNB3_BLV_del3[i], 3])
}
BNB3_mas3

#Third lactation period
BNB3_lac3_period <- ifelse(BNB3_mas3==1, ifelse(BNB3_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
BNB3_lac3_yield <- ifelse(BNB3_mas3==1, ifelse(BNB3_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
BNB3age_lac3e <- BNB3age_del3 + BNB3_lac3_period 

#Third lactation end simulation day
BNB3_simd_lac3e <- BNB3_simd_del3 + BNB3_lac3_period

#Third lactation income #linear score 5 milk price not considered
BNB3_lac3_income <- ifelse(BNB3_simd_del3 > sim_d, 0,
                           ifelse(BNB3_simd_lac3e < sim_discard, 0,
                                  ifelse(BNB3_simd_lac3e > sim_d, 
                                         BNB3_lac3_yield * milk_price * (sim_d - BNB3_simd_del3)/BNB3_lac3_period *
                                           (1 - BNB3_preg3_removed_cumulative),
                                         ifelse(BNB3_simd_del3 < sim_discard,
                                                BNB3_lac3_yield * milk_price *
                                                  (BNB3_simd_lac3e - sim_discard)/BNB3_lac3_period *
                                                  (1 - BNB3_preg3_removed_cumulative),
                                                BNB3_lac3_yield * milk_price * BNB3_lac3_period / milk_day *
                                                  (1 - BNB3_preg3_removed_cumulative)))))

#AI for fourth pregnancy
BNB3_AI4 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_AI4[i] <- rpois(1, ifelse(BNB3_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
BNB3_AI4

#Removal of cattle
BNB3_preg4_removed <- ifelse(BNB3_AI4 > nAI_remove-1, 1, 
                             ifelse(BNB3_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
BNB3_preg4_removed_cumulative <- ifelse(BNB3_preg3_removed_cumulative==1, 1, 
                                        ifelse(BNB3_AI4 > nAI_remove-1, 1, 
                                               ifelse(BNB3_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
BNB3age_preg4 <- BNB3age_del3 + day_wait_AI + 21*BNB3_AI4

#Fourth pregnancy simulation day
BNB3_simd_preg4 <- BNB3_simd_del3 + day_wait_AI + 21*BNB3_AI4


#A2.6.3.4. Fourth parity----
#Fourth parity age
BNB3age_del4 <- BNB3age_preg4 + preg_period

#Fourth parity simulation day
BNB3_simd_del4 <- BNB3_simd_preg4 + preg_period 

#BLV status of NB1 at fourth pregnancy
BNB3_BLV_del4 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_BLV_del4[i] <- ifelse(BNB3_BLV_del3[i] == 0, 
                             rbinom(1, 1, (pBLV3 - pBLV2)/(1 - pBLV2))*(1 + rbinom(1, 1, pL_BLV3)), 
                             ifelse(BNB3_BLV_del3[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV3 - pL_BLV2)/(1 - pL_BLV2)), 2))
}
BNB3_BLV_del4

#Mastitis at fourth parity
BNB3_mas4 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_mas4[i] <- rbinom(1, 1, pmasmat[1 + BNB3_BLV_del4[i], 4])
}
BNB3_mas4

#Fourth lactation period
BNB3_lac4_period <- ifelse(BNB3_mas4==1, ifelse(BNB3_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
BNB3_lac4_yield <- ifelse(BNB3_mas4==1, ifelse(BNB3_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
BNB3age_lac4e <- BNB3age_del4 + BNB3_lac4_period 

#Fourth lactation end simulation day
BNB3_simd_lac4e <- BNB3_simd_del4 + BNB3_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
BNB3_lac4_income <- ifelse(BNB3_simd_del4 > sim_d, 0,
                           ifelse(BNB3_simd_lac4e < sim_discard, 0,
                                  ifelse(BNB3_simd_lac4e > sim_d, 
                                         BNB3_lac4_yield * milk_price * (sim_d - BNB3_simd_del4)/BNB3_lac4_period *
                                           (1 - BNB3_preg4_removed_cumulative),
                                         ifelse(BNB3_simd_del4 < sim_discard,
                                                BNB3_lac4_yield * milk_price *
                                                  (BNB3_simd_lac4e - sim_discard)/BNB3_lac4_period *
                                                  (1 - BNB3_preg4_removed_cumulative),
                                                BNB3_lac4_yield * milk_price * BNB3_lac4_period / milk_day *
                                                  (1 - BNB3_preg4_removed_cumulative)))))


#AI for fifth pregnancy
BNB3_AI5 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_AI5[i] <- rpois(1, ifelse(BNB3_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
BNB3_AI5

#Removal of cattle
BNB3_preg5_removed <- ifelse(BNB3_AI5 > nAI_remove-1, 1, 
                             ifelse(BNB3_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
BNB3_preg5_removed_cumulative <- ifelse(BNB3_preg4_removed_cumulative==1, 1, 
                                        ifelse(BNB3_AI5 > nAI_remove-1, 1, 
                                               ifelse(BNB3_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
BNB3age_preg5 <- BNB3age_del4 + day_wait_AI + 21*BNB3_AI5

#Fifth pregnancy simulation day
BNB3_simd_preg5 <- BNB3_simd_del4 + day_wait_AI + 21*BNB3_AI5


#A2.6.3.5. Fifth parity----
#Fifth parity age
BNB3age_del5 <- BNB3age_preg5 + preg_period

#Fifth parity simulation day
BNB3_simd_del5 <- BNB3_simd_preg5 + preg_period 

#BLV status of IP0 at fifth pregnancy
BNB3_BLV_del5 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_BLV_del5[i] <- ifelse(BNB3_BLV_del4[i] == 0, 
                             rbinom(1, 1, (pBLV4 - pBLV3)/(1 - pBLV3))*(1 + rbinom(1, 1, pL_BLV4)), 
                             ifelse(BNB3_BLV_del4[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV4 - pL_BLV3)/(1 - pL_BLV3)), 2))
}
BNB3_BLV_del5

#Mastitis at fifth parity
BNB3_mas5 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_mas5[i] <- rbinom(1, 1, pmasmat[1 + BNB3_BLV_del5[i], 5])
}
BNB3_mas5

#Fifth lactation period
BNB3_lac5_period <- ifelse(BNB3_mas5==1, ifelse(BNB3_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
BNB3_lac5_yield <- ifelse(BNB3_mas5==1, ifelse(BNB3_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
BNB3age_lac5e <- BNB3age_del5 + BNB3_lac5_period 

#Fifth lactation end simulation day
BNB3_simd_lac5e <- BNB3_simd_del5 + BNB3_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
BNB3_lac5_income <- ifelse(BNB3_simd_del5 > sim_d, 0,
                           ifelse(BNB3_simd_lac5e < sim_discard, 0,
                                  ifelse(BNB3_simd_lac5e > sim_d, 
                                         BNB3_lac5_yield * milk_price * (sim_d - BNB3_simd_del5)/BNB3_lac5_period *
                                           (1 - BNB3_preg5_removed_cumulative),
                                         ifelse(BNB3_simd_del5 < sim_discard,
                                                BNB3_lac5_yield * milk_price *
                                                  (BNB3_simd_lac5e - sim_discard)/BNB3_lac5_period *
                                                  (1 - BNB3_preg5_removed_cumulative),
                                                BNB3_lac5_yield * milk_price * BNB3_lac5_period / milk_day *
                                                  (1 - BNB3_preg5_removed_cumulative)))))

#AI for sixth pregnancy
BNB3_AI6 <- numeric(NB3)
for (i in 1:NB3){
  BNB3_AI6[i] <- rpois(1, ifelse(BNB3_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
BNB3_AI6

#Removal of cattle
BNB3_preg6_removed <- ifelse(BNB3_AI6 > nAI_remove-1, 1, 
                             ifelse(BNB3_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
BNB3_preg6_removed_cumulative <- ifelse(BNB3_preg5_removed_cumulative==1, 1, 
                                        ifelse(BNB3_AI6 > nAI_remove-1, 1, 
                                               ifelse(BNB3_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
BNB3age_preg6 <- BNB3age_del5 + day_wait_AI + 21*BNB3_AI6

#Sixth pregnancy simulation day
BNB3_simd_preg6 <- BNB3_simd_del5 + day_wait_AI + 21*BNB3_AI6


#A2.6.4. Borne in year 4----
#A2.6.4.1. First parity----
#Simulation day of first parity
BNB4_simd_del1 <- DB_NB4 + BNB4_day_del1

#Mastitis at first parity
BNB4_mas1 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_mas1[i] <- rbinom(1, 1, pmasmat[1 + BNB4_BLV[i], 1])
}
BNB4_mas1

#First lactation period
BNB4_lac1_period <- ifelse(BNB4_mas1==1, ifelse(BNB4_BLV==2, days_mas_L, days_mas_SI), milk_day)

#First lactation milk yield
BNB4_lac1_yield <- ifelse(BNB4_mas1==1, ifelse(BNB4_BLV==2, yield_P1 * days_mas_L / milk_day, 
                                               yield_P1 * days_mas_SI / milk_day), yield_P1)

#First lactation end age
BNB4age_lac1e <- BNB4_day_del1 + BNB4_lac1_period 

#First lactation end simulation day
BNB4_simd_lac1e <- BNB4_simd_del1 + BNB4_lac1_period

#First lactation income #linear score 5 milk price not considered
BNB4_lac1_income <- ifelse(BNB4_simd_del1 > sim_d, 0,
                           ifelse(BNB4_simd_lac1e < sim_discard, 0,
                                  ifelse(BNB4_simd_lac1e > sim_d, 
                                         BNB4_lac1_yield * milk_price * (sim_d - BNB4_simd_del1)/BNB4_lac1_period,
                                         ifelse(BNB4_simd_del1 < sim_discard,
                                                BNB4_lac1_yield * milk_price *
                                                  (BNB4_simd_lac1e - sim_discard)/BNB4_lac1_period,
                                                BNB4_lac1_yield * milk_price * BNB4_lac1_period / milk_day))))

#AI for second pregnancy
BNB4_AI2 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_AI2[i] <- rpois(1, ifelse(BNB4_mas1[i]==1, nAI_masmat[1,1], nAI_masmat[2,1]))
}
BNB4_AI2

#Removal of cattle
BNB4_preg2_removed <- ifelse(BNB4_AI2 > nAI_remove-1, 1, 
                             ifelse(BNB4_mas1 == 1, rbinom(1, 1, cull_mas1), 
                                    rbinom(1, 1, cull_nomas1)))

#Second pregnancy age
BNB4age_preg2 <- BNB4_day_del1 + day_wait_AI + 21*BNB4_AI2

#Second pregnancy simulation day
BNB4_simd_preg2 <- BNB4_simd_del1 + day_wait_AI + 21*BNB4_AI2


#A2.6.4.2. Second parity----
#Second parity age
BNB4age_del2 <- BNB4age_preg2 + preg_period

#Second parity simulation day
BNB4_simd_del2 <- BNB4_simd_preg2 + preg_period 

#BLV status of NB1 at second pregnancy
BNB4_BLV_del2 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_BLV_del2[i] <- ifelse(BNB4_BLV[i] == 0, 
                             rbinom(1, 1, (pBLV1 - pBLV0)/(1 - pBLV0))*(1 + rbinom(1, 1, pL_BLV1)), 
                             ifelse(BNB4_BLV[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV1 - pL_BLV0)/(1 - pL_BLV0)), 2))
}

#Mastitis at second parity
BNB4_mas2 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_mas2[i] <- rbinom(1, 1, pmasmat[1 + BNB4_BLV_del2[i], 2])
}
BNB4_mas2

#Second lactation period
BNB4_lac2_period <- ifelse(BNB4_mas2==1, ifelse(BNB4_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
BNB4_lac2_yield <- ifelse(BNB4_mas2==1, ifelse(BNB4_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
BNB4age_lac2e <- BNB4age_del2 + BNB4_lac2_period 

#Second lactation end simulation day
BNB4_simd_lac2e <- BNB4_simd_del2 + BNB4_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
BNB4_lac2_income <- ifelse(BNB4_simd_del2 > sim_d, 0,
                           ifelse(BNB4_simd_lac2e < sim_discard, 0,
                                  ifelse(BNB4_simd_lac2e > sim_d, 
                                         BNB4_lac2_yield * milk_price * (sim_d - BNB4_simd_del2)/BNB4_lac2_period *
                                           (1 - BNB4_preg2_removed),
                                         ifelse(BNB4_simd_del2 < sim_discard,
                                                BNB4_lac2_yield * milk_price *
                                                  (BNB4_simd_lac2e - sim_discard)/BNB4_lac2_period *
                                                  (1 - BNB4_preg2_removed),
                                                BNB4_lac2_yield * milk_price * BNB4_lac2_period / milk_day *
                                                  (1 - BNB4_preg2_removed)))))

#AI for third pregnancy
BNB4_AI3 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_AI3[i] <- rpois(1, ifelse(BNB4_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
BNB4_AI3

#Removal of cattle
BNB4_preg3_removed <- ifelse(BNB4_AI3 > nAI_remove-1, 1, 
                             ifelse(BNB4_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))
BNB4_preg3_removed_cumulative <- ifelse(BNB4_preg2_removed==1, 1, 
                                        ifelse(BNB4_AI3 > nAI_remove-1, 1, 
                                               ifelse(BNB4_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                                      rbinom(1, 1, cull_nomas2))))

#Third pregnancy age
BNB4age_preg3 <- BNB4age_del2 + day_wait_AI + 21*BNB4_AI3

#Third pregnancy simulation day
BNB4_simd_preg3 <- BNB4_simd_del2 + day_wait_AI + 21*BNB4_AI3


#A2.6.4.3. Third parity----
#Third parity age
BNB4age_del3 <- BNB4age_preg3 + preg_period

#Third parity simulation day
BNB4_simd_del3 <- BNB4_simd_preg3 + preg_period 

#BLV status of NB1 at third pregnancy
BNB4_BLV_del3 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_BLV_del3[i] <- ifelse(BNB4_BLV_del2[i] == 0, 
                             rbinom(1, 1, (pBLV2 - pBLV1)/(1 - pBLV1))*(1 + rbinom(1, 1, pL_BLV2)), 
                             ifelse(BNB4_BLV_del2[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV2 - pL_BLV1)/(1 - pL_BLV1)), 2))
}
BNB4_BLV_del3

#Mastitis at third parity
BNB4_mas3 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_mas3[i] <- rbinom(1, 1, pmasmat[1 + BNB4_BLV_del3[i], 3])
}
BNB4_mas3

#Third lactation period
BNB4_lac3_period <- ifelse(BNB4_mas3==1, ifelse(BNB4_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
BNB4_lac3_yield <- ifelse(BNB4_mas3==1, ifelse(BNB4_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
BNB4age_lac3e <- BNB4age_del3 + BNB4_lac3_period 

#Third lactation end simulation day
BNB4_simd_lac3e <- BNB4_simd_del3 + BNB4_lac3_period

#Third lactation income #linear score 5 milk price not considered
BNB4_lac3_income <- ifelse(BNB4_simd_del3 > sim_d, 0,
                           ifelse(BNB4_simd_lac3e < sim_discard, 0,
                                  ifelse(BNB4_simd_lac3e > sim_d, 
                                         BNB4_lac3_yield * milk_price * (sim_d - BNB4_simd_del3)/BNB4_lac3_period *
                                           (1 - BNB4_preg3_removed_cumulative),
                                         ifelse(BNB4_simd_del3 < sim_discard,
                                                BNB4_lac3_yield * milk_price *
                                                  (BNB4_simd_lac3e - sim_discard)/BNB4_lac3_period *
                                                  (1 - BNB4_preg3_removed_cumulative),
                                                BNB4_lac3_yield * milk_price * BNB4_lac3_period / milk_day *
                                                  (1 - BNB4_preg3_removed_cumulative)))))

#AI for fourth pregnancy
BNB4_AI4 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_AI4[i] <- rpois(1, ifelse(BNB4_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
BNB4_AI4

#Removal of cattle
BNB4_preg4_removed <- ifelse(BNB4_AI4 > nAI_remove-1, 1, 
                             ifelse(BNB4_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
BNB4_preg4_removed_cumulative <- ifelse(BNB4_preg3_removed_cumulative==1, 1, 
                                        ifelse(BNB4_AI4 > nAI_remove-1, 1, 
                                               ifelse(BNB4_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
BNB4age_preg4 <- BNB4age_del3 + day_wait_AI + 21*BNB4_AI4

#Fourth pregnancy simulation day
BNB4_simd_preg4 <- BNB4_simd_del3 + day_wait_AI + 21*BNB4_AI4


#A2.6.3.4. Fourth parity----
#Fourth parity age
BNB4age_del4 <- BNB4age_preg4 + preg_period

#Fourth parity simulation day
BNB4_simd_del4 <- BNB4_simd_preg4 + preg_period 

#BLV status of NB1 at fourth pregnancy
BNB4_BLV_del4 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_BLV_del4[i] <- ifelse(BNB4_BLV_del3[i] == 0, 
                             rbinom(1, 1, (pBLV3 - pBLV2)/(1 - pBLV2))*(1 + rbinom(1, 1, pL_BLV3)), 
                             ifelse(BNB4_BLV_del3[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV3 - pL_BLV2)/(1 - pL_BLV2)), 2))
}
BNB4_BLV_del4

#Mastitis at fourth parity
BNB4_mas4 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_mas4[i] <- rbinom(1, 1, pmasmat[1 + BNB4_BLV_del4[i], 4])
}
BNB4_mas4

#Fourth lactation period
BNB4_lac4_period <- ifelse(BNB4_mas4==1, ifelse(BNB4_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
BNB4_lac4_yield <- ifelse(BNB4_mas4==1, ifelse(BNB4_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
BNB4age_lac4e <- BNB4age_del4 + BNB4_lac4_period 

#Fourth lactation end simulation day
BNB4_simd_lac4e <- BNB4_simd_del4 + BNB4_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
BNB4_lac4_income <- ifelse(BNB4_simd_del4 > sim_d, 0,
                           ifelse(BNB4_simd_lac4e < sim_discard, 0,
                                  ifelse(BNB4_simd_lac4e > sim_d, 
                                         BNB4_lac4_yield * milk_price * (sim_d - BNB4_simd_del4)/BNB4_lac4_period *
                                           (1 - BNB4_preg4_removed_cumulative),
                                         ifelse(BNB4_simd_del4 < sim_discard,
                                                BNB4_lac4_yield * milk_price *
                                                  (BNB4_simd_lac4e - sim_discard)/BNB4_lac4_period *
                                                  (1 - BNB4_preg4_removed_cumulative),
                                                BNB4_lac4_yield * milk_price * BNB4_lac4_period / milk_day *
                                                  (1 - BNB4_preg4_removed_cumulative)))))


#AI for fifth pregnancy
BNB4_AI5 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_AI5[i] <- rpois(1, ifelse(BNB4_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
BNB4_AI5

#Removal of cattle
BNB4_preg5_removed <- ifelse(BNB4_AI5 > nAI_remove-1, 1, 
                             ifelse(BNB4_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
BNB4_preg5_removed_cumulative <- ifelse(BNB4_preg4_removed_cumulative==1, 1, 
                                        ifelse(BNB4_AI5 > nAI_remove-1, 1, 
                                               ifelse(BNB4_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
BNB4age_preg5 <- BNB4age_del4 + day_wait_AI + 21*BNB4_AI5

#Fifth pregnancy simulation day
BNB4_simd_preg5 <- BNB4_simd_del4 + day_wait_AI + 21*BNB4_AI5


#A2.6.3.5. Fifth parity----
#Fifth parity age
BNB4age_del5 <- BNB4age_preg5 + preg_period

#Fifth parity simulation day
BNB4_simd_del5 <- BNB4_simd_preg5 + preg_period 

#BLV status of IP0 at fifth pregnancy
BNB4_BLV_del5 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_BLV_del5[i] <- ifelse(BNB4_BLV_del4[i] == 0, 
                             rbinom(1, 1, (pBLV4 - pBLV3)/(1 - pBLV3))*(1 + rbinom(1, 1, pL_BLV4)), 
                             ifelse(BNB4_BLV_del4[i] == 1, 
                                    1 + rbinom(1, 1, (pL_BLV4 - pL_BLV3)/(1 - pL_BLV3)), 2))
}
BNB4_BLV_del5

#Mastitis at fifth parity
BNB4_mas5 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_mas5[i] <- rbinom(1, 1, pmasmat[1 + BNB3_BLV_del5[i], 5])
}
BNB4_mas5

#Fifth lactation period
BNB4_lac5_period <- ifelse(BNB4_mas5==1, ifelse(BNB4_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
BNB4_lac5_yield <- ifelse(BNB4_mas5==1, ifelse(BNB4_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
BNB4age_lac5e <- BNB4age_del5 + BNB4_lac5_period 

#Fifth lactation end simulation day
BNB4_simd_lac5e <- BNB4_simd_del5 + BNB4_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
BNB4_lac5_income <- ifelse(BNB4_simd_del5 > sim_d, 0,
                           ifelse(BNB4_simd_lac5e < sim_discard, 0,
                                  ifelse(BNB4_simd_lac5e > sim_d, 
                                         BNB4_lac5_yield * milk_price * (sim_d - BNB4_simd_del5)/BNB4_lac5_period *
                                           (1 - BNB4_preg5_removed_cumulative),
                                         ifelse(BNB4_simd_del5 < sim_discard,
                                                BNB4_lac5_yield * milk_price *
                                                  (BNB4_simd_lac5e - sim_discard)/BNB4_lac5_period *
                                                  (1 - BNB4_preg5_removed_removed_cumulative),
                                                BNB4_lac5_yield * milk_price * BNB4_lac5_period / milk_day *
                                                  (1 - BNB4_preg5_removed_cumulative)))))

#AI for sixth pregnancy
BNB4_AI6 <- numeric(NB4)
for (i in 1:NB4){
  BNB4_AI6[i] <- rpois(1, ifelse(BNB4_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
BNB4_AI6

#Removal of cattle
BNB4_preg6_removed <- ifelse(BNB4_AI6 > nAI_remove-1, 1, 
                             ifelse(BNB4_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
BNB4_preg6_removed_cumulative <- ifelse(BNB4_preg5_removed_cumulative==1, 1, 
                                        ifelse(BNB4_AI6 > nAI_remove-1, 1, 
                                               ifelse(BNB4_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
BNB4age_preg6 <- BNB4age_del5 + day_wait_AI + 21*BNB4_AI6

#Sixth pregnancy simulation day
BNB4_simd_preg6 <- BNB4_simd_del5 + day_wait_AI + 21*BNB4_AI6


#B. BLV-non-infected farm----
#B1. Setting initial status----
#B1.1. Assignment of ID in initial herd----
#h: healthy
hID_IP0 <- 1 : IP0
hID_IP1 <- IP0 + 1 : IP1
hID_IP2 <- IP1 + 1 : IP2
hID_IP3 <- IP2 + 1 : IP3
hID_IP4 <- IP3 + 1 : IP4
hID_IP5 <- IP4 + 1 : IP5
hID <- c(hID_IP0, hID_IP1, hID_IP2, hID_IP3, hID_IP4, hID_IP5)

#B1.2. Assignment of age in days at the simulation day 0----
#hIPage is the age in days at the simulation day 0
hIP0age <- sort(sample(ageIP0[1]:ageIP0[2], size = IP0, replace = TRUE))
hIP1age <- sort(sample(ageIP1[1]:ageIP1[2], size = IP1, replace = TRUE))
hIP2age <- sort(sample(ageIP2[1]:ageIP2[2], size = IP2, replace = TRUE))
hIP3age <- sort(sample(ageIP3[1]:ageIP3[2], size = IP3, replace = TRUE))
hIP4age <- sort(sample(ageIP4[1]:ageIP4[2], size = IP4, replace = TRUE))
hIP5age <- sort(sample(ageIP5[1]:ageIP5[2], size = IP5, replace = TRUE))
hIPage <- c(hIP0age, hIP1age, hIP2age, hIP3age, hIP4age, hIP5age)

#B1.3. Assignment of BLV status (SIL) at the simulation day 0----
#0: S, 1: I, 2:L
hIP0_BLV <- rep(0, IP0)
hIP1_BLV <- rep(0, IP1)
hIP2_BLV <- rep(0, IP2)
hIP3_BLV <- rep(0, IP3)
hIP4_BLV <- rep(0, IP4)
hIP5_BLV <- rep(0, IP5)
hIP_BLV <- c(hIP0_BLV, hIP1_BLV, hIP2_BLV, hIP3_BLV, hIP4_BLV, hIP5_BLV)

hNB1_BLV <- rep(0,NB1) 
hNB2_BLV <- rep(0,NB2)
hNB3_BLV <- rep(0,NB3)
hNB4_BLV <- rep(0,NB4)

#B1.4. Delivery date----
#B1.4.1. Expected delivery date in age (day) in IP0 heifers----
#正規分布を使っても良い。その場合、平均をmu、標準偏差をsdとして
#hIP0_day_del <- rnorm(IPO, mu, sd)
hIP0_day_del1 <- sample(age_del1[1]:age_del1[2], size = IP0, replace = TRUE)

#B1.4.2. Previous delivery date in age (day)----
hIP1_day_del2 <- sample(age_del2[1]:age_del2[2], size = IP1, replace = TRUE)
hIP2_day_del3 <- sample(age_del3[1]:age_del3[2], size = IP2, replace = TRUE)
hIP3_day_del4 <- sample(age_del4[1]:age_del4[2], size = IP3, replace = TRUE)  
hIP4_day_del5 <- sample(age_del5[1]:age_del5[2], size = IP4, replace = TRUE)
hIP5_day_del6 <- sample(age_del6[1]:age_del6[2], size = IP5, replace = TRUE)

#B1.4.3. Expected delivery date in age (day) of new borne calves----
hNB1_day_del1 <- sample(age_del1[1]:age_del1[2], size = NB1, replace = TRUE)
hNB2_day_del1 <- sample(age_del1[1]:age_del1[2], size = NB2, replace = TRUE)
hNB3_day_del1 <- sample(age_del1[1]:age_del1[2], size = NB3, replace = TRUE)
hNB4_day_del1 <- sample(age_del1[1]:age_del1[2], size = NB4, replace = TRUE)

#B2. Simulation----
#B2.0. IP0 simulation----
#B2.0.1. First parity----
#Simulation day of first parity
hIP0_simd_del1 <- hIP0_day_del1 - hIP0age

#Mastitis at first parity
hIP0_mas1 <- numeric(IP0)
for (i in 1:IP0){
  hIP0_mas1[i] <- rbinom(1, 1, pmasmat[1 + hIP0_BLV[i], 1])
}
hIP0_mas1

#First lactation period
hIP0_lac1_period <- ifelse(hIP0_mas1==1, ifelse(hIP0_BLV==2, days_mas_L, days_mas_SI), milk_day)

#First lactation milk yield
hIP0_lac1_yield <- ifelse(hIP0_mas1==1, ifelse(hIP0_BLV==2, yield_P1 * days_mas_L / milk_day, 
                                               yield_P1 * days_mas_SI / milk_day), yield_P1)

#First lactation end age
hIP0age_lac1e <- hIP0_day_del1 + hIP0_lac1_period 

#First lactation end simulation day
hIP0_simd_lac1e <- hIP0_simd_del1 + hIP0_lac1_period

#First lactation income #linear score 5 milk price not considered
hIP0_lac1_income <- ifelse(hIP0_simd_del1 > sim_d, 0,
                           ifelse(hIP0_simd_lac1e < sim_discard, 0,
                                  ifelse(hIP0_simd_lac1e > sim_d, 
                                         hIP0_lac1_yield * milk_price * (sim_d - hIP0_simd_del1)/hIP0_lac1_period,
                                         ifelse(hIP0_simd_del1 < sim_discard,
                                                hIP0_lac1_yield * milk_price *
                                                  (hIP0_simd_lac1e - sim_discard)/hIP0_lac1_period,
                                                hIP0_lac1_yield * milk_price * hIP0_lac1_period / milk_day))))

#AI for second pregnancy
hIP0_AI2 <- numeric(IP0)
for (i in 1:IP0){
  hIP0_AI2[i] <- rpois(1, ifelse(hIP0_mas1[i]==1, nAI_masmat[1,1], nAI_masmat[2,1]))
}
hIP0_AI2

#Removal of cattle
hIP0_preg2_removed <- ifelse(hIP0_AI2 > nAI_remove-1, 1, 
                             ifelse(hIP0_mas1 == 1, rbinom(1, 1, cull_mas1), 
                                    rbinom(1, 1, cull_nomas1)))

#Second pregnancy age
hIP0age_preg2 <- hIP0_day_del1 + day_wait_AI + 21*hIP0_AI2

#Second pregnancy simulation day
hIP0_simd_preg2 <- hIP0_simd_del1 + day_wait_AI + 21*hIP0_AI2


#B2.0.2. Second parity----
#Second parity age
hIP0age_del2 <- hIP0age_preg2 + preg_period

#Second parity simulation day
hIP0_simd_del2 <- hIP0_simd_preg2 + preg_period 

#BLV status of IP0 at second pregnancy
hIP0_BLV_del2 <- rep(0, IP0)

#Mastitis at second parity
hIP0_mas2 <- numeric(IP0)
for (i in 1:IP0){
  hIP0_mas2[i] <- rbinom(1, 1, pmasmat[1 + hIP0_BLV_del2[i], 2])
}
hIP0_mas2

#Second lactation period
hIP0_lac2_period <- ifelse(hIP0_mas2==1, ifelse(hIP0_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
hIP0_lac2_yield <- ifelse(hIP0_mas2==1, ifelse(hIP0_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
hIP0age_lac2e <- hIP0age_del2 + hIP0_lac2_period 

#Second lactation end simulation day
hIP0_simd_lac2e <- hIP0_simd_del2 + hIP0_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
hIP0_lac2_income <- ifelse(hIP0_simd_del2 > sim_d, 0,
                           ifelse(hIP0_simd_lac2e < sim_discard, 0,
                                  ifelse(hIP0_simd_lac2e > sim_d, 
                                         hIP0_lac2_yield * milk_price * (sim_d - hIP0_simd_del2)/hIP0_lac2_period *
                                           (1 - hIP0_preg2_removed),
                                         ifelse(hIP0_simd_del2 < sim_discard,
                                                hIP0_lac2_yield * milk_price *
                                                  (hIP0_simd_lac2e - sim_discard)/hIP0_lac2_period *
                                                  (1 - hIP0_preg2_removed),
                                                hIP0_lac2_yield * milk_price * hIP0_lac2_period / milk_day *
                                                  (1 - hIP0_preg2_removed)))))

#AI for third pregnancy
hIP0_AI3 <- numeric(IP0)
for (i in 1:IP0){
  hIP0_AI3[i] <- rpois(1, ifelse(hIP0_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
hIP0_AI3

#Removal of cattle
hIP0_preg3_removed <- ifelse(hIP0_AI3 > nAI_remove-1, 1, 
                             ifelse(hIP0_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))
hIP0_preg3_removed_cumulative <- ifelse(hIP0_preg2_removed==1, 1, 
                                        ifelse(hIP0_AI3 > nAI_remove-1, 1, 
                                               ifelse(hIP0_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                                      rbinom(1, 1, cull_nomas2))))

#Third pregnancy age
hIP0age_preg3 <- hIP0age_del2 + day_wait_AI + 21*hIP0_AI3

#Third pregnancy simulation day
hIP0_simd_preg3 <- hIP0_simd_del2 + day_wait_AI + 21*hIP0_AI3


#B2.0.3. Third parity----
#Third parity age
hIP0age_del3 <- hIP0age_preg3 + preg_period

#Third parity simulation day
hIP0_simd_del3 <- hIP0_simd_preg3 + preg_period 

#BLV status of IP0 at third pregnancy
hIP0_BLV_del3 <- rep(0, IP0)

#Mastitis at third parity
hIP0_mas3 <- numeric(IP0)
for (i in 1:IP0){
  hIP0_mas3[i] <- rbinom(1, 1, pmasmat[1 + hIP0_BLV_del3[i], 3])
}
hIP0_mas3

#Third lactation period
hIP0_lac3_period <- ifelse(hIP0_mas3==1, ifelse(hIP0_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
hIP0_lac3_yield <- ifelse(hIP0_mas3==1, ifelse(hIP0_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
hIP0age_lac3e <- hIP0age_del3 + hIP0_lac3_period 

#Third lactation end simulation day
hIP0_simd_lac3e <- hIP0_simd_del3 + hIP0_lac3_period

#Third lactation income #linear score 5 milk price not considered
hIP0_lac3_income <- ifelse(hIP0_simd_del3 > sim_d, 0,
                           ifelse(hIP0_simd_lac3e < sim_discard, 0,
                                  ifelse(hIP0_simd_lac3e > sim_d, 
                                         hIP0_lac3_yield * milk_price * (sim_d - hIP0_simd_del3)/hIP0_lac3_period *
                                           (1 - hIP0_preg3_removed_cumulative),
                                         ifelse(hIP0_simd_del3 < sim_discard,
                                                hIP0_lac3_yield * milk_price *
                                                  (hIP0_simd_lac3e - sim_discard)/hIP0_lac3_period *
                                                  (1 - hIP0_preg3_removed_cumulative),
                                                hIP0_lac3_yield * milk_price * hIP0_lac3_period / milk_day *
                                                  (1 - hIP0_preg3_removed_cumulative)))))

#AI for fourth pregnancy
hIP0_AI4 <- numeric(IP0)
for (i in 1:IP0){
  hIP0_AI4[i] <- rpois(1, ifelse(hIP0_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
hIP0_AI4

#Removal of cattle
hIP0_preg4_removed <- ifelse(hIP0_AI4 > nAI_remove-1, 1, 
                             ifelse(hIP0_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
hIP0_preg4_removed_cumulative <- ifelse(BIP0_preg3_removed_cumulative==1, 1, 
                                        ifelse(hIP0_AI4 > nAI_remove-1, 1, 
                                               ifelse(hIP0_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
hIP0age_preg4 <- hIP0age_del3 + day_wait_AI + 21*hIP0_AI4

#Fourth pregnancy simulation day
hIP0_simd_preg4 <- hIP0_simd_del3 + day_wait_AI + 21*hIP0_AI4


#B2.0.4. Fourth parity----
#Fourth parity age
hIP0age_del4 <- hIP0age_preg4 + preg_period

#Fourth parity simulation day
hIP0_simd_del4 <- hIP0_simd_preg4 + preg_period 

#BLV status of IP0 at fourth pregnancy
hIP0_BLV_del4 <- rep(0,IP0)

#Mastitis at fourth parity
hIP0_mas4 <- numeric(IP0)
for (i in 1:IP0){
  hIP0_mas4[i] <- rbinom(1, 1, pmasmat[1 + hIP0_BLV_del4[i], 4])
}
hIP0_mas4

#Fourth lactation period
hIP0_lac4_period <- ifelse(hIP0_mas4==1, ifelse(hIP0_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
hIP0_lac4_yield <- ifelse(hIP0_mas4==1, ifelse(hIP0_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
hIP0age_lac4e <- hIP0age_del4 + hIP0_lac4_period 

#Fourth lactation end simulation day
hIP0_simd_lac4e <- hIP0_simd_del4 + hIP0_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
hIP0_lac4_income <- ifelse(hIP0_simd_del4 > sim_d, 0,
                           ifelse(hIP0_simd_lac4e < sim_discard, 0,
                                  ifelse(hIP0_simd_lac4e > sim_d, 
                                         hIP0_lac4_yield * milk_price * (sim_d - hIP0_simd_del4)/hIP0_lac4_period *
                                           (1 - hIP0_preg4_removed_cumulative),
                                         ifelse(hIP0_simd_del4 < sim_discard,
                                                hIP0_lac4_yield * milk_price *
                                                  (hIP0_simd_lac4e - sim_discard)/hIP0_lac4_period *
                                                  (1 - hIP0_preg4_removed_cumulative),
                                                hIP0_lac4_yield * milk_price * hIP0_lac4_period / milk_day *
                                                  (1 - hIP0_preg4_removed_cumulative)))))


#AI for fifth pregnancy
hIP0_AI5 <- numeric(IP0)
for (i in 1:IP0){
  hIP0_AI5[i] <- rpois(1, ifelse(hIP0_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
hIP0_AI5

#Removal of cattle
hIP0_preg5_removed <- ifelse(hIP0_AI5 > nAI_remove-1, 1, 
                             ifelse(hIP0_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
hIP0_preg5_removed_cumulative <- ifelse(hIP0_preg4_removed_cumulative==1, 1, 
                                        ifelse(hIP0_AI5 > nAI_remove-1, 1, 
                                               ifelse(hIP0_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
hIP0age_preg5 <- hIP0age_del4 + day_wait_AI + 21*hIP0_AI5

#Fifth pregnancy simulation day
hIP0_simd_preg5 <- hIP0_simd_del4 + day_wait_AI + 21*hIP0_AI5


#B2.0.5. Fifth parity----
#Fifth parity age
hIP0age_del5 <- hIP0age_preg5 + preg_period

#Fifth parity simulation day
hIP0_simd_del5 <- hIP0_simd_preg5 + preg_period 

#BLV status of IP0 at fifth pregnancy
hIP0_BLV_del5 <- rep(0,IP0)

#Mastitis at fifth parity
hIP0_mas5 <- numeric(IP0)
for (i in 1:IP0){
  hIP0_mas5[i] <- rbinom(1, 1, pmasmat[1 + hIP0_BLV_del5[i], 5])
}
hIP0_mas5

#Fifth lactation period
hIP0_lac5_period <- ifelse(hIP0_mas5==1, ifelse(hIP0_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
hIP0_lac5_yield <- ifelse(hIP0_mas5==1, ifelse(hIP0_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
hIP0age_lac5e <- hIP0age_del5 + hIP0_lac5_period 

#Fifth lactation end simulation day
hIP0_simd_lac5e <- hIP0_simd_del5 + hIP0_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
hIP0_lac5_income <- ifelse(hIP0_simd_del5 > sim_d, 0,
                           ifelse(hIP0_simd_lac5e < sim_discard, 0,
                                  ifelse(hIP0_simd_lac5e > sim_d, 
                                         hIP0_lac5_yield * milk_price * (sim_d - hIP0_simd_del5)/hIP0_lac5_period *
                                           (1 - hIP0_preg5_removed_cumulative),
                                         ifelse(hIP0_simd_del5 < sim_discard,
                                                hIP0_lac5_yield * milk_price *
                                                  (hIP0_simd_lac5e - sim_discard)/hIP0_lac5_period *
                                                  (1 - hIP0_preg5_removed_cumulative),
                                                hIP0_lac5_yield * milk_price * hIP0_lac5_period / milk_day *
                                                  (1 - hIP0_preg5_removed_cumulative)))))

#AI for sixth pregnancy
hIP0_AI6 <- numeric(IP0)
for (i in 1:IP0){
  hIP0_AI6[i] <- rpois(1, ifelse(hIP0_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
hIP0_AI6

#Removal of cattle
hIP0_preg6_removed <- ifelse(hIP0_AI6 > nAI_remove-1, 1, 
                             ifelse(hIP0_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
hIP0_preg6_removed_cumulative <- ifelse(hIP0_preg5_removed_cumulative==1, 1, 
                                        ifelse(hIP0_AI6 > nAI_remove-1, 1, 
                                               ifelse(hIP0_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
hIP0age_preg6 <- hIP0age_del5 + day_wait_AI + 21*hIP0_AI6

#Sixth pregnancy simulation day
hIP0_simd_preg6 <- hIP0_simd_del5 + day_wait_AI + 21*hIP0_AI6


#B2.1. IP1 simulation----
#B2.1.2. Second parity----
#Simulation day of second parity
hIP1_simd_del2 <- hIP1_day_del2 - hIP1age

#BLV status of IP1 at second pregnancy
hIP1_BLV_del2 <- rep(0, IP1)

#Mastitis at second parity
hIP1_mas2 <- numeric(IP1)
for (i in 1:IP1){
  hIP1_mas2[i] <- rbinom(1, 1, pmasmat[1 + hIP1_BLV_del2[i], 2])
}
hIP1_mas2

#Second lactation period
hIP1_lac2_period <- ifelse(hIP1_mas2==1, ifelse(hIP1_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
hIP1_lac2_yield <- ifelse(hIP1_mas2==1, ifelse(hIP1_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
hIP1age_lac2e <- hIP1_day_del2 + hIP1_lac2_period 

#Second lactation end simulation day
hIP1_simd_lac2e <- hIP1_simd_del2 + hIP1_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
hIP1_lac2_income <- ifelse(hIP1_simd_del2 > sim_d, 0,
                           ifelse(hIP1_simd_lac2e < sim_discard, 0,
                                  ifelse(hIP1_simd_lac2e > sim_d, 
                                         hIP1_lac2_yield * milk_price * (sim_d - hIP1_simd_del2)/hIP1_lac2_period,
                                         ifelse(hIP1_simd_del2 < sim_discard,
                                                hIP1_lac2_yield * milk_price *
                                                  (hIP1_simd_lac2e - sim_discard)/hIP1_lac2_period,
                                                hIP1_lac2_yield * milk_price * hIP1_lac2_period / milk_day))))

#AI for third pregnancy
hIP1_AI3 <- numeric(IP1)
for (i in 1:IP1){
  hIP1_AI3[i] <- rpois(1, ifelse(hIP1_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
hIP1_AI3

#Removal of cattle
hIP1_preg3_removed <- ifelse(hIP1_AI3 > nAI_remove-1, 1, 
                             ifelse(hIP1_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))

#Third pregnancy age
hIP1age_preg3 <- hIP1_day_del2 + day_wait_AI + 21*BIP1_AI3

#Third pregnancy simulation day
hIP1_simd_preg3 <- hIP1_simd_del2 + day_wait_AI + 21*hIP1_AI3


#B2.1.3. Third parity----
#Third parity age
hIP1age_del3 <- hIP1age_preg3 + preg_period

#Third parity simulation day
hIP1_simd_del3 <- hIP1_simd_preg3 + preg_period 

#BLV status of IP1 at third pregnancy
hIP1_BLV_del3 <- rep(0, IP1)

#Mastitis at third parity
hIP1_mas3 <- numeric(IP1)
for (i in 1:IP1){
  hIP1_mas3[i] <- rbinom(1, 1, pmasmat[1 + hIP1_BLV_del3[i], 3])
}
hIP1_mas3

#Third lactation period
hIP1_lac3_period <- ifelse(BIP1_mas3==1, ifelse(BIP1_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
hIP1_lac3_yield <- ifelse(hIP1_mas3==1, ifelse(hIP1_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
hIP1age_lac3e <- hIP1age_del3 + hIP1_lac3_period 

#Third lactation end simulation day
hIP1_simd_lac3e <- hIP1_simd_del3 + hIP1_lac3_period

#Third lactation income #linear score 5 milk price not considered
hIP1_lac3_income <- ifelse(hIP1_simd_del3 > sim_d, 0,
                           ifelse(hIP1_simd_lac3e < sim_discard, 0,
                                  ifelse(hIP1_simd_lac3e > sim_d, 
                                         hIP1_lac3_yield * milk_price * (sim_d - hIP1_simd_del3)/hIP1_lac3_period *
                                           (1 - hIP1_preg3_removed),
                                         ifelse(hIP1_simd_del3 < sim_discard,
                                                hIP1_lac3_yield * milk_price *
                                                  (hIP1_simd_lac3e - sim_discard)/hIP1_lac3_period *
                                                  (1 - hIP1_preg3_removed),
                                                hIP1_lac3_yield * milk_price * hIP1_lac3_period / milk_day *
                                                  (1 - hIP1_preg3_removed)))))

#AI for fourth pregnancy
hIP1_AI4 <- numeric(IP1)
for (i in 1:IP1){
  hIP1_AI4[i] <- rpois(1, ifelse(hIP1_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
hIP1_AI4

#Removal of cattle
hIP1_preg4_removed <- ifelse(hIP1_AI4 > nAI_remove-1, 1, 
                             ifelse(hIP1_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
hIP1_preg4_removed_cumulative <- ifelse(BIP1_preg3_removed==1, 1, 
                                        ifelse(hIP1_AI4 > nAI_remove-1, 1, 
                                               ifelse(hIP1_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
hIP1age_preg4 <- hIP1age_del3 + day_wait_AI + 21*hIP1_AI4

#Fourth pregnancy simulation day
hIP1_simd_preg4 <- hIP1_simd_del3 + day_wait_AI + 21*hIP1_AI4


#B2.1.4. Fourth parity----
#Fourth parity age
hIP1age_del4 <- hIP1age_preg4 + preg_period

#Fourth parity simulation day
hIP1_simd_del4 <- hIP1_simd_preg4 + preg_period 

#BLV status of IP1 at fourth pregnancy
hIP1_BLV_del4 <- rep(0, IP1)

#Mastitis at fourth parity
hIP1_mas4 <- numeric(IP1)
for (i in 1:IP1){
  hIP1_mas4[i] <- rbinom(1, 1, pmasmat[1 + hIP1_BLV_del4[i], 4])
}
hIP1_mas4

#Fourth lactation period
hIP1_lac4_period <- ifelse(hIP1_mas4==1, ifelse(hIP1_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
hIP1_lac4_yield <- ifelse(hIP1_mas4==1, ifelse(hIP1_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
hIP1age_lac4e <- hIP1age_del4 + hIP1_lac4_period 

#Fourth lactation end simulation day
hIP1_simd_lac4e <- hIP1_simd_del4 + hIP1_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
hIP1_lac4_income <- ifelse(hIP1_simd_del4 > sim_d, 0,
                           ifelse(hIP1_simd_lac4e < sim_discard, 0,
                                  ifelse(hIP1_simd_lac4e > sim_d, 
                                         hIP1_lac4_yield * milk_price * (sim_d - hIP1_simd_del4)/hIP1_lac4_period *
                                           (1 - hIP1_preg4_removed_cumulative),
                                         ifelse(hIP1_simd_del4 < sim_discard,
                                                hIP1_lac4_yield * milk_price *
                                                  (hIP1_simd_lac4e - sim_discard)/hIP1_lac4_period *
                                                  (1 - hIP1_preg4_removed_cumulative),
                                                hIP1_lac4_yield * milk_price * hIP1_lac4_period / milk_day *
                                                  (1 - hIP1_preg4_removed_cumulative)))))

#AI for fifth pregnancy
hIP1_AI5 <- numeric(IP1)
for (i in 1:IP1){
  hIP1_AI5[i] <- rpois(1, ifelse(hIP1_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
hIP1_AI5

#Removal of cattle
hIP1_preg5_removed <- ifelse(hIP1_AI5 > nAI_remove-1, 1, 
                             ifelse(hIP1_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
hIP1_preg5_removed_cumulative <- ifelse(hIP1_preg4_removed_cumulative==1, 1, 
                                        ifelse(hIP1_AI5 > nAI_remove-1, 1, 
                                               ifelse(hIP1_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
hIP1age_preg5 <- hIP1age_del4 + day_wait_AI + 21*hIP1_AI5

#Fifth pregnancy simulation day
hIP1_simd_preg5 <- hIP1_simd_del4 + day_wait_AI + 21*hIP1_AI5


#B2.1.5. Fifth parity----
#Fifth parity age
hIP1age_del5 <- hIP1age_preg5 + preg_period

#Fifth parity simulation day
hIP1_simd_del5 <- hIP1_simd_preg5 + preg_period 

#BLV status of IP1 at fifth pregnancy
hIP1_BLV_del5 <- rep(0, IP1)

#Mastitis at fifth parity
hIP1_mas5 <- numeric(IP1)
for (i in 1:IP1){
  hIP1_mas5[i] <- rbinom(1, 1, pmasmat[1 + hIP1_BLV_del5[i], 5])
}
hIP1_mas5

#Fifth lactation period
hIP1_lac5_period <- ifelse(hIP1_mas5==1, ifelse(hIP1_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
hIP1_lac5_yield <- ifelse(hIP1_mas5==1, ifelse(hIP1_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
hIP1age_lac5e <- hIP1age_del5 + hIP1_lac5_period 

#Fifth lactation end simulation day
hIP1_simd_lac5e <- hIP1_simd_del5 + hIP1_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
hIP1_lac5_income <- ifelse(hIP1_simd_del5 > sim_d, 0,
                           ifelse(hIP1_simd_lac5e < sim_discard, 0,
                                  ifelse(hIP1_simd_lac5e > sim_d, 
                                         hIP1_lac5_yield * milk_price * (sim_d - hIP1_simd_del5)/hIP1_lac5_period *
                                           (1 - hIP1_preg5_removed_cumulative),
                                         ifelse(hIP1_simd_del5 < sim_discard,
                                                hIP1_lac5_yield * milk_price *
                                                  (hIP1_simd_lac5e - sim_discard)/hIP1_lac5_period *
                                                  (1 - hIP1_preg5_removed_cumulative),
                                                hIP1_lac5_yield * milk_price * hIP1_lac5_period / milk_day *
                                                  (1 - hIP1_preg5_removed_cumulative)))))

#AI for sixth pregnancy
hIP1_AI6 <- numeric(IP1)
for (i in 1:IP1){
  hIP1_AI6[i] <- rpois(1, ifelse(hIP1_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
hIP1_AI6

#Removal of cattle
hIP1_preg6_removed <- ifelse(hIP1_AI6 > nAI_remove-1, 1, 
                             ifelse(hIP1_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
hIP1_preg6_removed_cumulative <- ifelse(hIP1_preg5_removed_cumulative==1, 1, 
                                        ifelse(hIP1_AI6 > nAI_remove-1, 1, 
                                               ifelse(hIP1_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
hIP1age_preg6 <- hIP1age_del5 + day_wait_AI + 21*hIP1_AI6

#Sixth pregnancy simulation day
hIP1_simd_preg6 <- hIP1_simd_del5 + day_wait_AI + 21*hIP1_AI6


#B2.2. IP2 simulation----
#B2.2.3. Third parity----
#Simulation day of third parity
hIP2_simd_del3 <- hIP2_day_del3 - hIP2age

#BLV status of IP2 at third pregnancy
hIP2_BLV_del3 <- rep(0, IP2)

#Mastitis at third parity
hIP2_mas3 <- numeric(IP2)
for (i in 1:IP2){
  hIP2_mas3[i] <- rbinom(1, 1, pmasmat[1 + hIP2_BLV_del3[i], 3])
}
hIP2_mas3

#Third lactation period
hIP2_lac3_period <- ifelse(hIP2_mas3==1, ifelse(hIP2_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
hIP2_lac3_yield <- ifelse(hIP2_mas3==1, ifelse(hIP2_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
hIP2age_lac3e <- hIP2_day_del3 + hIP2_lac3_period 

#Third lactation end simulation day
hIP2_simd_lac3e <- hIP2_simd_del3 + hIP2_lac3_period

#Third lactation income #linear score 5 milk price not considered
hIP2_lac3_income <- ifelse(hIP2_simd_del3 > sim_d, 0,
                           ifelse(hIP2_simd_lac3e < sim_discard, 0,
                                  ifelse(hIP2_simd_lac3e > sim_d, 
                                         hIP2_lac3_yield * milk_price * (sim_d - hIP2_simd_del3)/hIP2_lac3_period,
                                         ifelse(hIP2_simd_del3 < sim_discard,
                                                hIP2_lac3_yield * milk_price *
                                                  (hIP2_simd_lac3e - sim_discard)/hIP2_lac3_period,
                                                hIP2_lac3_yield * milk_price * hIP2_lac3_period / milk_day))))

#AI for fourth pregnancy
hIP2_AI4 <- numeric(IP2)
for (i in 1:IP2){
  hIP2_AI4[i] <- rpois(1, ifelse(hIP2_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
hIP2_AI4

#Removal of cattle
hIP2_preg4_removed <- ifelse(hIP2_AI4 > nAI_remove-1, 1, 
                             ifelse(hIP2_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))

#Fourth pregnancy age
hIP2age_preg4 <- hIP2_day_del3 + day_wait_AI + 21*hIP2_AI4

#Fourth pregnancy simulation day
hIP2_simd_preg4 <- hIP2_simd_del3 + day_wait_AI + 21*hIP2_AI4


#B2.2.4. Fourth parity----
#Fourth parity age
hIP2age_del4 <- hIP2age_preg4 + preg_period

#Fourth parity simulation day
hIP2_simd_del4 <- hIP2_simd_preg4 + preg_period 

#BLV status of IP1 at fourth pregnancy
hIP2_BLV_del4 <- rep(0, IP2)

#Mastitis at fourth parity
hIP2_mas4 <- numeric(IP2)
for (i in 1:IP2){
  hIP2_mas4[i] <- rbinom(1, 1, pmasmat[1 + hIP2_BLV_del4[i], 4])
}
hIP2_mas4

#Fourth lactation period
hIP2_lac4_period <- ifelse(hIP2_mas4==1, ifelse(hIP2_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
hIP2_lac4_yield <- ifelse(hIP2_mas4==1, ifelse(hIP2_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)


#Fourth lactation end age
hIP2age_lac4e <- hIP2age_del4 + hIP2_lac4_period 

#Fourth lactation end simulation day
hIP2_simd_lac4e <- hIP2_simd_del4 + hIP2_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
hIP2_lac4_income <- ifelse(hIP2_simd_del4 > sim_d, 0,
                           ifelse(hIP2_simd_lac4e < sim_discard, 0,
                                  ifelse(hIP2_simd_lac4e > sim_d, 
                                         hIP2_lac4_yield * milk_price * (sim_d - hIP2_simd_del4)/hIP2_lac4_period *
                                           (1 - hIP2_preg4_removed),
                                         ifelse(hIP2_simd_del4 < sim_discard,
                                                hIP2_lac4_yield * milk_price *
                                                  (hIP2_simd_lac4e - sim_discard)/hIP2_lac4_period *
                                                  (1 - hIP2_preg4_removed),
                                                hIP2_lac4_yield * milk_price * hIP2_lac4_period / milk_day *
                                                  (1 - hIP2_preg4_removed)))))

#AI for fifth pregnancy
hIP2_AI5 <- numeric(IP2)
for (i in 1:IP2){
  hIP2_AI5[i] <- rpois(1, ifelse(hIP2_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
hIP2_AI5

#Removal of cattle
hIP2_preg5_removed <- ifelse(hIP2_AI5 > nAI_remove-1, 1, 
                             ifelse(hIP2_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
hIP2_preg5_removed_cumulative <- ifelse(hIP2_preg4_removed==1, 1, 
                                        ifelse(hIP2_AI5 > nAI_remove-1, 1, 
                                               ifelse(hIP2_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
hIP2age_preg5 <- hIP2age_del4 + day_wait_AI + 21*hIP2_AI5

#Fifth pregnancy simulation day
hIP2_simd_preg5 <- hIP2_simd_del4 + day_wait_AI + 21*hIP2_AI5


#B2.2.5. Fifth parity----
#Fifth parity age
hIP2age_del5 <- hIP2age_preg5 + preg_period

#Fifth parity simulation day
hIP2_simd_del5 <- hIP2_simd_preg5 + preg_period 

#BLV status of IP2 at fifth pregnancy
hIP2_BLV_del5 <- rep(0, IP2)

#Mastitis at fifth parity
hIP2_mas5 <- numeric(IP2)
for (i in 1:IP2){
  hIP2_mas5[i] <- rbinom(1, 1, pmasmat[1 + hIP2_BLV_del5[i], 5])
}
hIP2_mas5

#Fifth lactation period
hIP2_lac5_period <- ifelse(hIP2_mas5==1, ifelse(hIP2_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
hIP2_lac5_yield <- ifelse(hIP2_mas5==1, ifelse(hIP2_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
hIP2age_lac5e <- hIP2age_del5 + hIP2_lac5_period 

#Fifth lactation end simulation day
hIP2_simd_lac5e <- hIP2_simd_del5 + hIP2_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
hIP2_lac5_income <- ifelse(BIP2_simd_del5 > sim_d, 0,
                           ifelse(hIP2_simd_lac5e < sim_discard, 0,
                                  ifelse(hIP2_simd_lac5e > sim_d, 
                                         BIP2_lac5_yield * milk_price * (sim_d - hIP2_simd_del5)/hIP2_lac5_period *
                                           (1 - hIP2_preg5_removed_cumulative),
                                         ifelse(hIP2_simd_del5 < sim_discard,
                                                hIP2_lac5_yield * milk_price *
                                                  (hIP2_simd_lac5e - sim_discard)/hIP2_lac5_period *
                                                  (1 - hIP2_preg5_removed_cumulative),
                                                hIP2_lac5_yield * milk_price * hIP2_lac5_period / milk_day *
                                                  (1 - hIP2_preg5_removed_cumulative)))))

#AI for sixth pregnancy
hIP2_AI6 <- numeric(IP2)
for (i in 1:IP2){
  hIP2_AI6[i] <- rpois(1, ifelse(hIP2_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
hIP2_AI6

#Removal of cattle
hIP2_preg6_removed <- ifelse(hIP2_AI6 > nAI_remove-1, 1, 
                             ifelse(hIP2_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
hIP2_preg6_removed_cumulative <- ifelse(hIP2_preg5_removed_cumulative==1, 1, 
                                        ifelse(hIP2_AI6 > nAI_remove-1, 1, 
                                               ifelse(hIP2_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
hIP2age_preg6 <- hIP2age_del5 + day_wait_AI + 21*hIP2_AI6

#Sixth pregnancy simulation day
hIP2_simd_preg6 <- hIP2_simd_del5 + day_wait_AI + 21*hIP2_AI6


#B2.3. IP3 simulation----
#B2.3.4. Fourth parity----
#Simulation day of fourth parity
hIP3_simd_del4 <- hIP3_day_del4 - hIP3age

#BLV status of IP3 at fourth pregnancy
hIP3_BLV_del4 <- rep(0, IP3)

#Mastitis at fourth parity
hIP3_mas4 <- numeric(IP3)
for (i in 1:IP3){
  hIP3_mas4[i] <- rbinom(1, 1, pmasmat[1 + hIP3_BLV_del4[i], 4])
}
hIP3_mas4

#Fourth lactation period
hIP3_lac4_period <- ifelse(hIP3_mas4==1, ifelse(hIP3_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
hIP3_lac4_yield <- ifelse(hIP3_mas4==1, ifelse(hIP3_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)

#Fourth lactation end age
hIP3age_lac4e <- hIP3_day_del4 + hIP3_lac4_period 

#Fourth lactation end simulation day
hIP3_simd_lac4e <- hIP3_simd_del4 + hIP3_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
hIP3_lac4_income <- ifelse(hIP3_simd_del4 > sim_d, 0,
                           ifelse(hIP3_simd_lac4e < sim_discard, 0,
                                  ifelse(hIP3_simd_lac4e > sim_d, 
                                         hIP3_lac4_yield * milk_price * (sim_d - hIP3_simd_del4)/hIP3_lac4_period,
                                         ifelse(hIP3_simd_del4 < sim_discard,
                                                hIP3_lac4_yield * milk_price *
                                                  (hIP3_simd_lac4e - sim_discard)/hIP3_lac4_period,
                                                hIP3_lac4_yield * milk_price * hIP3_lac4_period / milk_day))))

#AI for fifth pregnancy
hIP3_AI5 <- numeric(IP3)
for (i in 1:IP3){
  hIP3_AI5[i] <- rpois(1, ifelse(hIP3_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
hIP3_AI5

#Removal of cattle
hIP3_preg5_removed <- ifelse(hIP3_AI5 > nAI_remove-1, 1, 
                             ifelse(hIP3_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))

#Fifth pregnancy age
hIP3age_preg5 <- hIP3_day_del4 + day_wait_AI + 21*hIP3_AI5

#Fifth pregnancy simulation day
hIP3_simd_preg5 <- hIP3_simd_del4 + day_wait_AI + 21*hIP3_AI5


#B2.3.5. Fifth parity----
#Fifth parity age
hIP3age_del5 <- hIP3age_preg5 + preg_period

#Fifth parity simulation day
hIP3_simd_del5 <- hIP3_simd_preg5 + preg_period 

#BLV status of IP3 at fifth pregnancy
hIP3_BLV_del5 <- rep(0, IP3)

#Mastitis at fifth parity
hIP3_mas5 <- numeric(IP3)
for (i in 1:IP3){
  hIP3_mas5[i] <- rbinom(1, 1, pmasmat[1 + hIP3_BLV_del5[i], 5])
}
hIP3_mas5

#Fifth lactation period
hIP3_lac5_period <- ifelse(hIP3_mas5==1, ifelse(hIP3_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
hIP3_lac5_yield <- ifelse(hIP3_mas5==1, ifelse(hIP3_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
hIP3age_lac5e <- hIP3age_del5 + hIP3_lac5_period 

#Fifth lactation end simulation day
hIP3_simd_lac5e <- hIP3_simd_del5 + hIP3_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
hIP3_lac5_income <- ifelse(hIP3_simd_del5 > sim_d, 0,
                           ifelse(hIP3_simd_lac5e < sim_discard, 0,
                                  ifelse(hIP3_simd_lac5e > sim_d, 
                                         hIP3_lac5_yield * milk_price * (sim_d - hIP3_simd_del5)/hIP3_lac5_period * 
                                           (1 - hIP3_preg5_removed),
                                         ifelse(hIP3_simd_del5 < sim_discard,
                                                hIP3_lac5_yield * milk_price *
                                                  (hIP3_simd_lac5e - sim_discard)/hIP3_lac5_period * 
                                                  (1 - hIP3_preg5_removed),
                                                hIP3_lac5_yield * milk_price * hIP3_lac5_period / milk_day) * 
                                           (1 - hIP3_preg5_removed))))

#AI for sixth pregnancy
hIP3_AI6 <- numeric(IP3)
for (i in 1:IP3){
  hIP3_AI6[i] <- rpois(1, ifelse(hIP3_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
hIP3_AI6

#Removal of cattle
hIP3_preg6_removed <- ifelse(hIP3_AI6 > nAI_remove-1, 1, 
                             ifelse(hIP3_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
hIP3_preg6_removed_cumulative <- ifelse(hIP3_preg5_removed==1, 1, 
                                        ifelse(hIP3_AI6 > nAI_remove-1, 1, 
                                               ifelse(BIP3_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
hIP3age_preg6 <- hIP3age_del5 + day_wait_AI + 21*hIP3_AI6

#Sixth pregnancy simulation day
hIP3_simd_preg6 <- hIP3_simd_del5 + day_wait_AI + 21*hIP3_AI6


#B2.4. IP4 simulation----
#B2.4.5. Fifth parity----
#Simulation day of fifth parity
hIP4_simd_del5 <- hIP4_day_del5 - hIP4age

#BLV status of IP4 at fifth pregnancy
hIP4_BLV_del5 <- rep(0, IP4)

#Mastitis at fifth parity
hIP4_mas5 <- numeric(IP4)
for (i in 1:IP4){
  hIP4_mas5[i] <- rbinom(1, 1, pmasmat[1 + hIP4_BLV_del5[i], 5])
}
hIP4_mas5

#Fifth lactation period
hIP4_lac5_period <- ifelse(hIP4_mas5==1, ifelse(hIP4_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
hIP4_lac5_yield <- ifelse(hIP4_mas5==1, ifelse(hIP4_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
hIP4age_lac5e <- hIP4_day_del5 + hIP4_lac5_period 

#Fifth lactation end simulation day
hIP4_simd_lac5e <- hIP4_simd_del5 + hIP4_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
hIP4_lac5_income <- ifelse(hIP4_simd_del5 > sim_d, 0,
                           ifelse(hIP4_simd_lac5e < sim_discard, 0,
                                  ifelse(hIP4_simd_lac5e > sim_d, 
                                         hIP4_lac5_yield * milk_price * (sim_d - hIP4_simd_del5)/hIP4_lac5_period,
                                         ifelse(hIP4_simd_del5 < sim_discard,
                                                hIP4_lac5_yield * milk_price *
                                                  (hIP4_simd_lac5e - sim_discard)/hIP4_lac5_period,
                                                hIP4_lac5_yield * milk_price * hIP4_lac5_period / milk_day))))

#AI for sixth pregnancy
hIP4_AI6 <- numeric(IP4)
for (i in 1:IP4){
  hIP4_AI6[i] <- rpois(1, ifelse(hIP4_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
hIP4_AI6

#Removal of cattle
hIP4_preg6_removed <- ifelse(hIP4_AI6 > nAI_remove-1, 1, 
                             ifelse(hIP4_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))

#Sixth pregnancy age
hIP4age_preg6 <- hIP4_day_del5 + day_wait_AI + 21*hIP4_AI6

#Sixth pregnancy simulation day
hIP4_simd_preg6 <- hIP4_simd_del5 + day_wait_AI + 21*hIP4_AI6


#B2.5. IP5 simulation----
#This model assumes that after milking fifth parity, cattle are culled. 
#By the end of 365 days discarding time, cattle would not be in the herd. Therefore parity five is not simulated.


#B2.6. New borne cows simulation----
#B2.6.1. Borne in year 1----
#B2.6.1.1. First parity----
#Simulation day of first parity
hNB1_simd_del1 <- DB_NB1 + hNB1_day_del1

#Mastitis at first parity
hNB1_mas1 <- numeric(NB1)
for (i in 1:NB1){
  hNB1_mas1[i] <- rbinom(1, 1, pmasmat[1 + hNB1_BLV[i], 1])
}
hNB1_mas1

#First lactation period
hNB1_lac1_period <- ifelse(hNB1_mas1==1, ifelse(hNB1_BLV==2, days_mas_L, days_mas_SI), milk_day)

#First lactation milk yield
hNB1_lac1_yield <- ifelse(hNB1_mas1==1, ifelse(hNB1_BLV==2, yield_P1 * days_mas_L / milk_day, 
                                               yield_P1 * days_mas_SI / milk_day), yield_P1)

#First lactation end age
hNB1age_lac1e <- hNB1_day_del1 + hNB1_lac1_period 

#First lactation end simulation day
hNB1_simd_lac1e <- hNB1_simd_del1 + hNB1_lac1_period

#First lactation income #linear score 5 milk price not considered
hNB1_lac1_income <- ifelse(hNB1_simd_del1 > sim_d, 0,
                           ifelse(hNB1_simd_lac1e < sim_discard, 0,
                                  ifelse(hNB1_simd_lac1e > sim_d, 
                                         hNB1_lac1_yield * milk_price * (sim_d - hNB1_simd_del1)/hNB1_lac1_period,
                                         ifelse(hNB1_simd_del1 < sim_discard,
                                                hNB1_lac1_yield * milk_price *
                                                  (hNB1_simd_lac1e - sim_discard)/hNB1_lac1_period,
                                                hNB1_lac1_yield * milk_price * hNB1_lac1_period / milk_day))))

#AI for second pregnancy
hNB1_AI2 <- numeric(NB1)
for (i in 1:NB1){
  hNB1_AI2[i] <- rpois(1, ifelse(hNB1_mas1[i]==1, nAI_masmat[1,1], nAI_masmat[2,1]))
}
hNB1_AI2

#Removal of cattle
hNB1_preg2_removed <- ifelse(hNB1_AI2 > nAI_remove-1, 1, 
                             ifelse(hNB1_mas1 == 1, rbinom(1, 1, cull_mas1), 
                                    rbinom(1, 1, cull_nomas1)))

#Second pregnancy age
hNB1age_preg2 <- hNB1_day_del1 + day_wait_AI + 21*hNB1_AI2

#Second pregnancy simulation day
hNB1_simd_preg2 <- hNB1_simd_del1 + day_wait_AI + 21*hNB1_AI2


#B2.6.1.2. Second parity----
#Second parity age
hNB1age_del2 <- hNB1age_preg2 + preg_period

#Second parity simulation day
hNB1_simd_del2 <- hNB1_simd_preg2 + preg_period 

#BLV status of NB1 at second pregnancy
hNB1_BLV_del2 <- rep(0, NB1)

#Mastitis at second parity
hNB1_mas2 <- numeric(NB1)
for (i in 1:NB1){
  hNB1_mas2[i] <- rbinom(1, 1, pmasmat[1 + hNB1_BLV_del2[i], 2])
}
hNB1_mas2

#Second lactation period
hNB1_lac2_period <- ifelse(hNB1_mas2==1, ifelse(hNB1_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
hNB1_lac2_yield <- ifelse(hNB1_mas2==1, ifelse(hNB1_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
hNB1age_lac2e <- hNB1age_del2 + hNB1_lac2_period 

#Second lactation end simulation day
hNB1_simd_lac2e <- hNB1_simd_del2 + hNB1_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
hNB1_lac2_income <- ifelse(hNB1_simd_del2 > sim_d, 0,
                           ifelse(hNB1_simd_lac2e < sim_discard, 0,
                                  ifelse(hNB1_simd_lac2e > sim_d, 
                                         hNB1_lac2_yield * milk_price * (sim_d - hNB1_simd_del2)/hNB1_lac2_period *
                                           (1 - hNB1_preg2_removed),
                                         ifelse(hNB1_simd_del2 < sim_discard,
                                                hNB1_lac2_yield * milk_price *
                                                  (hNB1_simd_lac2e - sim_discard)/hNB1_lac2_period *
                                                  (1 - hNB1_preg2_removed),
                                                hNB1_lac2_yield * milk_price * hNB1_lac2_period / milk_day *
                                                  (1 - hNB1_preg2_removed)))))

#AI for third pregnancy
hNB1_AI3 <- numeric(NB1)
for (i in 1:NB1){
  hNB1_AI3[i] <- rpois(1, ifelse(hNB1_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
hNB1_AI3

#Removal of cattle
hNB1_preg3_removed <- ifelse(hNB1_AI3 > nAI_remove-1, 1, 
                             ifelse(hNB1_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))
hNB1_preg3_removed_cumulative <- ifelse(hNB1_preg2_removed==1, 1, 
                                        ifelse(hNB1_AI3 > nAI_remove-1, 1, 
                                               ifelse(hNB1_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                                      rbinom(1, 1, cull_nomas2))))

#Third pregnancy age
hNB1age_preg3 <- hNB1age_del2 + day_wait_AI + 21*hNB1_AI3

#Third pregnancy simulation day
hNB1_simd_preg3 <- hNB1_simd_del2 + day_wait_AI + 21*hNB1_AI3


#B2.6.1.3. Third parity----
#Third parity age
hNB1age_del3 <- hNB1age_preg3 + preg_period

#Third parity simulation day
hNB1_simd_del3 <- hNB1_simd_preg3 + preg_period 

#BLV status of NB1 at third pregnancy
hNB1_BLV_del3 <- rep(0, NB1)

#Mastitis at third parity
hNB1_mas3 <- numeric(NB1)
for (i in 1:NB1){
  hNB1_mas3[i] <- rbinom(1, 1, pmasmat[1 + hNB1_BLV_del3[i], 3])
}
hNB1_mas3

#Third lactation period
hNB1_lac3_period <- ifelse(hNB1_mas3==1, ifelse(hNB1_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
hNB1_lac3_yield <- ifelse(hNB1_mas3==1, ifelse(hNB1_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
hNB1age_lac3e <- hNB1age_del3 + hNB1_lac3_period 

#Third lactation end simulation day
hNB1_simd_lac3e <- hNB1_simd_del3 + hNB1_lac3_period

#Third lactation income #linear score 5 milk price not considered
hNB1_lac3_income <- ifelse(hNB1_simd_del3 > sim_d, 0,
                           ifelse(hNB1_simd_lac3e < sim_discard, 0,
                                  ifelse(hNB1_simd_lac3e > sim_d, 
                                         hNB1_lac3_yield * milk_price * (sim_d - hNB1_simd_del3)/hNB1_lac3_period *
                                           (1 - hNB1_preg3_removed_cumulative),
                                         ifelse(hNB1_simd_del3 < sim_discard,
                                                hNB1_lac3_yield * milk_price *
                                                  (hNB1_simd_lac3e - sim_discard)/hNB1_lac3_period *
                                                  (1 - hNB1_preg3_removed_cumulative),
                                                hNB1_lac3_yield * milk_price * hNB1_lac3_period / milk_day *
                                                  (1 - hNB1_preg3_removed_cumulative)))))

#AI for fourth pregnancy
hNB1_AI4 <- numeric(NB1)
for (i in 1:NB1){
  hNB1_AI4[i] <- rpois(1, ifelse(hNB1_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
hNB1_AI4

#Removal of cattle
hNB1_preg4_removed <- ifelse(hNB1_AI4 > nAI_remove-1, 1, 
                             ifelse(hNB1_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
hNB1_preg4_removed_cumulative <- ifelse(hNB1_preg3_removed_cumulative==1, 1, 
                                        ifelse(hNB1_AI4 > nAI_remove-1, 1, 
                                               ifelse(hNB1_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
hNB1age_preg4 <- hNB1age_del3 + day_wait_AI + 21*hNB1_AI4

#Fourth pregnancy simulation day
hNB1_simd_preg4 <- hNB1_simd_del3 + day_wait_AI + 21*hNB1_AI4


#B2.6.1.4. Fourth parity----
#Fourth parity age
hNB1age_del4 <- hNB1age_preg4 + preg_period

#Fourth parity simulation day
hNB1_simd_del4 <- hNB1_simd_preg4 + preg_period 

#BLV status of NB1 at fourth pregnancy
hNB1_BLV_del4 <- rep(0, NB1)

#Mastitis at fourth parity
hNB1_mas4 <- numeric(NB1)
for (i in 1:NB1){
  hNB1_mas4[i] <- rbinom(1, 1, pmasmat[1 + hNB1_BLV_del4[i], 4])
}
hNB1_mas4

#Fourth lactation period
hNB1_lac4_period <- ifelse(hNB1_mas4==1, ifelse(hNB1_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
hNB1_lac4_yield <- ifelse(hNB1_mas4==1, ifelse(hNB1_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)

#Fourth lactation end age
hNB1age_lac4e <- hNB1age_del4 + hNB1_lac4_period 

#Fourth lactation end simulation day
hNB1_simd_lac4e <- hNB1_simd_del4 + hNB1_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
hNB1_lac4_income <- ifelse(hNB1_simd_del4 > sim_d, 0,
                           ifelse(hNB1_simd_lac4e < sim_discard, 0,
                                  ifelse(hNB1_simd_lac4e > sim_d, 
                                         hNB1_lac4_yield * milk_price * (sim_d - hNB1_simd_del4)/hNB1_lac4_period *
                                           (1 - hNB1_preg4_removed_cumulative),
                                         ifelse(hNB1_simd_del4 < sim_discard,
                                                hNB1_lac4_yield * milk_price *
                                                  (hNB1_simd_lac4e - sim_discard)/hNB1_lac4_period *
                                                  (1 - hNB1_preg4_removed_cumulative),
                                                hNB1_lac4_yield * milk_price * hNB1_lac4_period / milk_day *
                                                  (1 - hNB1_preg4_removed_cumulative)))))


#AI for fifth pregnancy
hNB1_AI5 <- numeric(NB1)
for (i in 1:NB1){
  hNB1_AI5[i] <- rpois(1, ifelse(hNB1_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
hNB1_AI5

#Removal of cattle
hNB1_preg5_removed <- ifelse(hNB1_AI5 > nAI_remove-1, 1, 
                             ifelse(hNB1_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
hNB1_preg5_removed_cumulative <- ifelse(hNB1_preg4_removed_cumulative==1, 1, 
                                        ifelse(hNB1_AI5 > nAI_remove-1, 1, 
                                               ifelse(hNB1_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
hNB1age_preg5 <- hNB1age_del4 + day_wait_AI + 21*hNB1_AI5

#Fifth pregnancy simulation day
hNB1_simd_preg5 <- hNB1_simd_del4 + day_wait_AI + 21*hNB1_AI5


#B2.6.1.5. Fifth parity----
#Fifth parity age
hNB1age_del5 <- hNB1age_preg5 + preg_period

#Fifth parity simulation day
hNB1_simd_del5 <- hNB1_simd_preg5 + preg_period 

#BLV status of NB1 at fifth pregnancy
hNB1_BLV_del5 <- rep(0, NB1)

#Mastitis at fifth parity
hNB1_mas5 <- numeric(NB1)
for (i in 1:NB1){
  hNB1_mas5[i] <- rbinom(1, 1, pmasmat[1 + hNB1_BLV_del5[i], 5])
}
hNB1_mas5

#Fifth lactation period
hNB1_lac5_period <- ifelse(hNB1_mas5==1, ifelse(hNB1_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
hNB1_lac5_yield <- ifelse(hNB1_mas5==1, ifelse(hNB1_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)


#Fifth lactation end age
hNB1age_lac5e <- hNB1age_del5 + hNB1_lac5_period 

#Fifth lactation end simulation day
hNB1_simd_lac5e <- hNB1_simd_del5 + hNB1_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
hNB1_lac5_income <- ifelse(hNB1_simd_del5 > sim_d, 0,
                           ifelse(hNB1_simd_lac5e < sim_discard, 0,
                                  ifelse(hNB1_simd_lac5e > sim_d, 
                                         hNB1_lac5_yield * milk_price * (sim_d - hNB1_simd_del5)/hNB1_lac5_period *
                                           (1 - hNB1_preg5_removed_cumulative),
                                         ifelse(hNB1_simd_del5 < sim_discard,
                                                hNB1_lac5_yield * milk_price *
                                                  (hNB1_simd_lac5e - sim_discard)/hNB1_lac5_period *
                                                  (1 - hNB1_preg5_removed_cumulative),
                                                hNB1_lac5_yield * milk_price * hNB1_lac5_period / milk_day *
                                                  (1 - hNB1_preg5_removed_cumulative)))))

#AI for sixth pregnancy
hNB1_AI6 <- numeric(NB1)
for (i in 1:NB1){
  hNB1_AI6[i] <- rpois(1, ifelse(hNB1_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
hNB1_AI6

#Removal of cattle
hNB1_preg6_removed <- ifelse(hNB1_AI6 > nAI_remove-1, 1, 
                             ifelse(hNB1_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
hNB1_preg6_removed_cumulative <- ifelse(hNB1_preg5_removed_cumulative==1, 1, 
                                        ifelse(hNB1_AI6 > nAI_remove-1, 1, 
                                               ifelse(hNB1_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
hNB1age_preg6 <- hNB1age_del5 + day_wait_AI + 21*hNB1_AI6

#Sixth pregnancy simulation day
hNB1_simd_preg6 <- hNB1_simd_del5 + day_wait_AI + 21*hNB1_AI6


#B2.6.2. Borne in year 2----
#B2.6.2.1. First parity----
#Simulation day of first parity
hNB2_simd_del1 <- DB_NB2 + hNB2_day_del1

#Mastitis at first parity
hNB2_mas1 <- numeric(NB2)
for (i in 1:NB2){
  hNB2_mas1[i] <- rbinom(1, 1, pmasmat[1 + hNB2_BLV[i], 1])
}
hNB2_mas1

#First lactation period
hNB2_lac1_period <- ifelse(hNB2_mas1==1, ifelse(hNB2_BLV==2, days_mas_L, days_mas_SI), milk_day)

#First lactation milk yield
hNB2_lac1_yield <- ifelse(hNB2_mas1==1, ifelse(hNB2_BLV==2, yield_P1 * days_mas_L / milk_day, 
                                               yield_P1 * days_mas_SI / milk_day), yield_P1)

#First lactation end age
hNB2age_lac1e <- hNB2_day_del1 + hNB2_lac1_period 

#First lactation end simulation day
hNB2_simd_lac1e <- hNB2_simd_del1 + hNB2_lac1_period

#First lactation income #linear score 5 milk price not considered
hNB2_lac1_income <- ifelse(hNB2_simd_del1 > sim_d, 0,
                           ifelse(hNB2_simd_lac1e < sim_discard, 0,
                                  ifelse(hNB2_simd_lac1e > sim_d, 
                                         hNB2_lac1_yield * milk_price * (sim_d - hNB2_simd_del1)/hNB2_lac1_period,
                                         ifelse(hNB2_simd_del1 < sim_discard,
                                                hNB2_lac1_yield * milk_price *
                                                  (hNB2_simd_lac1e - sim_discard)/hNB2_lac1_period,
                                                hNB2_lac1_yield * milk_price * hNB2_lac1_period / milk_day))))

#AI for second pregnancy
hNB2_AI2 <- numeric(NB2)
for (i in 1:NB2){
  hNB2_AI2[i] <- rpois(1, ifelse(hNB2_mas1[i]==1, nAI_masmat[1,1], nAI_masmat[2,1]))
}
hNB2_AI2

#Removal of cattle
hNB2_preg2_removed <- ifelse(hNB2_AI2 > nAI_remove-1, 1, 
                             ifelse(hNB2_mas1 == 1, rbinom(1, 1, cull_mas1), 
                                    rbinom(1, 1, cull_nomas1)))

#Second pregnancy age
hNB2age_preg2 <- hNB2_day_del1 + day_wait_AI + 21*hNB2_AI2

#Second pregnancy simulation day
hNB2_simd_preg2 <- hNB2_simd_del1 + day_wait_AI + 21*hNB2_AI2


#B2.6.2.2. Second parity----
#Second parity age
hNB2age_del2 <- hNB2age_preg2 + preg_period

#Second parity simulation day
hNB2_simd_del2 <- hNB2_simd_preg2 + preg_period 

#BLV status of NB1 at second pregnancy
hNB2_BLV_del2 <- rep(0, NB2)

#Mastitis at second parity
hNB2_mas2 <- numeric(NB2)
for (i in 1:NB2){
  hNB2_mas2[i] <- rbinom(1, 1, pmasmat[1 + hNB2_BLV_del2[i], 2])
}
hNB2_mas2

#Second lactation period
hNB2_lac2_period <- ifelse(hNB2_mas2==1, ifelse(hNB2_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
hNB2_lac2_yield <- ifelse(hNB2_mas2==1, ifelse(hNB2_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
hNB2age_lac2e <- hNB2age_del2 + hNB2_lac2_period 

#Second lactation end simulation day
hNB2_simd_lac2e <- hNB2_simd_del2 + hNB2_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
hNB2_lac2_income <- ifelse(hNB2_simd_del2 > sim_d, 0,
                           ifelse(hNB2_simd_lac2e < sim_discard, 0,
                                  ifelse(hNB2_simd_lac2e > sim_d, 
                                         hNB2_lac2_yield * milk_price * (sim_d - hNB2_simd_del2)/hNB2_lac2_period *
                                           (1 - hNB2_preg2_removed),
                                         ifelse(hNB2_simd_del2 < sim_discard,
                                                hNB2_lac2_yield * milk_price *
                                                  (hNB2_simd_lac2e - sim_discard)/hNB2_lac2_period *
                                                  (1 - hNB2_preg2_removed),
                                                hNB2_lac2_yield * milk_price * hNB2_lac2_period / milk_day *
                                                  (1 - hNB2_preg2_removed)))))

#AI for third pregnancy
hNB2_AI3 <- numeric(NB2)
for (i in 1:NB2){
  hNB2_AI3[i] <- rpois(1, ifelse(hNB2_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
hNB2_AI3

#Removal of cattle
hNB2_preg3_removed <- ifelse(hNB2_AI3 > nAI_remove-1, 1, 
                             ifelse(hNB2_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))
hNB2_preg3_removed_cumulative <- ifelse(hNB2_preg2_removed==1, 1, 
                                        ifelse(hNB2_AI3 > nAI_remove-1, 1, 
                                               ifelse(hNB2_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                                      rbinom(1, 1, cull_nomas2))))

#Third pregnancy age
hNB2age_preg3 <- hNB2age_del2 + day_wait_AI + 21*hNB2_AI3

#Third pregnancy simulation day
hNB2_simd_preg3 <- hNB2_simd_del2 + day_wait_AI + 21*hNB2_AI3


#B2.6.2.3. Third parity----
#Third parity age
hNB2age_del3 <- hNB2age_preg3 + preg_period

#Third parity simulation day
hNB2_simd_del3 <- hNB2_simd_preg3 + preg_period 

#BLV status of NB1 at third pregnancy
hNB2_BLV_del3 <- rep(0, NB2)

#Mastitis at third parity
hNB2_mas3 <- numeric(NB2)
for (i in 1:NB2){
  hNB2_mas3[i] <- rbinom(1, 1, pmasmat[1 + hNB2_BLV_del3[i], 3])
}
hNB2_mas3

#Third lactation period
hNB2_lac3_period <- ifelse(hNB2_mas3==1, ifelse(hNB2_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
hNB2_lac3_yield <- ifelse(hNB2_mas3==1, ifelse(hNB2_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
hNB2age_lac3e <- hNB2age_del3 + hNB2_lac3_period 

#Third lactation end simulation day
hNB2_simd_lac3e <- hNB2_simd_del3 + hNB2_lac3_period

#Third lactation income #linear score 5 milk price not considered
hNB2_lac3_income <- ifelse(hNB2_simd_del3 > sim_d, 0,
                           ifelse(hNB2_simd_lac3e < sim_discard, 0,
                                  ifelse(hNB2_simd_lac3e > sim_d, 
                                         hNB2_lac3_yield * milk_price * (sim_d - hNB2_simd_del3)/hNB2_lac3_period *
                                           (1 - hNB2_preg3_removed_cumulative),
                                         ifelse(hNB2_simd_del3 < sim_discard,
                                                hNB2_lac3_yield * milk_price *
                                                  (hNB2_simd_lac3e - sim_discard)/hNB2_lac3_period *
                                                  (1 - hNB2_preg3_removed_cumulative),
                                                hNB2_lac3_yield * milk_price * hNB2_lac3_period / milk_day *
                                                  (1 - hNB2_preg3_removed_cumulative)))))

#AI for fourth pregnancy
hNB2_AI4 <- numeric(NB2)
for (i in 1:NB2){
  hNB2_AI4[i] <- rpois(1, ifelse(hNB2_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
hNB2_AI4

#Removal of cattle
hNB2_preg4_removed <- ifelse(hNB2_AI4 > nAI_remove-1, 1, 
                             ifelse(hNB2_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
hNB2_preg4_removed_cumulative <- ifelse(hNB2_preg3_removed_cumulative==1, 1, 
                                        ifelse(hNB2_AI4 > nAI_remove-1, 1, 
                                               ifelse(hNB2_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
hNB2age_preg4 <- hNB2age_del3 + day_wait_AI + 21*hNB2_AI4

#Fourth pregnancy simulation day
hNB2_simd_preg4 <- hNB2_simd_del3 + day_wait_AI + 21*hNB2_AI4


#B2.6.2.4. Fourth parity----
#Fourth parity age
hNB2age_del4 <- hNB2age_preg4 + preg_period

#Fourth parity simulation day
hNB2_simd_del4 <- hNB2_simd_preg4 + preg_period 

#BLV status of NB1 at fourth pregnancy
hNB2_BLV_del4 <- rep(0, NB2)

#Mastitis at fourth parity
hNB2_mas4 <- numeric(NB2)
for (i in 1:NB2){
  hNB2_mas4[i] <- rbinom(1, 1, pmasmat[1 + hNB2_BLV_del4[i], 4])
}
hNB2_mas4

#Fourth lactation period
hNB2_lac4_period <- ifelse(hNB2_mas4==1, ifelse(hNB2_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
hNB2_lac4_yield <- ifelse(hNB2_mas4==1, ifelse(hNB2_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)

#Fourth lactation end age
hNB2age_lac4e <- hNB2age_del4 + hNB2_lac4_period 

#Fourth lactation end simulation day
hNB2_simd_lac4e <- hNB2_simd_del4 + hNB2_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
hNB2_lac4_income <- ifelse(hNB2_simd_del4 > sim_d, 0,
                           ifelse(hNB2_simd_lac4e < sim_discard, 0,
                                  ifelse(hNB2_simd_lac4e > sim_d, 
                                         hNB2_lac4_yield * milk_price * (sim_d - hNB2_simd_del4)/hNB2_lac4_period *
                                           (1 - hNB2_preg4_removed_cumulative),
                                         ifelse(hNB2_simd_del4 < sim_discard,
                                                hNB2_lac4_yield * milk_price *
                                                  (hNB2_simd_lac4e - sim_discard)/hNB2_lac4_period *
                                                  (1 - hNB2_preg4_removed_cumulative),
                                                hNB2_lac4_yield * milk_price * hNB2_lac4_period / milk_day *
                                                  (1 - hNB2_preg4_removed_cumulative)))))


#AI for fifth pregnancy
hNB2_AI5 <- numeric(NB2)
for (i in 1:NB2){
  hNB2_AI5[i] <- rpois(1, ifelse(hNB2_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
hNB2_AI5

#Removal of cattle
hNB2_preg5_removed <- ifelse(hNB2_AI5 > nAI_remove-1, 1, 
                             ifelse(hNB2_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
hNB2_preg5_removed_cumulative <- ifelse(hNB2_preg4_removed_cumulative==1, 1, 
                                        ifelse(hNB2_AI5 > nAI_remove-1, 1, 
                                               ifelse(hNB2_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
hNB2age_preg5 <- hNB2age_del4 + day_wait_AI + 21*hNB2_AI5

#Fifth pregnancy simulation day
hNB2_simd_preg5 <- hNB2_simd_del4 + day_wait_AI + 21*hNB2_AI5


#B2.6.2.5. Fifth parity----
#Fifth parity age
hNB2age_del5 <- hNB2age_preg5 + preg_period

#Fifth parity simulation day
hNB2_simd_del5 <- hNB2_simd_preg5 + preg_period 

#BLV status of NB2 at fifth pregnancy
hNB2_BLV_del5 <- rep(0, NB2)

#Mastitis at fifth parity
hNB2_mas5 <- numeric(NB2)
for (i in 1:NB2){
  hNB2_mas5[i] <- rbinom(1, 1, pmasmat[1 + hNB2_BLV_del5[i], 5])
}
hNB2_mas5

#Fifth lactation period
hNB2_lac5_period <- ifelse(hNB2_mas5==1, ifelse(hNB2_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
hNB2_lac5_yield <- ifelse(hNB2_mas5==1, ifelse(hNB2_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)

#Fifth lactation end age
hNB2age_lac5e <- hNB2age_del5 + hNB2_lac5_period 

#Fifth lactation end simulation day
hNB2_simd_lac5e <- hNB2_simd_del5 + hNB2_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
hNB2_lac5_income <- ifelse(hNB2_simd_del5 > sim_d, 0,
                           ifelse(hNB2_simd_lac5e < sim_discard, 0,
                                  ifelse(hNB2_simd_lac5e > sim_d, 
                                         hNB2_lac5_yield * milk_price * (sim_d - hNB2_simd_del5)/hNB2_lac5_period *
                                           (1 - hNB2_preg5_removed_cumulative),
                                         ifelse(hNB2_simd_del5 < sim_discard,
                                                hNB2_lac5_yield * milk_price *
                                                  (hNB2_simd_lac5e - sim_discard)/hNB2_lac5_period *
                                                  (1 - hNB2_preg5_removed_cumulative),
                                                hNB2_lac5_yield * milk_price * hNB2_lac5_period / milk_day *
                                                  (1 - hNB2_preg5_removed_cumulative)))))

#AI for sixth pregnancy
hNB2_AI6 <- numeric(NB2)
for (i in 1:NB2){
  hNB2_AI6[i] <- rpois(1, ifelse(hNB2_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
hNB2_AI6

#Removal of cattle
hNB2_preg6_removed <- ifelse(hNB2_AI6 > nAI_remove-1, 1, 
                             ifelse(hNB2_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
hNB2_preg6_removed_cumulative <- ifelse(hNB2_preg5_removed_cumulative==1, 1, 
                                        ifelse(hNB2_AI6 > nAI_remove-1, 1, 
                                               ifelse(hNB2_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
hNB2age_preg6 <- hNB2age_del5 + day_wait_AI + 21*hNB2_AI6

#Sixth pregnancy simulation day
hNB2_simd_preg6 <- hNB2_simd_del5 + day_wait_AI + 21*hNB2_AI6


#B2.6.3. Borne in year 3----
#B2.6.3.1. First parity----
#Simulation day of first parity
hNB3_simd_del1 <- DB_NB3 + hNB3_day_del1

#Mastitis at first parity
hNB3_mas1 <- numeric(NB3)
for (i in 1:NB3){
  hNB3_mas1[i] <- rbinom(1, 1, pmasmat[1 + hNB3_BLV[i], 1])
}
hNB3_mas1

#First lactation period
hNB3_lac1_period <- ifelse(hNB3_mas1==1, ifelse(hNB3_BLV==2, days_mas_L, days_mas_SI), milk_day)

#First lactation milk yield
hNB3_lac1_yield <- ifelse(hNB3_mas1==1, ifelse(hNB3_BLV==2, yield_P1 * days_mas_L / milk_day, 
                                               yield_P1 * days_mas_SI / milk_day), yield_P1)

#First lactation end age
hNB3age_lac1e <- hNB3_day_del1 + hNB3_lac1_period 

#First lactation end simulation day
hNB3_simd_lac1e <- hNB3_simd_del1 + hNB3_lac1_period

#First lactation income #linear score 5 milk price not considered
hNB3_lac1_income <- ifelse(hNB3_simd_del1 > sim_d, 0,
                           ifelse(hNB3_simd_lac1e < sim_discard, 0,
                                  ifelse(hNB3_simd_lac1e > sim_d, 
                                         hNB3_lac1_yield * milk_price * (sim_d - hNB3_simd_del1)/hNB3_lac1_period,
                                         ifelse(hNB3_simd_del1 < sim_discard,
                                                hNB3_lac1_yield * milk_price *
                                                  (hNB3_simd_lac1e - sim_discard)/hNB3_lac1_period,
                                                hNB3_lac1_yield * milk_price * hNB3_lac1_period / milk_day))))

#AI for second pregnancy
hNB3_AI2 <- numeric(NB3)
for (i in 1:NB3){
  hNB3_AI2[i] <- rpois(1, ifelse(hNB3_mas1[i]==1, nAI_masmat[1,1], nAI_masmat[2,1]))
}
hNB3_AI2

#Removal of cattle
hNB3_preg2_removed <- ifelse(hNB3_AI2 > nAI_remove-1, 1, 
                             ifelse(hNB3_mas1 == 1, rbinom(1, 1, cull_mas1), 
                                    rbinom(1, 1, cull_nomas1)))

#Second pregnancy age
hNB3age_preg2 <- hNB3_day_del1 + day_wait_AI + 21*hNB3_AI2

#Second pregnancy simulation day
hNB3_simd_preg2 <- hNB3_simd_del1 + day_wait_AI + 21*hNB3_AI2


#B2.6.3.2. Second parity----
#Second parity age
hNB3age_del2 <- hNB3age_preg2 + preg_period

#Second parity simulation day
hNB3_simd_del2 <- hNB3_simd_preg2 + preg_period 

#BLV status of NB1 at second pregnancy
hNB3_BLV_del2 <- rep(0, NB3)

#Mastitis at second parity
hNB3_mas2 <- numeric(NB3)
for (i in 1:NB3){
  hNB3_mas2[i] <- rbinom(1, 1, pmasmat[1 + hNB3_BLV_del2[i], 2])
}
hNB3_mas2

#Second lactation period
hNB3_lac2_period <- ifelse(hNB3_mas2==1, ifelse(hNB3_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
hNB3_lac2_yield <- ifelse(hNB3_mas2==1, ifelse(hNB3_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
hNB3age_lac2e <- hNB3age_del2 + hNB3_lac2_period 

#Second lactation end simulation day
hNB3_simd_lac2e <- hNB3_simd_del2 + hNB3_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
hNB3_lac2_income <- ifelse(hNB3_simd_del2 > sim_d, 0,
                           ifelse(hNB3_simd_lac2e < sim_discard, 0,
                                  ifelse(hNB3_simd_lac2e > sim_d, 
                                         hNB3_lac2_yield * milk_price * (sim_d - hNB3_simd_del2)/hNB3_lac2_period *
                                           (1 - hNB3_preg2_removed),
                                         ifelse(hNB3_simd_del2 < sim_discard,
                                                hNB3_lac2_yield * milk_price *
                                                  (hNB3_simd_lac2e - sim_discard)/hNB3_lac2_period *
                                                  (1 - hNB3_preg2_removed),
                                                hNB3_lac2_yield * milk_price * hNB3_lac2_period / milk_day *
                                                  (1 - hNB3_preg2_removed)))))

#AI for third pregnancy
hNB3_AI3 <- numeric(NB3)
for (i in 1:NB3){
  hNB3_AI3[i] <- rpois(1, ifelse(hNB3_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
hNB3_AI3

#Removal of cattle
hNB3_preg3_removed <- ifelse(hNB3_AI3 > nAI_remove-1, 1, 
                             ifelse(hNB3_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))
hNB3_preg3_removed_cumulative <- ifelse(hNB3_preg2_removed==1, 1, 
                                        ifelse(hNB3_AI3 > nAI_remove-1, 1, 
                                               ifelse(hNB3_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                                      rbinom(1, 1, cull_nomas2))))

#Third pregnancy age
hNB3age_preg3 <- hNB3age_del2 + day_wait_AI + 21*hNB3_AI3

#Third pregnancy simulation day
hNB3_simd_preg3 <- hNB3_simd_del2 + day_wait_AI + 21*hNB3_AI3


#B2.6.3.3. Third parity----
#Third parity age
hNB3age_del3 <- hNB3age_preg3 + preg_period

#Third parity simulation day
hNB3_simd_del3 <- hNB3_simd_preg3 + preg_period 

#BLV status of NB1 at third pregnancy
hNB3_BLV_del3 <- rep(0, NB3)

#Mastitis at third parity
hNB3_mas3 <- numeric(NB3)
for (i in 1:NB3){
  hNB3_mas3[i] <- rbinom(1, 1, pmasmat[1 + hNB3_BLV_del3[i], 3])
}
hNB3_mas3

#Third lactation period
hNB3_lac3_period <- ifelse(hNB3_mas3==1, ifelse(hNB3_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
hNB3_lac3_yield <- ifelse(hNB3_mas3==1, ifelse(hNB3_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
hNB3age_lac3e <- hNB3age_del3 + hNB3_lac3_period 

#Third lactation end simulation day
hNB3_simd_lac3e <- hNB3_simd_del3 + hNB3_lac3_period

#Third lactation income #linear score 5 milk price not considered
hNB3_lac3_income <- ifelse(hNB3_simd_del3 > sim_d, 0,
                           ifelse(hNB3_simd_lac3e < sim_discard, 0,
                                  ifelse(hNB3_simd_lac3e > sim_d, 
                                         hNB3_lac3_yield * milk_price * (sim_d - hNB3_simd_del3)/hNB3_lac3_period *
                                           (1 - hNB3_preg3_removed_cumulative),
                                         ifelse(hNB3_simd_del3 < sim_discard,
                                                hNB3_lac3_yield * milk_price *
                                                  (hNB3_simd_lac3e - sim_discard)/hNB3_lac3_period *
                                                  (1 - hNB3_preg3_removed_cumulative),
                                                hNB3_lac3_yield * milk_price * hNB3_lac3_period / milk_day *
                                                  (1 - hNB3_preg3_removed_cumulative)))))

#AI for fourth pregnancy
hNB3_AI4 <- numeric(NB3)
for (i in 1:NB3){
  hNB3_AI4[i] <- rpois(1, ifelse(hNB3_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
hNB3_AI4

#Removal of cattle
hNB3_preg4_removed <- ifelse(hNB3_AI4 > nAI_remove-1, 1, 
                             ifelse(hNB3_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
hNB3_preg4_removed_cumulative <- ifelse(hNB3_preg3_removed_cumulative==1, 1, 
                                        ifelse(hNB3_AI4 > nAI_remove-1, 1, 
                                               ifelse(hNB3_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
hNB3age_preg4 <- hNB3age_del3 + day_wait_AI + 21*hNB3_AI4

#Fourth pregnancy simulation day
hNB3_simd_preg4 <- hNB3_simd_del3 + day_wait_AI + 21*hNB3_AI4


#B2.6.3.4. Fourth parity----
#Fourth parity age
hNB3age_del4 <- hNB3age_preg4 + preg_period

#Fourth parity simulation day
hNB3_simd_del4 <- hNB3_simd_preg4 + preg_period 

#BLV status of NB1 at fourth pregnancy
hNB3_BLV_del4 <- rep(0, NB3)

#Mastitis at fourth parity
hNB3_mas4 <- numeric(NB3)
for (i in 1:NB3){
  hNB3_mas4[i] <- rbinom(1, 1, pmasmat[1 + hNB3_BLV_del4[i], 4])
}
hNB3_mas4

#Fourth lactation period
hNB3_lac4_period <- ifelse(hNB3_mas4==1, ifelse(hNB3_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
hNB3_lac4_yield <- ifelse(hNB3_mas4==1, ifelse(hNB3_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)

#Fourth lactation end age
hNB3age_lac4e <- hNB3age_del4 + hNB3_lac4_period 

#Fourth lactation end simulation day
hNB3_simd_lac4e <- hNB3_simd_del4 + hNB3_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
hNB3_lac4_income <- ifelse(hNB3_simd_del4 > sim_d, 0,
                           ifelse(hNB3_simd_lac4e < sim_discard, 0,
                                  ifelse(hNB3_simd_lac4e > sim_d, 
                                         hNB3_lac4_yield * milk_price * (sim_d - hNB3_simd_del4)/hNB3_lac4_period *
                                           (1 - hNB3_preg4_removed_cumulative),
                                         ifelse(hNB3_simd_del4 < sim_discard,
                                                hNB3_lac4_yield * milk_price *
                                                  (hNB3_simd_lac4e - sim_discard)/hNB3_lac4_period *
                                                  (1 - hNB3_preg4_removed_cumulative),
                                                hNB3_lac4_yield * milk_price * hNB3_lac4_period / milk_day *
                                                  (1 - hNB3_preg4_removed_cumulative)))))


#AI for fifth pregnancy
hNB3_AI5 <- numeric(NB3)
for (i in 1:NB3){
  hNB3_AI5[i] <- rpois(1, ifelse(hNB3_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
hNB3_AI5

#Removal of cattle
hNB3_preg5_removed <- ifelse(hNB3_AI5 > nAI_remove-1, 1, 
                             ifelse(hNB3_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
hNB3_preg5_removed_cumulative <- ifelse(hNB3_preg4_removed_cumulative==1, 1, 
                                        ifelse(hNB3_AI5 > nAI_remove-1, 1, 
                                               ifelse(hNB3_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
hNB3age_preg5 <- hNB3age_del4 + day_wait_AI + 21*hNB3_AI5

#Fifth pregnancy simulation day
hNB3_simd_preg5 <- hNB3_simd_del4 + day_wait_AI + 21*hNB3_AI5


#B2.6.3.5. Fifth parity----
#Fifth parity age
hNB3age_del5 <- hNB3age_preg5 + preg_period

#Fifth parity simulation day
hNB3_simd_del5 <- hNB3_simd_preg5 + preg_period 

#BLV status of IP0 at fifth pregnancy
hNB3_BLV_del5 <- rep(0, NB3)

#Mastitis at fifth parity
hNB3_mas5 <- numeric(NB3)
for (i in 1:NB3){
  hNB3_mas5[i] <- rbinom(1, 1, pmasmat[1 + hNB3_BLV_del5[i], 5])
}
hNB3_mas5

#Fifth lactation period
hNB3_lac5_period <- ifelse(hNB3_mas5==1, ifelse(hNB3_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
hNB3_lac5_yield <- ifelse(hNB3_mas5==1, ifelse(hNB3_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)

#Fifth lactation end age
hNB3age_lac5e <- hNB3age_del5 + hNB3_lac5_period 

#Fifth lactation end simulation day
hNB3_simd_lac5e <- hNB3_simd_del5 + hNB3_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
hNB3_lac5_income <- ifelse(hNB3_simd_del5 > sim_d, 0,
                           ifelse(hNB3_simd_lac5e < sim_discard, 0,
                                  ifelse(hNB3_simd_lac5e > sim_d, 
                                         hNB3_lac5_yield * milk_price * (sim_d - hNB3_simd_del5)/hNB3_lac5_period *
                                           (1 - hNB3_preg5_removed_cumulative),
                                         ifelse(hNB3_simd_del5 < sim_discard,
                                                hNB3_lac5_yield * milk_price *
                                                  (hNB3_simd_lac5e - sim_discard)/hNB3_lac5_period *
                                                  (1 - hNB3_preg5_removed_cumulative),
                                                hNB3_lac5_yield * milk_price * hNB3_lac5_period / milk_day *
                                                  (1 - hNB3_preg5_removed_cumulative)))))

#AI for sixth pregnancy
hNB3_AI6 <- numeric(NB3)
for (i in 1:NB3){
  hNB3_AI6[i] <- rpois(1, ifelse(hNB3_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
hNB3_AI6

#Removal of cattle
hNB3_preg6_removed <- ifelse(hNB3_AI6 > nAI_remove-1, 1, 
                             ifelse(hNB3_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
hNB3_preg6_removed_cumulative <- ifelse(hNB3_preg5_removed_cumulative==1, 1, 
                                        ifelse(hNB3_AI6 > nAI_remove-1, 1, 
                                               ifelse(hNB3_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
hNB3age_preg6 <- hNB3age_del5 + day_wait_AI + 21*hNB3_AI6

#Sixth pregnancy simulation day
hNB3_simd_preg6 <- hNB3_simd_del5 + day_wait_AI + 21*hNB3_AI6


#B2.6.4. Borne in year 4----
#B2.6.4.1. First parity----
#Simulation day of first parity
hNB4_simd_del1 <- DB_NB4 + hNB4_day_del1

#Mastitis at first parity
hNB4_mas1 <- numeric(NB4)
for (i in 1:NB4){
  hNB4_mas1[i] <- rbinom(1, 1, pmasmat[1 + hNB4_BLV[i], 1])
}
hNB4_mas1

#First lactation period
hNB4_lac1_period <- ifelse(hNB4_mas1==1, ifelse(hNB4_BLV==2, days_mas_L, days_mas_SI), milk_day)

#First lactation milk yield
hNB4_lac1_yield <- ifelse(hNB4_mas1==1, ifelse(hNB4_BLV==2, yield_P1 * days_mas_L / milk_day, 
                                               yield_P1 * days_mas_SI / milk_day), yield_P1)

#First lactation end age
hNB4age_lac1e <- hNB4_day_del1 + hNB4_lac1_period 

#First lactation end simulation day
hNB4_simd_lac1e <- hNB4_simd_del1 + hNB4_lac1_period

#First lactation income #linear score 5 milk price not considered
hNB4_lac1_income <- ifelse(hNB4_simd_del1 > sim_d, 0,
                           ifelse(hNB4_simd_lac1e < sim_discard, 0,
                                  ifelse(hNB4_simd_lac1e > sim_d, 
                                         hNB4_lac1_yield * milk_price * (sim_d - hNB4_simd_del1)/hNB4_lac1_period,
                                         ifelse(hNB4_simd_del1 < sim_discard,
                                                hNB4_lac1_yield * milk_price *
                                                  (hNB4_simd_lac1e - sim_discard)/hNB4_lac1_period,
                                                hNB4_lac1_yield * milk_price * hNB4_lac1_period / milk_day))))

#AI for second pregnancy
hNB4_AI2 <- numeric(NB4)
for (i in 1:NB4){
  hNB4_AI2[i] <- rpois(1, ifelse(hNB4_mas1[i]==1, nAI_masmat[1,1], nAI_masmat[2,1]))
}
hNB4_AI2

#Removal of cattle
hNB4_preg2_removed <- ifelse(hNB4_AI2 > nAI_remove-1, 1, 
                             ifelse(hNB4_mas1 == 1, rbinom(1, 1, cull_mas1), 
                                    rbinom(1, 1, cull_nomas1)))

#Second pregnancy age
hNB4age_preg2 <- hNB4_day_del1 + day_wait_AI + 21*hNB4_AI2

#Second pregnancy simulation day
hNB4_simd_preg2 <- hNB4_simd_del1 + day_wait_AI + 21*hNB4_AI2


#B2.6.4.2. Second parity----
#Second parity age
hNB4age_del2 <- hNB4age_preg2 + preg_period

#Second parity simulation day
hNB4_simd_del2 <- hNB4_simd_preg2 + preg_period 

#BLV status of NB1 at second pregnancy
hNB4_BLV_del2 <- rep(0, NB4)

#Mastitis at second parity
hNB4_mas2 <- numeric(NB4)
for (i in 1:NB4){
  hNB4_mas2[i] <- rbinom(1, 1, pmasmat[1 + hNB4_BLV_del2[i], 2])
}
hNB4_mas2

#Second lactation period
hNB4_lac2_period <- ifelse(hNB4_mas2==1, ifelse(hNB4_BLV_del2==2, days_mas_L, days_mas_SI), milk_day)

#Second lactation milk yield
hNB4_lac2_yield <- ifelse(hNB4_mas2==1, ifelse(hNB4_BLV_del2==2, yield_P2 * days_mas_L / milk_day, 
                                               yield_P2 * days_mas_SI / milk_day), yield_P2)

#Second lactation end age
hNB4age_lac2e <- hNB4age_del2 + hNB4_lac2_period 

#Second lactation end simulation day
hNB4_simd_lac2e <- hNB4_simd_del2 + hNB4_lac2_period

#Second lactation income #linear score 5 milk price not considered
#Removed cow is modeled but penalty in milk is not counted
hNB4_lac2_income <- ifelse(hNB4_simd_del2 > sim_d, 0,
                           ifelse(hNB4_simd_lac2e < sim_discard, 0,
                                  ifelse(hNB4_simd_lac2e > sim_d, 
                                         hNB4_lac2_yield * milk_price * (sim_d - hNB4_simd_del2)/hNB4_lac2_period *
                                           (1 - hNB4_preg2_removed),
                                         ifelse(hNB4_simd_del2 < sim_discard,
                                                hNB4_lac2_yield * milk_price *
                                                  (hNB4_simd_lac2e - sim_discard)/hNB4_lac2_period *
                                                  (1 - hNB4_preg2_removed),
                                                hNB4_lac2_yield * milk_price * hNB4_lac2_period / milk_day *
                                                  (1 - hNB4_preg2_removed)))))

#AI for third pregnancy
hNB4_AI3 <- numeric(NB4)
for (i in 1:NB4){
  hNB4_AI3[i] <- rpois(1, ifelse(hNB4_mas2[i]==1, nAI_masmat[1,2], nAI_masmat[2,2]))
}
hNB4_AI3

#Removal of cattle
hNB4_preg3_removed <- ifelse(hNB4_AI3 > nAI_remove-1, 1, 
                             ifelse(hNB4_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                    rbinom(1, 1, cull_nomas2)))
hNB4_preg3_removed_cumulative <- ifelse(hNB4_preg2_removed==1, 1, 
                                        ifelse(hNB4_AI3 > nAI_remove-1, 1, 
                                               ifelse(hNB4_mas2 == 1, rbinom(1, 1, cull_mas2), 
                                                      rbinom(1, 1, cull_nomas2))))

#Third pregnancy age
hNB4age_preg3 <- hNB4age_del2 + day_wait_AI + 21*hNB4_AI3

#Third pregnancy simulation day
hNB4_simd_preg3 <- hNB4_simd_del2 + day_wait_AI + 21*hNB4_AI3


#B2.6.4.3. Third parity----
#Third parity age
hNB4age_del3 <- hNB4age_preg3 + preg_period

#Third parity simulation day
hNB4_simd_del3 <- hNB4_simd_preg3 + preg_period 

#BLV status of NB1 at third pregnancy
hNB4_BLV_del3 <- rep(0, NB4)

#Mastitis at third parity
hNB4_mas3 <- numeric(NB4)
for (i in 1:NB4){
  hNB4_mas3[i] <- rbinom(1, 1, pmasmat[1 + hNB4_BLV_del3[i], 3])
}
hNB4_mas3

#Third lactation period
hNB4_lac3_period <- ifelse(hNB4_mas3==1, ifelse(hNB4_BLV_del3==2, days_mas_L, days_mas_SI), milk_day)

#Third lactation milk yield
hNB4_lac3_yield <- ifelse(hNB4_mas3==1, ifelse(hNB4_BLV_del3==2, yield_P3 * days_mas_L / milk_day, 
                                               yield_P3 * days_mas_SI / milk_day), yield_P3)

#Third lactation end age
hNB4age_lac3e <- hNB4age_del3 + hNB4_lac3_period 

#Third lactation end simulation day
hNB4_simd_lac3e <- hNB4_simd_del3 + hNB4_lac3_period

#Third lactation income #linear score 5 milk price not considered
hNB4_lac3_income <- ifelse(hNB4_simd_del3 > sim_d, 0,
                           ifelse(hNB4_simd_lac3e < sim_discard, 0,
                                  ifelse(hNB4_simd_lac3e > sim_d, 
                                         hNB4_lac3_yield * milk_price * (sim_d - hNB4_simd_del3)/hNB4_lac3_period *
                                           (1 - hNB4_preg3_removed_cumulative),
                                         ifelse(hNB4_simd_del3 < sim_discard,
                                                hNB4_lac3_yield * milk_price *
                                                  (hNB4_simd_lac3e - sim_discard)/hNB4_lac3_period *
                                                  (1 - hNB4_preg3_removed_cumulative),
                                                hNB4_lac3_yield * milk_price * hNB4_lac3_period / milk_day *
                                                  (1 - hNB4_preg3_removed_cumulative)))))

#AI for fourth pregnancy
hNB4_AI4 <- numeric(NB4)
for (i in 1:NB4){
  hNB4_AI4[i] <- rpois(1, ifelse(hNB4_mas3[i]==1, nAI_masmat[1,3], nAI_masmat[2,3]))
}
hNB4_AI4

#Removal of cattle
hNB4_preg4_removed <- ifelse(hNB4_AI4 > nAI_remove-1, 1, 
                             ifelse(hNB4_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                    rbinom(1, 1, cull_nomas3)))
hNB4_preg4_removed_cumulative <- ifelse(hNB4_preg3_removed_cumulative==1, 1, 
                                        ifelse(hNB4_AI4 > nAI_remove-1, 1, 
                                               ifelse(hNB4_mas3 == 1, rbinom(1, 1, cull_mas3), 
                                                      rbinom(1, 1, cull_nomas3))))

#Fourth pregnancy age
hNB4age_preg4 <- hNB4age_del3 + day_wait_AI + 21*hNB4_AI4

#Fourth pregnancy simulation day
hNB4_simd_preg4 <- hNB4_simd_del3 + day_wait_AI + 21*hNB4_AI4


#B2.6.3.4. Fourth parity----
#Fourth parity age
hNB4age_del4 <- hNB4age_preg4 + preg_period

#Fourth parity simulation day
hNB4_simd_del4 <- hNB4_simd_preg4 + preg_period 

#BLV status of NB1 at fourth pregnancy
hNB4_BLV_del4 <- rep(0, NB4)

#Mastitis at fourth parity
hNB4_mas4 <- numeric(NB4)
for (i in 1:NB4){
  hNB4_mas4[i] <- rbinom(1, 1, pmasmat[1 + hNB4_BLV_del4[i], 4])
}
hNB4_mas4

#Fourth lactation period
hNB4_lac4_period <- ifelse(hNB4_mas4==1, ifelse(hNB4_BLV_del4==2, days_mas_L, days_mas_SI), milk_day)

#Fourth lactation milk yield
hNB4_lac4_yield <- ifelse(hNB4_mas4==1, ifelse(hNB4_BLV_del4==2, yield_P4 * days_mas_L / milk_day, 
                                               yield_P4 * days_mas_SI / milk_day), yield_P4)

#Fourth lactation end age
hNB4age_lac4e <- hNB4age_del4 + hNB4_lac4_period 

#Fourth lactation end simulation day
hNB4_simd_lac4e <- hNB4_simd_del4 + hNB4_lac4_period

#Fourth lactation income #linear score 5 milk price not considered
hNB4_lac4_income <- ifelse(hNB4_simd_del4 > sim_d, 0,
                           ifelse(hNB4_simd_lac4e < sim_discard, 0,
                                  ifelse(hNB4_simd_lac4e > sim_d, 
                                         hNB4_lac4_yield * milk_price * (sim_d - hNB4_simd_del4)/hNB4_lac4_period *
                                           (1 - hNB4_preg4_removed_cumulative),
                                         ifelse(hNB4_simd_del4 < sim_discard,
                                                hNB4_lac4_yield * milk_price *
                                                  (hNB4_simd_lac4e - sim_discard)/hNB4_lac4_period *
                                                  (1 - hNB4_preg4_removed_cumulative),
                                                hNB4_lac4_yield * milk_price * hNB4_lac4_period / milk_day *
                                                  (1 - hNB4_preg4_removed_cumulative)))))


#AI for fifth pregnancy
hNB4_AI5 <- numeric(NB4)
for (i in 1:NB4){
  hNB4_AI5[i] <- rpois(1, ifelse(hNB4_mas4[i]==1, nAI_masmat[1,4], nAI_masmat[2,4]))
}
hNB4_AI5

#Removal of cattle
hNB4_preg5_removed <- ifelse(hNB4_AI5 > nAI_remove-1, 1, 
                             ifelse(hNB4_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                    rbinom(1, 1, cull_nomas4)))
hNB4_preg5_removed_cumulative <- ifelse(hNB4_preg4_removed_cumulative==1, 1, 
                                        ifelse(hNB4_AI5 > nAI_remove-1, 1, 
                                               ifelse(hNB4_mas4 == 1, rbinom(1, 1, cull_mas4), 
                                                      rbinom(1, 1, cull_nomas4))))

#Fifth pregnancy age
hNB4age_preg5 <- hNB4age_del4 + day_wait_AI + 21*hNB4_AI5

#Fifth pregnancy simulation day
hNB4_simd_preg5 <- hNB4_simd_del4 + day_wait_AI + 21*hNB4_AI5


#B2.6.3.5. Fifth parity----
#Fifth parity age
hNB4age_del5 <- hNB4age_preg5 + preg_period

#Fifth parity simulation day
hNB4_simd_del5 <- hNB4_simd_preg5 + preg_period 

#BLV status of IP0 at fifth pregnancy
hNB4_BLV_del5 <- rep(0, NB4)

#Mastitis at fifth parity
hNB4_mas5 <- numeric(NB4)
for (i in 1:NB4){
  hNB4_mas5[i] <- rbinom(1, 1, pmasmat[1 + hNB3_BLV_del5[i], 5])
}
hNB4_mas5

#Fifth lactation period
hNB4_lac5_period <- ifelse(hNB4_mas5==1, ifelse(hNB4_BLV_del5==2, days_mas_L, days_mas_SI), milk_day)

#Fifth lactation milk yield
hNB4_lac5_yield <- ifelse(hNB4_mas5==1, ifelse(hNB4_BLV_del5==2, yield_P5 * days_mas_L / milk_day, 
                                               yield_P5 * days_mas_SI / milk_day), yield_P5)

#Fifth lactation end age
hNB4age_lac5e <- hNB4age_del5 + hNB4_lac5_period 

#Fifth lactation end simulation day
hNB4_simd_lac5e <- hNB4_simd_del5 + hNB4_lac5_period

#Fifth lactation income #linear score 5 milk price not considered
hNB4_lac5_income <- ifelse(hNB4_simd_del5 > sim_d, 0,
                           ifelse(hNB4_simd_lac5e < sim_discard, 0,
                                  ifelse(hNB4_simd_lac5e > sim_d, 
                                         hNB4_lac5_yield * milk_price * (sim_d - hNB4_simd_del5)/hNB4_lac5_period *
                                           (1 - hNB4_preg5_removed_cumulative),
                                         ifelse(hNB4_simd_del5 < sim_discard,
                                                hNB4_lac5_yield * milk_price *
                                                  (hNB4_simd_lac5e - sim_discard)/hNB4_lac5_period *
                                                  (1 - hNB4_preg5_removed_cumulative),
                                                hNB4_lac5_yield * milk_price * hNB4_lac5_period / milk_day *
                                                  (1 - hNB4_preg5_removed_cumulative)))))

#AI for sixth pregnancy
hNB4_AI6 <- numeric(NB4)
for (i in 1:NB4){
  hNB4_AI6[i] <- rpois(1, ifelse(hNB4_mas5[i]==1, nAI_masmat[1,5], nAI_masmat[2,5]))
}
hNB4_AI6

#Removal of cattle
hNB4_preg6_removed <- ifelse(hNB4_AI6 > nAI_remove-1, 1, 
                             ifelse(hNB4_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                    rbinom(1, 1, cull_nomas5)))
hNB4_preg6_removed_cumulative <- ifelse(hNB4_preg5_removed_cumulative==1, 1, 
                                        ifelse(hNB4_AI6 > nAI_remove-1, 1, 
                                               ifelse(hNB4_mas5 == 1, rbinom(1, 1, cull_mas5), 
                                                      rbinom(1, 1, cull_nomas5))))

#Sixth pregnancy age
hNB4age_preg6 <- hNB4age_del5 + day_wait_AI + 21*hNB4_AI6

#Sixth pregnancy simulation day
hNB4_simd_preg6 <- hNB4_simd_del5 + day_wait_AI + 21*hNB4_AI6



#C. Economic analysis----
#C1. Income----
#C1.1. Income from milk sales in BLV-infected farm----
#各結果をシミュレーション毎に記録してください。
BIPO_lac1_total <- sum(BIP0_lac1_income)
BIP0_lac2_total <- sum(BIP0_lac2_income)
BIP0_lac3_total <- sum(BIP0_lac3_income)
BIP0_lac4_total <- sum(BIP0_lac4_income)
BIP0_lac5_total <- sum(BIP0_lac5_income)
BIP1_lac2_total <- sum(BIP1_lac2_income)
BIP1_lac3_total <- sum(BIP1_lac3_income)
BIP1_lac4_total <- sum(BIP1_lac4_income)
BIP1_lac5_total <- sum(BIP1_lac5_income)
BIP2_lac3_total <- sum(BIP2_lac3_income)
BIP2_lac4_total <- sum(BIP2_lac4_income)
BIP2_lac5_total <- sum(BIP2_lac5_income)
BIP3_lac4_total <- sum(BIP3_lac4_income)
BIP3_lac5_total <- sum(BIP3_lac5_income)
BIP4_lac5_total <- sum(BIP4_lac5_income)
BNB1_lac1_total <- sum(BNB1_lac1_income)
BNB1_lac2_total <- sum(BNB1_lac2_income)
BNB1_lac3_total <- sum(BNB1_lac3_income)
BNB1_lac4_total <- sum(BNB1_lac4_income)
BNB1_lac5_total <- sum(BNB1_lac5_income)
BNB2_lac1_total <- sum(BNB2_lac1_income)
BNB2_lac2_total <- sum(BNB2_lac2_income)
BNB2_lac3_total <- sum(BNB2_lac3_income)
BNB2_lac4_total <- sum(BNB2_lac4_income)
BNB2_lac5_total <- sum(BNB2_lac5_income)
BNB3_lac1_total <- sum(BNB3_lac1_income)
BNB3_lac2_total <- sum(BNB3_lac2_income)
BNB3_lac3_total <- sum(BNB3_lac3_income)
BNB3_lac4_total <- sum(BNB3_lac4_income)
BNB3_lac5_total <- sum(BNB3_lac5_income)
BNB4_lac1_total <- sum(BNB4_lac1_income)
BNB4_lac2_total <- sum(BNB4_lac2_income)
BNB4_lac3_total <- sum(BNB4_lac3_income)
BNB4_lac4_total <- sum(BNB4_lac4_income)
BNB4_lac5_total <- sum(BNB4_lac5_income)

Bincome <- BIPO_lac1_total + BIP0_lac2_total + BIP0_lac3_total + BIP0_lac4_total + BIP0_lac5_total +
  BIP1_lac2_total + BIP1_lac3_total + BIP1_lac4_total + BIP1_lac5_total +
  BIP2_lac3_total + BIP2_lac4_total + BIP2_lac5_total +
  BIP3_lac4_total + BIP3_lac5_total + 
  BIP4_lac5_total +
  BNB1_lac1_total + BNB1_lac2_total + BNB1_lac3_total + BNB1_lac4_total + BNB1_lac5_total +
  BNB2_lac1_total + BNB2_lac2_total + BNB2_lac3_total + BNB2_lac4_total + BNB2_lac5_total +
  BNB3_lac1_total + BNB3_lac2_total + BNB3_lac3_total + BNB3_lac4_total + BNB3_lac5_total +
  BNB4_lac1_total + BNB4_lac2_total + BNB4_lac3_total + BNB4_lac4_total + BNB4_lac5_total

#115110860
#378615247---1
#357864207---2
#379000289---3
#371838762---4
#378009832---5
#350708295---6
#391090643---7
#383658840---8
#343298685---9
#322247062---10
#384241898---11
#368894483---12
#362250119---13
#331682803---14
#386545401---15
#358220397---16
#322461575---17
#372664645---18
#327161103---19
#373497336---20
#378543158---21
#374064109---22
#379804087---23
#405445897---24
#315755181---25
#381143656---26
#379233572---27
#357427179---28
#351358571---29
#353704283---30

Bincome_t <- c(378615247, 357864207, 379000289, 371838762, 378009832, 
               350708295, 391090643, 383658840, 343298685, 322247062, 
               384241898, 368894483, 362250119, 331682803, 386545401, 
               358220397, 322461575, 372664645, 327161103, 373497336, 
               378543158, 374064109, 379804087, 405445897, 315755181, 
               381143656, 379233572, 357427179, 351358571, 353704283)
mean_value_B <- mean(Bincome_t)
#364014377
sd_value_B <- sd(Bincome_t)
#22679480
hist(Bincome_t, breaks = 10, probability = TRUE,
     col = "lightblue", border = "white",
     main = "ヒストグラムと正規分布", xlab = "値")
x <- seq(min(Bincome_t), max(Bincome_t), length = 100)
y <- dnorm(x, mean = mean_value_B, sd = sd_value_B)
lines(x, y, col = "red", lwd = 2)
364014377/5
#72802875

#C1.2. Income from milk sales in BLV free farm----
hIPO_lac1_total <- sum(hIP0_lac1_income)
hIP0_lac2_total <- sum(hIP0_lac2_income)
hIP0_lac3_total <- sum(hIP0_lac3_income)
hIP0_lac4_total <- sum(hIP0_lac4_income)
hIP0_lac5_total <- sum(hIP0_lac5_income)
hIP1_lac2_total <- sum(hIP1_lac2_income)
hIP1_lac3_total <- sum(hIP1_lac3_income)
hIP1_lac4_total <- sum(hIP1_lac4_income)
hIP1_lac5_total <- sum(hIP1_lac5_income)
hIP2_lac3_total <- sum(hIP2_lac3_income)
hIP2_lac4_total <- sum(hIP2_lac4_income)
hIP2_lac5_total <- sum(hIP2_lac5_income)
hIP3_lac4_total <- sum(hIP3_lac4_income)
hIP3_lac5_total <- sum(hIP3_lac5_income)
hIP4_lac5_total <- sum(hIP4_lac5_income)
hNB1_lac1_total <- sum(hNB1_lac1_income)
hNB1_lac2_total <- sum(hNB1_lac2_income)
hNB1_lac3_total <- sum(hNB1_lac3_income)
hNB1_lac4_total <- sum(hNB1_lac4_income)
hNB1_lac5_total <- sum(hNB1_lac5_income)
hNB2_lac1_total <- sum(hNB2_lac1_income)
hNB2_lac2_total <- sum(hNB2_lac2_income)
hNB2_lac3_total <- sum(hNB2_lac3_income)
hNB2_lac4_total <- sum(hNB2_lac4_income)
hNB2_lac5_total <- sum(hNB2_lac5_income)
hNB3_lac1_total <- sum(hNB3_lac1_income)
hNB3_lac2_total <- sum(hNB3_lac2_income)
hNB3_lac3_total <- sum(hNB3_lac3_income)
hNB3_lac4_total <- sum(hNB3_lac4_income)
hNB3_lac5_total <- sum(hNB3_lac5_income)
hNB4_lac1_total <- sum(hNB4_lac1_income)
hNB4_lac2_total <- sum(hNB4_lac2_income)
hNB4_lac3_total <- sum(hNB4_lac3_income)
hNB4_lac4_total <- sum(hNB4_lac4_income)
hNB4_lac5_total <- sum(hNB4_lac5_income)


hincome <- hIPO_lac1_total + hIP0_lac2_total + hIP0_lac3_total + hIP0_lac4_total + hIP0_lac5_total +
  hIP1_lac2_total + hIP1_lac3_total + hIP1_lac4_total + hIP1_lac5_total +
  hIP2_lac3_total + hIP2_lac4_total + hIP2_lac5_total +
  hIP3_lac4_total + hIP3_lac5_total + 
  hIP4_lac5_total +
  hNB1_lac1_total + hNB1_lac2_total + hNB1_lac3_total + hNB1_lac4_total + hNB1_lac5_total +
  hNB2_lac1_total + hNB2_lac2_total + hNB2_lac3_total + hNB2_lac4_total + hNB2_lac5_total +
  hNB3_lac1_total + hNB3_lac2_total + hNB3_lac3_total + hNB3_lac4_total + hNB3_lac5_total +
  hNB4_lac1_total + hNB4_lac2_total + hNB4_lac3_total + hNB4_lac4_total + hNB4_lac5_total

#114884812
#379858062---1
#364834956---2
#347954518---3
#343041308---4
#354455362---5
#393581883---6
#342245282---7
#375257495---8
#346036344---9
#333849140---10
#385947387---11
#370682983---12
#360595938---13
#357685384---14
#389430690---15
#406515059---16
#381501835---17
#368391144---18
#362254498---19
#369873083---20
#352852651---21
#399560999---22
#360305591---23
#404768206---24
#349249871---25
#354169977---26
#377114577---27
#369353803---28
#382476538---29
#389035158---30

hincome_t <- c(379858062, 364834956, 347954518, 343041308, 354455362, 
               393581883, 342245282, 375257495, 346036344, 333849140, 
               385947387, 370682983, 360595938, 357685384, 389430690, 
               406515059, 381501835, 368391144, 362254498, 369873083, 
               352852651, 399560999, 360305591, 404768206, 349249871, 
               354169977, 377114577, 369353803, 382476538, 389035158)
mean_value_h <- mean(hincome_t)
#369095991
sd_value_h <- sd(hincome_t)
#19396511
hist(hincome_t, breaks = 10, probability = TRUE,
     col = "lightblue", border = "white",
     main = "ヒストグラムと正規分布", xlab = "値")
x <- seq(min(hincome_t), max(hincome_t), length = 100)
y <- dnorm(x, mean = mean_value_h, sd = sd_value_h)
lines(x, y, col = "red", lwd = 2)
369095991/5
#73819198

73819198-72802875
#1016323

#C2. Number of culled cows----
#C2.1. Number of culled cows in BLV-infected farm----
BIP0_culled <- sum(BIP0_preg6_removed_cumulative)
BIP1_culled <- sum(BIP1_preg6_removed_cumulative)
BIP2_culled <- sum(BIP2_preg6_removed_cumulative)
BIP3_culled <- sum(BIP3_preg6_removed_cumulative)
BIP4_culled <- sum(BIP4_preg6_removed)
BNB1_culled <- sum(BNB1_preg6_removed_cumulative)
BNB2_culled <- sum(BNB2_preg6_removed_cumulative)
BNB3_culled <- sum(BNB3_preg6_removed_cumulative)
BNB4_culled <- sum(BNB4_preg6_removed_cumulative)
#以下続けてください。

Bculled <- BIP0_culled + BIP3_culled + BIP3_culled + BIP4_culled + 
  BNB1_culled + BNB2_culled + BNB3_culled + BNB4_culled

#156---1
#157---2
#157---3
#177---4
#144---5
#168---6
#176---7
#169---8
#131---9
#177---10
#171---11
#163---12
#184---13
#168---14
#168---15
#173---16
#171---17
#163---18
#158---19
#146---20
#172---21
#167---22
#176---23
#167---24
#157---25
#175---26
#184---27
#183---28
#184---29
#169---30

#C2.2. Number of culled cows in BLV-free farm----
hIP0_culled <- sum(hIP0_preg6_removed_cumulative)
hIP1_culled <- sum(hIP1_preg6_removed_cumulative)
hIP2_culled <- sum(hIP2_preg6_removed_cumulative)
hIP3_culled <- sum(hIP3_preg6_removed_cumulative)
hIP4_culled <- sum(hIP4_preg6_removed)
hNB1_culled <- sum(hNB1_preg6_removed_cumulative)
hNB2_culled <- sum(hNB2_preg6_removed_cumulative)
hNB3_culled <- sum(hNB3_preg6_removed_cumulative)
hNB4_culled <- sum(hNB4_preg6_removed_cumulative)
#以下続けてください。


hculled <- hIP0_culled + hIP1_culled + hIP2_culled + hIP3_culled + hIP4_culled + 
  hNB1_culled + hNB2_culled + hNB3_culled + hNB4_culled

#189---1
#164---2
#201---3
#197---4
#182---5
#184---6
#195---7
#177---8
#199---9
#200---10
#182---11
#199---12
#201---13
#196---14
#184---15
#173---16
#166---17
#203---18
#199---19
#200---20
#180---21
#180---22
#206---23
#204---24
#193---25
#202---26
#186---27
#173---28
#194---29
#204---30