library(tidyverse)

# (1) どのような情報が必要か？
# 顧客ID情報、購買金額

# (2)顧客別の売上金額の合計を計算し、
# (3)非会員の情報を除く
# (4)買上金額に基づいて、顧客ランクを作る
# (5)顧客ランクごとに、合計金額を算出する
# (6)合計金額を大きい順にする。
# (7)累計合計金額を算出する
# (8)累積金額の構成比

bunseki1 <- dat1 %>% select(顧客,金額) %>% 
  group_by(顧客) %>% 
  summarise(買上金額=sum(金額)) %>% ungroup() %>% 
  filter(顧客 != "9771007") %>% 
  mutate(rank=ntile(買上金額, 10)) %>% 
  group_by(rank) %>% 
  summarise(合計金額=sum(買上金額)) %>% 
  ungroup() %>% 
  arrange(-合計金額) %>% 
  mutate(累積購入金額=cumsum(合計金額), 累積構成比=cumsum(合計金額)/sum(合計金額)*100)

bunseki1 %>% write_csv("bunseki1.csv")
