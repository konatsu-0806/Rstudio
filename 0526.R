dat1 <- read_rds("201001.rds")
dat2 <- read_rds("201002.rds")
theme_set(theme_bw(base_family = "HiraKakuProN-W3"))

#復習課題:1月のデータについて、曜日別の平均購買金額の可視化をせよ。
kadai0 <- dat1 %>% select(日付,金額) %>% 
  group_by(日付) %>% 
  summarise(購買金額=sum(金額)) %>% 
  ungroup() %>% 
  mutate(曜日=wday(ymd(日付), label=T)) %>% 
  group_by(曜日) %>% summarise(平均購買金額=mean(購買金額, na.rm = T))

ggplot(data = kadai0, mapping = aes(x = 曜日, y = 平均購買金額)) +
  geom_col() #棒グラフ

#1月と2月のデータについて、日別の売上金額、数量と来店人数
uriage1 <- dat1 %>%  
  group_by(日付) %>% summarise(売上金額=sum(金額))
raiten1 <- dat1 %>% 
  distinct(店,日付,レジ,レシート) %>% 
  group_by(日付) %>% 
  summarise(来店人数=n())


kadai2 <- left_join(uriage1, raiten1)

ggplot(data = kadai2, mapping = aes(x = 来店人数, y = 売上金額)) +
  geom_point() + #散布図
  geom_smooth() #擬似曲線

ggplot(data = kadai2, mapping = aes(y = 来店人数)) + 
  geom_histogram(binwidth = 500) #bin=棒の数,binwidth=棒の幅

ggplot(data = kadai2, mapping = aes(x = 来店人数)) + 
  geom_boxplot() #箱ひげず 

#1月と2月のデータについて、
kadai3 <- kadai2 %>% mutate(曜日=wday(ymd(日付), label = T)) %>% 
  mutate(区分=case_when(
    曜日 =="Sun" | 曜日 =="Sat" ~ "週末",
    # 曜日 %in% c("Sat","Sun")
    TRUE ~ "平日"
  )) 

ggplot(data = kadai3, mapping = aes(x = 来店人数, y = 売上金額)) +
  geom_point(mapping = aes(col=区分)) +
  geom_smooth(method = "lm", se = F)


ggplot(data = kadai3, mapping = aes(x = 来店人数, y = 売上金額, col=区分)) +
  geom_point() +
  facet_wrap( ~ 曜日, ncol = 2)

#演習課題
#1一月のデータについて、「顧客」別の来店回数（レシート枚数）と買上金額の関係を可視化せよ。
raiten2 <- dat1 %>% distinct(日付,レジ,レシート,.keep_all = T) %>% 
  filter(顧客 != "9771007") %>% 
  group_by(顧客) %>% 
  summarise(来店回数=n())
kaiage1 <- dat1 %>% distinct(日付,レジ,レシート,.keep_all = T) %>% 
  group_by(顧客) %>% summarise(買上金額=sum(金額))

ensyu1 <- left_join(raiten2, kaiage1)

ggplot(data = ensyu1, mapping = aes(x = 来店回数, y = 買上金額)) +
  geom_point() +
  geom_smooth()

#顧客を「超優良顧客」、「優良顧客」、「一般顧客」に分けた上で、（１）の関係について、顧客レベルを区別して可視化せよ。
kokyaku1 <- dat1 %>% distinct(日付,レジ,レシート,.keep_all = T) %>% 
  filter(顧客 != "9771007") %>% 
  group_by(顧客) %>% 
  summarise(来店回数=n()) %>% 
  mutate(顧客レベル=case_when(
    来店回数 < 5 ~ "一般顧客",
    5 <= 来店回数 & 来店回数 < 20 ~ "優良顧客", 
    20 <= 来店回数 ~ "超優良顧客"
  ))

kaiage2 <- dat1 %>% distinct(日付,レジ,レシート,.keep_all = T) %>% 
  group_by(顧客) %>% summarise(買上金額=sum(金額))

ensyu2 <- left_join(kokyaku1,kaiage2)

ggplot(data = ensyu2,mapping = aes(x = 来店回数, y = 買上金額)) +
  geom_point() +
  geom_smooth()
