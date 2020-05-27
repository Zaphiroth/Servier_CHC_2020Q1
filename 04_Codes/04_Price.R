# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q1
# Purpose:      Price
# programmer:   Zhe Liu
# date:         2020-05-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Origin Price ----
total.imp <- total.in.imp
# total.proj <- read_feather("03_Outputs/04_Servier_CHC_Projection.feather")

price.origin <- total.imp %>% 
  filter(units > 0) %>% 
  group_by(packid, year, quarter, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by year ----
price.year <- total.imp %>% 
  filter(units > 0) %>% 
  group_by(packid, year, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_year = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by city ----
price.city <- total.imp %>% 
  filter(units > 0) %>% 
  group_by(packid, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_city = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by pack ID ----
price.pack <- total.imp %>% 
  filter(units > 0) %>% 
  group_by(packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_pack = sales / units) %>% 
  select(-sales, -units)


##---- Add new price ----
# target city
target.prov <- c("北京", "福建", "广东", "江苏", "上海", "浙江", "安徽", "山东")
target.city <- c("北京", "常州", "福州", "广州", "杭州", "南京", 
                 "宁波", "泉州", "厦门", "上海", "苏州", "温州", 
                 "无锡", "济南", "徐州", "合肥", "绍兴", "青岛")

total.price <- total.proj %>% 
  filter(province %in% target.prov, city %in% target.city) %>% 
  left_join(price.origin, by = c("province", "city", "year", "quarter", "packid")) %>% 
  left_join(price.year, by = c("province", "city", "year", "packid")) %>% 
  left_join(price.city, by = c("province", "city", "packid")) %>% 
  left_join(price.pack, by = c("packid")) %>% 
  mutate(price = ifelse(is.na(price), price_year, price),
         price = ifelse(is.na(price), price_city, price),
         price = ifelse(is.na(price), price_pack, price)) %>% 
  mutate(units = sales / price,
         units = ifelse(units <= 0, 0, units)) %>% 
  filter(!is.na(price), !is.na(sales)) %>% 
  select(year, quarter, province, city, pchc, market, atc3, 
         molecule_desc, packid, units, sales, price, panel)

write_feather(total.price, "03_Outputs/04_Servier_CHC_Price_2020Q1.feather")







