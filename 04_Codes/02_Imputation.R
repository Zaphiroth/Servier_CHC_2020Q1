# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q1
# Purpose:      Imputation
# programmer:   Zhe Liu
# date:         2020-05-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Imputing inside existing provinces ----
total.imp.raw <- total.raw %>% 
  filter(city %in% c("广州", "上海"))

# quarterly date continuity
date.continuity <- total.imp.raw %>% 
  distinct(province, city, pchc, market, year, date) %>% 
  count(province, city, pchc, market, year) %>% 
  setDT() %>% 
  dcast(province + city + pchc + market ~ year, value.var = "n", fill = 0) %>% 
  mutate(cnt_min = pmin(`2019`, `2020`),
         cnt_max = pmax(`2019`, `2020`))

# city molecule yearly growth
city.growth <- date.continuity %>% 
  filter(cnt_min >= 2) %>% 
  inner_join(total.imp.raw, by = c("province", "city", "pchc", "market")) %>% 
  group_by(province, city, year, market, atc3, molecule_desc) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "year", "market", "atc3", "molecule_desc"), 
       measure.vars = c("units", "sales"), variable.name = "cate", value.name = "value") %>% 
  unite("type", cate, year) %>% 
  setDT() %>% 
  dcast(province + city + market + atc3 + molecule_desc ~ type, value.var = "value", fill = 0) %>% 
  mutate(sales_growth = sales_2020 / sales_2019,
         units_growth = units_2020 / units_2019,
         sales_growth = if_else(is.na(sales_growth) | sales_growth < 0.1 | sales_growth > 10, 1, sales_growth),
         units_growth = if_else(is.na(units_growth) | units_growth < 0.1 | units_growth > 10, 1, units_growth)) %>% 
  select("province", "city", "market", "atc3", "molecule_desc", "sales_growth", "units_growth")

# imputing
imputing.data <- date.continuity %>% 
  filter(cnt_max >= 2) %>% 
  select(province, city, pchc, market) %>% 
  left_join(total.imp.raw, by = c("province", "city", "pchc", "market")) %>% 
  mutate(date = stri_sub(date, 5, 6)) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc", "year", "date", "market", "atc3", "molecule_desc", "packid"),
       measure.vars = c("units", "sales"), variable.name = "cate", value.name = "value") %>% 
  unite("type", cate, year) %>% 
  dcast(province + city + pchc + market + atc3 + molecule_desc + packid + date ~ type,
        value.var = "value", fill = -1) %>%
  left_join(city.growth, by = c("province", "city", "market", "atc3", "molecule_desc")) %>% 
  mutate(sales_growth = if_else(is.na(sales_growth), 1, sales_growth),
         units_growth = if_else(is.na(units_growth), 1, units_growth),
         flag_2019 = if_else(`sales_2019` == -1, 1, 0),
         flag_2020 = if_else(`sales_2020` == -1, 1, 0),
         sales_2019 = if_else(flag_2019 == 1, `sales_2020` / sales_growth, `sales_2019`),
         sales_2020 = if_else(flag_2020 == 1, `sales_2019` * sales_growth, `sales_2020`),
         units_2019 = if_else(flag_2019 == 1, `units_2020` / units_growth, `units_2019`),
         units_2020 = if_else(flag_2020 == 1, `units_2019` * units_growth, `units_2020`)) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc", "market", "atc3", "molecule_desc", "packid", "date"), 
       measure.vars = c("flag_2019", "flag_2020", "sales_2019", "sales_2020", "units_2019", "units_2020"), 
       variable.name = "cate", value.name = "value") %>% 
  separate(cate, c("type", "year"), sep = "_") %>% 
  dcast(province + city + pchc + year + date + market + atc3 + molecule_desc + packid ~ type, 
        value.var = "value") %>% 
  rename("units_imp" = "units",
         "sales_imp" = "sales") %>% 
  mutate(date = stri_paste(year, date),
         quarter = stri_paste(year, "Q1")) %>% 
  select(year, date, quarter, province, city, pchc, market, atc3, molecule_desc, 
         packid, units_imp, sales_imp, flag)

# joint
total.20q1.imp <- total.imp.raw %>% 
  full_join(imputing.data, by = c("year", "date", "quarter", "province", "city", "pchc", 
                                  "market", "atc3", "molecule_desc", "packid")) %>% 
  mutate(units = if_else(is.na(units), units_imp, units),
         sales = if_else(is.na(sales), sales_imp, sales),
         flag1 = if_else(is.na(flag), 0, flag)) %>% 
  select(year, date, quarter, province, city, pchc, market, atc3, molecule_desc, 
         packid, units, sales, flag1)

total.in.imp <- total.raw %>% 
  mutate(flag1 = 0) %>% 
  filter(city %in% c("北京")) %>% 
  # filter(province %in% c("福建", "浙江")) %>% 
  # filter(city %in% c("北京", "常州", "福州", "广州", "杭州", "南京", "宁波", 
  #                    "泉州", "厦门", "上海", "苏州", "温州", "无锡")) %>% 
  bind_rows(total.20q1.imp) %>% 
  mutate(seg_city = ifelse(is.na(seg_city), city, seg_city))


##---- Imputing outside existing provinces ----
# model data
model.data <- servier.history %>% 
  filter(city %in% target.city) %>% 
  group_by(year, date, quarter, province, city, pchc, market, atc3, molecule_desc, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# sample data
sample.data1 <- model.data %>% 
  filter(province %in% c("北京", "福建", "江苏", "安徽", "浙江")) %>% 
  mutate(flag = if_else(province %in% c("福建", "浙江"), 0, 1))

sample.data2 <- model.data %>% 
  filter(province %in% c("福建", "江苏", "安徽", "浙江")) %>% 
  mutate(flag = if_else(province %in% c("福建", "浙江"), 0, 1))

# summarising pack ID by PCHC
sample.pchc1 <- sample.data1 %>% 
  group_by(province, city, pchc, date, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

sample.pchc2 <- sample.data2 %>% 
  group_by(province, city, pchc, date, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# model1
model.set1 <- setDT(sample.pchc1) %>% 
  dcast(province + city + pchc + flag ~ date, value.var = "sales", fill = 0)

train.set1 <- model.set1[flag == 1, ]
test.set1 <- model.set1[flag == 0, ]

train.set.tmp1 <- train.set1[, -c("province", "city", "pchc")]
test.set.tmp1 <- test.set1[, -c("province", "city", "pchc")]

knn.model1 <- kknn(flag ~ ., train = train.set.tmp1, test = test.set.tmp1, k = 3, scale = TRUE)

# model2
model.set2 <- setDT(sample.pchc2) %>% 
  dcast(province + city + pchc + flag ~ date, value.var = "sales", fill = 0)

train.set2 <- model.set2[flag == 1, ]
test.set2 <- model.set2[flag == 0, ]

train.set.tmp2 <- train.set2[, -c("province", "city", "pchc")]
test.set.tmp2 <- test.set2[, -c("province", "city", "pchc")]

knn.model2 <- kknn(flag ~ ., train = train.set.tmp2, test = test.set.tmp2, k = 3, scale = TRUE)

# weightage extraction 1
model.indice1 <- as.data.frame(knn.model1$C) %>% 
  lapply(function(x) {
    train.set1$pchc[x]
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  bind_cols(test.set1[, c("province", "city", "pchc")]) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc"), variable.name = "knn_level", value.name = "knn_pchc")

model.weight1 <- as.data.frame(knn.model1$D) %>% 
  lapply(function(x) {
    1 / (x+1)
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  mutate(weight_sum = pchc_1 + pchc_2 + pchc_3,
         pchc_1 = pchc_1 / weight_sum,
         pchc_2 = pchc_2 / weight_sum,
         pchc_3 = pchc_3 / weight_sum) %>% 
  bind_cols(test.set1[, c("province", "city", "pchc")]) %>% 
  select(-weight_sum) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc"), variable.name = "knn_level", value.name = "knn_weight")

# weightage extraction 2
model.indice2 <- as.data.frame(knn.model2$C) %>% 
  lapply(function(x) {
    train.set2$pchc[x]
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  bind_cols(test.set2[, c("province", "city", "pchc")]) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc"), variable.name = "knn_level", value.name = "knn_pchc")

model.weight2 <- as.data.frame(knn.model2$D) %>% 
  lapply(function(x) {
    1 / (x+1)
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  mutate(weight_sum = pchc_1 + pchc_2 + pchc_3,
         pchc_1 = pchc_1 / weight_sum,
         pchc_2 = pchc_2 / weight_sum,
         pchc_3 = pchc_3 / weight_sum) %>% 
  bind_cols(test.set2[, c("province", "city", "pchc")]) %>% 
  select(-weight_sum) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc"), variable.name = "knn_level", value.name = "knn_weight")

# ah, bj, js - 1910~1912
ah.bj.js <- total.raw %>% 
  filter(province == "安徽") %>% 
  bind_rows(total.in.imp) %>% 
  filter(province %in% c("安徽", "北京", "江苏"), date %in% c("201910","201911","201912"))

# sample growth 1
sample.sales1 <- sample.data1 %>% 
  filter(flag == 1, date %in% c("201810","201811","201812")) %>% 
  mutate(year = stri_sub(date, 1, 4)) %>% 
  bind_rows(ah.bj.js) %>% 
  group_by(knn_pchc = pchc, molecule_desc, year) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

sample.growth1 <- model.indice1 %>% 
  left_join(model.weight1, by = c("province", "city", "pchc", "knn_level")) %>% 
  inner_join(sample.sales1, by = c("knn_pchc")) %>% 
  group_by(pchc, molecule_desc, year) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  setDT() %>% 
  dcast(pchc + molecule_desc ~ year, value.var = "sales", fill = 0) %>% 
  mutate(growth = `2019` / `2018`) %>% 
  select("pchc", "molecule_desc", "growth")

# sample growth 2
sample.sales2 <- sample.data2 %>% 
  filter(flag == 1, date %in% c("201810","201811","201812")) %>% 
  mutate(year = stri_sub(date, 1, 4)) %>% 
  bind_rows(ah.bj.js) %>% 
  group_by(knn_pchc = pchc, molecule_desc, year) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

sample.growth2 <- model.indice2 %>% 
  left_join(model.weight2, by = c("province", "city", "pchc", "knn_level")) %>% 
  inner_join(sample.sales2, by = c("knn_pchc")) %>% 
  group_by(pchc, molecule_desc, year) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  setDT() %>% 
  dcast(pchc + molecule_desc ~ year, value.var = "sales", fill = 0) %>% 
  mutate(growth = `2019` / `2018`) %>% 
  select("pchc", "molecule_desc", "growth")

# joint
total.out.imp1 <- total.in.imp %>% 
  filter(date %in% c("201810","201811","201812"),
         province %in% c("浙江","福建")) %>% 
  left_join(sample.growth1, by = c("pchc", "molecule_desc")) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units = units * growth,
         sales = sales * growth,
         date = gsub("2018", "2019", date),
         quarter = gsub("2018", "2019", quarter),
         year = "2019") %>% 
  filter(!(city %in% c("杭州","宁波")))

total.out.imp2 <- total.in.imp %>% 
  filter(date %in% c("201810","201811","201812"),
         city %in% c("杭州","宁波")) %>% 
  left_join(sample.growth2, by = c("pchc", "molecule_desc")) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units = units * growth,
         sales = sales * growth,
         date = gsub("2018", "2019", date),
         quarter = gsub("2018", "2019", quarter),
         year = "2019")

total.out.imp <- bind_rows(total.out.imp1, total.out.imp2) %>% 
  select(-growth) %>% 
  mutate(flag2 = 1)










