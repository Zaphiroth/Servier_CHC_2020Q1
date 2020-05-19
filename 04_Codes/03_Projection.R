# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q1
# Purpose:      Projection
# programmer:   Zhe Liu
# date:         2020-05-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
total.imp <- total.20q1.imp

# hospital universe
hospital.universe <- pchc.universe %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            district = first(na.omit(`区[县/县级市】`)),
            hospital = first(na.omit(`单位名称`))) %>% 
  ungroup()

# city segment
segment <- read_xlsx("02_Inputs/seg_45cities.xlsx") %>% 
  filter(city != "上海") %>% 
  select("city", "seg" = "seg_up")

# sample PCHC
sample.pchc.list <- unique(total.imp$pchc)

# argument
sd.argument <- pchc.universe %>% 
  filter(`省` == "山东") %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup() %>% 
  filter(!is.na(est))

proj.argument <- read_xlsx("02_Inputs/7省自变量.xlsx") %>% 
  distinct() %>% 
  filter(!is.na(Est_DrugIncome_RMB)) %>% 
  select("province" = "省", "city" = "地级市", "pchc" = "PCHC_Code", "est" = "Est_DrugIncome_RMB") %>% 
  bind_rows(sd.argument) %>% 
  mutate(panel = ifelse(pchc %in% proj.raw$pchc, 1, 0),
         panel = ifelse(is.na(est), 0, panel),
         panel_all = panel)

# outlier
outlier <- read_xlsx("02_Inputs/outlier.xlsx")

# projection flag
proj.flag <- read_xlsx("02_Inputs/ot2.xlsx") %>% 
  select(pchc = mapping, market = mkt) %>% 
  distinct() %>% 
  mutate(flag_ot = 1)

# projection factor
proj.factor <- read_xlsx("02_Inputs/factor4.xlsx") %>% 
  select("market" = "mkt", "seg", "factor")


##---- Projection without Fuzhou ----
# projection data
proj.raw <- total.imp %>% 
  filter(city != "福州", quarter == "2020Q1")

# quarter sales
proj.quarter <- proj.raw %>% 
  group_by(quarter, province, city, pchc, market, atc3, molecule_desc, packid) %>% 
  summarise(panel_sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# price
proj.price <- proj.raw %>% 
  filter(units > 0) %>% 
  group_by(quarter, city, packid) %>% 
  summarise(price = sum(sales, na.rm = TRUE) / sum(units, na.rm = TRUE)) %>% 
  ungroup()

# universe set
universe.set <- merge(distinct(proj.argument, pchc), 
                      distinct(proj.raw, quarter)) %>% 
  merge(distinct(proj.raw, market, atc3, molecule_desc, packid)) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  left_join(proj.argument, by = "pchc") %>% 
  left_join(proj.flag, by = c("pchc", "market")) %>% 
  left_join(segment, by = "city") %>% 
  mutate(seg = ifelse(is.na(seg), 1, seg),
         panel = ifelse(pchc %in% outlier, 0, panel),
         panel = ifelse(!is.na(flag_ot), 0, panel),
         panel = ifelse(!(pchc %in% sample.pchc.list), 0, panel),
         panel = ifelse(province %in% c("安徽", "山东"), 1, panel)) %>% 
  filter(panel == 1) %>% 
  filter(city == "广州") %>% 
  left_join(proj.quarter, by = c("province", "city", "pchc", "quarter", 
                                 "market", "atc3", "molecule_desc", "packid")) %>% 
  mutate(panel_sales = ifelse(is.na(panel_sales), 0, panel_sales))

# QC
sum(universe.set$panel_sales, na.rm = TRUE) <= sum(proj.quarter$panel_sales, na.rm = TRUE)
# TRUE: no multiple match

# projection parameter
proj.parm <- universe.set %>% 
  group_by(quarter, market, packid, seg) %>% 
  summarise(slope = mean(panel_sales, na.rm = TRUE) / mean(est, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(quarter, market, packid, seg)

# projection result
proj.most <- universe.set %>% 
  left_join(proj.parm, by = c("quarter", "market", "packid", "seg")) %>% 
  mutate(predict_sales = est * slope,
         predict_sales = ifelse(predict_sales < 0, 0, predict_sales)) %>% 
  left_join(proj.factor, by = c("market", "seg")) %>% 
  mutate(final_sales = ifelse(panel_all == 0, predict_sales * factor, panel_sales)) %>% 
  left_join(proj.price, by = c("quarter", "city", "packid")) %>% 
  mutate(final_units = round(final_sales / price)) %>% 
  filter(final_sales > 0) %>% 
  mutate(year = stri_sub(quarter, 1, 4)) %>% 
  select(year, quarter, province, city, pchc, market, atc3, molecule_desc, packid, 
         units = final_units, sales = final_sales, panel)


##---- Result ----
# total.proj <- bind_rows(proj.most, proj.fz)
total.proj <- proj.raw %>% 
  mutate(panel = 0) %>% 
  bind_rows(proj.most) %>% 
  group_by(year, quarter, province, city, pchc, market, atc3, molecule_desc, packid, panel) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write_feather(total.proj, "03_Outputs/03_Servier_CHC_Projection_2020Q1.feather")






