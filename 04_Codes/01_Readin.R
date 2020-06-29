# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q1
# Purpose:      Readin data
# programmer:   Zhe Liu
# Date:         2020-05-18
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
# PCHC code
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20200507.xlsx", sheet = "PCHC")

pchc.mapping1 <- pchc.universe %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc1 = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.universe %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc2 = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- pchc.mapping1 %>% 
  group_by(pchc = pchc1) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()

# IMS pack
ims.pack <- fread("02_Inputs/pfc与ims数据对应.csv") %>% 
  mutate(packid = stri_pad_left(Pack_Id, 7, 0),
         atc3 = stri_sub(ATC4_Code, 1, 4),
         atc2 = stri_sub(ATC4_Code, 1, 3)) %>% 
  distinct(packid, atc3, atc2, molecule_desc = Molecule_Desc)

# market definition
market.def <- read_xlsx("02_Inputs/Market_Definition.xlsx") %>% 
  distinct(molecule_desc = Molecule_Desc, market = TA) %>% 
  right_join(ims.pack, by = "molecule_desc") %>% 
  filter(!is.na(market))

# target city
target.prov <- c("北京", "福建", "广东", "江苏", "上海", "浙江", "安徽", "山东")
target.city <- c("北京", "常州", "福州", "广州", "杭州", "南京", 
                 "宁波", "泉州", "厦门", "上海", "苏州", "温州", 
                 "无锡", "济南", "徐州", "合肥", "绍兴", "青岛")


##---- Formatting ----
# Guangdong
servier.gd.raw <- read.csv("02_Inputs/raw data/gzs 20q1.csv")

servier.gd <- servier.gd.raw %>% 
  mutate(year = stri_sub(period, 1, 4),
         date = stri_replace_all_fixed(period, "-", "0"),
         quarter = stri_paste(year, "Q1"),
         province = "广东",
         city = "广州",
         packid = stri_pad_left(pfc, 7, 0)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "name" = "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "name" = "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(market.def, by = "packid") %>% 
  select(year, date, quarter, province, city, pchc, market, atc3, molecule_desc, 
         packid, units = unit, sales = value)

# Beijing
servier.bj.raw <- read.xlsx("02_Inputs/raw data/beijing2020Q1_packid_moleinfo.xlsx")

servier.bj <- servier.bj.raw %>% 
  filter(`项目` == "【CHC项目】-2020年北京") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         quarter = as.character(`季度`),
         province = "北京",
         city = "北京",
         hospital = `医院名称`,
         packid = stri_pad_left(packcode, 7, 0)) %>% 
  distinct() %>% 
  left_join(pchc.mapping1, by = c("province", "city", "hospital")) %>% 
  left_join(pchc.mapping2, by = c("province", "city", "hospital")) %>% 
  mutate(pchc = ifelse(is.na(pchc1), pchc2, pchc1)) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(market.def, by = "packid") %>% 
  select(year, date, quarter, province, city, pchc, market, atc3, molecule_desc, 
         packid, units = `数量`, sales = `金额`)

# Shanghai
servier.sh.raw <- read.xlsx("02_Inputs/raw data/shanghai_201805_202004_packid_moleinfo_PCHC.xlsx")

servier.sh <- servier.sh.raw %>% 
  mutate(year = as.character(Year),
         date = as.character(Month),
         quarter = ifelse(stri_sub(date, 5, 6) %in% c("01", "02", "03"), paste0(year, "Q1"), 
                          ifelse(stri_sub(date, 5, 6) %in% c("04", "05", "06"), paste0(year, "Q2"), 
                                 ifelse(stri_sub(date, 5, 6) %in% c("07", "08", "09"), paste0(year, "Q3"), 
                                        ifelse(stri_sub(date, 5, 6) %in% c("10", "11", "12"), paste0(year, "Q4"), 
                                               NA_character_)))),
         province = "上海",
         city = "上海",
         seg_city = paste0(`城市`, `区县`),
         pchc = PCHC_Code,
         packid = packcode) %>% 
  distinct() %>% 
  left_join(market.def, by = "packid") %>% 
  select(year, date, quarter, province, city, seg_city, pchc, market, atc3, molecule_desc, 
         packid, units = `数量`, sales = `金额`)

# history
servier.history <- read_feather("02_Inputs/raw data/Servier_CHC_Total_Raw_2017-2019.feather")


##---- Bind ----
total.raw <- bind_rows(servier.history, servier.gd, servier.bj, servier.sh) %>% 
  filter(pchc != "#N/A", !is.na(market), units > 0, sales > 0, quarter %in% c("2019Q1", "2020Q1")) %>% 
  filter(city %in% target.city) %>% 
  mutate(seg_city = ifelse(is.na(seg_city), city, seg_city)) %>% 
  group_by(year, date, quarter, pchc, market, atc3, molecule_desc, packid) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            seg_city = first(na.omit(seg_city)),
            units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write_feather(total.raw, "03_Outputs/01_Servier_CHC_Raw_2020Q1.feather")

total.raw.chk <- bind_rows(servier.history, servier.gd, servier.bj, servier.sh) %>% 
  filter(pchc != "#N/A", !is.na(market), units > 0, sales > 0, 
         quarter %in% c("2019Q1", "2019Q2", "2019Q3", "2019Q4", "2020Q1")) %>% 
  filter(city %in% target.city) %>% 
  mutate(seg_city = ifelse(is.na(seg_city), city, seg_city)) %>% 
  group_by(year, date, quarter, pchc, market, atc3, molecule_desc, packid) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            seg_city = first(na.omit(seg_city)),
            units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(total.raw.chk, "03_Outputs/01_Servier_CHC_Raw_2020Q1.xlsx")


chk.growth <- bind_rows(servier.history, servier.gd, servier.bj, servier.sh) %>% 
  filter(pchc != "#N/A", !is.na(market), units > 0, sales > 0) %>% 
  filter(city %in% target.city, market == "OAD", year %in% c("2017", "2018", "2019")) %>% 
  group_by(year, city, market) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(city, year) %>% 
  mutate(growth = sales / lag(sales) - 1) %>% 
  filter(year == "2019")

write.xlsx(chk.growth, "05_Internal_Review/Growth_2018_2019.xlsx")


chk <- bind_rows(servier.history, servier.gd, servier.bj, servier.sh) %>% 
  filter(pchc != "#N/A", !is.na(market), units > 0, sales > 0) %>% 
  filter(city %in% target.city, market == "OAD", year %in% c("2017", "2018", "2019")) %>% 
  group_by(year, market) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(year) %>% 
  mutate(growth = sales / lag(sales) - 1)
