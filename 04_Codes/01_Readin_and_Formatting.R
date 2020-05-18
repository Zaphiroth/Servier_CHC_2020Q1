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
  group_by(pchc1) %>% 
  summarise(province = first(province),
            city = first(city),
            district = first(district)) %>% 
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
         month = stri_replace_all_fixed(period, "-", "0"),
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
  select(year, month, quarter, province, city, pchc, market, packid, units = unit, sales = value)

# 2019Q1
servier.history <- read_feather("02_Inputs/raw data/Servier_CHC_Total_Raw.feather")


##---- Bind ----
total.raw <- bind_rows(servier.history, servier.gd) %>% 
  filter(pchc != "#N/A", !is.na(market), units > 0, sales > 0, quarter %in% c("2019Q1", "2020Q1")) %>% 
  group_by(year, month, quarter, province, city, pchc, market, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()





