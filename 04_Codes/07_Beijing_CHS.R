# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q1
# Purpose:      Projection of Beijing CHS
# programmer:   Zhe Liu
# date:         2020-06-08
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin ----
# pack info
chs.pack <- read_xlsx("02_Inputs/ims.mapping1904.xlsx") %>% 
  distinct(packid = stri_pad_left(Pack_Id, 7, 0), pack_desc = Pck_Desc, 
           corp_desc = Corp_Desc, pack_size = PckSize_Desc)

# pack.mapping.19 <- read.xlsx("02_Inputs/all_raw_data_m_19q4_ahbjjs_20200204-匹配完成.xlsx", sheet = 2) %>% 
#   select(`匹配名`, `剂型`, `规格`, `包装`, `生产企业`, packid = `七位产品编码`) %>% 
#   distinct() %>% 
#   filter(!is.na(`匹配名`), !is.na(packid)) %>% 
#   mutate(packid = stri_pad_left(packid, 7, 0),
#          `规格` = tolower(`规格`))

# trade name
tradename.mapping <- fread("02_Inputs/pfc与ims数据对应.csv") %>% 
  distinct(packid = stri_pad_left(Pack_Id, 7, 0), trade_name = `商品名`)


##---- Beijing CHS projection ----
# CHS raw
bj.chs.raw <- servier.bj.raw %>% 
  filter(`项目` == "【CHC项目】-2020年北京") %>% 
  mutate(year = as.character(`年`),
         date = as.character(`月份`),
         quarter = as.character(`季度`),
         province = "北京",
         city = "北京",
         hospital = `医院名称`,
         packid = stri_pad_left(packcode, 7, 0)) %>% 
  distinct() %>% 
  filter(quarter == "2020Q1", province == "北京", grepl("服务站", hospital)) %>% 
  left_join(market.def, by = "packid") %>% 
  filter(!is.na(market)) %>% 
  group_by(year, quarter, province, city, market, atc3, molecule_desc, packid) %>% 
  summarise(sales = sum(`金额`, na.rm = TRUE),
            units = sum(`数量`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(tradename.mapping, by = "packid") %>% 
  left_join(chs.pack, by = "packid") %>% 
  mutate(dosage_units = pack_size * units,
         atc2 = stri_sub(atc3, 1, 3),
         market = if_else(atc2 %in% c("C07", "C08"), "IHD", market)) %>% 
  filter(!(market == "IHD" & trade_name == "CORLENTOR")) %>% 
  filter(!(market == "OAD" & trade_name == "TANG LIN"))

# adjust
chs.ihd.adj <- bj.chs.raw %>% 
  filter(atc2 %in% c("C07", "C08")) %>% 
  mutate(factor = if_else(atc2 == "C07", 0.25, 0.1),
         sales = sales * factor,
         units = units * factor,
         dosage_units = dosage_units * factor,
         market = "IHD") %>% 
  select(-factor)

chs.htn.adj <- bj.chs.raw %>% 
  filter(atc2 %in% c("C07", "C08")) %>% 
  mutate(factor = if_else(atc2 == "C07", 0.75, 0.9),
         sales = sales * factor,
         units = units * factor,
         dosage_units = dosage_units * factor,
         market = "HTN") %>% 
  select(-factor)


##---- Result ----
bj.chs <- bj.chs.raw %>% 
  filter(!(atc2 %in% c("C05", "C07", "C08")), 
         stri_sub(pack_desc, 1, 4) %in% c("CAP ", "TAB ", "PILL")) %>% 
  bind_rows(chs.ihd.adj, chs.htn.adj) %>% 
  mutate(channel = "CHS",
         year = "2020") %>% 
  select(province, city, year, quarter, market, atc3, molecule_desc, packid, 
         pack_desc, trade_name, corp_desc, channel, units, dosage_units, sales)








