# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q1
# Purpose:      Summary
# programmer:   Zhe Liu
# date:         2020-05-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


#---- CHC ----
# target city
target.prov <- c("北京", "福建", "广东", "江苏", "上海", "浙江", "安徽", "山东")
target.city <- c("北京", "常州", "福州", "广州", "杭州", "南京", 
                 "宁波", "泉州", "厦门", "上海", "苏州", "温州", 
                 "无锡", "济南", "徐州", "合肥", "绍兴", "青岛")

# trade name
tradename.mapping <- fread("02_Inputs/pfc与ims数据对应.csv") %>% 
  distinct(packid = stri_pad_left(Pack_Id, 7, 0), trade_name = `商品名`)

# flag
flag.raw <- read.xlsx("02_Inputs/13城市的招标flag_zs_flag.xlsx")

flag <- flag.raw %>% 
  filter(!is.na(`是否是13城市`)) %>% 
  distinct(province = `省`, city = `地级市`, pchc = PCHC_Code, zs_flag)

total.zs <- total.scale.adj %>% 
  left_join(flag, by = c("province", "city", "pchc")) %>% 
  mutate(zs_flag = if_else(zs_flag != 1, 0, zs_flag)) %>% 
  filter(!(city %in% c("北京", "上海") & zs_flag != 1))

# corporation
corp.ref <- fread("02_Inputs/cn_corp_ref_201912_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct()

# pack
pack.ref <- fread("02_Inputs/cn_prod_ref_201912_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct() %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0))

corp.pack <- select(pack.ref, "Pack_Id", "Pck_Desc", "Corp_ID") %>% 
  distinct() %>% 
  left_join(corp.ref, by = "Corp_ID") %>% 
  select("packid" = "Pack_Id", "pack_desc" = "Pck_Desc", "corp_desc" = "Corp_Desc")

# pack size
pack.size <- distinct(pack.ref, packid = Pack_Id, pack_size = PckSize_Desc)

# join
chc.part <- bind_rows(total.zs, proj.sh) %>% 
  left_join(tradename.mapping, by = "packid") %>% 
  left_join(corp.pack, by = "packid") %>% 
  left_join(pack.size, by = "packid") %>% 
  filter(stri_sub(pack_desc, 1, 4) %in% c("CAP ", "TAB ", "PILL")) %>% 
  mutate(dosage_units = pack_size * units,
         channel = "CHC",
         corp_desc = if_else(corp_desc == "B.MYERS SQUIBB GRP" & trade_name == "GLUCOPHAGE", 
                             "MERCK GROUP", 
                             corp_desc)) %>% 
  group_by(province, city, year, quarter, market, atc3, molecule_desc, packid, 
           pack_desc, trade_name, corp_desc, channel) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            dosage_units = sum(dosage_units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(sales = if_else(sales < 0, 0, sales),
         units = if_else(units < 0, 0, units),
         sales = if_else(units == 0, 0, sales),
         units = if_else(sales == 0, 0, units))

# A10S
a10s <- total.scale.adj %>% 
  filter(atc3 == "A10S") %>% 
  left_join(tradename.mapping, by = "packid") %>% 
  left_join(corp.pack, by = "packid") %>% 
  left_join(pack.size, by = "packid") %>% 
  filter(stri_sub(pack_desc, 1, 4) %in% c("CAP ", "TAB ", "PILL")) %>% 
  mutate(dosage_units = pack_size * units,
         channel = "CHC") %>% 
  group_by(province, city, year, quarter, market, atc3, molecule_desc, packid, 
           pack_desc, trade_name, corp_desc, channel) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            dosage_units = sum(dosage_units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# total CHC
total.chc <- bind_rows(chc.part, a10s, bj.chs) %>% 
  select(Pack_ID = packid, Channel = channel, Province = province, City = city, 
         Date = quarter, ATC3 = atc3, MKT = market, Molecule_Desc = molecule_desc, 
         Prod_Desc = trade_name, Pck_Desc = pack_desc, Corp_Desc = corp_desc, 
         Sales = sales, Units = units, DosageUnits = dosage_units)


##---- Add flag ----
# readin 4+7 flag
capital.47 <- read_xlsx("02_Inputs/4+7+省会名单.xlsx") %>% 
  filter(`类别` %in% "4+7城市") %>% 
  mutate(City = gsub("市", "", `城市`)) %>% 
  select("City", "是否是4+7城市" = "类别")

# product bid
prod.bid <- read_xlsx("02_Inputs/Displayname Mapping.xlsx", sheet = 1) %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0),
         Pack_ID_5 = stri_sub(Pack_ID, 1, 5),
         `Display Name3 CN` = if_else(`Display Name3 CN` %in% c("中标品规", "非中标品规"), 
                                      "仿制", 
                                      `Display Name3 CN`),
         `Display Name3 CN` = gsub("-2|-1", "", `Display Name3 CN`),
         `Display Name2 CN` = gsub("-2|-1", "", `Display Name2 CN`)) %>% 
  distinct(name1 = `Display Name1 CN`,
           name2 = `Display Name2 CN`,
           name3 = `Display Name3 CN`,
           Pack_ID_5)

# join
chc.bid <- total.chc %>% 
  left_join(capital.47, by = "City") %>% 
  mutate(Pack_ID_5 = stri_sub(Pack_ID, 1, 5)) %>% 
  left_join(prod.bid, by = "Pack_ID_5")

# corporation, ATC3
corp.atc3 <- read_xlsx("02_Inputs/产品性质_chpa 08.23(1).xlsx", sheet = 1)

corp.type <- distinct(corp.atc3, Corp_Desc, Mnf_Type = `厂家性质`)

atc3.cn <- distinct(corp.atc3, ATC3 = ATC3_Code, `ATC3中文分类` = `类别`)

molecule.cn <- distinct(corp.atc3, Molecule_Desc, Molecule_CN = `分子`)

corp.add <- read_xlsx("02_Inputs/Corp_Info.xlsx") %>% 
  group_by(Corp_Desc) %>% 
  arrange(MnfType_Desc) %>% 
  summarise(Mnf_Type1 = first(MnfType_Desc)) %>% 
  ungroup() %>% 
  mutate(Mnf_Type1 = if_else(is.na(Mnf_Type1), "Local", Mnf_Type1),
         Mnf_Type1 = if_else(Mnf_Type1 %in% c("Imported", "Joint Venture"), "MNC", Mnf_Type1))

# molecule bid
molecule.bid <- distinct(chc.bid, Molecule_Desc, name1) %>% 
  filter(name1 == "4+7分子") %>% 
  right_join(molecule.cn, by = "Molecule_Desc") %>% 
  mutate(name1 = if_else(Molecule_CN %in% c("赖诺普利", "卡托普利"), "4+7分子", name1)) %>% 
  select(Molecule_Desc, name1)

# join
chc.flag <- chc.bid %>% 
  select(-name1) %>% 
  left_join(molecule.bid, by = "Molecule_Desc") %>% 
  left_join(corp.type, by = "Corp_Desc") %>% 
  left_join(corp.add, by = "Corp_Desc") %>% 
  left_join(atc3.cn, by = "ATC3") %>% 
  mutate(name3 = trimws(name3),
         Mnf_Type = if_else(is.na(Mnf_Type), Mnf_Type1, Mnf_Type),
         Mnf_Type = trimws(Mnf_Type),
         name3 = ifelse(is.na(name3) & Mnf_Type == "MNC", "原研", 
                        ifelse(is.na(name3) & Mnf_Type == "Local", "仿制", 
                               name3)),
         name2 = ifelse(name2 == "非中标产品", NA, name2)) %>% 
  rename("是否进入带量采购" = "name1",
         "是否是中标品种" = "name2",
         "是否是原研" = "name3",
         "是否是MNC" = "Mnf_Type") %>% 
  select(-Pack_ID_5, -Mnf_Type1) %>% 
  filter(!grepl("AMP", Pck_Desc))


##---- Add new column ----
packid.profile.raw <- read_xlsx("02_Inputs/packid_prod_20181112.xlsx")

packid.profile <- packid.profile.raw %>% 
  mutate(Prod_ID = substr(stri_pad_left(Pack_Id, 7, "0"), 1, 5)) %>% 
  distinct(Pack_Id, Prod_ID, ATC4_Code, ims_product_cn)

prod.profile <- packid.profile %>%
  select(Prod_ID, ATC4_Code, ims_product_cn) %>%
  distinct()

city.en <- read.xlsx("02_Inputs/CityEN.xlsx")

chc.add <- chc.flag %>% 
  mutate(first_num_position = stri_locate_first(Pck_Desc, regex = "\\d")[,1],
         last_space_position = stri_locate_last(Pck_Desc, regex = "\\s")[,1],
         Package = str_squish(substr(Pck_Desc, 1, first_num_position - 1)),
         Dosage = str_squish(substr(Pck_Desc, first_num_position, 
                                    last_space_position - 1)),
         Quantity = as.integer(str_squish(substr(Pck_Desc, last_space_position, 
                                                 nchar(Pck_Desc)))),
         Pack_ID = as.integer(Pack_ID)) %>% 
  left_join(packid.profile, by = c("Pack_ID" = "Pack_Id")) %>% 
  mutate(Prod_ID = substr(stri_pad_left(Pack_ID, 7, "0"), 1, 5)) %>% 
  left_join(prod.profile, by = c("Prod_ID")) %>% 
  mutate(ATC4_Code.x = ifelse(is.na(ATC4_Code.x) & !is.na(Pack_ID),
                              ATC4_Code.y, ATC4_Code.x),
         ims_product_cn.x = ifelse(is.na(ims_product_cn.x) & !is.na(Pack_ID),
                                   ims_product_cn.y, ims_product_cn.x)) %>% 
  rename(ATC4 = ATC4_Code.x, Prod_CN_Name = ims_product_cn.x) %>% 
  select(-c(Prod_ID, ATC4_Code.y, ims_product_cn.y)) %>% 
  mutate(
    Pack_ID = stri_pad_left(Pack_ID, 7, 0),
    TherapeuticClsII = case_when(
      ATC3 == "A10H" ~ "SULPHONYLUREA",
      ATC3 == "A10J" ~      "BIGUANIDE",
      ATC3 == "A10K" ~     "GLITAZONE",
      ATC3 == "A10L" ~     "AGIs",
      ATC3 == "A10M" ~    "METAGLINIDE",
      ATC3 == "A10N" ~    "DPP-IV",
      ATC3 == "A10P" ~     "SGLT2",
      ATC3 == "A10S" ~     "GLP-1",
      ATC3 == "A10X" ~     "OTHERS",
      ATC3 == "C02A" ~     "ANTI-HTN",
      ATC3 == "C02B" ~     "ANTI-HTN",
      ATC3 == "C02C" ~     "ANTI-HTN",
      ATC3 == "C03A" ~     "DIURETICS",
      ATC3 == "C07A" ~     "BB",
      ATC3 == "C08A" ~     "CCB",
      ATC3 == "C08B" ~     "CCB",
      ATC3 == "C09A" ~     "ACEi PLAIN",
      ATC3 == "C09C" ~     "ARB PLAIN",
      ATC3 == "C07A" ~     "BB",
      ATC3 == "C08A" ~     "CCB",
      ATC3 == "C01E" ~     "NITRITES",
      Molecule_Desc == "TRIMETAZIDINE" ~   "TMZ",
      ATC3 == "C01D" & Molecule_Desc != "TRIMETAZIDINE" ~  "OTHERS",
      ATC4 %in% c("C09B3", "C09D3") ~ "A+C FDC",
      ATC4 %in% c("C09B1", "C09D1") ~ "A+D FDC",
      !is.na(ATC4) ~ "OTHERS",
      TRUE ~ NA_character_
    )
  ) %>% 
  select(Pack_ID, 
         Channel, 
         Province, 
         City,  
         Date, 
         ATC3, 
         MKT, Molecule_Desc,
         Prod_Desc, Pck_Desc, Corp_Desc, Sales,  Units, DosageUnits,
         ATC4, TherapeuticClsII, Prod_CN_Name,  Package, Dosage, Quantity, 
         `是否是4+7城市`, 是否进入带量采购, 是否是原研, 是否是中标品种,
         是否是MNC, ATC3中文分类) %>% 
  left_join(city.en, by = "City") %>% 
  mutate(`Period-MAT` = case_when(
    Date %in% c("2020Q1", "2020Q2", "2020Q3", "2020Q4") ~ "MAT20Q4",
    Date %in% c("2019Q1", "2019Q2", "2019Q3", "2019Q4") ~ "MAT19Q4",
    Date %in% c("2018Q1", "2018Q2", "2018Q3", "2018Q4") ~ "MAT18Q4",
    Date %in% c("2017Q1", "2017Q2", "2017Q3", "2017Q4") ~ "MAT17Q4",
    TRUE ~ NA_character_
  ),
  TherapeuticClsII = case_when(
    MKT == "HTN" & ATC3 == "C09A" ~ "RAASi Plain",
    MKT == "HTN" & ATC3 == "C09C" ~ "RAASi Plain",
    MKT == "HTN" & ATC3 == "C09B" ~ "RAASi FDC",
    MKT == "HTN" & ATC3 == "C09D" ~ "RAASi FDC",
    MKT == "HTN" & ATC3 == "C02A" ~ "ANTI-HTN",
    MKT == "HTN" & ATC3 == "C02B" ~ "ANTI-HTN",
    MKT == "HTN" & ATC3 == "C02C" ~ "ANTI-HTN",
    MKT == "HTN" & ATC3 == "C03A" ~ "DIURETICS",
    MKT == "HTN" & ATC3 == "C07A" ~ "BB",
    MKT == "HTN" & ATC3 == "C08A" ~ "CCB",
    MKT == "HTN" & ATC3 == "C08B" ~ "CCB",
    MKT == "OAD" & ATC3 == "A10H" ~ "SULPHONYLUREA",
    MKT == "OAD" & ATC3 == "A10J" ~ "BIGUANIDE",
    MKT == "OAD" & ATC3 == "A10K" ~ "GLITAZONE",
    MKT == "OAD" & ATC3 == "A10L" ~ "AG Is",
    MKT == "OAD" & ATC3 == "A10M" ~ "METAGLINIDE",
    MKT == "OAD" & ATC3 == "A10N" ~ "DPP-IV",
    MKT == "OAD" & ATC3 == "A10P" ~ "SGLT2",
    MKT == "OAD" & ATC3 == "A10S" ~ "GLP-1",
    MKT == "OAD" & ATC3 == "A10X" ~ "OTHERS",
    MKT == "IHD" & ATC3 == "C07A" ~ "BB",
    MKT == "IHD" & ATC3 == "C08A" ~ "CCB",
    MKT == "IHD" & ATC3 == "C01E" ~ "NITRITES",
    MKT == "IHD" & ATC3 == "C01D" & Molecule_Desc == "TRIMETAZIDINE" ~ "TMZ",
    MKT == "IHD" & ATC3 == "C01D" & Molecule_Desc != "TRIMETAZIDINE"~ "OTHERS",
    TRUE ~ NA_character_
  ),
  Sales = round(Sales, 2),
  Units = round(Units),
  DosageUnits = round(DosageUnits)) %>% 
  filter(Molecule_Desc != "ENALAPRIL+FOLIC ACID") %>% 
  # filter(Date %in% c("2017Q4", "2018Q1", "2018Q2", "2018Q3", "2018Q4", 
  #                    "2019Q1", "2019Q2", "2019Q3", "2019Q4")) %>% 
  # filter(Sales > 0, Units > 0) %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits, 
         `Period-MAT`, `CITY-EN`, TherapeuticClsII, Prod_CN_Name,  Package, 
         Dosage, Quantity, `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, 
         `是否是中标品种`, `是否是MNC`, `ATC3中文分类`)


##---- Result ----
chc.history <- read.xlsx("02_Inputs/CHC_MAX_16Q419Q4_0317.xlsx")

chc.result <- chc.history %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0)) %>% 
  bind_rows(chc.add) %>% 
  filter(Sales > 0, Units > 0, DosageUnits > 0, 
         !(MKT %in% c("HTN", "IHD") & 
             !(stri_sub(Package, 1, 4) %in% c("CAP ", "TAB ", "PILL"))), 
         Channel != "MAX") %>% 
  mutate(Corp_Desc = if_else(Corp_Desc == "LUYE GROUP", "LVYE GROUP", Corp_Desc),
         Corp_Desc = if_else(Prod_Desc == "GLUCOPHAGE", "MERCK GROUP", Corp_Desc),
         Pack_ID = if_else(Pack_ID == '4777502', '5890602', Pack_ID), 
         Corp_Desc = if_else(Pack_ID == '4777502', 'ASTRAZENECA GROUP', Corp_Desc)) %>% 
  group_by(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
           Prod_Desc, Pck_Desc, Corp_Desc, `Period-MAT`, `CITY-EN`, 
           TherapeuticClsII, Prod_CN_Name,  Package, Dosage, Quantity, 
           `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, `是否是中标品种`, 
           `是否是MNC`, `ATC3中文分类`) %>% 
  summarise(Sales = sum(Sales, na.rm = TRUE),
            Units = sum(Units, na.rm = TRUE),
            DosageUnits = sum(DosageUnits, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits, 
         `Period-MAT`, `CITY-EN`, TherapeuticClsII, Prod_CN_Name,  Package, 
         Dosage, Quantity, `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, 
         `是否是中标品种`, `是否是MNC`, `ATC3中文分类`) %>% 
  arrange(Channel, Date, Province, City, MKT, Pack_ID)

write.xlsx(chc.result, "03_Outputs/08_Servier_CHC_Summary_2020Q1.xlsx")


##---- BI check ----
bi.chk <- chc.result %>% 
  filter(Prod_CN_Name %in% c('爱通立', '沐舒坦', '泰毕全', '森福罗', '爱全乐', 
                             '思力华', '欧唐静', '欧唐宁'), 
         stri_sub(Date, 1, 4) == '2019')







