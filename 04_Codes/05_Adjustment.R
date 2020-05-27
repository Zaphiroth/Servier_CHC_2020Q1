# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q1
# Purpose:      Adjustment
# programmer:   Zhe Liu
# date:         2020-05-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- ATC2 adjustment ----
atc.adj.raw <- total.price %>% 
  mutate(atc2 = stri_sub(atc3, 1, 3),
         market = if_else(atc2 %in% c("C07", "C08"), "IHD", market)) %>% 
  filter(!(market == "IHD" & molecule_desc == "IVABRADINE")) %>% 
  filter(!(market == "OAD" & molecule_desc == "EPALRESTAT"))

ihd.adj <- atc.adj.raw %>% 
  filter(atc2 %in% c("C07", "C08")) %>% 
  mutate(factor = if_else(atc2 == "C07", 0.25, 0.1),
         sales = sales * factor,
         units = units * factor,
         market = "IHD") %>% 
  select(-factor)

htn.adj <- atc.adj.raw %>% 
  filter(atc2 %in% c("C07", "C08")) %>% 
  mutate(factor = if_else(atc2 == "C07", 0.75, 0.9),
         sales = sales * factor,
         units = units * factor,
         market = "HTN") %>% 
  select(-factor)

total.atc.adj <- atc.adj.raw %>% 
  filter(!(atc2 %in% c("C07", "C08"))) %>% 
  bind_rows(ihd.adj, htn.adj)


##---- Scale adjustment ----
scale.factor <- read_xlsx("02_Inputs/施维雅规模调整系数.xlsx", sheet = 2)

scale.adj <- total.atc.adj %>% 
  filter(panel != 1) %>% 
  left_join(scale.factor, by = c("city", "market" = "mkt")) %>% 
  mutate(factor = if_else(is.na(factor), 1, factor),
         factor = if_else(city == "上海", 1, factor),
         sales = sales * factor,
         units = units * factor) %>% 
  select(-factor)

total.scale.adj <- total.atc.adj %>% 
  filter(panel == 1) %>% 
  bind_rows(scale.adj)

write_feather(total.scale.adj, "03_Outputs/05_Servier_CHC_Adjustment_2020Q1.feather")



