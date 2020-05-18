chk <- servier.gd %>% 
  filter(is.na(pchc)) %>% 
  distinct(province, city, hospital = name) %>% 
  filter(grepl("服务中心", hospital)) %>% 
  filter(!grepl("服务部|社区站|服务站|卫生室|门诊|医务室|卫生所|中心分部|分院|分中心|计划生育|计生|保健计划|院区|南区|西区|诊所|药房", hospital)) %>% 
  arrange(province, city) %>% 
  mutate(flag = 1)

write.xlsx(chk, "05_Internal_Review/PCHC_Not_Matched.xlsx")
