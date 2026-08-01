# SCRAP CODE

# create df for air transport
air_imports <- select(imports_new, AIR_WGT_MO, MONTH_NUMBER)

# total air imports by month
air_over_time <- aggregate(.~MONTH_NUMBER, data = air_imports, FUN=sum)

?ggplot
?mutate()
?aggregate
?distinct()
mutate(diff = VES_WGT_MO - CNT_WGT_MO) %>% # examining containerized vs vessel weights since they appear mostly the same??
  arrange(desc(diff))