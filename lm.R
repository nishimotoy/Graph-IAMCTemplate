# 相関係数 ------------------------------------------------------

  df_His_R <- df_Graph %>% filter(SCENARIO=='Historical_R17') 
  attach(df_His_R)
  sink("lm.txt")

  df_His_R_selected <- df_His_R %>% select('Year', 'ChangeRate_Energy_Intensity')
  (summary(lm(ChangeRate_Energy_Intensity~., data=df_His_R_selected )))
  ('cor(Year, ChangeRate_Energy_Intensity)')
  cor(Year, ChangeRate_Energy_Intensity, use = "complete.obs")
  
  df_His_R_selected <- df_His_R %>% select('Year', 'Henkaryo_Carbon_Intensity')
  (summary(lm(Henkaryo_Carbon_Intensity~., data=df_His_R_selected )))
  ('cor(Year, Henkaryo_Carbon_Intensity)')
  cor(Year, Henkaryo_Carbon_Intensity, use = "complete.obs")
  
  df_His_R_selected <- df_His_R %>% select('Year', 'Henkaryo_Electricity_Rate_Total')
  (summary(lm(Henkaryo_Electricity_Rate_Total~., data=df_His_R_selected )))
  ('cor(Year, Henkaryo_Electricity_Rate_Total)')
  cor(Year, Henkaryo_Electricity_Rate_Total, use = "complete.obs")
  
  sink()
  detach(df_His_R)
  
# 相関係数 evalで書き直し ------------------------------------------------------
  
  df_His_R <- df_Graph %>% filter(SCENARIO=='Historical_R17') 
  attach(df_His_R)
  sink("lm.txt")

  test_items <- c(  'Energy_Intensity'
                  , 'ChangeRate_Energy_Intensity'
                  , 'Henkaryo_Energy_Intensity'
                  , 'Carbon_Intensity'
                  , 'ChangeRate_Carbon_Intensity'
                  , 'Henkaryo_Carbon_Intensity'
                  , 'Electricity_Rate_Total'
                  , 'ChangeRate_Electricity_Rate_Total'
                  , 'Henkaryo_Electricity_Rate_Total'
  )

  item <- 'Energy_Intensity' #  ", item, "
  eval(parse(text=paste0("df_His_R_selected <- df_His_R %>% select(Year, ", item, ")"))) 
  eval(parse(text=paste0(" (summary(lm(", item, "~., data=df_His_R_selected )))"))) 
  eval(parse(text=paste0(" ('cor(Year, ", item, ")')"))) 
  eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 
  
  item <- 'ChangeRate_Energy_Intensity' #  ", item, "
  eval(parse(text=paste0("df_His_R_selected <- df_His_R %>% select(Year, ", item, ")"))) 
  eval(parse(text=paste0(" (summary(lm(", item, "~., data=df_His_R_selected )))"))) 
  eval(parse(text=paste0(" ('cor(Year, ", item, ")')"))) 
  eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 
  
  item <- 'Henkaryo_Energy_Intensity' #  ", item, "
  eval(parse(text=paste0("df_His_R_selected <- df_His_R %>% select(Year, ", item, ")"))) 
  eval(parse(text=paste0(" (summary(lm(", item, "~., data=df_His_R_selected )))"))) 
  eval(parse(text=paste0(" ('cor(Year, ", item, ")')"))) 
  eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 
  
  item <- 'Carbon_Intensity' #  ", item, "
  eval(parse(text=paste0("df_His_R_selected <- df_His_R %>% select(Year, ", item, ")"))) 
  eval(parse(text=paste0(" (summary(lm(", item, "~., data=df_His_R_selected )))"))) 
  eval(parse(text=paste0(" ('cor(Year, ", item, ")')"))) 
  eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 

  item <- 'ChangeRate_Carbon_Intensity' #  ", item, "
  eval(parse(text=paste0("df_His_R_selected <- df_His_R %>% select(Year, ", item, ")"))) 
  eval(parse(text=paste0(" (summary(lm(", item, "~., data=df_His_R_selected )))"))) 
  eval(parse(text=paste0(" ('cor(Year, ", item, ")')"))) 
  eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 
  
  item <- 'Henkaryo_Carbon_Intensity' #  ", item, "
  eval(parse(text=paste0("df_His_R_selected <- df_His_R %>% select(Year, ", item, ")"))) 
  eval(parse(text=paste0(" (summary(lm(", item, "~., data=df_His_R_selected )))"))) 
  eval(parse(text=paste0(" ('cor(Year, ", item, ")')"))) 
  eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 
  
  item <- 'Electricity_Rate_Total' #  ", item, "
  eval(parse(text=paste0("df_His_R_selected <- df_His_R %>% select(Year, ", item, ")"))) 
  eval(parse(text=paste0(" (summary(lm(", item, "~., data=df_His_R_selected )))"))) 
  eval(parse(text=paste0(" ('cor(Year, ", item, ")')"))) 
  eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 
  
  item <- 'ChangeRate_Electricity_Rate_Total' #  ", item, "
  eval(parse(text=paste0("df_His_R_selected <- df_His_R %>% select(Year, ", item, ")"))) 
  eval(parse(text=paste0(" (summary(lm(", item, "~., data=df_His_R_selected )))"))) 
  eval(parse(text=paste0(" ('cor(Year, ", item, ")')"))) 
  eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 
  
  item <- 'Henkaryo_Electricity_Rate_Total' #  ", item, "
  eval(parse(text=paste0("df_His_R_selected <- df_His_R %>% select(Year, ", item, ")"))) 
  eval(parse(text=paste0(" (summary(lm(", item, "~., data=df_His_R_selected )))"))) 
  eval(parse(text=paste0(" ('cor(Year, ", item, ")')"))) 
  eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 
  
  sink()
  detach(df_His_R)
