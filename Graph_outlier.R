#Graph output for singular case ------------------------------------------------------
while (0) { # 確認用グラフ    while (0) for (dummyloop in 1)
  
  scenarioname_for_test <- 'WB2C' # '1.5C' #   
  countryname_for_test <-  'CIS' # 'XER' #
  scaling_for_ChangeRate <-  1
  
  df_Graph_tmp <- df_Graph_plot %>% filter(SCENARIO==scenarioname_for_test, Country==countryname_for_test
  ) %>% select(Year, ChangeRate_Carbon_Intensity, ChangeRateBY_Carbon_Intensity, 
               Carbon_Intensity, Carbon_Intensity_scaled, TES_Total, CO2_fuel_Total) 
  
  df_Graph_tmp <- df_Graph_tmp %>% mutate(TES_Total_scaled=TES_Total/TES_Total[Year==2010]  
  ) %>% mutate(ChangeRate_Carbon_Intensity_scaled=ChangeRate_Carbon_Intensity/scaling_for_ChangeRate  
  ) %>% mutate(ChangeRateBY_Carbon_Intensity_scaled=ChangeRateBY_Carbon_Intensity/scaling_for_ChangeRate
  ) %>% mutate(CO2_fuel_Total_scaled=CO2_fuel_Total/CO2_fuel_Total[Year==2010])
  
  g1 <- ggplot(df_Graph_tmp, aes(Year)) +
    geom_line(aes(y = ChangeRate_Carbon_Intensity, colour = '_ChangeRate_Carbon_Intensity'),size=1)+
    geom_line(aes(y = ChangeRateBY_Carbon_Intensity, colour = '_ChangeRateBY_Carbon_Intensity'),size=1)+
    geom_line(aes(y = Carbon_Intensity, colour = 'Carbon_Intensity'),size=1) +
    geom_line(aes(y = TES_Total, colour = 'TES_Total'),size=1)+
    geom_line(aes(y = CO2_fuel_Total, colour = 'CO2_fuel_Total'),size=1) +
    ylab('Variables')+
    annotate("text",x=Inf,y=Inf,label=paste(scenarioname_for_test,countryname_for_test),hjust=1.2,vjust=2)
  plot(g1)
  
  g2 <- ggplot(df_Graph_tmp, aes(Year)) +
    geom_line(aes(y = ChangeRate_Carbon_Intensity_scaled, colour = '_ChangeRate_Carbon_Intensity'),size=1) +
    geom_line(aes(y = ChangeRateBY_Carbon_Intensity_scaled, colour = '_ChangeRateBY_Carbon_Intensity'),size=1)+
    geom_line(aes(y = Carbon_Intensity_scaled, colour = 'Carbon_Intensity_scaled'),size=1) +
    geom_line(aes(y = TES_Total_scaled, colour = 'TES_Total_scaled'),size=1) +
    geom_line(aes(y = CO2_fuel_Total_scaled, colour = 'CO2_fuel_Total_scaled'),size=1) +
    ylab('Variables_scaled (Bese-Year value = 1.0)') +
    annotate("text",x=Inf,y=Inf,label=paste(scenarioname_for_test,countryname_for_test),hjust=1.2,vjust=3)
  plot(g2) 
  # ggsave(file=paste("./test/",scenarioname_for_test,"_",countryname_for_test,"_test.png", sep=""), width=6, height=4, dpi=100)
  
  # dev.off() 
} # 確認用グラフ

