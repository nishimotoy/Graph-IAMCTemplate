# 相関係数 ------------------------------------------------------

  test_items <- c(    'Energy_Intensity'
                      , 'ChangeRate_Energy_Intensity'
                      , 'Henkaryo_Energy_Intensity'
                      , 'Carbon_Intensity'
                      , 'ChangeRate_Carbon_Intensity'
                      , 'Henkaryo_Carbon_Intensity'
                      , 'Electricity_Rate_Total'
                      , 'ChangeRate_Electricity_Rate_Total'
                      , 'Henkaryo_Electricity_Rate_Total'
  )

  cat(" START global cor(Year, item) \n\n", file="cor.txt", append=F)
  df_His_R <- df_Graph %>% filter(SCENARIO=='Historical_R17') 
  attach(df_His_R)

  # Global
  # item <- 'ChangeRate_Energy_Intensity' #  ", item, "
  for (item in test_items) {
    df_His_R_item <- eval(parse(text=paste0("df_His_R %>% select(Year, ", item, ")"))) 
    cor_year_item <- eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 
    cat(" item=", item, "\n", 
        "cor_year_item=", cor_year_item, "\n\n", 
        file="cor.txt", append=TRUE)
  }
  detach(df_His_R)
  cat(" END \n\n", file="cor.txt", append=TRUE)
  
  # Regional
  cat(" START regional cor(Year, item) \n\n", file="cor.txt", append=TRUE)
  for (item in test_items) {
    # item <- 'Energy_Intensity' #
    for (region in region_order) {
      # region <- 'JPN' #  
      df_His_R_item_region <- eval(parse(text=paste0("
            df_His_R %>% filter(REGION=='", region, "') %>% select(Year, ", item, ")
        "))) 
      attach(df_His_R_item_region)
      cor_year_item <- eval(parse(text=paste0(" cor(Year, ", item, ", use='complete.obs')"))) 
      cat(" item=", item, " region=", region, "\n", 
          "cor_year_item_region=", cor_year_item, "\n\n", 
          file="cor.txt", append=TRUE)
      eval(parse(text=paste0("
        plot(", item, " ~ Year, main='", region," cor=", cor_year_item,"')
        abline(lm(", item, " ~ Year), col='red')
          "))) 
      detach(df_His_R_item_region)
    } # region
  } # item
  cat(" END \n\n", file="cor.txt", append=TRUE)
