# NG 相関係数 ------------------------------------------------------
while (0) { # 相関係数 for (dummyloop in 1)
  test_items <- c('ChangeRate_Energy_Intensity'
                  , 'ChangeRate_Carbon_Intensity'
                  , 'ChangeRate_Electricity_Rate_Total'
                  , 'Henkaryo_Energy_Intensity'
                  , 'Henkaryo_Carbon_Intensity'
                  , 'Henkaryo_Electricity_Rate_Total'
  )
  
  summary_lm_items <- data.frame()
  # sink("lm.txt")
  
  (item <- test_items[1])
  df_His_R <- df_Graph %>% filter(SCENARIO=='Historical_R17'
  ) %>% select('Year', all_of(item)
  ) %>% rename('Target'=item)
  df_His_R_lm <- lm(Target~., data=df_His_R )
  (summary_df_His_R_lm <- summary( df_His_R.lm ))
  summary_lm <- broom::tidy(summary_df_His_R_lm, conf.int = TRUE) %>% cbind(item=c(item)) 
  summary_lm_items <-  rbind(summary_lm_items, summary_lm)
  
  (item <- test_items[2])
  df_His_R <- df_Graph %>% filter(SCENARIO=='Historical_R17'
  ) %>% select('Year', all_of(item)
  ) %>% rename('Target'=item)
  df_His_R_lm <- lm(Target~., data=df_His_R )
  (summary_df_His_R_lm <- summary( df_His_R.lm ))
  summary_lm <- broom::tidy(summary_df_His_R_lm, conf.int = TRUE) %>% cbind(item=c(item)) 
  summary_lm_items <-  rbind(summary_lm_items, summary_lm)
  
  (item <- test_items[3])
  df_His_R <- df_Graph %>% filter(SCENARIO=='Historical_R17'
  ) %>% select('Year', all_of(item)
  ) %>% rename('Target'=item)
  df_His_R_lm <- lm(Target~., data=df_His_R )
  (summary_df_His_R_lm <- summary( df_His_R.lm ))
  summary_lm <- broom::tidy(summary_df_His_R_lm, conf.int = TRUE) %>% cbind(item=c(item)) 
  summary_lm_items <-  rbind(summary_lm_items, summary_lm)
  
  (item <- test_items[4])
  df_His_R <- df_Graph %>% filter(SCENARIO=='Historical_R17'
  ) %>% select('Year', all_of(item)
  ) %>% rename('Target'=item)
  df_His_R_lm <- lm(Target~., data=df_His_R )
  (summary_df_His_R_lm <- summary( df_His_R.lm ))
  summary_lm <- broom::tidy(summary_df_His_R_lm, conf.int = TRUE) %>% cbind(item=c(item)) 
  summary_lm_items <-  rbind(summary_lm_items, summary_lm)
  
  (item <- test_items[5])
  df_His_R <- df_Graph %>% filter(SCENARIO=='Historical_R17'
  ) %>% select('Year', all_of(item)
  ) %>% rename('Target'=item)
  df_His_R_lm <- lm(Target~., data=df_His_R )
  (summary_df_His_R_lm <- summary( df_His_R.lm ))
  summary_lm <- broom::tidy(summary_df_His_R_lm, conf.int = TRUE) %>% cbind(item=c(item)) 
  summary_lm_items <-  rbind(summary_lm_items, summary_lm)
  
  (item <- test_items[6])
  df_His_R <- df_Graph %>% filter(SCENARIO=='Historical_R17'
  ) %>% select('Year', all_of(item)
  ) %>% rename('Target'=item)
  df_His_R_lm <- lm(Target~., data=df_His_R )
  (summary_df_His_R_lm <- summary( df_His_R.lm ))
  summary_lm <- broom::tidy(summary_df_His_R_lm, conf.int = TRUE) %>% cbind(item=c(item)) 
  summary_lm_items <-  rbind(summary_lm_items, summary_lm)
  
  # sink()
  
  # sink("lm.txt")
  df_His_R <- df_Graph %>% filter(SCENARIO=='Historical_R17') %>%
    select('Year', 'Henkaryo_Carbon_Intensity')
  # (df_His_R.lm<-lm(Henkaryo_Carbon_Intensity~., data=df_His_R ))
  # (summary( df_His_R.lm ))
  (summary(df_His_R.lm<-lm(Henkaryo_Carbon_Intensity~., data=df_His_R )))
  # sink()
  
} # 相関係数

