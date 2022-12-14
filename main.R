# Packages ------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(ggpmisc)

root <- 'C:/_Nishimoto/R/WBAL_R02/'
Titlerow1 <- c('MODEL','SCENARIO','REGION','VARIABLE','UNIT')
Titlerow2 <- c('REGION','Country','VARIABLE','SCENARIO')
Titlerow3 <- c('SCENARIO','Country','REGION','Year')
scenarionames_order <- c('Historical','Historical_R17','Baseline','2.5C','2C','1.5C','WB2C') # ,'AnnexB'
BaseYear <- 2010  # %>% as.numeric()  # 基準年値
Sample_Country <- c('Former Soviet Union','Former Yugoslavia','South Sudan','Bosnia and Herzegovina')  # GDP(2010)が無い国
Interpolate_NA <- 'fill'   # 'fill_latest_or_first_existing_value'
Year5 <- c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015,
           2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065,
           2070, 2075, 2080, 2085, 2090, 2095, 2100) # %>% as.character() 
# Year==0 as Base-Year

df_vni <- matrix(c(  # 指標の定義 指標, 分子, 分母
  'GDP_Capita',    'GDP_IEA', 'POP_IEA', 
  'Energy_Intensity',    'TES_Total', 'GDP_IEA', 
  'Carbon_Intensity',    'CO2_fuel_Total', 'TES_Total', 
  'Electricity_Rate_Total',    'TFC_Elec_Total', 'TFC_Total_Total', 
  'Electricity_Rate_Ind',    'TFC_Elec_Ind',    'TFC_Total_Ind', 
  'Electricity_Rate_Tra',    'TFC_Elec_Tra',    'TFC_Total_Tra', 
  'Electricity_Rate_Res',    'TFC_Elec_Res',    'TFC_Total_Res', 
  'Electricity_Rate_Com',    'TFC_Elec_Com',    'TFC_Total_Com'), 
  ncol=8, nrow=3) # Variable_Names_for_Indicators df_vni <- indicator, numerator, denominator

indicators <- c(  # 出力対象の指標
  'Energy_Intensity_scaled','Carbon_Intensity_scaled','Electricity_Rate_Total_scaled'
  ,'Electricity_Rate_Ind_scaled','Electricity_Rate_Tra_scaled'
  ,'Electricity_Rate_Res_scaled','Electricity_Rate_Com_scaled'
  ,'Henkaryo_Energy_Intensity','Henkaryo_Carbon_Intensity','Henkaryo_Electricity_Rate_Total'
  ,'Henkaryo_Electricity_Rate_Ind','Henkaryo_Electricity_Rate_Tra'
  ,'Henkaryo_Electricity_Rate_Res','Henkaryo_Electricity_Rate_Com'
# ,'HenkaryoBY_Energy_Intensity','HenkaryoBY_Carbon_Intensity','HenkaryoBY_Electricity_Rate_Total'
# ,'HenkaryoBY_Electricity_Rate_Ind','HenkaryoBY_Electricity_Rate_Tra'
# ,'HenkaryoBY_Electricity_Rate_Res','HenkaryoBY_Electricity_Rate_Com'
  ,'ChangeRate_Energy_Intensity','ChangeRate_Carbon_Intensity','ChangeRate_Electricity_Rate_Total'
  ,'ChangeRate_Electricity_Rate_Ind','ChangeRate_Electricity_Rate_Tra'
  ,'ChangeRate_Electricity_Rate_Res','ChangeRate_Electricity_Rate_Com'
)

# Past ------------------------------------------------------
setwd(paste(root,"2_data/REF", sep="")) 

while (0) {
  # 単位の連想配列＞ファイル名にマッチさせる予定
  df_unit <- read.delim(file="./unit.txt", header=T)
  # view(df_unit)
  Unit_of_Var        <- df_unit$unit
  names(Unit_of_Var) <- df_unit$filename
  # Unit_of_Var[names(Unit_of_Var)]
}

# 国コードの連想配列
df_CC <- read.delim(file="./CC.txt", header=T) 
df_CC <- rename(df_CC, 'Country'='IEA国名')
# view(df_CC)
Region_Code        <- df_CC$AIM17
names(Region_Code) <- df_CC$Country
# Region_Code[names(Region_Code)]
B_flag <- as.character(df_CC$AnnexB)
names(B_flag) <- df_CC$Country
# B[names(B)]
# Usage : B[['Japan']] > 1

# タイトル行（ダミー）の作成
df_past <- read_csv("./POP_IEA.csv") 
df_past <- df_past  %>% mutate('REGION'='region', 'VARIABLE'='variable'
                  ) %>% rename('Country'='TIME') # 'TIME' OR 'X1'
df_past <- df_past[1,c(ncol(df_past),ncol(df_past)-1,1:(ncol(df_past)-2))] # 列の入替
# View(df_past)

# 1ファイル毎に追加
files  <- list.files()    # 指定ディレクトリのファイル一覧を代入
for (file_name in files) {
  if ( regexpr('\\.csv$', file_name) < 0 ) { next } 
  d  <- read_csv(file_name)  # ファイルを仮変数に読み込む
  file_name <- gsub(".csv", "", file_name)
  d  <- d %>% rename(REGION=TIME
        ) %>% mutate(VARIABLE=file_name)
  
  # 国コード付与
  d  <- d   %>% mutate(AIM17=Region_Code[d$REGION]
          ) %>% rename('Country'='REGION'
          ) %>% rename('REGION'='AIM17'
          ) %>% drop_na('REGION')  # 国コードのない行は無視
  #  d <- d[1,c(ncol(d),1:(ncol(d)-1))] # 列の入替
  df_past <- rbind(df_past, d)
}
setwd(paste(root,"4_output/", sep="")) 
unlink("./png", recursive=T)
dir.create("./png")
dir.create("./png/ylim")
dir.create("./png/yall")
dir.create("./png/ylim_compare")

df_past <- df_past %>% filter(REGION!='region')  # ダミー行のデータを削除
df_past <- df_past %>% mutate(SCENARIO='Historical')   # 書式を揃える
df_past <- df_past %>% mutate(Country = str_replace_all(Country, 
                       pattern = c("Memo.: "="", "Memo: "="", " .if no detail."="")))
write_csv(df_past, "./df_past_written_everyYear.csv") # VARIABLE REGION Country 

df_past_long <- df_past %>% gather(key='Year', value='Value', -all_of(Titlerow2))
df_past_long$Year  <- as.numeric(df_past_long$Year) 
df_past_long$Value <- as.numeric(df_past_long$Value)   # NA warning ＞ 確認済 

# Fill past ------------------------------------------------------
# 基準年データがない国の処理 for (dummyloop in 1) { 

df_past_long <- df_past_long %>% group_by(VARIABLE,Country
      ) %>% arrange(VARIABLE, Country, Year                 
      ) %>% mutate(Value2=Value
      ) %>% fill(Value2, .direction='down' # 前年値を優先
      ) %>% fill(Value2, .direction='up'   # 前年値がなければ後年値
      ) %>% mutate(SCENARIO2=if_else(is.na(Value), Interpolate_NA, SCENARIO)
      ) %>% ungroup()
  
df_past_BaseYear <- df_past_long %>% filter(Year==BaseYear
                               ) %>% mutate(Year=0)

while (0) { # PDF出力
  pdf(file=paste("./","past_filled.pdf", sep=""))   # PDF出力開始
  for (y_name in unique(df_past_long$VARIABLE)) { # 補完値の確認出力 by サンプル国
    df_Graph_past <- df_past_long %>% filter(Country %in% Sample_Country
                                ) %>% filter(VARIABLE==y_name)
    g <- ggplot(df_Graph_past, aes(x=Year,y=Value2, 
                                   color=Country, shape=SCENARIO2)) +
      geom_point() +
      # theme(legend.position='none') +
      ylab(y_name) +
      scale_shape_manual(values=c(24,19))
    plot(g)
    filename <- paste("Interpolated_",y_name,"_",Interpolate_NA, sep="")
    ## ggsave(file=paste("./png/",filename,".png", sep=""))
  }  
  dev.off() # PDF出力終了
} # PDF出力

# 基準年データがない国の処理 } 

write_csv(df_past_long, "./df_past_long_written_everyYear.csv") # VARIABLE REGION Country 
write_csv(df_past, "./df_past_written_everyYear.csv") # VARIABLE REGION Country 

# Future ------------------------------------------------------
# while (0) {  # 将来シナリオの読込
df_future <- read_csv(paste(root,"2_data/REF2/","IAMCTemplate.csv", sep=""))
# View(df_future)

df_future <- df_future %>% select(-c('MODEL','UNIT')
# ) %>% filter(SCENARIO == scenarioname # rbind前にシナリオを絞る場合
) %>% filter(!REGION %in% c('ASIA2', 'World')
) %>% mutate(Country = REGION)   # 書式を揃える # 国名=地域名

df_future <- df_future %>% mutate('0'=as.numeric(df_future$'2010')) # 基準年値を列コピーしておく

# IAMCTemplete(future) の名前を IEA(past) に揃える＠ '|'対策  # recode でも出来るらしい
df_future <- df_future %>% mutate(VARIABLE = str_replace_all(VARIABLE, pattern = c(
  'GDP.MER' = 'GDP_IEA',
  'Population' = 'POP_IEA',
  'Primary Energy' = 'TES_Total',
  'Emissions.CO2.Energy' = 'CO2_fuel_Total',
  'Final Energy.Industry.Electricity' = 'TFC_Elec_Ind',
  'Final Energy.Transportation.Electricity' = 'TFC_Elec_Tra',
  'Final Energy.Residential.Electricity' = 'TFC_Elec_Res',
  'Final Energy.Commercial.Electricity' = 'TFC_Elec_Com',
  'Final Energy.Electricity' = 'TFC_Elec_Total',
  'Final Energy.Industry' = 'TFC_Total_Ind',
  'Final Energy.Transportation' = 'TFC_Total_Tra',
  'Final Energy.Residential' = 'TFC_Total_Res',
  'Final Energy.Commercial' = 'TFC_Total_Com',
  'Final Energy' = 'TFC_Total_Total')))  # 順番注意／.最長マッチ
# View(df_future)
write_csv(df_future, "./df_future_pre.csv") 
# ここで、上記以外のデータは捨てる
df_future <- df_future %>% filter(VARIABLE %in% 
              c('GDP_IEA','POP_IEA','TES_Total','CO2_fuel_Total',
                'TFC_Total_Total','TFC_Total_Ind','TFC_Total_Tra','TFC_Total_Res','TFC_Total_Com',
                'TFC_Elec_Total','TFC_Elec_Ind','TFC_Elec_Tra','TFC_Elec_Res','TFC_Elec_Com'))
write_csv(df_future, "./df_future.csv") 
# }  # 将来シナリオの読込

# Connect Past & Future ------------------------------------------------------
df_long_past <- df_past_long %>% rbind(df_past_BaseYear) 
df_long_past <- df_long_past %>% select(-Value2, -SCENARIO2
                      ) %>% filter(Year %in% c(Year5, 0))   # 各年から5年置きに
df_long_future <- gather(df_future, key=Year, value=Value, -all_of(Titlerow2))
df_long_future$Year  <- as.numeric(df_long_future$Year) 
df_long <- rbind(df_long_past, df_long_future)
df_long$Year  <- as.numeric(df_long$Year) 
df_long$Value <- as.numeric(df_long$Value)   # NA warning ＞ 確認済 
write_csv(df_long, "./df_long.csv")  

# Aggregation to Region ------------------------------------------------------
for (dummyloop in 1) {  # 地域集約 # while (0)
  df_long_agg <- aggregate(Value~VARIABLE+REGION+SCENARIO+Year, df_long, sum) # 集約対象=Country
  df_long_agg <- df_long_agg %>% mutate(Country=REGION)
  # df_long <- df_long_agg  # 過去17地域のみを出力する場合
  for (dummyloop in 1) {  # 過去17地域と過去約2百数ヶ国を併記する場合
    df_long_agg <- df_long_agg %>% filter(SCENARIO=='Historical'
                             ) %>% mutate(SCENARIO='Historical_R17')
    df_long <- df_long %>% rbind(df_long_agg)
  } # 併記する場合
}  # 地域集約
write_csv(df_long, "./df_long_after_R17.csv") 

# Historical/Future基準年値 (Year=0) から調整用の値 (Year=1) を作る
df_long_BaseYear <- df_long %>% filter(Year==0) %>% mutate(Year=1)  # 基準年値をコピーして調整年値を作る
df_long_BaseYear_1 <- df_long_BaseYear %>% filter(SCENARIO=='Historical') 
df_long_BaseYear_2 <- df_long_BaseYear %>% filter(SCENARIO=='Historical_R17') 
df_long_BaseYear_3 <- df_long_BaseYear %>% filter(SCENARIO!='Historical', SCENARIO!='Historical_R17') 
df_long_BaseYear_4 <- full_join(select(df_long_BaseYear_3, -Value), select(df_long_BaseYear_2, -SCENARIO)) 
df_long <- df_long %>% rbind(df_long_BaseYear_1) %>% rbind(df_long_BaseYear_2) %>% rbind(df_long_BaseYear_4)
write_csv(df_long, "./df_long_after_add1.csv") 

# Global path 算出用
df_long_global <- aggregate(Value~VARIABLE+SCENARIO+Year, df_long, sum) # 集約対象=REGION
write_csv(df_long_global, "./df_long_global.csv") 


# Table format and Indicator  ------------------------------------------------------
# 指標の処理  

for (dummyloop in 1) {  # 国名のみのダミー列の作成
  df_Graph <- df_long %>% select(c('Country')) %>% arrange(Country) %>% distinct() 
}  # ダミー列の作成

for (i in 1:ncol(df_vni)) { # 指標毎の処理1   # テスト後に戻す (i in 1:ncol(df_vni))
  indicator   <- df_vni[1,i]
  numerator   <- df_vni[2,i]
  denominator <- df_vni[3,i]

  for (variable_name in c(numerator, denominator)) {
    df_toJoin <- df_long %>% filter(VARIABLE==variable_name
                     ) %>% select(-c('VARIABLE')
                     ) %>% arrange(Year)
    df_toJoin <- eval(parse(text=paste0("df_toJoin %>% rename(",variable_name,"=Value)")))
    # View(df_toJoin)
    df_Graph <- df_Graph %>% full_join(df_toJoin)
  }
  df_Graph <- df_Graph %>% drop_na('REGION','Year')  # ダミー列のデータを削除
  df_Graph <- eval(parse(text=paste0(
              "df_Graph %>% mutate(",indicator,"=",numerator,"/",denominator,")"))) # 指標の算出
  write_csv(df_Graph, "./df_Graph_afterfulljoin.csv") 
  
  for (dummyloop in 1) { # 基準年値で調整した値をdf_Graphに追加する
    
    df_Graph <- df_Graph %>% group_by(SCENARIO,Country) %>% arrange(SCENARIO,Country,Year)
    df_Graph<- eval(parse(text=paste0(
      "df_Graph %>% mutate(",numerator,"_scaled=",numerator,"/",numerator,"[Year==0]*",numerator,"[Year==1]
              ) %>% mutate(",denominator,"_scaled=",denominator,"/",denominator,"[Year==0]*",denominator,"[Year==1]
              ) %>% mutate(",indicator,"_scaled=",numerator,"_scaled/",denominator,"_scaled
              )")))                      # _scaled=基準年の Historical に合わせる
    while (0) {  # 基準年値=1 とする場合（このloopを活かすと直前の上書き）
      df_Graph<- eval(parse(text=paste0(
        "df_Graph %>% mutate(",indicator,"_scaled=",indicator,"/",indicator,"[Year==0]
                ) %>% mutate(",numerator,"_scaled=",numerator,"/",numerator,"[Year==0]
                ) %>% mutate(",denominator,"_scaled=",denominator,"/",denominator,"[Year==0])"
      )))                     # indicator_scaled = I(t,F/H)/I(t=BaseYear,F/H)
    }
    df_Graph <- df_Graph %>% ungroup()
    
  } # 基準年値で調整した値をdf_Graphに追加する
  
} # 指標毎の処理1
df_Graph$SCENARIO <- factor(df_Graph$SCENARIO, levels=scenarionames_order)
#        levels=c('Historical','Historical_R17','Baseline','2.5C','2C','1.5C','WB2C'))
df_Graph <- df_Graph %>% group_by(SCENARIO,Country) %>% arrange(SCENARIO,Country,Year)
write_csv(df_Graph, "./df_Graph_afterfulljoin.csv") 
df_Graph <- df_Graph %>% filter(Year!=0 & Year!=1) 

# Change rate ------------------------------------------------------
K_value <- c(0, 0, -1, 1, 1, 1, 1, 1)  # df_vni の行に対応
# -200(Carbon_Intensity)  -2(Carbon_Intensity_scaled) 
df_Graph <- df_Graph  %>% mutate(Carbon_Intensity_saved=Carbon_Intensity 
                    ) %>% mutate(Carbon_Intensity=Carbon_Intensity_scaled) # for K-type 

for (i in 1:ncol(df_vni)) { # 指標毎の処理2   # テスト後に戻す (i in 1:ncol(df_vni))

  indicator   <- df_vni[1,i]
  K <- K_value[i]
  
  # 指標の変化率（t年比）　
  df_Graph <- eval(parse(text=paste0(
        "df_Graph %>% mutate(ChangeRate_",indicator,
        "=if_else(lag(",indicator,", n=1)==", K, ", NA_real_, 
          ((",indicator,"-lag(",indicator,",n=1))
              /abs(lag(",indicator,", n=1)-", K, ") # Ｋタイプ変化率
 #            /abs(lag(",indicator,",n=1)) # 旧タイプ変化率
              /(Year-lag(Year, n=1)))
                  ))")))
 
 #  指標の変化率  過去の試み
 #  "=(",indicator,"/lag(",indicator,",n=1)-1)  t-1 期で割る
 #  "=(",indicator,"-lag(",indicator,",n=1))  共通
 #    /lag(",indicator,",n=1)       t-1 期で割る
 #    /(",indicator,"+lag(",indicator,",n=1))*2   t期と(t-1)期の平均
 #    /(sqrt(",indicator,"^2)+sqrt(lag(",indicator,",n=1)^2))*2　ABS1
 #    /(abs(",indicator,")+abs(lag(",indicator,",n=1)))*2     ABS2
 #    /(sqrt(",indicator,"^2)+sqrt(lag(",indicator,",n=1)^2))*2 

# while (0) { # 指標の変化速度
  df_Graph <- eval(parse(text=paste0(
    "df_Graph %>%  mutate(Henkaryo_",indicator,
    "=(",indicator,"_scaled-lag(",indicator,"_scaled, n=1))
     /(Year-lag(Year, n=1)))"  
    )))  
#  } # 指標の変化速度（旧式）
} # 指標毎の処理2
df_Graph <- df_Graph  %>% mutate(Carbon_Intensity=Carbon_Intensity_saved) # for K-type 

# Henkaryo ------------------------------------------------------
while (0) { # for (i in 1:ncol(df_vni)) { # 指標毎の処理3
  indicator   <- df_vni[1,i]
  numerator   <- df_vni[2,i]
  denominator <- df_vni[3,i]
  
  # 指標の変化速度（新式） Henkaryo_I(t) = [numerator(t)-numerator(t-1)]/denominator(t-1)
  df_Graph <- eval(parse(text=paste0(
    "df_Graph %>% mutate(Henkaryo_",indicator,
    "=(",numerator,"_scaled-lag(",numerator,"_scaled ,n=1))
              /abs(lag(",denominator,"_scaled ,n=1))
              /(Year-lag(Year, n=1)) )"
    )))
  } # 指標毎の処理3

df_Graph <- df_Graph %>% ungroup() %>% arrange(SCENARIO,Country,Year)
# df_Graph <- df_Graph %>% ungroup() %>% group_by(SCENARIO,REGION) %>% arrange(SCENARIO,Country,Year)
# View(df_Graph)
write_csv(df_Graph, "./df_Graph.csv") 

# Annex-B ------------------------------------------------------
df_Graph_B <- df_Graph %>% filter(SCENARIO=='Historical')
df_Graph_B <- df_Graph_B %>% mutate('B_flag'=B_flag[df_Graph_B$Country]
                       ) %>% drop_na('B_flag'
                       ) %>% select(-'B_flag'
                       ) %>% filter(Year>=1995
                       ) %>% mutate(SCENARIO='Historical_B')
df_Graph <- df_Graph %>% rbind(df_Graph_B)
scenarionames_order[length(scenarionames_order)+1] <- c('Historical_B')
                                  
#Summary ------------------------------------------------------

# ここで置換 df_Graph のうち Inf を NA に
# df_Graph_bk <- df_Graph
df_Graph[df_Graph==Inf] <- NA
# write_csv(anti_join(df_Graph, df_Graph_bk), "./df_Graph_antijoin.csv") 

df_indicator <- df_Graph %>% select(one_of(Titlerow3),one_of(indicators)
                       ) %>% group_by(SCENARIO)
write_csv(df_indicator, "./df_indicator.csv")  # Year, Country, REGION入りのデータで保存

df_summary <- df_indicator %>% select(-c(Year, Country, REGION)
  ) %>% group_by(SCENARIO
  ) %>% summarise_at(vars(everything()),
                     funs('01_length'=length, '02_n'=length(na.omit(.)), '03_min'=min(., na.rm=T), '04_max'=max(., na.rm=T), 
                          '05_median'=median(., na.rm=T), '06_mean'=mean(., na.rm=T), '07_sd'=sd(., na.rm=T),
                          '08_q05%'=quantile(., probs=0.05, na.rm=T), 
                          '09_q95%'=quantile(., probs=0.95, na.rm=T), 
                          '10_q25%'=quantile(., probs=0.25, na.rm=T), 
                          '11_q50%'=quantile(., probs=0.50, na.rm=T), 
                          '12_q75%'=quantile(., probs=0.75, na.rm=T), 
                     )
  ) # %>% arrange(colnames(df_summary ))

sorted_names_list <- sort(names(df_summary))
df_summary <- df_summary %>% select(all_of(sorted_names_list))
                                      
df_summary_quantile <- df_summary %>% select(SCENARIO, contains('_q')) 
df_summary_ChangeRate <- df_summary %>% select(SCENARIO, starts_with("ChangeRate_")) 
write.csv(t(df_summary), "./df_summary.csv") 
write.csv(t(df_summary_ChangeRate), "./df_summary_ChangeRate.csv") 
write.csv(t(df_summary_quantile), "./df_summary_quantile.csv") 

df_indicator <- df_indicator %>% ungroup()

#Feasibility Test ------------------------------------------------------
for (dummyloop in 1) { # Feasibility Test
  # test_items <- c('ChangeRate_Energy_Intensity', 'ChangeRate_Carbon_Intensity', 'ChangeRate_Electricity_Rate_Total')
  test_items <- indicators
  future_scenarios <- levels(df_Graph$SCENARIO) #  'Historical', 'Historical_R17'
  vector_Rate_test_OK <- future_scenarios
  
  for (item in test_items) {
    # item <- 'ChangeRate_Energy_Intensity'
    df_feasibility_window <- df_indicator %>% filter(SCENARIO=='Historical_R17' # 注意直接指定
                                        ) %>% select(all_of(item),-SCENARIO
                                        ) %>% rename('Target'=item) 
    vector_feasibility_window <- as.vector(df_feasibility_window$Target) %>% na.omit()
    qua_05 <- quantile(vector_feasibility_window, probs=0.05, na.rm=T)
    qua_95 <- quantile(vector_feasibility_window, probs=0.95, na.rm=T)
    names(qua_05) <- NULL
    names(qua_95) <- NULL
    
    for (scenarioname in future_scenarios) {
      
      # scenarioname <- 'Baseline'
      df_feasibility_test <- df_indicator %>% filter(SCENARIO==scenarioname
                                        ) %>% select(all_of(item),-SCENARIO
                                        ) %>% rename('Target'=item) 
      vector_feasibility_test <- as.vector(df_feasibility_test$Target) %>% na.omit()
      # length_passed <- length(which(vector_feasibility_test<qua_05) && which(vector_feasibility_test>qua_95))
      # length_passed <- length(which(vector_feasibility_test<qua_05 && vector_feasibility_test>qua_95))
      length_total <- length(vector_feasibility_test)
      length_under05 <- length(which(vector_feasibility_test<qua_05))
      length_over95 <- length(which(vector_feasibility_test>qua_95))
      
      Rate_feasibility_test_OK <- (length_total-length_under05-length_over95)/length_total
      vector_Rate_test_OK <- append(vector_Rate_test_OK, Rate_feasibility_test_OK)
      # 指定した値を格納

    } # scenarioname
  } # item
  df_Rate_feasibility_test_OK <- data.frame(matrix(vector_Rate_test_OK, 
                                            nrow=length(future_scenarios)))
  colnames(df_Rate_feasibility_test_OK) <- append(c('SCENARIO'), test_items)
  write.csv(t(df_Rate_feasibility_test_OK), "./df_Rate_feasibility_test_OK.csv") 
  
} # Feasibility Test

# Test範囲外リスト ------------------------------------------------------
for (dummyloop in 1) { # Test範囲外リスト
  test_items <- c('ChangeRate_Energy_Intensity', 'Henkaryo_Carbon_Intensity', 'Henkaryo_Electricity_Rate_Total')
  # test_items <- indicators
  future_scenarios <- levels(df_Graph$SCENARIO)
  future_scenarios <- future_scenarios[-which(future_scenarios %in% c('Historical', 'Historical_B'))] 
  vector_Rate_test_OK <- future_scenarios
  df_test_NG <- data.frame()

  for (item in test_items) {
    # item <- 'Energy_Intensity_scaled'
    df_feasibility_window <- df_indicator %>% filter(SCENARIO=='Historical_R17' # 注意直接指定
                                        ) %>% select(all_of(item),-SCENARIO
                                        ) %>% rename('Target'=item) 
    vector_feasibility_window <- as.vector(df_feasibility_window$Target) %>% na.omit()
    qua_05 <- quantile(vector_feasibility_window, probs=0.05, na.rm=T)
    qua_95 <- quantile(vector_feasibility_window, probs=0.95, na.rm=T)
    names(qua_05) <- NULL
    names(qua_95) <- NULL
    
    for (scenarioname in future_scenarios) {
      # scenarioname <- 'Baseline'
      
      df_test_item <- df_indicator  %>% filter(SCENARIO==scenarioname
                   ) %>% select(c(Year, SCENARIO, REGION, all_of(item))
                   ) %>% rename(Value=item
                   ) %>% drop_na(Value)
      
      df_test_under <- df_test_item %>% filter(Value<qua_05
                   ) %>% mutate('item'=paste(item, '_under05_',qua_05))
      df_test_over <- df_test_item %>% filter(Value>qua_95
                    ) %>% mutate('item'=paste(item,  '_over95_',qua_95))
      df_test_NG <- df_test_NG %>% rbind(df_test_under, df_test_over) 
    } # scenarioname
  } # item
  write.csv(df_test_NG, "./df_test_NG.csv") 
  
} # Test範囲外リスト

source("./../3_prog/graph_paper2.r", encoding="utf-8")
