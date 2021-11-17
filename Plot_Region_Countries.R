#Packages------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(patchwork)

setwd("C:/_Nishimoto/R/WBAL_R02/2_data/REF") 

if (0) {
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

# タイトル行（ダミー）の作成
df_past <- read_csv("./POP_IEA.csv") 
df_past <- df_past  %>% mutate('REGION'='region', 'VARIABLE'='variable'
                  ) %>% rename('Country'='TIME') # 'TIME' OR 'X1'
df_past <- df_past[1,c(ncol(df_past),ncol(df_past)-1,1:(ncol(df_past)-2))] # 列の入替
# View(df_past)

# 1ファイル毎に追加
files  <- list.files()    # 指定ディレクトリのファイル一覧を代入
for (file.name in files) {
  if ( regexpr('\\.csv$', file.name) < 0 ) { next } 
  d  <- read_csv(file.name)  # ファイルを仮変数に読み込む
  file.name <- gsub(".csv", "", file.name)
  d  <- d %>% rename('REGION'='TIME'
        ) %>% mutate('VARIABLE'=file.name)
  
  # 国コード付与
  d  <- d   %>% mutate(AIM17=Region_Code[d$REGION]
          ) %>% rename('Country'='REGION'
          ) %>% rename('REGION'='AIM17'
          ) %>% drop_na('REGION')  # 国コードのない行は無視
#         ) %>% na.omit()         # 空行は無視
  d <- d %>% mutate(Country = str_replace_all(Country, pattern = c("Memo.: "= "", "Memo: "= "")))
#  d <- d[1,c(ncol(d),1:(ncol(d)-1))] # 列の入替
  View(d)
  df_past <- rbind(df_past, d)
}
View(df_past)

Titlerow1 <- c('MODEL','SCENARIO','REGION','VARIABLE','UNIT')
Titlerow2 <- c('REGION','Country','VARIABLE')
Titlerow3 <- c('SCENARIO','Country')

scenarioname <- 'Baseline'

# df_past を5年置きにする 
# names_df_past <- names(df_past)   
# names_df_past <- names_df_past[-which(names_df_past %in% all_of(Titlerow2))]
# Year_all <- as.numeric(names_df_past)
# 列名から5年置きの年を取得＞先送り＞直接入力（仮）
Year5 <- c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015) %>% as.character()
df_past <- df_past %>% select(all_of(Titlerow2), all_of(Year5))
View(df_past)

df_past <- df_past %>% mutate(SCENARIO='Historical')   # 書式を揃える
View(df_past)
write_csv(df_past, "./../df_past_written.csv") # VARIABLE REGION Country 

# if (0) {  # 将来シナリオの読込
  df_future <- read_csv("C:/_Nishimoto/R/WBAL_R02/2_data/REF2/IAMCTemplate.csv")
  # View(df_future)
  
  df_future <- df_future %>% select(-c('MODEL','UNIT')
                       ) %>% filter(SCENARIO == 'Baseline'
                       ) %>% filter(!REGION %in% c('ASIA2', 'World')
                       ) %>% mutate(Country = REGION) # 書式を揃える #シナリオ名
  df_future <- df_future %>% mutate(VARIABLE = str_replace_all(VARIABLE, pattern = c(
    "GDP.MER" = "GDP_IEA", 
    "Population" = "POP_IEA", 
    "Primary Energy" = "TES_Total", 
    "Emissions.CO2.Energy" = "CO2_fuel_Total", 
    "Final Energy.Electricity" = "TFC_Elec_Total", 
    "Final Energy.Industry.Electricity" = "TFC_Elec_Ind", 
    "Final Energy.Transportation.Electricity" = "TFC_Total_Tra", 
    "Final Energy.Residential.Electricity" = "TFC_Elec_Res", 
    "Final Energy.Commercial.Electricity" = "TFC_Elec_Com", 
    "Final Energy" = "TFC_Total_Total" )))
  View(df_future)
  write_csv(df_future, "./../df_future_written.csv") 
# }  # 将来シナリオの読込

if (0) {  # 過去/将来の変数名
  df_vni_past <- matrix(c(
    'GDP_Capita',	'GDP_IEA', 'POP_IEA', 
    'Energy_Intensity',	'TES_Total', 'GDP_IEA', 
    'Carbon_Intensity',	'CO2_fuel_Total', 'TES_Total', 
    'Electricity_Rate_Total',	'TFC_Elec_Total', 'TFC_Total_Total', 
    'Electricity_Rate_Ind',	'TFC_Elec_Ind',	'TFC_Total_Ind', 
    'Electricity_Rate_Tra',	'TFC_Elec_Tra',	'TFC_Total_Tra', 
    'Electricity_Rate_Res',	'TFC_Elec_Res',	'TFC_Total_Res', 
    'Electricity_Rate_Com',	'TFC_Elec_Com',	'TFC_Total_Com'), 
    ncol=8, nrow=3)
  
  df_vni_future <- matrix(c(
    'GDP_Capita',	"GDP|MER", 'Population', 
    'Energy_Intensity',	'Primary Energy', "GDP|MER", 
    'Carbon_Intensity',	'Emissions|CO2|Energy',	'Primary Energy', 
    'Electricity_Rate_Total',	'Final Energy|Electricity', 'Final Energy', 
    'Electricity_Rate_Ind',	'Final Energy|Industry|Electricity', 'Final Energy|Industry', 
    'Electricity_Rate_Tra',	'Final Energy|Transportation|Electricity', 'Final Energy|Transportation', 
    'Electricity_Rate_Res',	'Final Energy|Residential|Electricity',	'Final Energy|Residential', 
    'Electricity_Rate_Com',	'Final Energy|Commercial|Electricity', 'Final Energy|Commercial'), 
    ncol=8, nrow=3)
}  # 過去/将来の変数名


# 過去と将来を結合する
df_long_past <- gather(df_past, key="Year", value="Value", -all_of(Titlerow2),-SCENARIO)
View(df_long_past)
df_long_future <- gather(df_future, key="Year", value="Value", -all_of(Titlerow2),-SCENARIO)
View(df_long_future)

df_long <- rbind(gather(df_past, key="Year", value="Value", -all_of(Titlerow2),-SCENARIO),
                gather(df_future, key="Year", value="Value", -all_of(Titlerow2),-SCENARIO))
df_long$Year  <- as.numeric(df_long$Year) 
df_long$Value <- as.numeric(df_long$Value)   # NA warning ＞ 確認済 
df_long <- df_long %>% na.omit()
View(df_long)
write_csv(df_long, "./../df_long_written.csv")  

# 指標の処理
# Variable_Names_for_Indicators df_vni <- indicator, numerator, denominator
df_vni <- matrix(c(
  'GDP_Capita',	'GDP_IEA', 'POP_IEA', 
  'Energy_Intensity',	'TES_Total', 'GDP_IEA', 
  'Carbon_Intensity',	'CO2_fuel_Total', 'TES_Total', 
  'Electricity_Rate_Total',	'TFC_Elec_Total', 'TFC_Total_Total', 
  'Electricity_Rate_Ind',	'TFC_Elec_Ind',	'TFC_Total_Ind', 
  'Electricity_Rate_Tra',	'TFC_Elec_Tra',	'TFC_Total_Tra', 
  'Electricity_Rate_Res',	'TFC_Elec_Res',	'TFC_Total_Res', 
  'Electricity_Rate_Com',	'TFC_Elec_Com',	'TFC_Total_Com'), 
  ncol=8, nrow=3)

if (0) { # タイトル列の作成
  df_Graph <- df_long %>% select(c('Country','Year')
                 ) %>% arrange(Year, Country
                 ) %>% distinct() 
}  # タイトル列の作成

# 指標毎の処理
for (i in 1:8) {
  
  indicator   <- df_vni[1,i]
  numerator   <- df_vni[2,i]
  denominator <- df_vni[3,i]

  for (variable_name in c(numerator, denominator)) {
    df_toMerge <- df_long %>% filter(VARIABLE==variable_name
                     ) %>% select(-c('VARIABLE')
                     ) %>% arrange(Year)
    df_toMerge <- eval(parse(text=paste0("rename(df_toMerge,", variable_name,"=Value)")))
    # View(df_toMerge)
    df_Graph <- full_join(df_Graph, df_toMerge)
  }
  # 指標
  df_Graph <- eval(parse(text=paste0(
              "df_Graph %>% mutate(",indicator,"=",numerator,"/",denominator,")")))

  # 指標の変化率　RatePre_Indicator=I(t)/I(t-1)  ChangeRate_Indicator=(I(t)-I(t-1))/((t)-(t-1))
  df_Graph <- eval(parse(text=paste0(
    "df_Graph %>% group_by(Country
            ) %>% arrange(Year
            ) %>% mutate(","Year","_pre=lag(","Year",", n=1) 
            ) %>% mutate(RatePre_","Year","=","Year","/","Year","_pre
            ) %>% mutate(",indicator,"_pre=lag(",indicator,", n=1) 
            ) %>% mutate(RatePre_",indicator,"=",indicator,"/",indicator,"_pre
            ) %>% mutate(ChangeRate_",indicator,"=(",
                                      indicator,"-",indicator,"_pre)/(","Year","-","Year","_pre)
            )")))

}
View(df_toMerge)
View(df_Graph)
write_csv(df_Graph, "./../df_Graph_written.csv") 

setwd("C:/_Nishimoto/R/WBAL_R02/4_output/") 
# if (0) {  # グラフ出力 
  
  # scenarionames <- c("Baseline","2C")    # c("Baseline","2C","1.5C","2.5C","WB2C")
  scenarionames <- c('Baseline')
  indicators <- c("GDP_Capita",
                  "Energy_Intensity","ChangeRate_Energy_Intensity",
                  "Carbon_Intensity","ChangeRate_Carbon_Intensity",
                  "Electricity_Rate_Total","ChangeRate_Electricity_Rate_Total",
                  "Electricity_Rate_Ind","ChangeRate_Electricity_Rate_Ind")
  
  # 出力対象のXY軸を指定する　x_names(n) vs y_names(n)のグラフが出力される
  
  x_names <- c(rep("Year",length(indicators)),
               rep("GDP_Capita",length(indicators)-1),
               rep("REGION",length(indicators)-1)
  )
  y_names <- c(indicators,
               indicators[-1],
               indicators[-1])
  y2_names <- indicators[c(3,5,7,9)]
  
  for (scenarioname in scenarionames) {
    
    # XY散布図 by 17地域
    pdf(file=paste("./",scenarioname,"_XY.pdf", sep=""))    
    for (num in 1:length(x_names)) {
      
      g <- eval(parse(text=paste0(
        "ggplot(df_Graph, aes(x=",x_names[num],",y=",y_names[num], 
        ",color=REGION,shape=SCENARIO)) +
  #       geom_line() +
          geom_point() + 
          scale_shape_manual(values=c(19,21))")))
      plot(g)
      filename <- paste(scenarioname,num,"_",x_names[num],"-",y_names[num], sep="")
      ggsave(file=paste("./png/",filename,".png"))
    }
    dev.off() 
    
    # XY散布図 by 国別
    pdf(file=paste("./",scenarioname,"_XY_Country.pdf", sep=""))    
    for (num in 1:length(x_names)) {
      
      g <- eval(parse(text=paste0(
        "ggplot(df_Graph, aes(x=",x_names[num],",y=",y_names[num], 
        ",color=Country,shape=SCENARIO)) +
          geom_line() +
          geom_point() +
          theme(legend.position='none') +
          scale_shape_manual(values=c(19,21))")))
      plot(g)
      filename <- paste(scenarioname,"_",num,"_",x_names[num],"-",y_names[num],"_CN", sep="")
      ggsave(file=paste("./png/",filename,".png"))
    }
    dev.off() 
    
    # 箱ヒゲ図
    pdf(file=paste("./",scenarioname,"_boxplot.pdf", sep=""))    
    for (indicator in indicators) {
      
      g <- eval(parse(text=paste0(
        "ggplot(df_Graph, aes(x=","REGION",",y=",indicator, 
        ",color=SCENARIO)) +
          geom_boxplot() +
          geom_jitter(shape=20, position=position_dodge(0.8))")))
      plot(g)
      filename <- paste(scenarioname,"_","boxplot_",indicator, sep="")
      ggsave(file=paste("./png/",filename,".png", sep=""), width=6, height=4, dpi=100)
    }
    dev.off() 
    
    # 頻度分布
    pdf(file=paste("./",scenarioname,"_histogram.pdf", sep=""))    
    for (indicator in indicators) {
      
      g <- eval(parse(text=paste0(
        "ggplot(df_Graph, aes(x=",indicator, 
        ",color=SCENARIO)) +
          geom_histogram(bins=50) +
          ylab('Count of Region-Year')")))
      plot(g)
      filename <- paste(scenarioname,"_","histogram_",indicator, sep="")
      ggsave(file=paste("./png/",filename,".png", sep=""), width=6, height=4, dpi=100)
    }
    dev.off() 
    
    # 確率密度分布
    pdf(file=paste("./",scenarioname,"_density.pdf", sep=""))    
    for (indicator in indicators) {
      
      g <- eval(parse(text=paste0(
        "ggplot(df_Graph, aes(x=",indicator, 
        ",color=SCENARIO)) +
          geom_density() +
          ylab('Density (Count scaled to 1) of Region-Year')")))
      plot(g)
      filename <- paste(scenarioname,"_","density_",indicator, sep="")
      ggsave(file=paste("./png/",filename,".png", sep=""), width=6, height=4, dpi=100)
    }
    dev.off() 
    
  }
# } # グラフ出力
