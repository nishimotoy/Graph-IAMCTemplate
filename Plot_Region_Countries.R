#Packages------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(patchwork)

setwd("C:/_Nishimoto/R/WBAL_R02/2_data/REF") 

Titlerow1 <- c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")
Titlerow2 <- c('REGION','Country_Name','VARIABLE')
Titlerow3 <- c('YEAR','Value')

# 単位の連想配列＞ファイル名にマッチさせる予定
df_unit <- read.delim(file="./unit.txt", header=T)
view(df_unit)
Unit_of_Var        <- df_unit$unit
names(Unit_of_Var) <- df_unit$filename
# Unit_of_Var[names(Unit_of_Var)]

# 国コードの連想配列
df_CC <- read.delim(file="./CC.txt", header=T) 
df_CC <- rename(df_CC, 'Country_Name'='IEA国名')
view(df_CC)
Region_Code        <- df_CC$AIM17
names(Region_Code) <- df_CC$Country_Name
# Region_Code[names(Region_Code)]

# タイトル行（ダミー行）の作成
df_past <- read_csv("./POP_IEA.csv") 
df_past <- df_past  %>% mutate('REGION'='region', 'VARIABLE'='variable'
                  ) %>% rename('Country_Name'='TIME') # 'TIME' OR 'X1'
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
          ) %>% rename('Country_Name'='REGION'
          ) %>% rename('REGION'='AIM17'
          ) %>% na.omit()    # 国コードのない行は無視
# View(d)
  df_past <- rbind(df_past, d) %>% na.omit()
}

# df_past$'1960' <- df_past$'1960' %>% as.numeric()
View(df_past)
write_csv(df_past, "./../df_past_written.csv") # VARIABLE REGION Country_Name 

# Indicatorを算出

Indicators <- c("GDP_Capita",
                "Energy_Intensity","ChangeRate_Energy_Intensity",
                "Carbon_Intensity","ChangeRate_Carbon_Intensity",
                "Electricity_Rate_Total","ChangeRate_Electricity_Rate_Total")

# Variables_for_Indicators <- c(分子, 分母) numerator / denominator

# Variable_Names_for_Indicators  df_vni
df_vni <- matrix(c(
    "GDP_Capita",	"GDP_IEA", "POP_IEA", 
    "Energy_Intensity",	"TES_Total", "GDP_IEA", 
    "Carbon_Intensity",	"CO2_fuel_Total",	"TES_Total", 
    "Electricity_Rate_Total",	"TFC_Elec_Total",	"TFC_Total", 
    "Electricity_Rate_Ind",	"TFC_Elec_Ind",	"TFC_Ind", 
    "Electricity_Rate_Tra",	"TFC_Elec_Tra",	"TFC_Tra", 
    "Electricity_Rate_Res",	"TFC_Elec_Res",	"TFC_Res", 
    "Electricity_Rate_Com",	"TFC_Elec_Com",	"TFC_Com"), 
    ncol=3, nrow=8)

# 縦型にして、as.numeric()する
tmp3 <- rbind(gather(filter(df_past, VARIABLE==df_vni[2,1]), key="Year", value="Value", -all_of(Titlerow2)), 
              gather(filter(df_past, VARIABLE==df_vni[3,1]), key="Year", value="Value", -all_of(Titlerow2)))
tmp3$Year  <- as.numeric(tmp3$Year) 
tmp3$Value <- as.numeric(tmp3$Value)   # NA warning ＞ 確認済 
tmp3 <- tmp3 %>% na.omit()
View(tmp3)

# 横型に戻す
# tmp4 <- spread(tmp3, key='Year', value='Value')
# View(tmp4)

# 縦型で列がVARIABLE
# タイトル列の作成
df_Graph <- tmp3 %>% select(c('Country_Name','Year')
               ) %>% distinct() 
# View(df_Graph)

i <- 2
i
for (indicator in c(df_vni[2,i], df_vni[3,i])) {
  df_toMerge <- tmp3 %>% filter(VARIABLE==indicator
                   ) %>% select(-c('VARIABLE')
                   ) %>% arrange(Year)
  df_toMerge <- eval(parse(text=paste0("rename(df_toMerge,", indicator, "=Value)")))
  df_Graph <- merge(df_Graph, df_toMerge)
}

df_Graph <- eval(parse(text=paste0("mutate(df_Graph, NewValue=",df_vni[2,i]," / ",df_vni[3,i],
                             ") %>% rename(",df_vni[1,i],"=NewValue)")))
View(df_Graph)





