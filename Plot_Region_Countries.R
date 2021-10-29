#Packages------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(patchwork)

setwd("C:/_Nishimoto/R/WBAL_R02/2_data/REF") 

Titlerow1 <- c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")
# Titlerow2 <- c('Country_name','VARIABLE')
Titlerow2 <- c('REGION','Country_name','VARIABLE')

# 単位の連想配列＞ファイル名にマッチさせる予定
df_unit <- read.delim(file="./unit.txt", header=T)
view(df_unit)
Unit_of_Var        <- df_unit$unit
names(Unit_of_Var) <- df_unit$filename
# Unit_of_Var[names(Unit_of_Var)]

# 国コードの連想配列
df_CC <- read.delim(file="./CC.txt", header=T) 
df_CC <- rename(df_CC, 'Country_name'='IEA国名')
view(df_CC)
Region_Code        <- df_CC$AIM17
names(Region_Code) <- df_CC$Country_name
# Region_Code[names(Region_Code)]

# タイトル行（ダミー行）の作成
df_past <- read_csv("./POP_IEA.csv") 
df_past <- df_past  %>% mutate(df_past, 'VARIABLE'='variable', 'REGION'='region'
) %>% rename('Country_name'='TIME') # 'TIME' OR 'X1'
num_colmuns <- ncol(df_past)
df_past <- df_past[1,c(num_colmuns,1:num_colmuns-1)] # 列の入替
View(df_past)

# 1ファイル毎に追加
files  <- list.files()    # 指定ディレクトリのファイル一覧を代入
for (file.name in files) {
  if ( regexpr('\\.csv$', file.name) < 0 ) { next } 
  d  <- read_csv(file.name)  # ファイルを仮変数に読み込む
  file.name <- gsub(".csv", "", file.name)
  d  <- rename(d, 'REGION'='TIME')  # 国コードの形式がREGIONなので
  d  <- mutate(d, 'VARIABLE'= file.name)
  
  # 国コード付与
  d  <- d   %>% mutate(AIM17=Region_Code[d$REGION]
  ) %>% rename('Country_name'='REGION'
  ) %>% rename('REGION'='AIM17'
  ) %>% na.omit()    # 国コードのない行は無視
# View(d)
  df_past <- rbind(df_past, d) %>% na.omit()
# View(df_past)
}
write_csv(df_past, "./../df_past_written.csv") # REGION Country_name VARIABLE

#  df_past <- mutate(df_past, SCENARIO="Historical"
#      ) %>% filter(Country_name != "COUNTRY"
#      ) %>% na.omit() 



# Indicatorを算出





Indicators <- c("GDP_Capita",
                "Energy_Intensity","ChangeRate_Energy_Intensity",
                "Carbon_Intensity","ChangeRate_Carbon_Intensity",
                "Electricity_Rate_Total","ChangeRate_Electricity_Rate_Total")

# Variables_for_Indicators <- c(分子, 分母) numerator / denominator

df_Variables_for_Indicator <- c(
"GDP_Capita",	"GDP_IEA",	"POP_IEA"
"Energy_Intensity",	"TES_Total",	"GDP_IEA"
"Carbon_Intensity",	"CO2_fuel_Total",	"TES_Total"
"Electricity_Rate_Total",	"TFC_Elec_Total",	"TFC_Total"
"Electricity_Rate_Ind",	"TFC_Elec_Ind",	"TFC_Ind"
"Electricity_Rate_Tra",	"TFC_Elec_Tra",	"TFC_Tra"
"Electricity_Rate_Res",	"TFC_Elec_Res",	"TFC_Res"
"Electricity_Rate_Com",	"TFC_Elec_Com",	"TFC_Com")


