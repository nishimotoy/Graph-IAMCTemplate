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
# view(df_unit)
Unit_of_Var        <- df_unit$unit
names(Unit_of_Var) <- df_unit$filename
# Unit_of_Var[names(Unit_of_Var)]

# 国コードの連想配列
df_CC <- read.delim(file="./CC.txt", header=T) 
df_CC <- rename(df_CC, 'Country_Name'='IEA国名')
# view(df_CC)
Region_Code        <- df_CC$AIM17
names(Region_Code) <- df_CC$Country_Name
# Region_Code[names(Region_Code)]

# タイトル行（ダミー）の作成
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
View(df_past)
write_csv(df_past, "./../df_past_written.csv") # VARIABLE REGION Country_Name 

# df_past を5年置きにする 
# names_df_past <- names(df_past)   
# names_df_past <- names_df_past[-which(names_df_past %in% all_of(Titlerow2))]
# Year_all <- as.numeric(names_df_past)
# 列名から5年置きの年を取得＞先送り＞直接入力（仮）
Year5 <- c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015)
Year5 <- all_of(Year5) %>% as.character()
df_past <- df_past %>% select(all_of(Titlerow2), all_of(Year5))


# 指標の処理
# Variable_Names_for_Indicators df_vni <- indicator, numerator, denominator
df_vni <- matrix(c(
    "GDP_Capita",	"GDP_IEA", "POP_IEA", 
    "Energy_Intensity",	"TES_Total", "GDP_IEA", 
    "Carbon_Intensity",	"CO2_fuel_Total",	"TES_Total", 
    "Electricity_Rate_Total",	"TFC_Elec_Total",	"TFC_Total_Total", 
    "Electricity_Rate_Ind",	"TFC_Elec_Ind",	"TFC_Total_Ind", 
    "Electricity_Rate_Tra",	"TFC_Elec_Tra",	"TFC_Total_Tra", 
    "Electricity_Rate_Res",	"TFC_Elec_Res",	"TFC_Total_Res", 
    "Electricity_Rate_Com",	"TFC_Elec_Com",	"TFC_Total_Com"), 
    ncol=8, nrow=3)

# 縦型にして、as.numeric()する
tmp3 <- gather(df_past, key="Year", value="Value", -all_of(Titlerow2))
tmp3$Year  <- as.numeric(tmp3$Year) 
tmp3$Value <- as.numeric(tmp3$Value)   # NA warning ＞ 確認済 
tmp3 <- tmp3 %>% na.omit()
View(tmp3)

# 縦型で列がVARIABLE
# タイトル列の作成
# df_Graph <- data.frame()
df_Graph <- tmp3 %>% select(c('Country_Name','Year')
               ) %>% arrange(Year, Country_Name
               ) %>% distinct() 

for (i in 1:4) {
  
  indicator   <- df_vni[1,i]
  numerator   <- df_vni[2,i]
  denominator <- df_vni[3,i]

  for (variable_name in c(df_vni[2,i], df_vni[3,i])) {
    df_toMerge <- tmp3 %>% filter(VARIABLE==variable_name
                     ) %>% select(-c('VARIABLE')
                     ) %>% arrange(Year)
    df_toMerge <- eval(parse(text=paste0("rename(df_toMerge,", variable_name,"=Value)")))
    View(df_toMerge)
    df_Graph <- full_join(df_Graph, df_toMerge)
  }
  # 指標
  df_Graph <- eval(parse(text=paste0(
              "df_Graph %>% mutate(",df_vni[1,i],"=",df_vni[2,i],"/",df_vni[3,i],")")))

  # df_Graph <- df_Graph %>% group_by(Country_Name)
  
  # 指標の変化率　RatePre_Indicator=I(t)/I(t-1)  ChangeRate_Indicator=(I(t)-I(t-1))/((t)-(t-1))
  df_Graph <- eval(parse(text=paste0(
    "df_Graph %>% group_by(Country_Name
            ) %>% arrange(Year
            ) %>% mutate(","Year","_pre=lag(","Year",", n=1) 
            ) %>% mutate(RatePre_","Year","=","Year","/","Year","_pre
            ) %>% mutate(",df_vni[1,i],"_pre=lag(",df_vni[1,i],", n=1) 
            ) %>% mutate(RatePre_",df_vni[1,i],"=",df_vni[1,i],"/",df_vni[1,i],"_pre
            ) %>% mutate(ChangeRate_",df_vni[1,i],"=(",df_vni[1,i],"-",df_vni[1,i],"_pre)/(","Year","-","Year","_pre)
            ) %>% mutate(SCENARIO='Historical'
            )")))

  # 予定＞("Year","-","Year","_pre) がマイナスの場合は ChangeRate_Indicator=Na とする or ロジック変更
     
}
View(df_Graph)
write_csv(df_Graph, "./../df_Graph_written.csv") 


for (tmp in 0) {
#指標名とシナリオ名 で繰り返し処理＠グラフ出力 -------------------------------------------------------

setwd("C:/_Nishimoto/R/WBAL_R02/4_output/") 
# scenarionames <- c("Baseline","2C")    # c("Baseline","2C","1.5C","2.5C","WB2C")
scenarionames <- c('Historical')
indicators <- c("GDP_Capita",
                "Electricity_Rate_Total","ChangeRate_Electricity_Rate_Total",
                "Carbon_Intensity","ChangeRate_Carbon_Intensity",
                "Energy_Intensity","ChangeRate_Energy_Intensity")

# 出力対象のXY軸を指定する　x_names(n) vs y_names(n)のグラフが出力される

x_names <- c(rep("Year",length(indicators)),
             rep("GDP_Capita",length(indicators)-1),
             rep("REGION",length(indicators)-1)
)
y_names <- c(indicators,
             indicators[-1],
             indicators[-1])
y2_names <- indicators[c(3,5,7)]

for (scenarioname in scenarionames) {
  
  # XY散布図 by 軸名のテキスト指定
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
  pdf(file=paste("./",scenarioname,"_XY_CN.pdf", sep=""))    
  for (num in 1:length(x_names)) {
    
    g <- eval(parse(text=paste0(
      "ggplot(df_Graph, aes(x=",x_names[num],",y=",y_names[num], 
      ",color=Country_Name,shape=SCENARIO)) +
        geom_line() +
        geom_point() +
        theme(legend.position='none') +
        scale_shape_manual(values=c(19,21))")))
    plot(g)
    filename <- paste(scenarioname,"_",num,"_",x_names[num],"-",y_names[num],"_CN", sep="")
    ggsave(file=paste("./png/",filename,".png"))
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
}
