#Packages------------------------------------------------------
# update.packages("ggplot2")
library(ggplot2)
# update.packages("tidyverse")
library(tidyverse)

# getwd() 
# setwd("c:/usr") 

# 一時退避（ここから）（次工程で使う）
# 過去ファイル（IEA形式、各国毎）の読み込み＞ファイル名でloopさせて追加書きする予定
df_CO2_fuel_Bio <- read_csv("./../2_data/REF/CO2_fuel_Bio.csv") 
df_CO2_fuel_Bio <- rename(df_CO2_fuel_Bio, 'IEA_coutry_name'='X1')
# write_csv(df_CO2_fuel_Bio, "./../2_data/REF2/CO2_fuel_Bio_written.csv")
view(df_CO2_fuel_Bio)

# 単位の連想配列＞ファイル名にマッチさせる予定
df_unit <- read.delim(file="./../2_data/REF/unit.txt", header=T)
view(df_unit)
Unit_of_Var        <- df_unit$unit
names(Unit_of_Var) <- df_unit$filename
# Unit_of_Var[names(Unit_of_Var)]
# 一時退避（ここまで）


# 国コードの連想配列
df_CC <- read.delim(file="./../2_data/REF/CC.txt", header=T) 
df_CC <- rename(df_CC, 'IEA_coutry_name'='IEA国名')
view(df_CC)
Region_Code        <- df_CC$AIM17
names(Region_Code) <- df_CC$IEA_coutry_name
# Region_Code[names(Region_Code)]

# IEAデータ（整形済）読み込み
df_countries <- read_csv("./../2_data/REF2/IAMCTemplate_out_countries.csv")
view(df_countries)

# 国コード付与
df_countries <- mutate(df_countries, AIM17=Region_Code[df_countries$REGION]
          ) %>% rename('IEA_coutry_name'='REGION'
          ) %>% rename('REGION'='AIM17')
select(df_countries,'IEA_coutry_name','REGION')

# write_csv(df_countries, "./../2_data/REF2/IAMCTemplate_out_countries_written.csv")

df_past <- df_countries
# df_past <- rename(=)

# データの読み込み---------------------------------------------------------------
# df_past <- read_csv("./../2_data/REF2/IAMCTemplate_i_past.csv")
# view(df_past)

df_future <- read_csv("./../2_data/REF2/IAMCTemplate_i_future.csv")
# view(df_future)


titlerow <- c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")

df_all <- rbind(gather(df_past, key="Year", value="Value", -titlerow ),
                gather(df_future, key="Year", value="Value", -titlerow )
    ) %>% mutate(SCENARIO=gsub("Reference","Historical",SCENARIO)
    ) %>% filter(MODEL != "MODEL"
    ) %>% na.omit() 
df_all$Year <- as.numeric(df_all$Year)
view(df_all)


#指標名とシナリオ名 で繰り返し処理＠グラフ出力 -------------------------------------------------------

scenarionames <- c("Baseline","2C")    # c("Baseline","2C","1.5C","2.5C","WB2C")
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

for (scenarioname in scenarionames) {

  df_forMerge <- select(df_all, c("REGION","Year")) %>% distinct()
#  view(df_forMerge)
  
  for (indicator in indicators) {

    df_Graph <- filter(df_all, VARIABLE==indicator, SCENARIO %in% c('Historical',scenarioname)
          ) %>% select(-c('MODEL','UNIT','VARIABLE'))
    df_Graph <- eval(parse(text=paste0("rename(df_Graph,", indicator, "=Value)")))
    df_Graph <- df_Graph[order(df_Graph$Year),]
    df_forMerge <- merge(df_forMerge, df_Graph)
    View(df_forMerge)

  }

  # グラフ描画 by XYのテキスト指定
  pdf(file=paste("./../4_output/figures_",scenarioname,".pdf", sep=""))    
  par(mfrow = c(4, 4))
  for (num in 1:length(x_names)) {

    g <- eval(parse(text=paste0(
            "ggplot(df_forMerge, aes(x=",x_names[num],",y=",y_names[num], 
            ",color=REGION,shape=SCENARIO)) +
        geom_line() +
        geom_point() + 
        scale_shape_manual(values=c(19,21))")))
      plot(g)
      filename <- paste(scenarioname,num,"_",x_names[num],"-",y_names[num], sep="")
      # ggsave(file=paste("./../4_output/",filename,".png"))
  }
  dev.off() 
}
