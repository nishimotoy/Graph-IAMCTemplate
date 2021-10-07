#Packages------------------------------------------------------
# update.packages("ggplot2")
library(ggplot2)
# update.packages("tidyverse")
library(tidyverse)

getwd() 
# setwd("c:/usr") 

# データの読み込み---------------------------------------------------------------
df_past <- read_csv("./../2_data/REF2/IAMCTemplate_i_past.csv")
view(df_past)

df_future <- read_csv("./../2_data/REF2/IAMCTemplate_i_future.csv")
view(df_future)

title <- c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")

df_all <- rbind(gather(df_past, key="Year", value="Value", -title),
                gather(df_future, key="Year", value="Value", -title)
    ) %>% mutate(SCENARIO=gsub("Reference","Historical",SCENARIO)
    ) %>% filter(MODEL != "MODEL"
    ) %>% na.omit() 
df_all$Year <- as.numeric(df_all$Year)
view(df_all)


#指標名とシナリオ名 で繰り返し処理＠グラフ出力 -------------------------------------------------------

indicators <- c("GDP_Capita","Electricity_Rate_Total","ChangeRate_Electricity_Rate_Total")
scenarionames <- c("Baseline")    # c("Baseline","2C","1.5C","2.5C","WB2C")

x_names <- c("Year","Year","Year","GDP_Capita","GDP_Capita")
y_names <- c("GDP_Capita","Electricity_Rate_Total","ChangeRate_Electricity_Rate_Total",
                          "Electricity_Rate_Total","ChangeRate_Electricity_Rate_Total")

for (scenarioname in scenarionames) {

  df_forMerge <- select(df_all, c("REGION","Year",)
           ) %>% distinct()
  view(df_forMerge)
  
  for (indicator in indicators) {

    df_Graph <- filter(df_all, VARIABLE==indicator, SCENARIO %in% c("Historical",scenarioname))
    View(df_Graph)
    df_Graph_tmp <- select(df_Graph, -c("MODEL","UNIT","VARIABLE"))
    df_Graph_tmp <- eval(parse(text=paste0("rename(df_Graph_tmp,", indicator, "=Value)")))
    View(df_Graph_tmp)

    df_forMerge <- merge(df_forMerge, df_Graph_tmp)    
    View(df_forMerge)
  }

  # グラフ描画 by XYのテキスト指定
  for (num in 1:length(x_names)) {

    g <- eval(parse(text=paste0(
            "ggplot(df_forMerge, aes(x=",x_names[num],",y=",y_names[num], 
            ",color=REGION,shape=SCENARIO)) +
        geom_line() +
        geom_point() + 
        scale_shape_manual(values=c(19,21))")))
      plot(g)
      filename <- paste(scenarioname,"_",x_names[num],"-",y_names[num])
      ggsave(file=paste("./../4_output/",filename,".png"))
  }  
}
