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

# df_yokogata <- spread(df_all, key="Year",value="Value")
# view(df_yokogata)


#指標名とシナリオ名 で繰り返し処理 -------------------------------------------------------

indicators <- c("GDP_Capita","Electricity_Rate_Total","ChangeRate_Electricity_Rate_Total")
scenarionames <- c("Baseline")    # c("Baseline","2C","1.5C","2.5C","WB2C")

for (scenarioname in scenarionames) {
  # view(scenarioname)

  df_forMerge <- select(df_all, c("REGION","Year",)
           ) %>% distinct()
  view(df_forMerge)
  
  for (indicator in indicators) {
    # view(indicator)
  
    df_Graph <- filter(df_all, VARIABLE==indicator, SCENARIO %in% c("Historical",scenarioname))
    View(df_Graph)

    g <- ggplot(df_Graph, aes(x=Year, y=Value, color=REGION, shape=SCENARIO)) +
      geom_line() +
      geom_point() + 
      scale_shape_manual(values=c(19,21)) +
      ylab(indicator)
    plot(g)
    filename <- paste(scenarioname,"_Year-",indicator)
    ggsave(file=paste("./../4_output/",filename,".pdf"))

    # df_Graph_tmp1 <- rename(df_Graph, GDP_Capita=Value)

    df_Graph_tmp <- select(df_Graph, -c("MODEL","UNIT","VARIABLE"))
    df_Graph_tmp <- eval(parse(text=paste0("rename(df_Graph_tmp,", indicator, "=Value)")))
    View(df_Graph_tmp)

    df_forMerge <- merge(df_forMerge, df_Graph_tmp)    
    View(df_forMerge)
  }

  # GDPcapita vs. ER, CRER プロット用のdf作成

  # x=GDP_Capita, y=Electricity_Rate_Total
  g <- ggplot(df_forMerge, aes(x=GDP_Capita, y=Electricity_Rate_Total, color=REGION, shape=SCENARIO)) +
    geom_line() +
    geom_point() + 
    scale_shape_manual(values=c(19,21)) 
  plot(g)
  filename <- paste(scenarioname,"_GDPcapita-Electricity_Rate")
  ggsave(file=paste("./../4_output/",filename,".pdf"))


  # グラフ描画 by XYのテキスト指定

  xname <- "GDP_Capita"
  yname <- "Electricity_Rate_Total"
  
  g <- eval(parse(text=paste0(
            "ggplot(df_forMerge, aes(x=",xname,",y=",yname, ",color=REGION,shape=SCENARIO)) +
    geom_line() +
    geom_point() + 
    scale_shape_manual(values=c(19,21))")))
  plot(g)
  filename <- paste(scenarioname,"_",xname,"-",yname)
  ggsave(file=paste("./../4_output/",filename,".png"))
  
}
