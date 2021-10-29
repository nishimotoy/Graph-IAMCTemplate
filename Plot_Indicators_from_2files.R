#Packages------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(patchwork)

# getwd() 
setwd("C:/_Nishimoto/R/WBAL_R02/") 

# データの読み込み---------------------------------------------------------------
df_past <- read_csv("./2_data/REF2/IAMCTemplate_i_past.csv")
# view(df_past)

df_future <- read_csv("./2_data/REF2/IAMCTemplate_i_future.csv")
# view(df_future)

titlerow <- c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")

df_all <- rbind(gather(df_past, key="Year", value="Value", -all_of(titlerow) ),
                gather(df_future, key="Year", value="Value", -all_of(titlerow) )
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
y2_names <- indicators[c(3,5,7)]

for (scenarioname in scenarionames) {
  
  df_forMerge <- select(df_all, c("REGION","Year")) %>% distinct()
  #  view(df_forMerge)
  
  for (indicator in indicators) {
    
    df_Graph <- filter(df_all, VARIABLE==indicator, SCENARIO %in% c('Historical',scenarioname)
    ) %>% select(-c('MODEL','UNIT','VARIABLE'))
    df_Graph <- eval(parse(text=paste0("rename(df_Graph,", indicator, "=Value)")))
    df_Graph <- df_Graph[order(df_Graph$Year),]
    View(df_Graph)
    df_forMerge <- merge(df_forMerge, df_Graph)
  }
  View(df_forMerge)
  
  # XY散布図 by 軸名のテキスト指定
  pdf(file=paste("./4_output/XY_",scenarioname,".pdf", sep=""))    
  for (num in 1:length(x_names)) {
    
    g <- eval(parse(text=paste0(
      "ggplot(df_forMerge, aes(x=",x_names[num],",y=",y_names[num], 
      ",color=REGION,shape=SCENARIO)) +
        geom_line() +
        geom_point() + 
        scale_shape_manual(values=c(19,21))")))
    plot(g)
    filename <- paste(scenarioname,num,"_",x_names[num],"-",y_names[num], sep="")
    # ggsave(file=paste("./4_output/",filename,".png"))
  }
  dev.off() 

  # 4in1 レイアウト
  pdf(file=paste("./5_test/figures_",scenarioname,".pdf", sep=""))    
  for (num in 1:length(x_names)) {
    
    p1 <- eval(parse(text=paste0(
      "ggplot(df_forMerge, aes(x=",x_names[num],",y=",y_names[num], 
      ",color=REGION,shape=SCENARIO)) +
        geom_line() +
        geom_point() + 
        scale_shape_manual(values=c(19,21))")))
    
    if ( num%%4==1 ) { p <- p1 } else { p <- p + p1 }
    if ( num%%4==0 ) { plot(p) } 
  }
  dev.off() 
  
  # バイオリン
  pdf(file=paste("./4_output/violin_",scenarioname,".pdf", sep=""))    
  for (indicator in indicators) {

    g <- eval(parse(text=paste0(
      "ggplot(df_forMerge, aes(x=","REGION",",y=",indicator, 
      ",color=SCENARIO)) +
        geom_violin() + 
        geom_jitter(shape=20, position=position_dodge(1.0))")))
    plot(g)
  }
  dev.off() 
  
  # 箱ヒゲ図
  pdf(file=paste("./4_output/boxplot_",scenarioname,".pdf", sep=""))    
  for (indicator in indicators) {
    
    g <- eval(parse(text=paste0(
      "ggplot(df_forMerge, aes(x=","REGION",",y=",indicator, 
      ",color=SCENARIO)) +
        geom_boxplot() + 
        geom_jitter(shape=20, position=position_dodge(0.8))")))
    plot(g)
  }
  dev.off() 
  
  # 頻度分布
  pdf(file=paste("./4_output/histogram_",scenarioname,".pdf", sep=""))    
  for (indicator in indicators) {
    
    g <- eval(parse(text=paste0(
      "ggplot(df_forMerge, aes(x=",indicator, 
      ",color=SCENARIO)) +
        geom_histogram(bins=50) +
        ylab('Count of Region-Year')")))
    plot(g)
    filename <- paste("histogram_",scenarioname,"_",indicator, sep="")
    ggsave(file=paste("./4_output/",filename,".png", sep=""), width=6, height=4, dpi=100)
  }
  dev.off() 

  # 確率密度分布
  pdf(file=paste("./4_output/density_",scenarioname,".pdf", sep=""))    
  for (indicator in indicators) {
    
    g <- eval(parse(text=paste0(
      "ggplot(df_forMerge, aes(x=",indicator, 
      ",color=SCENARIO)) +
        geom_density() +
        ylab('Density (Count scaled to 1) of Region-Year')")))
    plot(g)
    filename <- paste("density_",scenarioname,"_",indicator, sep="")
    ggsave(file=paste("./4_output/",filename,".png", sep=""), width=6, height=4, dpi=100)
  }
  dev.off() 
  
}
