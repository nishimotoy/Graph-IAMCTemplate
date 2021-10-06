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

df_forMerge <- select(df_all, c("REGION","Year")
         ) %>% distinct()
view(df_forMerge)

# df_yokogata <- spread(df_all, key="Year",value="Value")
# view(df_yokogata)


#指標名とシナリオ名 で繰り返し処理 -------------------------------------------------------

indicators <- c("GDP_Capita")   # c("GDP_Capita","Electricity_Rate_Total","ChangeRate_Electricity_Rate_Total")
scenarionames <- c("Baseline")    # c("Baseline","2C","1.5C","2.5C","WB2C")

for (scenarioname in scenarionames) {
  view(scenarioname)
  
  for (indicator in indicators) {
    view(indicator)
  
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

    
#   eval保留＞GDPcapita vs. ER, CRER プロット用のdf作成
#   eval(parse(text=paste("df_",indicator))) <- select(df_Graph, "SCENARIO","REGION","Year","Value")
#                              ) %% rename(Indicator="Value")
#   View(eval(parse(text=paste("df_",indicator))))
    df_Graph_forMerge <- select(df_Graph, -c("MODEL","UNIT")) # MODEL(冗長) UNIT(不統一)を削除
    
    }
  
  df_forMerge <- mutate(df_forMerge,GDP_Capita=df_Graph$Value)
  view(df_forMerge)  
  
}

# GDPcapita vs. ER, CRER プロット用のdf作成






#GDP_Capita vs. ChangeRate_Electricity_Rate_Total の表示 -------------------------------------------------------

df_GDP_ER <- merge(df_GDPcapita[, !(colnames(df_GDPcapita) %in% c("VARIABLE", "UNIT"))], 
                     df_ER[, !(colnames(df_ER) %in% c("VARIABLE", "UNIT"))], 
                     all=T) %>%
               merge(df_CR_ER[, !(colnames(df_CR_ER) %in% c("VARIABLE", "UNIT"))], 
                     all=T)
View(df_GDP_ER)
write_csv(df_GDP_ER, "./../4_output/df_GDP_ER.csv")

# GDP 出力
g <- ggplot(df_GDP_ER, aes(x=Year,y=GDPcapita,color=REGION)) + 
        geom_line() + 
        geom_point(shape=21)
plot(g)
ggsave(file = "./../4_output/Year-GDPcapita.pdf")

# ER 出力
g <- ggplot(df_GDP_ER, aes(x=Year,y=Electricity_Rate,color=REGION)) + 
        geom_line() + 
        geom_point(shape=21)
plot(g)
ggsave(file = "./../4_output/Year-Electricity_Rate.pdf")

g <- ggplot(df_GDP_ER, aes(x=GDPcapita,y=Electricity_Rate,color=REGION)) + 
        geom_line() + 
        geom_point(shape=21) 
plot(g)
ggsave(file = "./../4_output/GDPcapita-Electricity_Rate.pdf")


# CR_ER 出力
g <- ggplot(df_GDP_ER, aes(x=Year,y=CR_Electricity_Rate,color=REGION)) + 
  geom_line() + 
  geom_point(shape=21)
plot(g)
ggsave(file = "./../4_output/Year-CR_Electricity_Rate.pdf")

g <- ggplot(df_GDP_ER, aes(x=GDPcapita,y=CR_Electricity_Rate,color=REGION)) + 
  geom_line() + 
  geom_point(shape=21) 
plot(g)
ggsave(file = "./../4_output/GDPcapita-CR_Electricity_Rate.pdf")


# g <- ggplot(df_GDP_ER, aes(x=GDPcapita,y=CR_Electricity_Rate,color=REGION)) + 
#   geom_line() + geom_point(x=GDPcapita,y=CR_Electricity_Rate,shape = 21) + scale_shape_identity()
# plot(g)
# ggsave(file = "./../4_output/Test.pdf")

