#Packages------------------------------------------------------
update.packages("ggplot2")
library(ggplot2)
update.packages("tidyverse")
library(tidyverse)

getwd() 
# setwd("c:/usr") 

# データの読み込み---------------------------------------------------------------
df_past <- read_csv("./../2_data/REF2/IAMCTemplate_i_past.csv")
view(df_past)

df_future <- read_csv("./../2_data/REF2/IAMCTemplate_i_future.csv")
view(df_future)

title <- c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")

df_all <- rbind(gather(df_past, key="Year", value="Value", -title
                       ) %>%  mutate(H_F="Historical"),
                gather(df_future, key="Year", value="Value", -title
                       ) %>%  mutate(H_F="Future")
                ) %>% filter(MODEL != "MODEL"
                ) %>% na.omit() 

df_all$Year <- as.numeric(df_all$Year)
view(df_all)


#GDP_Capita の作成と表示 -------------------------------------------------------

df_GDPcapita <- filter(df_all, VARIABLE=="GDP_Capita", SCENARIO %in% c("Reference","Baseline")
                       ) %>% rename ("GDP_Capita"="Value")
View(df_GDPcapita)

g <- ggplot(df_GDPcapita, aes(x=Year, y=GDP_Capita, color=REGION, shape=SCENARIO)) +
  geom_line() + 
  geom_point()
plot(g)

# 続きは "GDP_Capita"をloopにするところから




#GDP_Capita の作成と表示 -------------------------------------------------------
df_GDPcapita_past <- gather(df_past,key="Year",value="GDPcapita",
                            -c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")
                    ) %>%  filter(VARIABLE == "GDP_Capita", SCENARIO == "Reference")

df_GDPcapita_future <- gather(df_future,key="Year",value="GDPcapita",
                              -c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")
                      ) %>%  filter(VARIABLE == "GDP_Capita", SCENARIO == "Baseline")

df_GDPcapita <- merge(df_GDPcapita_past[, !(colnames(df_GDPcapita_past) %in% c("MODEL","SCENARIO","VARIABLE", "UNIT"))], 
                      df_GDPcapita_future[, !(colnames(df_GDPcapita_future) %in% c("MODEL","SCENARIO","VARIABLE", "UNIT"))], 
                      all=T)
df_GDPcapita$Year <- as.numeric(df_GDPcapita$Year)
View(df_GDPcapita)

# g <- ggplot(df_GDPcapita, aes(x=Year,y=GDPcapita,color=REGION)) + geom_line()
# plot(g)


#Electricity_Rate_Total の作成と表示 -------------------------------------------------------
df_ER_past <- gather(df_past,key="Year",value="Electricity_Rate",
                -c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")
) %>%  filter(VARIABLE == "Electricity_Rate_Total", SCENARIO == "Reference")

df_ER_future <- gather(df_future,key="Year",value="Electricity_Rate",
                     -c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")
) %>%  filter(VARIABLE == "Electricity_Rate_Total", SCENARIO == "Baseline")

df_ER <- merge(df_ER_past[, !(colnames(df_ER_past) %in% c("MODEL","SCENARIO","VARIABLE", "UNIT"))], 
               df_ER_future[, !(colnames(df_ER_future) %in% c("MODEL","SCENARIO","VARIABLE", "UNIT"))], 
               all=T)
df_ER$Year <- as.numeric(df_ER$Year)
View(df_ER)

# g <- ggplot(df_ER, aes(x=Year,y=Electricity_Rate,color=REGION)) + geom_line()
# plot(g)


#ChangeRate_Electricity_Rate_Total の作成と表示 -------------------------------------------------------
df_CR_ER_past <- gather(df_past,key="Year",value="CR_Electricity_Rate",
                -c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")
) %>%  filter(VARIABLE == "ChangeRate_Electricity_Rate_Total", SCENARIO == "Reference")

df_CR_ER_future <- gather(df_future,key="Year",value="CR_Electricity_Rate",
                        -c("MODEL","SCENARIO","REGION","VARIABLE","UNIT")
) %>%  filter(VARIABLE == "ChangeRate_Electricity_Rate_Total", SCENARIO == "Baseline")

df_CR_ER <- merge(df_CR_ER_past[, !(colnames(df_CR_ER_past) %in% c("MODEL","SCENARIO","VARIABLE", "UNIT"))], 
                  df_CR_ER_future[, !(colnames(df_CR_ER_future) %in% c("MODEL","SCENARIO","VARIABLE", "UNIT"))], 
                  all=T)
df_CR_ER$Year <- as.numeric(df_CR_ER$Year)
View(df_CR_ER)

# g <- ggplot(df_CR_ER, aes(x=Year,y=CR_Electricity_Rate,color=REGION)) + geom_line()
# plot(g)


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

