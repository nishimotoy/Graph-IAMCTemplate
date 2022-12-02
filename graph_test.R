# 検証用
x_names <- c('Energy_Intensity_scaled','Carbon_Intensity_scaled','Electricity_Rate_Total',
             'Electricity_Rate_Ind','Electricity_Rate_Tra',
             'Electricity_Rate_Res','Electricity_Rate_Com')
y_names <- c('ChangeRate_Energy_Intensity','Henkaryo_Carbon_Intensity','Henkaryo_Electricity_Rate_Total',
             'Henkaryo_Electricity_Rate_Ind','Henkaryo_Electricity_Rate_Tra',
             'Henkaryo_Electricity_Rate_Res','Henkaryo_Electricity_Rate_Com') # y_names_box in graph_paper.R

df_Graph_p <- df_Graph %>% select('SCENARIO', 'REGION', 'Year', unique(sort(c(x_names,y_names)))) 


library(RColorBrewer)
region_color <- c(brewer.pal(5,"Dark2"),brewer.pal(5,"Set1"),brewer.pal(7,"Paired"))  
scenario_color <- c('#AAAA11', '#329262', '#FF9900', '#DD4477', '#651067', '#3366CC', '#84919E','#dda0dd') # df_Graph登場順
scenario_shape <- c(19,4,22,23,24,25,1,19)
dummy_shape <- rep(19,length(scenario_color))
window_prob <- 0.05

df_Graph_plot <- df_Graph_p %>% filter(SCENARIO!='Historical' & SCENARIO!='Historical_B') # 
df_Graph_plot_HisR <- df_Graph_plot %>% filter(SCENARIO=='Historical_R17' )
write_csv(df_Graph_plot, "./df_Graph_plot.csv") 


pdf(file=paste("./graph_test.pdf", sep=""))    
for (num in 1:length(x_names)) { #num # XYグラフの出力
  
  g <- eval(parse(text=paste0(
    "ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
    ",color=REGION, shape=SCENARIO)) +
              geom_point() + 
              scale_color_manual(values=c(rep(region_color,3))) +
              scale_shape_manual(values=scenario_shape)"))) 
  if ( regexpr('^Electricity_Rate_*', x_names[num])==1 ) {
    g <- g + xlim(0,1) + ylim(-0.1,0.1)
  }
  vec_data <- eval(parse(text=paste0("df_Graph_plot_HisR$",y_names[num])))  # 窓の追加
  window_range <- quantile(vec_data, probs=c(window_prob, (1-window_prob)), na.rm=T)
  g <- g + eval(parse(text=paste0( "annotate('rect', xmin=",-Inf,", ymin=",window_range[1], 
                                   ", xmax=",Inf, ", ymax=",window_range[2], 
                                   ", alpha=.22, fill='#329262')"))) 
  plot(g)
  
  g <- eval(parse(text=paste0(
    "ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
    ",color=SCENARIO, shape=SCENARIO)) +
              geom_point() + 
              scale_color_manual(values=scenario_color[-1]) +
              scale_shape_manual(values=dummy_shape)"))) # SCENARIO数
  
  if ( regexpr('^Electricity_Rate_*', x_names[num])==1 ) {
    g <- g + xlim(0,1) + ylim(-0.1,0.1)
  }
  vec_data <- eval(parse(text=paste0("df_Graph_plot_HisR$",y_names[num])))  # 窓の追加
  window_range <- quantile(vec_data, probs=c(window_prob, (1-window_prob)), na.rm=T)
  g <- g + eval(parse(text=paste0( "annotate('rect', xmin=",-Inf,", ymin=",window_range[1], 
                                   ", xmax=",Inf, ", ymax=",window_range[2], 
                                   ", alpha=.22, fill='#329262')"))) 
  plot(g)
  
  # boxplot
  g <- eval(parse(text=paste0(
    "ggplot(df_Graph_plot, aes(x=SCENARIO, y=",y_names[num], ", color=SCENARIO)) +
            geom_boxplot() +
            stat_boxplot(geom='errorbar', width=0.3) + # ヒゲ先端の横線
            scale_color_manual(values=c(scenario_color[-1])) 
           ")))
  # g <- g + coord_flip(ylim = c(-0.1, 0.1))
  # g <- g + coord_flip()
  plot(g)

  g <- eval(parse(text=paste0(
    "ggplot(df_Graph_plot, aes(x=SCENARIO, y=",x_names[num], ", color=SCENARIO)) +
            geom_boxplot() +
          # scale_x_discrete(limit=rev(scenarionames_order)) +  # 系列の順序 # x=SCENARIO 必要
            coord_flip() + # 指定する場合
            stat_boxplot(geom='errorbar', width=0.3) + # ヒゲ先端の横線
            scale_color_manual(values=c(scenario_color[-1])) 
           ")))
  plot(g)

}  #num # XYグラフの出力


# 部門別プロット
df_Graph_sector <- df_Graph_plot  %>% select('SCENARIO', 'REGION', 'Year', 'Electricity_Rate_Total', 'Henkaryo_Electricity_Rate_Total'
) %>% mutate(Sector='Total'
) %>% rename('Electricity_Rate'='Electricity_Rate_Total'
) %>% rename('Henkaryo_Electricity_Rate'='Henkaryo_Electricity_Rate_Total'
)
df_Graph_sec   <- df_Graph_plot   %>% select('SCENARIO', 'REGION', 'Year', 'Electricity_Rate_Ind', 'Henkaryo_Electricity_Rate_Ind'
) %>% mutate(Sector='Industory'
) %>% rename('Electricity_Rate'='Electricity_Rate_Ind'
) %>% rename('Henkaryo_Electricity_Rate'='Henkaryo_Electricity_Rate_Ind'
) %>% rbind(df_Graph_sector)
df_Graph_sector <- df_Graph_sec

df_Graph_sec   <- df_Graph_plot   %>% select('SCENARIO', 'REGION', 'Year', 'Electricity_Rate_Tra', 'Henkaryo_Electricity_Rate_Tra'
) %>% mutate(Sector='Transport'
) %>% rename('Electricity_Rate'='Electricity_Rate_Tra'
) %>% rename('Henkaryo_Electricity_Rate'='Henkaryo_Electricity_Rate_Tra'
) %>% rbind(df_Graph_sector)
df_Graph_sector <- df_Graph_sec

df_Graph_sec   <- df_Graph_plot   %>% select('SCENARIO', 'REGION', 'Year', 'Electricity_Rate_Res', 'Henkaryo_Electricity_Rate_Res'
) %>% mutate(Sector='Residential'
) %>% rename('Electricity_Rate'='Electricity_Rate_Res'
) %>% rename('Henkaryo_Electricity_Rate'='Henkaryo_Electricity_Rate_Res'
) %>% rbind(df_Graph_sector)
df_Graph_sector <- df_Graph_sec

df_Graph_sec   <- df_Graph_plot   %>% select('SCENARIO', 'REGION', 'Year', 'Electricity_Rate_Com', 'Henkaryo_Electricity_Rate_Com'
) %>% mutate(Sector='Commercial'
) %>% rename('Electricity_Rate'='Electricity_Rate_Com'
) %>% rename('Henkaryo_Electricity_Rate'='Henkaryo_Electricity_Rate_Com'
) %>% rbind(df_Graph_sector)
df_Graph_sector <- df_Graph_sec

g <- eval(parse(text=paste0(
  "ggplot(df_Graph_sector, aes(x=","Electricity_Rate",",y=","Henkaryo_Electricity_Rate", 
  ",color=Sector, shape=SCENARIO)) +
              geom_point() + 
              xlim(0,1) + ylim(-0.1,0.1) +
              scale_color_manual(values=scenario_color[c(2,3,5,6,7)]) +
              scale_shape_manual(values=scenario_shape)"))) 
plot(g)

g <- eval(parse(text=paste0(
  "ggplot(df_Graph_sector, aes(x=","Electricity_Rate",",y=","Henkaryo_Electricity_Rate", 
  ",color=SCENARIO, shape=Sector)) +
              geom_point() + 
              xlim(0,1) + ylim(-0.1,0.1) +
              scale_color_manual(values=scenario_color[-1]) +
              scale_shape_manual(values=scenario_shape)"))) 
plot(g)

g <- eval(parse(text=paste0(
  "ggplot(df_Graph_sector, aes(x=","Electricity_Rate",",y=","Henkaryo_Electricity_Rate", 
  ",color=Sector)) +
              geom_point() + 
              xlim(0,1) + ylim(-0.1,0.1) +
              scale_color_manual(values=scenario_color[c(2,3,5,6,7)]) +
              scale_shape_manual(values=scenario_shape)"))) 
plot(g)

df_Graph_sector_his <- df_Graph_sector %>% filter(SCENARIO=='Historical_R17') 
g <- eval(parse(text=paste0(
  "ggplot(df_Graph_sector_his, aes(x=","Electricity_Rate",",y=","Henkaryo_Electricity_Rate", 
  ",color=Sector)) +
              geom_point() + 
              xlim(0,1) + ylim(-0.1,0.1) +
              scale_color_manual(values=scenario_color[c(2,3,5,6,7)]) +
              scale_shape_manual(values=scenario_shape)"))) 
plot(g)

dev.off() 

