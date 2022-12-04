# 検証用
x_names <- c(
  'Year', 'Year', 'Energy_Intensity_scaled', 'TES_Total_scaled', 'GDP_IEA_scaled' 
  , 'Year', 'Year', 'Carbon_Intensity_scaled', 'CO2_fuel_Total_scaled', 'TES_Total_scaled' 
  , 'Year', 'Year', 'Electricity_Rate_Total', 'TFC_Elec_Total_scaled', 'TFC_Total_Total_scaled'
  , 'Year', 'Year', 'Energy_Intensity_scaled', 'TES_Total_scaled', 'GDP_IEA_scaled' 
  , 'Year', 'Year', 'Carbon_Intensity_scaled', 'CO2_fuel_Total_scaled', 'TES_Total_scaled' 
  , 'Year', 'Year', 'Electricity_Rate_Total', 'TFC_Elec_Total_scaled', 'TFC_Total_Total_scaled'
) 
y_names <- c( 'Energy_Intensity_scaled', rep('ChangeRate_Energy_Intensity',4)
              ,'Carbon_Intensity_scaled', rep('ChangeRate_Carbon_Intensity',4)
              ,'Electricity_Rate_Total', rep('ChangeRate_Electricity_Rate_Total',4)
              ,'Energy_Intensity_scaled', rep('Henkaryo_Energy_Intensity',4)
              ,'Carbon_Intensity_scaled', rep('Henkaryo_Carbon_Intensity',4)
              ,'Electricity_Rate_Total', rep('Henkaryo_Electricity_Rate_Total',4)
) 

library(RColorBrewer)
region17_color <- c(brewer.pal(5,"Dark2"),brewer.pal(8,"Accent"),brewer.pal(4,"Set1"))  
scenario_color <- c('#AAAA11', '#329262', '#FF9900', '#DD4477', '#651067', '#3366CC', '#84919E', '#dda0dd') # df_Graph登場順
scenario_shape <- c(19,21,22,23,24,25,1,19)
scenario_fill <- c('white','red','white','white','white','white','white','white')
scenario_size <- c(1,4,4,4,4,4,4,1)
scenario_shape1 <- c(19,19,21,21,21,21,21,19)
scenario_shape2 <- c(19,21,22,23,24,25,1,19)
axis_cutoff_prob <- 0.01   # 軸の表示において切り捨てる分位範囲 （0.01: 両端1% cutoff）

df_Graph_plot <- df_Graph %>% filter(SCENARIO=='Historical_R17')

for (dummyloop in 1) { # item 指定出力
  library(RColorBrewer)
  df_Graph_plotXY <- df_Graph_plot 
  df_Graph_plotXY <- df_Graph_plot %>% filter(SCENARIO!='Historical')
  df_Graph_plotXY_His <- df_Graph_plotXY %>% filter(SCENARIO=='Historical_R17')
  write_csv(df_Graph_plotXY, "./df_Graph_plotXY.csv") 
  write_csv(df_Graph_plotXY_His, "./df_Graph_plotXY_His.csv") 
  y_axis_val <- c(-0.5, 0.1)
  
  pdf(file=paste("./graph_XY_item.pdf", sep=""))    
  for (num in 1:length(x_names)) { #num   
    
    x_axis_min <- min(eval(parse(text=paste0("df_Graph_plotXY$",x_names[num]))), na.rm=T)
    x_axis_max <- max(eval(parse(text=paste0("df_Graph_plotXY$",x_names[num]))), na.rm=T)
    y_axis_min <- min(eval(parse(text=paste0("df_Graph_plotXY$",y_names[num]))), na.rm=T)
    y_axis_max <- max(eval(parse(text=paste0("df_Graph_plotXY$",y_names[num]))), na.rm=T)
    cutoff_prob <- 0.05
    vec_data <- eval(parse(text=paste0("df_Graph_plotXY_His$",y_names[num]))) 
    axis_range <- quantile(vec_data, probs=c(cutoff_prob, (1-cutoff_prob)), na.rm=T)
    
    x_axis_right <- x_axis_max    - 0.1*(x_axis_max - x_axis_min)
    y_axis_high  <- axis_range[2] + 0.2*(y_axis_max - y_axis_min)
    y_axis_low   <- axis_range[1] - 0.2*(y_axis_max - y_axis_min)
    y_axis_top   <- y_axis_max    - 0.1*(y_axis_max - y_axis_min)
    
    if ((regexpr('^ChangeRate*', y_names[num]))||(regexpr('*Electricity_Rate*', y_names[num]))) { 
      annotate_text <- eval(parse(text=paste0(
        "annotate('text', x=",x_axis_right,", y=",y_axis_top,", 
        label='y-percentile: ",100*cutoff_prob,"-",100*(1-cutoff_prob),"%\n", 
        round(100*axis_range[2], digits=2),"%\n",
        round(100*axis_range[1], digits=2),"%\n')"
      ))) 
    } else {
      annotate_text <- eval(parse(text=paste0(
        "annotate('text', x=",x_axis_right,", y=",y_axis_top,",
        label='y-percentile: ",100*cutoff_prob,"-",100*(1-cutoff_prob),"%\n", 
        formatC(axis_range[2], digits=2, format='e'),"\n",
        formatC(axis_range[1], digits=2, format='e'),"\n')"
      ))) 
    }
    g <- eval(parse(text=paste0(
      "ggplot(df_Graph_plotXY, aes(x=",x_names[num],",y=",y_names[num], 
      ",color=REGION, shape=SCENARIO)) +
              geom_point() + 
              geom_line() + 
              scale_color_manual(values=c(rep(region17_color,3))) +
              scale_shape_manual(values=scenario_shape2)"))) # SCENARIO数
    g <- g + geom_hline(yintercept=c(axis_range[1], axis_range[2])) + annotate_text 
    plot(g)
    
    filename <- paste(scenarioname,num,"_",x_names[num],"-",y_names[num], sep="")
    # ggsave(file=paste("./png/R17_",filename,".png", sep=""), width=5, height=4, dpi=100)
    
    # if (y_names[num]=='ChangeRate_Carbon_Intensity') { #CI
    #   y_axis_top <- y_axis_val[2]-0.1*(y_axis_val[2]-y_axis_val[1])
    #   annotate_text_ylim <- eval(parse(text=paste0(
    #     "annotate('text', x=",x_axis_right,", y=",y_axis_top,", 
    #     label='y-percentile:" ,100*cutoff_prob,"-",100*(1-cutoff_prob),"%\n", 
    #     round(100*axis_range[2], digits=2),"%\n",
    #     round(100*axis_range[1], digits=2),"%\n')"
    #   ))) 
    #   g <- g + ylim(c(y_axis_val[1], y_axis_val[2])) + annotate_text_ylim 
    #   plot(g)
    # } #CI
    
    # if (x_names[num]=='Year') { #Year
    #   g <- eval(parse(text=paste0(
    #     "ggplot(df_Graph_plotXY_His, aes(x=",x_names[num],",y=",y_names[num], 
    #     ", shape=SCENARIO)) +
    #           geom_smooth(method=lm) +
    #           geom_point() + 
    #           xlim(",1970, ", ",2100, ") "))) 
    #   g <- g + geom_hline(yintercept=c(axis_range[1], axis_range[2]))
    #   g <- g + stat_poly_eq(formula=y~x, aes(label=paste(
    #     "atop(", paste(stat(eq.label),stat(rr.label),stat(adj.rr.label),sep="~~~"), ",",
    #     paste(stat(f.value.label),stat(p.value.label),stat(AIC.label),stat(BIC.label),sep="~~~"),
    #     ")", sep="")),
    #     label.x="right", parse=TRUE)
    #   plot(g)
    # } #Year
  } #num
  dev.off() 
} # item 指定出力
