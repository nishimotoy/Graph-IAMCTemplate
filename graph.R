#Graph output ------------------------------------------------------
for (dummyloop in 1) {  # グラフ出力 for (dummyloop in 1) while (0)
  
  # 出力対象のXY軸を指定する　x_names(n) vs y_names(n)のグラフが出力される
  x_names <- c(rep('Year',length(indicators)),
               rep('GDP_Capita',length(indicators)) 
  ) # rep('REGION',length(indicators)),
  y_names <- c(rep(indicators,2)) #3
  # scenario_color <- c('#3366CC', '#66AA00', '#0099C6', '#DD4477', '#BB2E2E', '#990099', '#651067', '#22AA99')
  scenario_color <- c('#AAAA11', '#329262', '#FF9900', '#DD4477', '#651067', '#3366CC', '#84919E', '#dda0dd') # df_Graph登場順
  scenario_shape <- c(19,21,22,23,24,25,1,19)
  scenario_fill <- c('white','red','white','white','white','white','white','white')
  scenario_size <- c(1,4,4,4,4,4,4,1)
  scenario_shape1 <- c(19,19,21,21,21,21,21,19)
  scenario_shape2 <- c(19,21,22,23,24,25,1,19)
  axis_cutoff_prob <- 0.01   # 軸の表示において切り捨てる分位範囲 （0.01: 両端1% cutoff）
  
  # scenarionames <- levels(df_Graph$SCENARIO)    # c('Baseline','2C','1.5C','2.5C','WB2C') # 'Historical'
  scenarionames <- c('Multi') 
  for (scenarioname in scenarionames) { 
    if (scenarioname=='Multi') { 
      df_Graph_plot <- df_Graph
    } else if (scenarioname=='Historical') { 
      df_Graph_plot <- df_Graph %>% filter(SCENARIO=='Historical')
    } else {
      df_Graph_plot <- rbind(filter(df_Graph, SCENARIO==scenarioname),
                             filter(df_Graph, SCENARIO=='Historical'))
    }
    
    # Graph ------------------------------------------------------
    
    while (0) { # for (dummyloop in 1) { # XY散布図 by 17地域 bk
      pdf(file=paste("./",scenarioname,"_XY.pdf", sep=""))    
      for (num in 1:length(x_names)) {
        g <- eval(parse(text=paste0(
          "ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
          ",color=REGION, shape=SCENARIO)) +
              geom_line() +
              geom_point() + 
            # scale_color_manual(values=c(rep(scenario_color,3))) +
              scale_shape_manual(values=scenario_shape1)"))) # Historical2本
        plot(g)
        filename <- paste(scenarioname,num,"_",x_names[num],"-",y_names[num], sep="")
        ## ggsave(file=paste("./png/",filename,".png", sep=""), width=5, height=4, dpi=100)
      }
      dev.off() 
    } # XY散布図 by 17地域 bk
    
    while (0) { # 箱ヒゲ図  地域別
      pdf(file=paste("./",scenarioname,"_boxplot_Region.pdf", sep=""))    
      for (indicator in indicators) {
        g <- eval(parse(text=paste0(
          "ggplot(df_Graph_plot, aes(x=REGION ,y=",indicator, ", color=SCENARIO)) +
            geom_boxplot() +
            scale_x_discrete(limit=rev(scenarionames)) +
         
          # geom_jitter(shape=20, position=position_dodge(0.8)) +
            scale_color_manual(values=c(scenario_color)) ")))
        plot(g)
        filename <- paste(scenarioname,"_","boxplot_Region_",indicator, sep="")
        ## ggsave(file=paste("./png/",filename,".png", sep=""), width=5, height=4, dpi=100)
      }
      dev.off() 
    } # 箱ヒゲ図  地域別
    
    for (dummyloop in 1) { # 箱ヒゲ図  全世界
      pdf(file=paste("./",scenarioname,"_boxplot_World.pdf", sep=""))    
      for (indicator in indicators) {
        # indicator <- 'Henkaryo_Energy_Intensity' # for test
        filename <- paste(scenarioname,"_","boxplot_World_",indicator, sep="")
        
        sample_vec_data <- eval(parse(text=paste0( 
          "as.vector(df_Graph_plot$", indicator, ") %>% na.omit()"  # all SCENARIO
        )))
        yall_range <- quantile(sample_vec_data, probs=c(0.00, 1.00), na.rm=T)
        ylim_range <- quantile(sample_vec_data, probs=c(0.03, 0.97), na.rm=T)
        
        df_Graph_plot_HisR17 <- df_Graph_plot %>% filter(SCENARIO=='Historical_R17')
        sample_vec_data <- eval(parse(text=paste0( 
          "as.vector(df_Graph_plot_HisR17$", indicator, ") %>% na.omit()"
        )))
        window_range <- quantile(sample_vec_data, probs=c(0.05, 0.95), na.rm=T)
        
        if ( regexpr('^ChangeRate*', indicator)==1 ) { 
          ylim_range <- c(-0.11, 0.11) 
        } 
        g1 <- eval(parse(text=paste0(
          "ggplot(df_Graph_plot, aes(x=SCENARIO, y=",indicator, ", color=SCENARIO)) +
            geom_boxplot() +
            scale_x_discrete(limit=rev(scenarionames_order)) +  # 系列の順序 # x=SCENARIO 必要
          # guides(color = guide_legend(reverse = TRUE)) +      # 凡例の順序
            stat_boxplot(geom='errorbar') + # ヒゲ先端の横線
            scale_color_manual(values=c(scenario_color)) + 
            coord_flip(ylim = ylim_range) + 
            annotate('rect', alpha=.26, fill='#329262', 
            xmin=",5.8, ", ymin=",window_range[1], 
          ",xmax=",6.2, ", ymax=",window_range[2], ")  # HisR17:6th 6-0.2 6+0.2
          ")))
        plot(g1)
        ggsave(plot=g1, file=paste("./png/ylim/",filename,"_ylim.png", sep=""), width=6.3, height=2.5, dpi=100)
        
        g2 <- g1 + coord_flip(ylim = yall_range)
        plot(g2)
        ggsave(plot=g2, file=paste("./png/yall/",filename,".png", sep=""), width=6.3, height=2.5, dpi=100)
        
        g1 <- ggplotGrob(g1)
        g2 <- ggplotGrob(g2)
        gb <- rbind(g2, g1, size = "first")
        # gb$widths = grid::unit.pmax(g2$widths, g1$widths) # 今回の g1/g2 の組合せであれば不要
        plot(gb)
        ggsave(plot=gb, file=paste("./png/ylim_compare/",filename,"_ylim_compare.png", sep=""), width=6.3, height=5.0, dpi=100)
      } # indicator
      dev.off() 
    } # 箱ヒゲ図  全世界
    
    for (dummyloop in 1) { # 頻度分布
      pdf(file=paste("./",scenarioname,"_histogram.pdf", sep=""))    
      for (indicator in indicators) {
        g <- eval(parse(text=paste0(
          "ggplot(df_Graph_plot, aes(x=",indicator, ",color=SCENARIO)) +
          # geom_histogram(bins=50) + # 積み上げ
          # geom_histogram(bins=50, position='identity', alpha=0.3) + # 透過重ね
            geom_histogram(bins=50, position='dodge', alpha=0) + # 隣接バー
            ylab('Count of Region-Year') +
            scale_color_manual(values=c(rep(scenario_color,3))) ")))
        plot(g)
        filename <- paste(scenarioname,"_","histogram_",indicator, sep="")
        # ggsave(file=paste("./png/",filename,".png", sep=""), width=5, height=4, dpi=100)
        
        vec_data <- eval(parse(text=paste0("df_Graph_plot$",indicator))) 
        axis_range <- quantile(vec_data, probs=c(axis_cutoff_prob, (1-axis_cutoff_prob)), na.rm=T)
        g <- eval(parse(text=paste0(
          "ggplot(df_Graph_plot, aes(x=",indicator, ",color=SCENARIO)) +
           geom_histogram(bins=50, position='dodge', alpha=0) + # 隣接バー
            ylab('Count of Region-Year') +
          # xlim(",axis_range[1], ", ",axis_range[2], ") +
            xlim(-0.1,0.1) +
            scale_color_manual(values=c(scenario_color)) ")))
        plot(g)
        filename <- paste(scenarioname,"_","histogram_xlim_",indicator, sep="")
        ## ggsave(file=paste("./png/",filename,".png", sep=""), width=5, height=4, dpi=100)
      } 
      dev.off()
    } # 頻度分布
    
    for (dummyloop in 1) { # 確率密度分布
      
      df_Graph_plotXY_HisR <- df_Graph_plot %>% filter(SCENARIO=='Historical_R17')
      
      pdf(file=paste("./",scenarioname,"_density.pdf", sep=""))    
      for (indicator in indicators) {
        g <- eval(parse(text=paste0(
          "ggplot(df_Graph_plot, aes(x=",indicator, ",color=SCENARIO)) +
            geom_density(size=0.7) +
            scale_color_manual(values=c(scenario_color)) +
          # xlim(-0.2,0.2) +
            ylab('Density (Counts scaled to 1) of Region-Year')")))
        plot(g)
        filename <- paste(scenarioname,"_","density_",indicator, sep="")
        # ggsave(file=paste("./png/",filename,".png", sep=""), width=5, height=4, dpi=100)
        
        vec_data <- eval(parse(text=paste0("df_Graph_plot$",indicator))) 
        axis_range <- quantile(vec_data, probs=c(axis_cutoff_prob, (1-axis_cutoff_prob)), na.rm=T)
        g <- eval(parse(text=paste0(
          "ggplot(df_Graph_plot, aes(x=",indicator, ",color=SCENARIO)) +
            geom_density(size=0.7) +
            scale_color_manual(values=c(scenario_color)) +
          # xlim(",axis_range[1], ", ",axis_range[2], ") +
            xlim(-0.1,0.1) +
            ylab('Density (Counts scaled to 1) of Region-Year')")))
        
        cutoff_prob <- 0.05
        vec_data <- eval(parse(text=paste0("df_Graph_plotXY_HisR$",indicator))) 
        axis_range <- quantile(vec_data, probs=c(cutoff_prob, (1-cutoff_prob)), na.rm=T)
        g <- g + eval(parse(text=paste0( "annotate('rect', xmin=",axis_range[1],", ymin=",-Inf, 
                                         ", xmax=",axis_range[2],", ymax=",0, 
                                         ", alpha=.26, fill='#329262')"))) 
        plot(g)
        filename <- paste(scenarioname,"_","density_xlim_",indicator, sep="")
        ## ggsave(file=paste("./png/",filename,".png", sep=""), width=5, height=4, dpi=100)
        
      }
      dev.off() 
    } # 確率密度分布
    
    for (dummyloop in 1) { # XY散布図 by 17地域 vs 17地域 
      library(RColorBrewer)
      region17_color <- c(brewer.pal(5,"Dark2"),brewer.pal(8,"Accent"),brewer.pal(4,"Set1"))  
      # df_Graph_plotXY <- data_frame()
      df_Graph_plotXY <- df_Graph_plot %>% filter(SCENARIO!='Historical')
      df_Graph_plotXY_His <- df_Graph_plotXY %>% filter(SCENARIO=='Historical_R17')
      
      # write_csv(df_Graph_plotXY, "./df_Graph_plotXY.csv") 
      # write_csv(df_Graph_plotXY_His, "./df_Graph_plotXY_His.csv") 
      
      pdf(file=paste("./",scenarioname,"_XY_R17.pdf", sep=""))    
      for (num in 1:length(x_names)) { #num   
        g1 <- eval(parse(text=paste0(
          "ggplot(df_Graph_plotXY, aes(x=",x_names[num],",y=",y_names[num], 
          ",color=REGION, shape=SCENARIO)) +
              geom_point() + 
              geom_line() +
              scale_color_manual(values=c(rep(region17_color,3))) +
              scale_shape_manual(values=scenario_shape2)"))) # SCENARIO数
        
        # 窓の追加
        vec_data <- eval(parse(text=paste0("df_Graph_plotXY_His$",y_names[num]))) 
        axis_range <- quantile(vec_data, probs=c(0.05, (1-0.05)), na.rm=T)
        g1 <- g1 + eval(parse(text=paste0( "annotate('rect', xmin=",-Inf,", ymin=",axis_range[1], 
                                           ", xmax=",Inf, ", ymax=",axis_range[2], 
                                           ", alpha=.22, fill='#329262')"))) 
        plot(g1)
        filename <- paste(scenarioname,num,"_",x_names[num],"-",y_names[num], sep="")
        # ggsave(file=paste("./png/R17",filename,".png", sep=""), width=5, height=4, dpi=100)
        
        small <- 0.001
        x_axis_min <- min(eval(parse(text=paste0("df_Graph_plotXY$",x_names[num]))), na.rm=T)*(1-small)
        y_axis_min <- min(eval(parse(text=paste0("df_Graph_plotXY$",y_names[num]))), na.rm=T)*(1-small)
        x_axis_max <- max(eval(parse(text=paste0("df_Graph_plotXY$",x_names[num]))), na.rm=T)*(1+small)
        y_axis_max <- max(eval(parse(text=paste0("df_Graph_plotXY$",y_names[num]))), na.rm=T)*(1+small)
        
        g2 <- eval(parse(text=paste0(
          "ggplot(df_Graph_plotXY_His, aes(x=",x_names[num],",y=",y_names[num], 
          ",color=REGION, shape=SCENARIO)) +
              geom_point() + 
              geom_line() +
              xlim(",x_axis_min, ", ",x_axis_max, ") +
              ylim(",y_axis_min, ", ",y_axis_max, ") +
              scale_color_manual(values=c(rep(region17_color,3))) +
              scale_shape_manual(values=scenario_shape2)"))) # SCENARIO数
        plot(g2)
        
        if ( y_names[num]=='ChangeRate_Carbon_Intensity' ) { # CIのみylim範囲指定 旧(num==2)    
          g1 <- eval(parse(text=paste0(
            "g1 + xlim(",x_axis_min, ", ",x_axis_max, ") +
                ylim(",-0.5, ", ",0.1, ")"))) 
          plot(g1)
          g2 <- eval(parse(text=paste0(
            "g2 + ylim(",-0.05, ", ",0.05, ")"))) 
          plot(g2)
        } # CIのみ範囲指定
      } #num
      
      # Stylized Fact 用出力
      df_Graph_plotXY_Baseline <- df_Graph_plotXY %>% filter(SCENARIO %in% c('Historical_R17', 'Baseline'))
      yname <- c( 'Energy_Intensity_scaled','Carbon_Intensity_scaled','Electricity_Rate_Total_scaled')
      for (num in 1:length(yname)) { #num
        g <- eval(parse(text=paste0(
          "ggplot(df_Graph_plotXY_Baseline, aes(x=","GDP_Capita",",y=",yname[num], 
          ",color=REGION, shape=SCENARIO)) +
              geom_point() + 
              geom_line() +
              scale_color_manual(values=c(rep(region17_color,3))) +
              scale_shape_manual(values=scenario_shape2)"))) # SCENARIO数
        plot(g)
        
      } #num
      
      
      dev.off() 
    } # XY散布図 by 17地域 vs 17地域
    
    while (0) { # XY散布図 by 国別 for (dummyloop in 1)
      df_Graph_plot <- df_Graph_plot %>% ungroup() %>% group_by(Country,SCENARIO)
      pdf(file=paste("./",scenarioname,"_XY_Country.pdf", sep=""))    
      for (num in 1:length(x_names)) {
        g <- eval(parse(text=paste0(
          "ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
          ",color=REGION, shape=SCENARIO)) +
            geom_line() +
            geom_point() +
            scale_color_manual(values=c(rep(region17_color,3))) +
          # theme(legend.position='none') +
            scale_shape_manual(values=scenario_shape1)")))
        plot(g)
        filename <- paste(scenarioname,"_",num,"_",x_names[num],"-",y_names[num],"_CN", sep="")
        # ggsave(file=paste("./png/",filename,".png", sep=""), width=5, height=4, dpi=100)
      }
      dev.off() 
    } # XY散布図 by 国別
    
    while (0) { # 特定パターンのみの確率密度分布  for (dummyloop in 1)
      df_Graph_developed <- df_Graph_plot %>% filter(REGION %in% c('CAN','CIS','JPN','USA','XE25','XER')) # 先進国
      df_Graph_developing <- anti_join(df_Graph_plot, df_Graph_developed) # 先進国以外
      df_Graph_filtered <- df_Graph_developing # グラフ対象
      
      # library(outliers)
      # indicator <- c('ChangeRate_Carbon_Intensity')
      # x_range_all <- df_Graph_filtered %>% select(all_of(indicator)) %>% drop_na()
      # x_range_outliered <- df_Graph_filtered %>% select(all_of(indicator)  
      #                               ) %>% rm.outlier(fill=F, median=F, opposite=F
      #                               ) %>% drop_na()  # 外れ値を除外
      # anti_join(x_range_all, x_range_outliered)
      # semi_join(x_range_all, x_range_outliered)
      
      pdf(file=paste("./",scenarioname,"_density_filtered.pdf", sep=""))    
      for (indicator in indicators) {
        g <- eval(parse(text=paste0(
          "ggplot(df_Graph_filtered, aes(x=",indicator, ",color=SCENARIO)) +
            geom_density(size=0.7) +
            ylab('Density (Counts scaled to 1) of Region-Year') +
            scale_color_manual(values=c(rep(region17_color,3)))")))
        plot(g)
        filename <- paste(scenarioname,"_","density_filtered_",indicator, sep="")
        # ggsave(file=paste("./filtered/",filename,".png", sep=""), width=5, height=4, dpi=100)
        
        # 範囲指定のグラフ
        vec_data <- eval(parse(text=paste0("df_Graph_plot$",indicator))) 
        axis_range <- quantile(vec_data, probs=c(axis_cutoff_prob, (1-axis_cutoff_prob)), na.rm=T)
        g <- eval(parse(text=paste0(
          "ggplot(df_Graph_filtered, aes(x=",indicator, ",color=SCENARIO)) +
            geom_density(size=0.7) +
            xlim(",axis_range[1], ", ",axis_range[2], ") +  # 
            ylab('Density (Counts scaled to 1) of Region-Year') +
            scale_color_manual(values=c(rep(region17_color,3)))")))
        plot(g)
        ## ggsave(file=paste("./filtered/",filename,"_xlim.png", sep=""), width=5, height=4, dpi=100)
        
      }
      dev.off() 
    } # 特定パターンのみの確率密度分布
    
  } # scenarioname loop
} # グラフ出力


# 項目指定出力
scenarioname <- 'Multi'
x_names <- c(
  'Year', 'Energy_Intensity_scaled', 'TES_Total_scaled', 'GDP_IEA_scaled' 
  , 'Year', 'Carbon_Intensity_scaled', 'CO2_fuel_Total_scaled', 'TES_Total_scaled' 
  , 'Year', 'Electricity_Rate_Total_scaled', 'TFC_Elec_Total_scaled', 'TFC_Total_Total_scaled'
  , 'Year', 'Energy_Intensity_scaled', 'TES_Total_scaled', 'GDP_IEA_scaled' 
  , 'Year', 'Carbon_Intensity_scaled', 'CO2_fuel_Total_scaled', 'TES_Total_scaled' 
  , 'Year', 'Electricity_Rate_Total_scaled', 'TFC_Elec_Total_scaled', 'TFC_Total_Total_scaled'
) 
y_names <- c( rep('ChangeRate_Energy_Intensity',4)
              ,rep('ChangeRate_Carbon_Intensity',4)
              ,rep('ChangeRate_Electricity_Rate_Total',4)
              ,rep('Henkaryo_Energy_Intensity',4)
              ,rep('Henkaryo_Carbon_Intensity',4)
              ,rep('Henkaryo_Electricity_Rate_Total',4)
) 

for (dummyloop in 1) { # item 指定出力
  library(RColorBrewer)
  df_Graph_plotXY <- df_Graph_plot 
  df_Graph_plotXY <- df_Graph_plot %>% filter(SCENARIO!='Historical')
  df_Graph_plotXY_His <- df_Graph_plotXY %>% filter(SCENARIO=='Historical_R17')
  write_csv(df_Graph_plotXY, "./df_Graph_plotXY.csv") 
  write_csv(df_Graph_plotXY_His, "./df_Graph_plotXY_His.csv") 
  y_axis_val <- c(-0.5, 0.1)
  
  pdf(file=paste("./",scenarioname,"_XY_item.pdf", sep=""))    
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
              scale_color_manual(values=c(rep(region17_color,3))) +
              scale_shape_manual(values=scenario_shape2)"))) # SCENARIO数
    g <- g + geom_hline(yintercept=c(axis_range[1], axis_range[2])) + annotate_text 
    plot(g)
    
    filename <- paste(scenarioname,num,"_",x_names[num],"-",y_names[num], sep="")
    # ggsave(file=paste("./png/R17_",filename,".png", sep=""), width=5, height=4, dpi=100)
    
    if (y_names[num]=='ChangeRate_Carbon_Intensity') { #CI
      y_axis_top <- y_axis_val[2]-0.1*(y_axis_val[2]-y_axis_val[1])
      annotate_text_ylim <- eval(parse(text=paste0(
        "annotate('text', x=",x_axis_right,", y=",y_axis_top,", 
        label='y-percentile:" ,100*cutoff_prob,"-",100*(1-cutoff_prob),"%\n", 
        round(100*axis_range[2], digits=2),"%\n",
        round(100*axis_range[1], digits=2),"%\n')"
      ))) 
      g <- g + ylim(c(y_axis_val[1], y_axis_val[2])) + annotate_text_ylim 
      plot(g)
    } #CI
    
    if (x_names[num]=='Year') { #Year
      g <- eval(parse(text=paste0(
        "ggplot(df_Graph_plotXY_His, aes(x=",x_names[num],",y=",y_names[num], 
        ", shape=SCENARIO)) +
              geom_smooth(method=lm) +
              geom_point() + 
              xlim(",1970, ", ",2100, ") "))) 
      g <- g + geom_hline(yintercept=c(axis_range[1], axis_range[2]))
      g <- g + stat_poly_eq(formula=y~x, aes(label=paste(
        "atop(", paste(stat(eq.label),stat(rr.label),stat(adj.rr.label),sep="~~~"), ",",
        paste(stat(f.value.label),stat(p.value.label),stat(AIC.label),stat(BIC.label),sep="~~~"),
        ")", sep="")),
        label.x="right", parse=TRUE)
      plot(g)
    } #Year
  } #num
  dev.off() 
} # item 指定出力

