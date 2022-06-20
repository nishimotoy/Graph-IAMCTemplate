# 土木学会用日本語出力
# df_long_global <- aggregate(Value~VARIABLE+SCENARIO+Year, df_long, sum) # 集約対象=REGION
# write_csv(df_long_global, "./df_long_global_written.csv") 


df_Graph_global <- aggregate(CO2_fuel_Total_scaled~SCENARIO+Year, df_Graph, sum) # 集約対象=REGION
df_Graph_global_wide <- df_Graph_global %>% spread(key=SCENARIO, value=CO2_fuel_Total_scaled)

write_csv(df_Graph_global, "./df_Graph_global_written.csv") 
write_csv(df_Graph_global_wide, "./df_Graph_global_wide_written.csv") 

# df_Graph_plot <- df_Graph_global %>% filter(SCENARIO!='Historical')
# df_Graph_plot <- df_Graph_plot %>% mutate(CO2_fuel_Total_scaled=CO2_fuel_Total_scaled/1000000)

x_names <- c('Year', rep('GDP_Capita',3), rep('Year',6),
             'ChangeRate_Energy_Intensity','ChangeRate_Carbon_Intensity','ChangeRate_Electricity_Rate_Total' )
y_names <- c('CO2_fuel_Total_scaled', 
             rep(c('Energy_Intensity_scaled','Carbon_Intensity_scaled','Electricity_Rate_Total_scaled'),2), 
             c('ChangeRate_Energy_Intensity','ChangeRate_Carbon_Intensity','ChangeRate_Electricity_Rate_Total'),
             rep('Density',3))
I_names <- c('エネルギー強度', '炭素強度', '電化率')
I_names_CR <- paste(I_names, 'の変化率 (%)', sep = "")  # c('エネルギー強度の変化率', '炭素強度の変化率', '電化率の変化率')
y_names_J  <- c('エネルギー起源CO2排出量', rep(I_names,2), I_names_CR, rep('確率密度',3))
x_names_J  <- c('年', rep('GDP/人',3), rep('年',6), I_names_CR)
cutoff_percentile <- 0.05

scenario_color <- c('#AAAA11', '#329262', '#FF9900', '#DD4477', '#651067', '#3366CC', '#84919E')
library(RColorBrewer)
region_color <- c(brewer.pal(5,"Dark2"),brewer.pal(5,"Set1"),brewer.pal(7,"Paired"))  

y_names_tmp <- y_names[-which(y_names %in% 'Density')] 
df_Graph_p <- df_Graph   %>% select('SCENARIO', 'REGION', unique(sort(c(x_names,y_names_tmp)))) 
df_Graph_p <- df_Graph_p %>% mutate(Energy_Intensity_scaled=Energy_Intensity_scaled/1000 #kJ>MJ
                      ) %>% mutate(Carbon_Intensity_scaled=Carbon_Intensity_scaled*100  #10^-6>10^-8
                      ) %>% mutate(Electricity_Rate_Total_scaled=Electricity_Rate_Total_scaled*100 #percent
                      ) %>% mutate(ChangeRate_Energy_Intensity=ChangeRate_Energy_Intensity*100 #percent
                      ) %>% mutate(ChangeRate_Carbon_Intensity=ChangeRate_Carbon_Intensity*100 #percent
                      ) %>% mutate(ChangeRate_Electricity_Rate_Total=ChangeRate_Electricity_Rate_Total*100 #percent
                      )
df_Graph_p <- df_Graph_p %>% mutate(SCENARIO2 = recode(SCENARIO, 
                             Historical='歴史的推移(国別)', Historical_R17='歴史的推移', Baseline='ベースライン'))
df_Graph_p <- df_Graph_p %>% mutate(SCENARIO_f=SCENARIO) %>% mutate(SCENARIO=SCENARIO2) 

  pdf(file=paste("./png2/JSCE_Graph.pdf", sep=""))    
  for (num in 1:length(x_names)) {

    if ( num==1 ) { 
      df_Graph_plot <- df_Graph_global %>% filter(SCENARIO!='Historical' 
                                     ) %>% mutate(CO2_fuel_Total_scaled=CO2_fuel_Total_scaled/1000000) #kt>Gt-CO2

      g <- eval(parse(text=paste0("
              ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
                                  ",color=SCENARIO)) +
              geom_line(size=1.6) +
              scale_color_manual(values=c(scenario_color[-1])) 
      "))) 
      plot(g)
      
    } else if ( num>=2 && num<=4 ) { 
      
      df_Graph_plot <- df_Graph_p %>% filter(SCENARIO_f %in% c('Historical_R17', 'Baseline'))
      
      g <- eval(parse(text=paste0("
              ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
                                  ",color=REGION, shape=SCENARIO)) +
              geom_line() +
              geom_point() + 
              scale_color_manual(values=c(rep(region_color,3))) +
              scale_shape_manual(values=c(19,21))"))) # 'Historical_R17', 'Baseline'
      plot(g)
      
    } else if ( num>=5 && num<=10 ) { # XY散布図 by 17地域 vs 17地域
      
      df_Graph_plot <- df_Graph_p %>% filter(SCENARIO_f!='Historical')
      df_Graph_plot_HisR <- df_Graph_plot %>% filter(SCENARIO_f=='Historical_R17' )
      
      g <- eval(parse(text=paste0(
        "ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
        ",color=REGION, shape=SCENARIO)) +
              geom_point() + 
              geom_line() +
              scale_color_manual(values=c(rep(region_color,3))) +
              scale_shape_manual(values=c(19,21,22,23,24,25,1))"))) # SCENARIO数
      
      if ( num>=8 && num<=10 ) { # 窓の追加
        
        vec_data <- eval(parse(text=paste0("df_Graph_plot_HisR$",y_names[num]))) 
        percentile_val <- percentitle_range(vec_data, cutoff_percentile)
        g <- g + eval(parse(text=paste0( "annotate('rect', xmin=",-Inf,", ymin=", percentile_val[1], 
                                         ", xmax=",Inf, ", ymax=",percentile_val[2], 
                                         ", alpha=.125, fill='#329262')"))) 
        
      } # 窓の追加
      if ( num==9 ) { g <- g + ylim(-10, 5) } #炭素強度の例外処理
      plot(g)

    } else if ( num>=11 && num<=13 ) { # 確率密度分布
      
      df_Graph_plot <- df_Graph_p
      df_Graph_plot_HisR <- df_Graph_plot %>% filter(SCENARIO_f=='Historical_R17' )

      indicator <- x_names[num]

      # vec_data <- eval(parse(text=paste0("df_Graph_plot$",indicator))) 
      # axis_range_value <- percentitle_range(vec_data, axis_cutoff_percentile)
      g <- eval(parse(text=paste0(
          "ggplot(df_Graph_plot, aes(x=",indicator, ",color=SCENARIO)) +
            geom_density(size=0.7) +
            scale_color_manual(values=c(scenario_color)) +
            xlim(-10,10) + 
            ylab('Density (Counts scaled to 1) of Region-Year')")))
        
        vec_data <- eval(parse(text=paste0("df_Graph_plot_HisR$",indicator))) 
        percentile_val <- percentitle_range(vec_data, cutoff_percentile)
        g <- g + eval(parse(text=paste0( "annotate('rect', xmin=",percentile_val[1],", ymin=",-Inf, 
                                         ", xmax=",percentile_val[2], ", ymax=",Inf, 
                                         ", alpha=.2, fill='#329262')"))) 
        plot(g)
        
          
    }  # （仮）df_Graph_plotの切替
    
    

    if ( y_names_J[num]=='エネルギー起源CO2排出量' ) { 
      ylab_name <- bquote("エネルギー起源   " ~ CO[2] ~ "排出量  " ~ (Gt-CO[2])) #bquote内では 変数名は文字列とされる
    } else if ( y_names_J[num]=='エネルギー強度' ) { ylab_name <- paste(y_names_J[num], '(MJ/$)')
    } else if ( y_names_J[num]=='炭素強度' )     { ylab_name <- bquote('炭素強度　' ~ (10^-8 ~ CO[2]/J) )
    } else if ( y_names_J[num]=='電化率' )       { ylab_name <- paste(y_names_J[num], '(%)')
    } else { ylab_name <-  y_names_J[num] }

    g <- g + xlab(x_names_J[num]) + ylab(ylab_name) + theme_bw() + theme(  panel.grid = element_blank() )
      # + MyThemeLine
    plot(g)

    filename <- paste("JSCE",num,"_",x_names[num],"-",y_names[num], sep="") # 土木学会用出力
    ggsave(file=paste("./png2/",filename,".png", sep=""), width=5, height=4, dpi=100)
    ggsave(file=paste("./png3/",filename,".png", sep=""), width=5, height=7, dpi=100)
    
  } # num
  
  
  dev.off() 



