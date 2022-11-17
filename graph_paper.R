# 土木学会用日本語出力 査読対応

x_names <- c('Year', rep('GDP_Capita',3), rep('Year',6),
             'ChangeRate_Energy_Intensity','Henkaryo_Carbon_Intensity','Henkaryo_Electricity_Rate_Total' )
y_names <- c('CO2_fuel_Total_scaled', 
             rep(c('Energy_Intensity_scaled','Carbon_Intensity_scaled','Electricity_Rate_Total_scaled'),2), 
             c('ChangeRate_Energy_Intensity','Henkaryo_Carbon_Intensity','Henkaryo_Electricity_Rate_Total'),
             rep('Density',3))
I_names <- c('エネルギー強度', '炭素強度', '電化率')
I_names_attach <- c('の変化率 (%)', 'の変化量 (unit)', 'の変化量 (%)')
I_names_C <- paste(I_names, I_names_attach, sep = "") 
y_names_J  <- c('エネルギー起源CO2排出量', rep(I_names,2), I_names_C, rep('確率密度',3), I_names_C)
x_names_J  <- c('年', rep('GDP/人',3), rep('年',6), I_names_C)
window_prob <- 0.05
cutoff_prob <- 0.04

y_names_box <- c('ChangeRate_Energy_Intensity','Henkaryo_Carbon_Intensity','Henkaryo_Electricity_Rate_Total',
                 'Henkaryo_Electricity_Rate_Ind','Henkaryo_Electricity_Rate_Tra',
                 'Henkaryo_Electricity_Rate_Res','Henkaryo_Electricity_Rate_Com')
sec_names <- c('工業部門', '交通部門', '家庭部門', '業務部門')
j_names_CR_sec  <- paste(sec_names, '電化率の変化量 (%)', sep = "　") 
j_names_box　 <- c(I_names_C, j_names_CR_sec)
names(j_names_box) <- y_names_box  # j_names_box[names(j_names_box)]

library(RColorBrewer)
region_color <- c(brewer.pal(5,"Dark2"),brewer.pal(5,"Set1"),brewer.pal(7,"Paired"))  
scenario_color <- c('#AAAA11', '#329262', '#FF9900', '#DD4477', '#651067', '#3366CC', '#84919E')
scenario_shape <- c(19,4,22,23,24,25,1)
scenario_size <- c(1,4,4,4,4,4,4)

y_names_tmp <- y_names[-which(y_names %in% 'Density')] 
df_Graph_p <- df_Graph   %>% select('SCENARIO', 'REGION', unique(sort(c(x_names,y_names_tmp, y_names_box)))) 
df_Graph_p <- df_Graph_p %>% mutate(Energy_Intensity_scaled=Energy_Intensity_scaled/1000 #kJ>MJ
                      ) %>% mutate(Carbon_Intensity_scaled=Carbon_Intensity_scaled*100  #10^-6>10^-8
                      ) %>% mutate(Electricity_Rate_Total_scaled=Electricity_Rate_Total_scaled*100 #percent
                      ) %>% mutate(ChangeRate_Energy_Intensity=ChangeRate_Energy_Intensity*100 #percent
                      ) %>% mutate(Henkaryo_Carbon_Intensity=Henkaryo_Carbon_Intensity*100  #10^-6>10^-8
                      ) %>% mutate(Henkaryo_Electricity_Rate_Total=Henkaryo_Electricity_Rate_Total*100 #percent
                      ) %>% mutate(Henkaryo_Electricity_Rate_Ind=Henkaryo_Electricity_Rate_Ind*100 #percent
                      ) %>% mutate(Henkaryo_Electricity_Rate_Tra=Henkaryo_Electricity_Rate_Tra*100 #percent
                      ) %>% mutate(Henkaryo_Electricity_Rate_Res=Henkaryo_Electricity_Rate_Res*100 #percent
                      ) %>% mutate(Henkaryo_Electricity_Rate_Com=Henkaryo_Electricity_Rate_Com*100 #percent
                      ) # which
df_Graph_p <- df_Graph_p %>% mutate(SCENARIO2 = recode(SCENARIO, 
                             Historical='歴史的推移\n(国レベル)', Historical_R17='歴史的推移', Baseline='ベースライン'))
df_Graph_p <- df_Graph_p %>% mutate(SCENARIO_f=SCENARIO) %>% mutate(SCENARIO=SCENARIO2) 
df_Graph_p <- df_Graph_p %>% mutate(REGION2 = recode(REGION
                                                     , USA='アメリカ合衆国' , XE25='EU25' , XER='その他のヨーロッパ' , TUR='トルコ' 
                                                     , XOC='オセアニア' , CHN='中国' , IND='インド' , JPN='日本' , XSE='東南アジア' 
                                                     , XSA='その他のアジア' , CAN='カナダ' , BRA='ブラジル' , XLM='その他の南アメリカ' 
                                                     , CIS='CIS諸国' , XME='中東' , XNF='北アフリカ' , XAF='その他のアフリカ' 
))
df_Graph_p <- df_Graph_p %>% mutate(REGION_f=REGION) %>% mutate(REGION=REGION2) 

df_Graph_global <- aggregate(CO2_fuel_Total_scaled~Year+SCENARIO_f+SCENARIO, df_Graph_p, sum) # 集約対象=REGION
df_Graph_global_wide <- df_Graph_global %>% spread(key=SCENARIO_f, value=CO2_fuel_Total_scaled)
write_csv(df_Graph_global, "./df_Graph_global.csv") 
write_csv(df_Graph_global_wide, "./df_Graph_global_wide.csv") 

unlink("./png2", recursive=T)
unlink("./png3", recursive=T)
if(!dir.exists("./png2")){ dir.create("./png2") }
if(!dir.exists("./png3")){ dir.create("./png3") }

pdf(file=paste("./png2/JSCE_Graph.pdf", sep=""))    
for (num in 1:length(x_names)) { #num # XYグラフの出力

  # 図1　世界の排出量  
  if ( num==1 ) {
    df_Graph_plot <- df_Graph_global %>% filter(SCENARIO_f!='Historical' 
    ) %>% mutate(CO2_fuel_Total_scaled=CO2_fuel_Total_scaled/1000000) #kt>Gt-CO2
    
    g <- eval(parse(text=paste0("
              ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
                                ", color=SCENARIO, size=SCENARIO)) +
              geom_line() +
              labs(color='シナリオ',size='シナリオ') +  # 凡例を指定しながらまとめる
              scale_color_manual(values=c(scenario_color[-1]))+
              scale_size_manual(values=c(2.2, rep(1.2, length(df_Graph_plot$SCENARIO)-1))) 
      "))) 
    plot(g)
    
  # 図2　GDP/人 vs 指標
  } else if ( num>=2 && num<=4 ) { 
    
    df_Graph_plot <- df_Graph_p %>% filter(SCENARIO_f %in% c('Historical_R17', 'Baseline'))
    
    g <- eval(parse(text=paste0("
              ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
                                ",color=REGION, shape=SCENARIO)) +
              geom_point() + 
              labs(color='地域 (a-c)共通') +
              labs(shape='シナリオ (a-c)共通',size='シナリオ (a-c)共通') +
              scale_color_manual(values=c(rep(region_color,3))) +
              scale_shape_manual(values=scenario_shape) + 
              geom_line() +
              scale_size_manual(values=c(5.0, rep(1.0, length(scenarionames_order)-1)))  # 無効?
              "))) 
    plot(g)
    
  # 図3 (a1)～(b3)　年 vs 指標・その変化率/変化量
  } else if ( num>=5 && num<=10 ) { # XY散布図 by 17地域 vs 17地域
    
    df_Graph_plot <- df_Graph_p %>% filter(SCENARIO_f!='Historical')
    df_Graph_plot_HisR <- df_Graph_plot %>% filter(SCENARIO_f=='Historical_R17' )
    
    g <- eval(parse(text=paste0(
      "ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
      ",color=REGION, shape=SCENARIO)) +
              geom_point() + 
              geom_line() +
              labs(color='地域 (a1-b3)共通') +
              labs(shape='シナリオ (a1-b3)共通') +
              scale_color_manual(values=c(rep(region_color,3))) +
              scale_shape_manual(values=scenario_shape)"))) # SCENARIO数
    
    # 図3 (b1)～(b3)　窓の追加
    if ( num>=8 && num<=10 ) { # 窓の追加
      
      vec_data <- eval(parse(text=paste0("df_Graph_plot_HisR$",y_names[num]))) 
      window_range <- quantile(vec_data, probs=c(window_prob, (1-window_prob)), na.rm=T)
      g <- g + eval(parse(text=paste0( "annotate('rect', xmin=",-Inf,", ymin=",window_range[1], 
                                       ", xmax=",Inf, ", ymax=",window_range[2], 
                                       ", alpha=.22, fill='#329262')"))) 
      
    } # 窓の追加
    # if ( num==9 ) { g <- g + ylim(-10, 5) } #炭素強度の例外処理
    plot(g)
    
  # 図3 (c1)～(c3)　確率密度分布
  } else if ( num>=11 && num<=13 ) { # 確率密度分布
    
    df_Graph_plot <- df_Graph_p
    df_Graph_plot_HisR <- df_Graph_plot %>% filter(SCENARIO_f=='Historical_R17' )
    
    indicator <- x_names[num]
    
    g <- eval(parse(text=paste0(
      "ggplot(df_Graph_plot, aes(x=",indicator, ",color=SCENARIO)) +
            geom_density(size=0.7) +
            labs(color='シナリオ (c1-3)共通') +
            scale_color_manual(values=c(scenario_color)) +
          # xlim(-10,10) + 
            ylab('Density (Counts scaled to 1) of Region-Year')")))
    
    vec_data <- eval(parse(text=paste0("df_Graph_plot_HisR$",indicator))) 
    window_range <- quantile(vec_data, probs=c(window_prob, (1-window_prob)), na.rm=T)
    g <- g + eval(parse(text=paste0( "annotate('rect', xmin=",window_range[1],", ymin=",-Inf, 
                                     ", xmax=",window_range[2], ", ymax=",Inf, 
                                     ", alpha=.22, fill='#329262')"))) 
    vec_data <- eval(parse(text=paste0("df_Graph_plot$",indicator))) 
    axis_range <- quantile(vec_data, probs=c(cutoff_prob, (1-cutoff_prob)), na.rm=T)
    g <- g + eval(parse(text=paste0( "xlim(",axis_range[1],", ",axis_range[2],")"))) 
    plot(g)
    
  }  # 確率密度分布　
  
  if ( y_names_J[num]=='エネルギー起源CO2排出量' ) { 
    ylab_name <- expression("エネルギー起源   " ~ CO[2] ~ "排出量  " ~ (Gt-CO[2])) #bquote内では 変数名は文字列とされる
  } else if ( y_names_J[num]=='エネルギー強度' ) { ylab_name <- paste(y_names_J[num], '(MJ/$)')
  } else if ( y_names_J[num]=='電化率' )       { ylab_name <- paste(y_names_J[num], '(%)')
  } else if ( y_names_J[num]=='炭素強度' )     { ylab_name <- bquote('炭素強度　' ~ (10^-8 ~ CO[2]/J) )
  } else if ( y_names_J[num]=='炭素強度の変化量 (unit)' ) { ylab_name <- bquote('炭素強度の変化量　' ~ (10^-8 ~ CO[2]/J) )
  } else { ylab_name <-  y_names_J[num] }
  
  g <- g + xlab(x_names_J[num]) + ylab(ylab_name) + theme_bw() + theme(panel.grid = element_blank()) # + MyThemeLine
  plot(g)
  
  filename <- paste("JSCE",num,"_",x_names[num],"-",y_names[num], sep="") # 土木学会用出力
  if ( num>=11 && num<=13 ) { 
    ggsave(file=paste("./png2/",filename,".png", sep=""), width=5.35, height=3.6, dpi=100) # 確率密度分布
  } else { 
    ggsave(file=paste("./png2/",filename,".png", sep=""), width=5.34, height=3.6, dpi=100) # XYグラフ
    ggsave(file=paste("./png3/",filename,".png", sep=""), width=5, height=7, dpi=100) # 凡例の出力(縦長)
  } 

}  #num # XYグラフの出力

# 図3 (d1)～(d3)　図4　箱ヒゲ図
for (indicator in y_names_box) { # indicator # 箱ヒゲ図
  df_Graph_plot <- df_Graph_p 
  df_Graph_plot_HisR <- df_Graph_plot %>% filter(SCENARIO_f=='Historical_R17' )
  
  g <- eval(parse(text=paste0(
    "ggplot(df_Graph_plot, aes(x=SCENARIO, y=",indicator, ", color=SCENARIO)) +
            geom_boxplot() +
            stat_boxplot(geom='errorbar', width=0.3) + # ヒゲ先端の横線
            scale_color_manual(values=c(scenario_color)) ")))
  
  vec_data <- eval(parse(text=paste0("df_Graph_plot_HisR$",indicator))) 
  window_range <- quantile(vec_data, probs=c(window_prob, (1-window_prob)), na.rm=T)
  g <- g + eval(parse(text=paste0( "annotate('rect', xmin=",1.8,", ymin=",window_range[1], 
                                   ", xmax=",2.2, ", ymax=",window_range[2], 
                                   ", alpha=.26, fill='#329262')"))) 
  vec_data <- eval(parse(text=paste0("df_Graph_plot$",indicator))) 
  axis_range <- quantile(vec_data, probs=c(cutoff_prob, (1-cutoff_prob)), na.rm=T)
  g <- g + eval(parse(text=paste0( "ylim(",axis_range[1],", ",axis_range[2],")"))) 
  g <- g + guides(color=guide_legend(reverse=TRUE))
  g <- g + xlab('') + ylab(j_names_box[indicator]) + labs(color='シナリオ (a-d)共通')
  ggsave(file=paste("./png3/",filename,"_legend.png", sep=""), width=5.0, height=2.5, dpi=100) # 凡例出力（仮）
  g <-  g + theme_bw() + theme(legend.position="none", panel.grid=element_blank()) 
                       # legend.positionとpanel.grid の順番が逆だとNG
  plot(g)
  num <- num+1
  filename <- paste("JSCE",num,"_", indicator, sep="") # 土木学会用出力
  ggsave(file=paste("./png2/",filename,".png", sep=""), width=4.55, height=2, dpi=100) # 箱ヒゲ図 width=4.56

} # indicator # 箱ヒゲ図

dev.off() 



