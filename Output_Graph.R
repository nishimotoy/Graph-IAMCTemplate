# Global path 算出用
# df_long_global <- aggregate(Value~VARIABLE+SCENARIO+Year, df_long, sum) # 集約対象=REGION
# write_csv(df_long_global, "./df_long_global_written.csv") 


# df_Graph_global <- df_Graph %>% aggregate(x=c('CO2_fuel_Total_scaled'), by=list(c('SCENARIO', 'Year'), sum) # 集約対象=REGION

df_Graph_global <- aggregate(CO2_fuel_Total_scaled~SCENARIO+Year, df_Graph, sum) # 集約対象=REGION
df_Graph_global_wide <- df_Graph_global %>% spread(key=SCENARIO, value=CO2_fuel_Total_scaled)

write_csv(df_Graph_global, "./df_Graph_global_written.csv") 
write_csv(df_Graph_global_wide, "./df_Graph_global_wide_written.csv") 

df_Graph_plot <- df_Graph_global %>% filter(SCENARIO!='Historical')
df_Graph_plot <- df_Graph_plot %>% mutate(CO2_fuel_Total_scaled=CO2_fuel_Total_scaled/1000000)

x_names <- c('Year')
y_names <- c('CO2_fuel_Total_scaled')
x_names_J <- c('年')
y_names_J <- c('CO2_fuel_Total_scaled')

scenario_color <- c('#AAAA11', '#329262', '#FF9900', '#DD4477', '#651067', '#3366CC', '#84919E')

for (dummyloop in 1) { # XY散布図 
  # pdf(file=paste("./",scenarioname,"_XY.pdf", sep=""))    
  for (num in 1:length(x_names)) {
    g <- eval(parse(text=paste0("
      ggplot(df_Graph_plot, aes(x=",x_names[num],",y=",y_names[num], 
              ",color=SCENARIO), shape=SCENARIO) +
              geom_line(size=2) +
            # geom_point() + 
              scale_color_manual(values=c(scenario_color[-1])) +
              xlab('') +
              ylab('')
      "))) 
#   g <- g + xlab('年') + ylab(expression(x[2]))
#   g <- g + xlab('年') + ylab(expression(paste(CO[2]," emissions reduction rate (%)")))
#   g <- g + xlab('年') + ylab(expression(paste("エネルギー起源   ",CO[2],"排出量  ",(Gt-CO[2]))))

    test <- bquote("エネルギー起源   " ~ CO[2] ~ "排出量  " ~ (Gt-CO[2]))
    g <- g + xlab('年') + ylab(test)
    plot(g)
    

    filename <- paste("JSCE",num,"_",x_names[num],"-",y_names[num], sep="") # 土木学会用出力
    ggsave(file=paste("./png2/",filename,"_global.png", sep=""), width=5, height=4, dpi=100)
  }
  
  
  # dev.off() 
} # XY散布図 


