
# 見本＜ここから＞
dis <- function(x, y) {
  sa <- x - y              # 差を求める
  return(sa)
}

dis(30, 10)  # 使い方1
dis(y = 10, x = 30)  # 使い方2
# 見本＜ここまで＞



axis_cutoff_percentile <- 0.005    # 軸の表示で切り捨てる範囲（分位）
vec_indicator <- eval(parse(text=paste0("df_Graph_plot$",indicator))) 
axis_range <- function(vec_indicator, axis_cutoff_percentile) {
  axis_range_return <- c(quantile(na.omit(vec_indicator), axis_cutoff_percentile),
                        quantile(na.omit(vec_indicator), (1-axis_cutoff_percentile))
                ) %>% as.numeric()
  return(axis_range_return)
}

# 頻度分布
pdf(file=paste("./",scenarioname,"_histogram.pdf", sep=""))    
for (indicator in indicators) {
  g <- eval(parse(text=paste0(
    "ggplot(df_Graph_plot, aes(x=",indicator, ",color=SCENARIO)) +
        # geom_histogram(bins=50, position='identity', alpha=0.2) + # 
          geom_histogram(bins=50, position='dodge', alpha=0) + 
          ylab('Count of Region-Year') +
          scale_colour_gdocs() ")))
  plot(g)
  filename <- paste(scenarioname,"_","histogram_",indicator, sep="")
  ggsave(file=paste("./png/",filename,".png", sep=""), width=5, height=4, dpi=100)

  vec_indicator <- eval(parse(text=paste0("df_Graph_plot$",indicator))) 
  axis_range_value <- axis_range(vec_indicator, axis_cutoff_percentile)
  g <- eval(parse(text=paste0(
    "ggplot(df_Graph_plot, aes(x=",indicator, ",color=SCENARIO)) +
        # geom_histogram(bins=50, position='identity', alpha=0.2) + # 
          geom_histogram(bins=50, position='dodge', alpha=0) + 
          ylab('Count of Region-Year') +
          xlim(",axis_range_value[1], ", ",axis_range_value[2], ") + 
          scale_colour_gdocs() ")))
  plot(g)
  filename <- paste(scenarioname,"_","histogram_xlim_",indicator, sep="")
  ggsave(file=paste("./png/",filename,".png", sep=""), width=5, height=4, dpi=100)
  
    
  
}
dev.off() 


ggplot()  + xlim(",axis_range[1], ", ",axis_range[2], ")  

  