# 参考資料コピー
# 各群の95%信頼区間を比較するグラフをつくる

xtreat <-c(5.23, 6.12, 5.79)
placebo <-c(4.02, 3.75, 3.68)

cixtreat <- t.test(xtreat)$conf.int
ciplacebo <- t.test(placebo)$conf.int
dotchart(c(mean(xtreat),mean(placebo)),pch = 16,
             xlim=range(cixtreat,ciplacebo),xlab="タンパク質Aの発現(a.u.)")
arrows(cixtreat[1],1,cixtreat[2],1,length=0.05,angle=90,code=3)
arrows(ciplacebo[1],2,ciplacebo[2],2,length=0.05,angle=90,code=3)
mtext(c("Xtreat","placebo"),side=2,at=1:2,line=0.5,las=1,)

# df_Graph にて書き下し（見本に忠実ver）

CI1 <- df_Graph %>% filter(SCENARIO=='Historical')
CI2 <- df_Graph %>% filter(SCENARIO=='Baseline')
CI3 <- df_Graph %>% filter(SCENARIO=='2C')

CI01 <- CI1$ChangeRate_Carbon_Intensity
CI02 <- CI2$ChangeRate_Carbon_Intensity
CI03 <- CI3$ChangeRate_Carbon_Intensity

CI01 <- CI01 %>% na.omit()
CI02 <- CI02 %>% na.omit()
CI03 <- CI03 %>% na.omit()

ciCI01 <- t.test(CI01)$conf.int
ciCI02 <- t.test(CI02)$conf.int
ciCI03 <- t.test(CI03)$conf.int
dotchart(c(mean(CI01),mean(CI02),mean(CI03)), pch = 16,
         xlim=range(ciCI01,ciCI02,ciCI03),xlab="Carbon Intensity Change Rate")
arrows(ciCI01[1],1,ciCI01[2],1,length=0.05,angle=90,code=3)
arrows(ciCI02[1],2,ciCI02[2],2,length=0.05,angle=90,code=3)
arrows(ciCI03[1],3,ciCI03[2],3,length=0.05,angle=90,code=3)
mtext(c("Historical","Baseline","2C"),side=2,at=1:3,line=0.5,las=1)

# EI <- df_Graph_tmp$ChangeRate_Energy_Intensity
# ER <- df_Graph_tmp$ChangeRate_Electricity_Rate_Total


# df_Graph にて書き下し（整理ver）
while (0) {  # 旧
  df_CI <- df_Graph %>% select('SCENARIO','ChangeRate_Carbon_Intensity'
  ) %>% na.omit(
  ) %>% mutate(id = rownames(df_CI)
  ) %>% spread('SCENARIO', value='ChangeRate_Carbon_Intensity'
  ) %>% select(-id)
  write_csv(df_CI, "./df_CI_written.csv") 
  
  ciCI01 <- t.test(na.omit(df_CI$Historical))$conf.int
  
  CI01 <- na.omit(df_CI$'Historical')
  CI02 <- na.omit(df_CI$'Baseline')
  CI03 <- na.omit(df_CI$'2.5C')
  CI04 <- na.omit(df_CI$'2C')
  CI05 <- na.omit(df_CI$'1.5C')
  CI06 <- na.omit(df_CI$'WB2C')
  
  # CI01 <- CI %>% filter(SCENARIO=='Historical') %>% select(-'SCENARIO') %>% row.names() %>% as.vector() 
  ## CI01 <- as.vector(select(filter(CI,SCENARIO=='Historical'),-'SCENARIO'))
  
  ciCI01 <- t.test(CI01)$conf.int
  ciCI02 <- t.test(CI02)$conf.int
  ciCI03 <- t.test(CI03)$conf.int
  ciCI04 <- t.test(CI04)$conf.int
  ciCI05 <- t.test(CI05)$conf.int
  ciCI06 <- t.test(CI06)$conf.int
  
  
  dotchart(c(mean(CI01),mean(CI02),mean(CI03),mean(CI04),mean(CI05),mean(CI06)), pch = 16,
           xlim=range(ciCI01,ciCI02,ciCI03,ciCI04,ciCI05,ciCI06),xlab="Carbon Intensity Change Rate")
  arrows(ciCI01[1],1,ciCI01[2],1,length=0.05,angle=90,code=3)
  arrows(ciCI02[1],2,ciCI02[2],2,length=0.05,angle=90,code=3)
  arrows(ciCI03[1],3,ciCI03[2],3,length=0.05,angle=90,code=3)
  arrows(ciCI04[1],4,ciCI03[2],4,length=0.05,angle=90,code=3)
  arrows(ciCI05[1],5,ciCI03[2],5,length=0.05,angle=90,code=3)
  arrows(ciCI06[1],6,ciCI03[2],6,length=0.05,angle=90,code=3)
  mtext(scenarionames,side=2,at=1:6,line=0.5,las=1)
} # 旧


# 変数にてloop

root <- 'C:/_Nishimoto/R/WBAL_R02/'
setwd(paste(root,"4_output/", sep="")) 
# pdf(file=paste("./","conf_interval.pdf", sep=""))   # PDF出力開始

scenarionames <- levels(df_Graph$SCENARIO) %>% as.vector()    # c('Baseline','2C','1.5C','2.5C','WB2C') # 'Historical'
ci_indicators <- c('ChangeRate_Energy_Intensity','ChangeRate_Carbon_Intensity','ChangeRate_Electricity_Rate_Total') 
mean_list <- c()
df_CI_all <- data.frame()
matrix_conf_CI <- matrix(nrow=length(scenarionames), ncol=2)
  
# for (ci_indicator in ci_indicators) { # 変数名でloop
  ci_indicator <- ci_indicators[2]

  # scenarioname <- scenarionames[1]
  # for (scenarioname in scenarionames) { # シナリオ名でloop
  for (num in 1:length(scenarionames)) { # シナリオNo. でloop

    df_CI <- df_Graph %>% select('SCENARIO', all_of(ci_indicator)
                    ) %>% filter(SCENARIO==scenarionames[num] 
                    ) %>% na.omit(
                    ) %>% select(-'SCENARIO')
    vec_CI <- df_CI %>% unlist() %>% as.vector(mode='numeric') 
    conf_CI <- t.test(vec_CI)$conf.int
    matrix_conf_CI[num,] <- conf_CI
    range
    
    mean_CI <- mean(vec_CI)
    mean_list <- append(mean_list, mean_CI)
  #  df_CI_all <- rbind(df_CI_all,df_CI)
  }  # シナリオでloop

    g <- dotchart(mean_list, pch=16, xlim=range(matrix_conf_CI),xlab=ci_indicator)
    for (num in 1:length(scenarionames)) { # シナリオNo.でloop
      g <-  arrows(matrix_conf_CI[num,1],num,matrix_conf_CI[num,2],num,length=0.05,angle=90,code=3)
    }  # シナリオでloop
    g <- mtext(scenarionames,side=2,at=1:length(scenarionames),line=0.5,las=1)
    # plot(g)
    filename <- paste("Conf_",ci_indicator,"_", sep="")
    # ggsave(file=paste("./png/",filename,".png", sep=""))
    
# }  # 変数名でloop

# dev.off() # PDF出力終了
    
