###四种SP实验设计策略的效度比较，具体见我的PPT吧


#需要加载的包
library(ggplot2)
library(DoE.base)
require(mlogit)
require(Cairo)
library(Cairo)


###这是一键生成的最大函数，里面嵌套了一些小函数
mall <- function(hehe5,hehe6,nn0,dumm){
  
  ###函数1A：提取成对正交表
  form_g <- function(hehe)
  {
    smp <- data.frame(X=1:nll,Num=0,Pic=0,Alt=0,score=0,choice=0,Prob=0)
    smp1 <- data.frame(matrix(0,nll,(nf+1)))
    smp <- cbind(smp,smp1)
    
    
    for (i in 1 : nl)
    {
      for (j in 1:nf)
      {
        smp[3*i-2,j+l-1] <- hehe5[i,j]
      }
      for (j in 1:nf)
      {
        smp[3*i-1,j+l-1] <- hehe5[i,j+nf]
      }
      
      smp2 <- smp
      if (n0 >= 2){
        for (i in 1:(n0-1))
        {
          smp2 <- rbind(smp2,smp)
        }
      }
    }
    
    smp2 <- rbind(smp2,smp2)
    smp2[,4] <- 1:3
    return(smp2)
  } 
  
  
  ###函数1B：随机生成表格
  form_sj <- function(v2)
  {
    v1 <- data.frame('A'=1:n1,'B'=0)
    nnn <- 1:nsj
    v1[,1] <-sample(nnn,n1,replace=T)
    v1[,2] <-sample(nnn,n1,replace=T)
    for (i in 1 : n1)
    {
      while( v1[i,1] == v1[i,2]){
        v1[i,2] <- sample(nnn,1)
        i <- i}
    }
    
    
    result <- data.frame(X=1:nn1,Num=0,Pic=0,Alt=0,score=0,choice=0,Prob=0)
    result1 <- data.frame(matrix(0,nn1,(nf+1)))
    result <- cbind(result,result1)
    
    
    for (i in 1:n1)
    {
      result[3*i-2,3] <- v1[i,1]
      result[3*i-1,3] <- v1[i,2]
      result[3*i,3] <- 0
      
      result$Alt[(3*i-2):(3*i)] <- 1:3
      
      for (j in l:(l+nf-1))
      {
        result[3*i-2,j] <- as.numeric(v2[as.numeric(rownames(v2)) == v1[i,1],j-l+1])
        result[3*i-1,j] <- as.numeric(v2[as.numeric(rownames(v2)) == v1[i,2],j-l+1])
      }
    }
    return(result)
  }
  
  
  ###函数1C：完全随机生成表
  fullran <- function(nn1)
  {
    nnn2 <- 1:sp
    result <- data.frame(X=1:(nn1),Num=0,Pic=0,Alt=0,score=0,choice=0,Prob=0)
    result1 <- data.frame(matrix(0,nn1,(nf+1)))
    for (i in 1:n1)
    {
      result1[3*i-2,] <- sample(nnn2,nf+1,replace = T)
      result1[3*i-1,] <- sample(nnn2,nf+1,replace = T)
    }
    result1[,nf+1] <- 0
    result <- cbind(result,result1)
    result[,4] <- 1:3
    return(result)
  }
  
  
  ###函数1D：成对-完全随机生成表
  cd_ran <- function(hehe)
  {
    smp <- data.frame(X=1:nll,Num=0,Pic=0,Alt=0,score=0,choice=0,Prob=0)
    smp1 <- data.frame(matrix(0,nll,(nf+1)))
    smp <- cbind(smp,smp1)
    
    
    for (i in 1 : nl)
    {
      for (j in 1:nf)
      {
        smp[3*i-2,j+l-1] <- hehe5[i,j]
      }
      for (j in 1:nf)
      {
        smp[3*i-1,j+l-1] <- hehe5[i,j+nf]
      }
      
      smp2 <- smp
      if (n0 >= 2){
        for (i in 1:(n0 -1))
        {
          smp2 <- rbind(smp2,smp)
        }
      }
    }
    smp2[,4] <- 1:3
    
    nnn2 <- 1:sp
    result <- data.frame(X=1:nrow(smp2),Num=0,Pic=0,Alt=0,score=0,choice=0,Prob=0)
    result1 <- data.frame(matrix(0,nrow(smp2),(nf+1)))
    for (i in 1:(nrow(result1)/3))
    {
      result1[3*i-2,] <- sample(nnn2,nf+1,replace = T)
      result1[3*i-1,] <- sample(nnn2,nf+1,replace = T)
    }
    result1[,nf+1] <- 0
    result <- cbind(result,result1)
    result[,4] <- 1:3
    
    smp2 <- rbind(smp2,result)
    return(smp2)
    
  } 
  
  
  
  
  ###函数2：模拟选择
  cp_simu <- function(result){
    
    #赋参数
    result2 <- result
    for (i in (1:nn1))
    {
      for (j in l:(l+nf))
      {
        if (result[i,j] ==1){result2[i,j] <- 0}
        if (result[i,j] ==2){result2[i,j] <- 1}
        if (result[i,j] ==3){result2[i,j] <- 2}
      }
    }  
    
    #给虚拟变量赋参数
    c <- c(0,0,dum)
    result2[, (l+nf)] <- c                                            
    
    #算得分
    result2[,1] <- 1:nrow(result2)
    
    for (i in 1:nn1)
    {
      for (j in l :(l+nf) )
      {
        result2$score[i] <- as.numeric(result2[i,j]) +result2$score[i]
      }
      result2$Num[i] <- ceiling( i/3)
    }
    
    sums <- data.frame(1:n1)
    for (i in 1:n1)
    {
      sums[i,1] <- (exp(result2$score[3*i-2]) + exp(result2$score[3*i-1]) + exp(result2$score[3*i]))
    }
    
    #算概率
    y <- 1
    for (i in 1:nn1)
    {
      if (result2$Num[i] == y){result2$Prob[i] <- (exp(result2$score[i])/sums[y,1])
      }else 
      {
        y <- y+1
        result2$Prob[i] <- (exp(result2$score[i])/sums[y,1])
      }
    }
    
    ###模拟做选择(gai)
    re1 <- runif(n=n1,min=0,max=1)#产生随机数
    for (i in 1:n1)
    {
      #result2$Alt[(3*i-2):(3*i)] <- 1:3
      
      if (re1[i] <= result2$Prob[3*i-2]) { result2$choice[3*i-2] <- 1
      }else
      {
        if ((re1[i] >= result2$Prob[3*i-2]) & (re1[i] <= (result2$Prob[3*i-1] +result2$Prob[3*i-2]) )) {result2$choice[3*i-1] <- 1
        }else
        {
          if (re1[i] >= (result2$Prob[3*i-1] +result2$Prob[3*i-2])) {result2$choice[3*i] <- 1}
        }
      }
    }
    
    
    result3 <- result2
    for (j in l:ncol(result3))
    {
      result3[,j] <- 0
    }
    
    
    #对模拟结果做模型表
    k <- l
    for (i in 1:nf)
    {
      x <- 0
      result3 <- cbind(result3[,1:k],x,result3[,(k+1):ncol(result3)])
      colnames(result3)[k+1] <- paste(colnames(result3[k]),"b",sep = "")
      colnames(result3)[k] <- paste(colnames(result3[k]),"a",sep = "")
      k <- k+2
    }
    colnames(result3)[ncol(result3)] <- "dummy"
    
    x <- 1
    for (i in (1:nrow(result3)) )
    {
      # result3[i,1] <- i
      
      # for (j in (2:(l-1)))
      # {
      #   result3[i,j] <- result2[i,j]
      # }
      
      k<-0
      for (j in 1:nf)
      {
        # if (result2[i,j+l-1] == 0) { x <-x }
        if (result2[i,j+l-1] == 1) { result3[i,j+l+k-1] <- 1}
        if (result2[i,j+l-1] == 2) { result3[i,j+l+k] <- 1}
        k <- k+1
      } 
      # result3[i,ncol(result3)] <- result2[i,(l+nf)]
    }
    
    # result3[result3[,ncol(result3)] == dum,ncol(result3)] <- 1
    c <- c(0,0,1)
    result3[,ncol(result3)] <- c
    
    return(result3)
  }
  
  
  
  ###函数3：选择模型
  mod_choice <- function(r)
  {
    require(mlogit)
    xxx <- paste(colnames(r)[l:ncol(r)])
    mlm1 <- as.formula(paste("choice ~ -1 + ", paste(xxx,collapse= "+")))
    
    var_choice <- "choice"
    var_alt <- "Alt"
    var_chid <- "num"
    mdata1 <- mlogit.data(r,choice = var_choice,shape = "long",alt.var = var_alt,chid.var = var_chid)
    m_result1 <- mlogit(mlm1,mdata1)
    return(m_result1)
  }
  
  ### 另一种检验方式 
  divp <- function(sde){
    
    pa <- c(1,2)
    pa <- rep(pa,times = nf)
    psd <- data.frame(n=1:(nf*2),n2=0,d=0,p=0,real=0)
    psd<- cbind(psd,pa)
    
    padu <-c(dum)
    psd <- rbind(psd,padu)
    psd[(nf*2+1),1] <- nf*2+1
    psd[,2] <- rownames(sde)
    
    k1<-0
    k2 <- 0 
    for (i in 1:nrow(sde))
    {
      psd[i,5] <- sde[i,1]
      
      if (sde[i,4] <= 0.05 ){
        psd[i,4] <- 'sig'
        k1 <- k1+1
      }else { psd[i,4] <- 'nosig'  }
      
      
      t2 <- psd[i,6]+psd[i,6]*0.05
      t1 <- psd[i,6]-psd[i,6]*0.05
      
      if ((sde[i,4] <= 0.05 ) & (psd[i,5] <= t2) & (psd[i,5] >= t1))
      {
        psd[i,3] <- 1
        k2 <- k2+1
      }else {psd[i,3] <- 0}
    }
    c <-0
    psd <- rbind(psd,c)
    psd[nrow(psd),4] <- k1
    psd[nrow(psd),3] <- k2
    
    return(psd)
    
  }
  
  
  
  
  
  ###函数4：提取系数做差值
  divv <- function(xxx){
    
    pa <- c(1,2)
    pa <- rep(pa,times = nf)
    pasd <- data.frame(n=0,n2=0,div=0,real=0)
    
    pasd<- cbind(pasd,pa)
    padu <-c(dum)
    pasd <- rbind(pasd,padu)
    
    
    for (i in 1:nrow(pasd))
    {
      pasd$n[i] <- i
      pasd$n2[i] <- rownames(xx1)[i]
      pasd$real[i] <- xxx[i]
      pasd$div[i] <- abs(pasd$pa[i]-pasd$real[i])
    }
    return(pasd)
    
  }
  
  ###函数5：检验
  sq_div <- function(pasd1,pasd2){
    
    s1 <- sum((pasd1$div)^2)
    s2 <- sum((pasd2$div)^2)
    s1 <- sqrt(s1/(nf*2+1))
    s2 <- sqrt(s2/(nf*2+1))
    
    pasd3 <- cbind(pasd1,pasd2)
    c <- 0
    pasd3 <- rbind(pasd3,c)
    pasd3[nrow(pasd1)+1,3] <- s1
    pasd3[nrow(pasd1)+1,8] <-s2
    #write.csv(pasd3,"H:/佳佳研究生/09课程作业/新建文件夹/1207/div15B.csv")
    return(pasd3)
  }
  
  ###函数6：估计参数是否达标
  cjy <- function(sde,llike){
    pa <- c(1,2)
    pa <- rep(pa,times = nf)
    psd <- data.frame(n=1:(nf*2),n2=0,d=0,p=0,real=0)
    psd<- cbind(psd,pa)
    
    padu <-c(dum)
    
    psd <- rbind(psd,padu)
    psd[(nf*2+1),1] <- nf*2+1
    psd[,2] <- rownames(sde)
    
    k<-0
    df <- n1*3*(nf*2+1+1)+(nf*2+1)-1
    
    for (i in 1:nrow(sde))
    {
      psd[i,5] <- sde[i,1]
      t2 <- ((psd[i,6]+abs(psd[i,6]*0.1)-sde[i,1])/sde[i,2])
      t1 <- ((psd[i,6]-abs(psd[i,6]*0.1)-sde[i,1])/sde[i,2])
      pp <- pt(t2,df)-pt(t1,df)
      psd[i,4] <- pp
      if (pp >= 0.9)
      {psd[i,3] <- 1
      k <- k+1
      }else {psd[i,3] <- 0}
    }
    
    c <-0
    psd <- rbind(psd,c)
    
    psd[nrow(psd),2] <- 'pass'
    psd[nrow(psd),3] <- k
    psd[nrow(psd),4] <- 'goodness of fit'
    psd[nrow(psd),5] <- exp(llike/n1)
    
    return(psd)
  }
  
  #画图1
  plott1 <- function(pasd1,pasd2,pasd3,pasd4){
    library(ggplot2)
    g1 <- ggplot() + geom_line(aes(x=reorder(pasd1$n2,pasd1$n),y=pasd1$div,color="成对",group=1),data = pasd1,size=1.5) + geom_line(aes(x=reorder(pasd2$n2,pasd2$n),y=pasd2$div,color="随机",group=1),data = pasd2,size=1.5)
    g1 <- g1+ geom_line(aes(x=reorder(pasd3$n2,pasd3$n),y=pasd3$div,color="组合",group=1),data = pasd3,size=1.5) + geom_line(aes(x=reorder(pasd4$n2,pasd4$n),y=pasd4$div,color="完全随机",group=1),data = pasd4,size=1.5)
    g1 <- g1+scale_color_discrete(name = "组合方式")
    g1 <- g1 + labs(title = paste("变量=",nf," / 样本量=",n1) ,x="样本属性", y="与设定参数的差值",size=20)#+theme(plot.title = element_text(hjust = 0.5))
    # g1 <- g1 + scale_y_continuous(limits=c(0,5),breaks=seq(0,5,0.5) )
    g1 <- g1 + theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14),title=element_text(size=16))
    g1 <- g1 + theme(legend.text = element_text(size=14))+theme(legend.key.width=unit(2,'cm'))
    return(g1)
  }
  
  #画图2
  plott2 <- function(pasd1,pasd2,pasd3,pasd4){
    library(ggplot2)
    g1 <- ggplot() + geom_line(aes(x=reorder(pasd1$n2,pasd1$n),y=pasd1$div,color="成对",group=1),data = pasd1,size=1.5) + geom_line(aes(x=reorder(pasd2$n2,pasd2$n),y=pasd2$div,color="随机",group=1),data = pasd2,size=1.5)
    g1 <- g1+ geom_line(aes(x=reorder(pasd3$n2,pasd3$n),y=pasd3$div,color="组合",group=1),data = pasd3,size=1.5) + geom_line(aes(x=reorder(pasd4$n2,pasd4$n),y=pasd4$div,color="完全随机",group=1),data = pasd4,size=1.5)
    
    g1 <- g1+scale_color_discrete(name = "组合方式")
    g1 <- g1 + labs(title = paste("变量=",nf," / 样本量=",n1) ,x="样本属性", y="与设定参数的差值",size=20)#+theme(plot.title = element_text(hjust = 0.5))
    g1 <- g1 + scale_y_continuous(limits=c(0,3),breaks=seq(0,3,0.5) )
    g1 <- g1 + theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14),title=element_text(size=16))
    g1 <- g1 + theme(legend.text = element_text(size=14))+theme(legend.key.width=unit(2,'cm'))
    return(g1)
  }
  
  
  
  n0 <- nn0
  dum <- dumm
  
  # n0 <- 1
  # dum <- 5
  n1 <- nl*n0
  nn1 <- n1*3
  l <- 8
  
  #1成对
  smp_new <- form_g(hehe5)
  # smp_new <- cd_ran(hehe5)
  n1 <- nrow(smp_new)/3
  nn1 <- nrow(smp_new)
  
  rrr <- cp_simu(smp_new)
  x1 <- mod_choice(rrr)
  xx1 <- as.matrix(x1$coefficients)
  sum_x1 <- summary(x1)
  llike1 <- as.numeric(sum_x1$logLik)
  
  #2随机
  sjj <- form_sj(hehe6)
  # sjj <- fullran(nn1)
  rsj <- cp_simu(sjj)
  x2 <- mod_choice(rsj)
  xx2 <- as.matrix(x2$coefficients)
  sum_x2 <- summary(x2)
  llike2 <- as.numeric(sum_x2$logLik)
  
  #3组合
  smp_new2 <- cd_ran(hehe5)
  rrr2 <- cp_simu(smp_new2)
  x3 <- mod_choice(rrr2)
  xx3 <- as.matrix(x3$coefficients)
  sum_x3 <- summary(x3)
  llike3 <- as.numeric(sum_x3$logLik)
  
  #4完全随机
  sjj2 <- fullran(nn1)
  rsj2 <- cp_simu(sjj2)
  x4 <- mod_choice(rsj2)
  xx4 <- as.matrix(x4$coefficients)
  sum_x4 <- summary(x4)
  llike4 <- as.numeric(sum_x4$logLik)
  
  
  #比较
  psdd <- divp(sum_x1$CoefTable)
  psdd2 <- divp(sum_x2$CoefTable)
  psdd3 <- divp(sum_x3$CoefTable)
  psdd4 <- divp(sum_x4$CoefTable)
  
  psd <- cjy(sum_x1$CoefTable,llike1)
  psd2 <- cjy(sum_x2$CoefTable,llike2)
  psd3 <- cjy(sum_x3$CoefTable,llike3)
  psd4 <- cjy(sum_x4$CoefTable,llike4)
  
  pasd1 <- divv(xx1)
  pasd2 <- divv(xx2)
  pasd3 <- divv(xx3)
  pasd4 <- divv(xx4)
  
  pasd01 <- sq_div(pasd1,pasd2)
  pasd02 <- sq_div(pasd3,pasd4)
  pasd <- cbind(pasd01,pasd02)
  
  psd <- cbind(psd,psd2)
  psd3 <- cbind(psd3,psd4)
  psd <- cbind(psd,psd3)
  
  psdd <- cbind(psdd,psdd2)
  psdd3 <- cbind(psdd3,psdd4)
  psdd <- cbind(psdd,psdd3)
  
  # write.csv(pasd,)
  
  if (n0 <= 4) { p <- plott1(pasd1,pasd2,pasd3,pasd4)
  }else { p <- plott2(pasd1,pasd2,pasd3,pasd4) }
  
  out1 <- list(p,pasd,psdd,psd)
  
  # file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\1\\",out1[[2:4]],".csv",sep=" ")
  # for (i in 2:4)
  # {
  #   write.csv(out1[[i]],file)
  # }
  
  return(out1)
}


###大函数2：循环CIR次检验参数可靠性（本来是循环20次求参数平均值检验可靠性，这里没有用到）
examination <- function(cir,n0,dum){
  
  exa <- data.frame(n=1:(nf*2+1),var=0,sig_cp=0,suc_CP=0,per_CP=0,sig_ran=0,suc_Ran=0,per_RAN=0)
  aa <- list()
  
  for (i in 1:cir){
    
    aa[(4*i-3):(4*i)] <- mall(hehe5,hehe6,n0,dum)
    exx <- as.data.frame(aa[(4*i)])
    
    for (j in 1:(nf*2+1))
    {
      if ( exx[j,3] ==1 ) { exa[j,4] <- exa[j,4] +1  }
      if ( exx[j,9] ==1 ) { exa[j,7] <- exa[j,7] +1  }
      
      if ( exx[j,4] =='sig' ){exa[j,3] <- exa[j,3] +1}
      if ( exx[j,10] =='sig' ){exa[j,6] <- exa[j,6] +1}
    }
  }
  
  exa[,2] <- exx[,2]
  
  for (i in 1:(nf*2+1))
  {
    exa[i,5] <- (exa[i,4]/cir)
    exa[i,8] <- (exa[i,7]/cir)
  }
  
  aa <- list(aa,exa)
  
  
  return(aa)
}



###程序开始
#调整可变参数
nf <- 5  #变量，可改
sp <- 3  #水平，可改

#正交生成问题，变量5个，样本27题(成对)
hehe5 <- oa.design(nfactors=nf*2, nlevels=sp)
#（随机）
hehe6 <- oa.design(nfactors=nf, nlevels=sp)

#成对行数(题数)
nl <- nrow(hehe5)
nll <- nl*3
#随机选项卡个数
nsj <- nrow(hehe6)


#输出单位样本量*1，都不选虚拟变量的参数为5
h1 <- mall(hehe5,hehe6,1,5)
h1
# outputt <- function(h){
#同时保存三个表一张图
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h1_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h1_1.pdf",h1[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h1[[i]],file)}
}
#       return()
# }


h3 <- mall(hehe5,hehe6,3,5)
h3
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h3_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h3_1.pdf",h3[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h3[[i]],file)}
}

h5 <- mall(hehe5,hehe6,5,5)
h5
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h5_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h5_1.pdf",h5[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h5[[i]],file)}
}


h15 <- mall(hehe5,hehe6,15,5)
h15
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h15_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h15_1.pdf",h15[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h15[[i]],file)}
}

h25 <- mall(hehe5,hehe6,25,5)
h25
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h25_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h25_1.pdf",h25[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h25[[i]],file)}
}


h55 <- mall(hehe5,hehe6,55,5)
h55
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h55_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h55_1.pdf",h55[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h55[[i]],file)}
}


h60 <- mall(hehe5,hehe6,60,5)
h60
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h60_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h60_1.pdf",h60[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h60[[i]],file)}
}

h70 <- mall(hehe5,hehe6,70,5)
h70
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h70_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h70_1.pdf",h70[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h70[[i]],file)}
}

h80 <- mall(hehe5,hehe6,80,5)
h80
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h80_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h80_1.pdf",h80[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h80[[i]],file)}
}


h100 <- mall(hehe5,hehe6,100,5)
h100
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h100_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h100_1.pdf",h100[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h100[[i]],file)}
}

h150 <- mall(hehe5,hehe6,150,5)
h150
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h150_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h150_1.pdf",h150[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h150[[i]],file)}
}

h200 <- mall(hehe5,hehe6,200,5)
h200
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h200_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h200_1.pdf",h200[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h200[[i]],file)}
}



h250<- mall(hehe5,hehe6,250,5)
h250
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h250_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\h250_1.pdf",h250[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h250[[i]],file)}
}

# h <- paste('h',1,sep="")
r1 <- data.frame(n=1:13,cd=0,sj=0,'cd-sj'=0,wqsj=0)
r2 <- data.frame(n=1:13,cd=0,sj=0,'cd-sj'=0,wqsj=0)

#算模型中变量概率的平均值
k <- 0
  for (j in 2:5)
  {
    r1[1,j] <- h1[[4]][12,3+k]
    r2[1,j] <- (sum(as.numeric(h1[[4]][1:11,4+k]))/11)
    
    r1[2,j] <- h3[[4]][12,3+k]
    r2[2,j] <- (sum(as.numeric(h3[[4]][1:11,4+k]))/11)
    
    r1[3,j] <- h5[[4]][12,3+k]
    r2[3,j] <- (sum(as.numeric(h5[[4]][1:11,4+k]))/11)
    
    r1[4,j] <- h15[[4]][12,3+k]
    r2[4,j] <- (sum(as.numeric(h15[[4]][1:11,4+k]))/11)
    
    r1[5,j] <- h25[[4]][12,3+k]
    r2[5,j] <- (sum(as.numeric(h25[[4]][1:11,4+k]))/11)
    
    r1[6,j] <- h55[[4]][12,3+k]
    r2[6,j] <- (sum(as.numeric(h55[[4]][1:11,4+k]))/11)
    
    r1[7,j] <- h60[[4]][12,3+k]
    r2[7,j] <- (sum(as.numeric(h55[[4]][1:11,4+k]))/11)
    
    r1[8,j] <- h70[[4]][12,3+k]
    r2[8,j] <- (sum(as.numeric(h70[[4]][1:11,4+k]))/11)
    
    r1[9,j] <- h80[[4]][12,3+k]
    r2[9,j] <- (sum(as.numeric(h80[[4]][1:11,4+k]))/11)
    
    r1[10,j] <- h100[[4]][12,3+k]
    r2[10,j] <- (sum(as.numeric(h100[[4]][1:11,4+k]))/11)
    
    r1[11,j] <- h150[[4]][12,3+k]
    r2[11,j] <- (sum(as.numeric(h150[[4]][1:11,4+k]))/11)
    
    r1[12,j] <- h200[[4]][12,3+k]
    r2[12,j] <- (sum(as.numeric(h200[[4]][1:11,4+k]))/11)
    
    r1[13,j] <- h250[[4]][12,3+k]
    r2[13,j] <- (sum(as.numeric(h250[[4]][1:11,4+k]))/11)
    
    k <- k+6
  }


write.csv(r1,"H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\001.csv")
write.csv(r2,"H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\3\\002.csv")



#计时
H1 <- Sys.time()
h300 <- mall(hehe5,hehe6,300,5)
h300
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\2\\h300_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\2\\h300_1.pdf",h300[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h300[[i]],file)}
}
H2 <- Sys.time()

H2-H1


h400 <- mall(hehe5,hehe6,400,5)
h400
for (i in 1:4)
{
  file=paste("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\2\\h400_",i,".csv",sep="")
  if (i ==1 ){
    ggsave("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1214\\2\\h400_1.pdf",h400[[i]],width = 40, height = 25,units = "cm",device="pdf",family="GB1")
  }else {write.csv(h400[[i]],file)}
}
H2 <- Sys.time()

H2-H1


