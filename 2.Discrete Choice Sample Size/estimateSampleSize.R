library(DoE.base)
require(mlogit)
require(Cairo)
library(Cairo)

mall <- function(hehe5,hehe6,nn0,beta11){
  
  ###函数1A：提取成对正交表
  form_g <- function(hehe)
  {
    smp <- data.frame(X=1:nll,Num=0,Pic=0,Alt=0,score=0,choice=0,Prob=0)
    smp1 <- data.frame(matrix(0,nll,(nf+1)))
    # smp2 <- smp1
    smp <- cbind(smp,smp1)
    
    n0 <- n1 %/% nl  #整除
    y <- n1 %% nl   #取余数
    
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
      
      if (n0 >= 2){
        smp2 <- smp
        for (i in 1:(n0-1))
        {
          smp2 <- rbind(smp2,smp)
        }
      }
    }
    
    if ((n0==0) && (y!=0))
    {
      smp2 <- smp[1:(y*3),]
    }
    
    if ((n0==1) && (y==0))
    {
      smp2 <- smp
    }
    
    if ((y != 0 ) && (n0 !=0))
    {
      if (n0 == 1){ smp2 <- smp } 
      smp3 <- smp[1:(y*3),]
      smp2 <- rbind(smp2,smp3) 
    }
  
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
      result1[3*i-2,] <- sample(nnn2,(nf+1),replace = T)
      result1[3*i-1,] <- sample(nnn2,(nf+1),replace = T)
    }
    result1[,nf+1] <- 0
    result <- cbind(result,result1)
    result[,4] <- 1:3
    return(result)
  }
  
  
  ###函数2：模拟选择
  cp_simu <- function(result){
    
    #赋参数
    result2 <- result
    
    for (i in (1:nn1))
    {
      for (j in l:(l+nf))
      {
        for (k in 1:sp)
        {
          if (result[i,j] == k) {result2[i,j] <- beta[k]
          }else { kk <- 0 }
        }
      }
    }  
    
    k <-0
    
    #给虚拟变量赋参数
    c <- c(0,0,dum)
    result2[, (l+nf)] <- c                                            
    
    #算得分
    result2[,1] <- 1:nrow(result2)
    
    for (i in 1:nn1)
    {

      result2$score[i] <- sum(as.numeric(result2[i,(l:ncol(result2))]) )

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
  #   return(result2)
  # }
  # 
  

    #对模拟结果做模型表  
    result3 <- result2
    for (j in l:ncol(result3))
    {
      result3[,j] <- 0
    }
    

    k <- l
    kk <- l
    for (i in 1:nf)
    {
      x <- 0
      if (sp > 2){
          
          for (j in 1:(sp-2))
          {
            result3 <- cbind(result3[,1:k],x,result3[,(k+1):ncol(result3)])
            colnames(result3)[kk+j] <- paste(colnames(result3[kk]),letters[j+1],sep = "")
            k <- k+1
          }
          colnames(result3)[kk] <- paste(colnames(result3[kk]),letters[1],sep = "")
          k <- k+1
          kk <- kk+(sp-1)
      }
    }
    colnames(result3)[ncol(result3)] <- "dummy"
    
    
    x <- 1
    for (i in (1:nrow(result3)) )
    {
      kk<-l
      for (j in 1:nf)
      {
        if (sp >2){
          
            for (p in 1:(sp-1))
            {
              if (result2[i,j+l-1] == beta[p+1]) { result3[i,kk-1+p] <- 1}
            }
            kk <- kk+(sp-1)
          }else
          {
            if (result2[i,j+l-1] != 0) { result3[i,j+l-1] <- 1}
          }

      } 
      }
  
    
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
  
  
  ###函数6：估计参数是否达标
  cjy <- function(sde,llike){
    pa <- beta[-1]
    pa <- rep(pa,times = nf)
    psd <- data.frame(n=1:(nf*(sp-1)),n2=0,d=0,p=0,real=0)
    psd<- cbind(psd,pa)
    
    padu <-c(dum)
    
    psd <- rbind(psd,padu)
    psd[(nf*(sp-1)+1),1] <- nf*(sp-1)+1
    psd[,2] <- rownames(sde)
    
    k<-0
    df <- n1*3*(nf*(sp-1)+1+1)+(nf*(sp-1)+1)-1
    
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
    psd[nrow(psd),4] <- (sum(psd[1:(nrow(psd)-1),4])/(nf*(sp-1)+1))
    psd[nrow(psd),5] <- exp(llike/n1)
    
    return(psd)
  }

  
  # n0 <- nn0
  # dum <- dumm
  
  l <- 8
  n1 <- nn0#样本量
  nn1 <- n1*3
  beta <- beta11#根据水平设定值
  dum <- ((beta[sp]*nf)/2)
  
  
  #1成对
  smp_new <- form_g(hehe5)
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

  #4完全随机
  sjj2 <- fullran(nn1)
  rsj2 <- cp_simu(sjj2)
  x4 <- mod_choice(rsj2)
  xx4 <- as.matrix(x4$coefficients)
  sum_x4 <- summary(x4)
  llike4 <- as.numeric(sum_x4$logLik)


  psd <- cjy(sum_x1$CoefTable,llike1)
  psd2 <- cjy(sum_x2$CoefTable,llike2)
  psd4 <- cjy(sum_x4$CoefTable,llike4)
  
  psd <- cbind(psd,psd2)
  psd <- cbind(psd,psd4)
  
  
  return(psd)
}



t1 <- oa.design(nlevels=c(9,3,3))
tt <- 1:27
cy <- sample(tt,1)
cy2 <- t1[cy,]

betaa <- c(0,0.5,1,2)
betab <- c(0,1,2,4)
betac <- c(0,2,4,8)

nf <- (as.numeric(cy2[1])+1)  #变量，可改
sp <- (as.numeric(cy2[2])+1)  #水平，可改
if (cy2[3]==1) { beta <- betaa }
if (cy2[3]==2) { beta <- betab }
if (cy2[3]==3) { beta <- betac }

#（正交）
hehe5 <- oa.design(nfactors=nf*2, nlevels=sp)
#（随机）
hehe6 <- oa.design(nfactors=nf, nlevels=sp)

#成对行数(题数)
nl <- nrow(hehe5)
nll <- nl*3
#随机选项卡个数
nsj <- nrow(hehe6)


m1 <- mall(hehe5,hehe6,500,beta)
m2 <- mall(hehe5,hehe6,800,beta)
