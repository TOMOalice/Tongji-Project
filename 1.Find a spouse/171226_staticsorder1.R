###根据朱老师的聚类得出的参数，求出满意度达到50%以上（都不选以上）的所有组合
#这个程序针对的是173人的那一类
#算法：1、先求出都不选效用和最大组合的效用，记录
#2、列出每个要素两个水平间的差值，排列组合，C10/1+C10/2+……并求出每个排列组合的和
#3、判断这些和是否大于等于（效用最大值-都不选效用），若大于等于，则记录这个对象
#4、将数值对应每个水平要素的名字，同时输出这个对象的效用和选择概率
#5、得到全部的组合后，通过选择概率的大小进行排序


theurl1<- "H:/佳佳研究生/09课程作业/新建文件夹/1216/CP.csv"
v1 <- read.csv(file = theurl1,stringsAsFactors = FALSE)

ori <- data.frame(n=1:29,x1=0,x2=0)

ori[,1] <- v1[,1]
ori[,2] <- v1[,16]
ori[,3] <- v1[,19]

l1 <- 8
n1 <- 2

#算最大得分
r <- 0
rec <- data.frame(matrix(0,0,(n1*2+1)))
c1 <- data.frame(1)

for (i in 1:10)
{
  c1 <- data.frame(1)
  for (j in 2:(n1+1))
  {
    
    c <- data.frame(0,0)
    if (ori[l1+n1*i-1,j] >= ori[l1+n1*i,j] ){ 
      c[1,1] <- ori[l1+n1*i-1,1]
      c[1,2] <- ori[l1+n1*i-1,j]
    }else{
      c[1,1] <- ori[l1+n1*i,1]
      c[1,2] <- ori[l1+n1*i,j] }
    c1 <- cbind(c1,c)
  }
  
  rec <- rbind(rec,c1)
  
}

ss1 <- sum(rec[,3])
ss2 <- sum(rec[,5])

c <- 0
rec <- rbind(rec,c)
rec[nrow(rec),1] <- 'sum'
rec[nrow(rec),3] <- ss1
rec[nrow(rec),5] <- ss2


#算最小得分
rec2 <- data.frame(matrix(0,0,(n1*2+1)))
c2 <- data.frame(1)

for (i in 1:10)
  
{
  c2 <- data.frame(1)
  for (j in 2:(n1+1))
  {
    
    c <- data.frame(0,0)
    if (ori[l1+n1*i-1,j] <= ori[l1+n1*i,j] ){ 
      c[1,1] <- ori[l1+n1*i-1,1]
      c[1,2] <- ori[l1+n1*i-1,j]
    }else{
      c[1,1] <- ori[l1+n1*i,1]
      c[1,2] <- ori[l1+n1*i,j] }
    c2 <- cbind(c2,c)
  }
  
  rec2 <- rbind(rec2,c2)
  
}

ss3 <- sum(rec2[,3])
ss4 <- sum(rec2[,5])

c <- 0
rec2 <- rbind(rec2,c)
rec2[nrow(rec2),1] <- 'sum'
rec2[nrow(rec2),3] <- ss3
rec2[nrow(rec2),5] <- ss4




#组合，找出最大值水平，找出对应的差值
ori1 <- ori[-(1:l1),]
ori1 <- ori1[-(nrow(ori1)),]
ori2 <- data.frame(x1=1:10,beta1=0,x2=0,beta2=0,delta=0,big=0,bbeta=0)
ori2$x1 <- ori1[1:10*2-1,1]
ori2$beta1 <- ori1[1:10*2-1,3]
ori2$x2 <- ori1[1:10*2,1]
ori2$beta2 <- ori1[1:10*2,3]
ori2$delta <- abs(ori2$beta1-ori2$beta2)
ori2$big <- rec[1:10,4]
ori2$bbeta <- rec[1:10,5]

ori3 <- ori2[order(ori2$delta),]

ori_r <- list()

cc <- ori3[,6:7]
c <- c('sum',0)
cc <- rbind(cc,c)
sum1 <- rec[11,5]
cc[nrow(cc),2] <- sum1
ori_r[[1]]<- cc

dd <- data.frame(n=1:10,ddd=0)
dd$ddd <- ori3$delta
for (i in 1:10)
{
  if (ori3[i,6] != ori3[i,1]) { 
    dd[i,1] <- ori3[i,1]}
  else {  dd[i,1] <- ori3[i,3]}
}


#小函数：判断组合出来的值相加是否超过（最大效用-都不选），如果不超过，则记录这一排和它们的和
dsum <- function(d)
{
  ds <- data.frame(matrix(0,1,(ncol(d)+1)))
  for (i in 1:nrow(d))
  {
    summ <- sum(d[i,])
    if (summ <= ( 4.581883786 - 3.1452672 )) 
    { ds1 <- data.frame(matrix(0,1,(ncol(d)+1)))
    ds1[,1:ncol(d)] <- d[i,]
    ds1[,ncol(d)+1] <- summ
    ds <- rbind(ds,ds1[1,])}
  }
  ds <- ds[-1,]
  
  return(ds)
}



#排列组合出被最大效用减并大于都不选效用的所有可能，这里用到combn这个排列组合函数
d1 <- t(combn(dd$ddd,1))
d1_1 <- dsum(d1)

d2 <- t(combn(dd$ddd,2))
d2_1 <- dsum(d2)

d3 <- t(combn(dd$ddd,3))
d3_1 <- dsum(d3)

d4 <- t(combn(dd$ddd,4))
d4_1 <- dsum(d4)

d5 <- t(combn(dd$ddd,5))
d5_1 <- dsum(d5)

d6 <- t(combn(dd$ddd,6))
d6_1 <- dsum(d6)

d7 <- t(combn(dd$ddd,7))
d7_1 <- dsum(d7)



##小函数2：找出所有组合的名称
sumname <- function(d){
  
  djj <- d
  
  orii <- ori3
  x <- 0
  orii <- cbind(orii[,1:4],x,orii[,5:ncol(orii)])
  orii$x <- dd[,1]
  orr <- orii$big
  orrr <- data.frame(matrix(0,1,(length(orr))))
  
  for (j in 1:nrow(djj))
  {
    
    for (i in 1:ncol(djj))
    {
      b1 <- orii[orii[,6]==as.numeric(djj[j,i]),5]
      orii[orii[,6]==as.numeric(djj[j,i]),7] <- b1
      
    }
    if (j==1) { orrr[1,] <- orii$big 
    }else
    { orrr1 <- orii$big
    orrr <- rbind(orrr,orrr1) }
    
    orii$big <- orr
    
  }
  
  orrr2 <- data.frame(matrix(0,(nrow(orrr)*2),(ncol(orrr)+2)))
  for (i in 1:nrow(orrr))
  {
    orrr2[2*i-1,1:ncol(orrr)] <- orrr[i,]
    orrr2[2*i-1,ncol(orrr)+1] <- 'SUM'
    orrr2[2*i-1,ncol(orrr)+2] <- 'PRO'
    for (j in 1:ncol(orrr))
    {
      orrr2[2*i,j] <- ori1[ori1[,1]==orrr[i,j],3]
    }
    orrr2[2*i,(ncol(orrr)+1)] <- sum(as.numeric(orrr2[2*i,1:ncol(orrr)]))
    ee <- as.numeric( orrr2[2*i,(ncol(orrr)+1)])
    orrr2[2*i,(ncol(orrr)+2)] <- (exp(ee)/(exp(ee)+exp(3.1452672)))
    
  }

  return(orrr2)
}


sn0 <- t(ori3[,6:7])
c <- c('SUM',0)
sn0 <- cbind(sn0,c)
sn0[2,ncol(sn0)] <- sum(as.numeric(sn0[2,1:10]))

c <- c('PRO',0)
sn0 <- cbind(sn0,c)
ee <- as.numeric(sn0[2,ncol(sn0)-1])
sn0[2,ncol(sn0)]<- (exp(ee)/(exp(ee)+exp(3.1452672)))

sn1 <- sumname(d1_1)
sn2 <- sumname(d2_1)
sn3 <- sumname(d3_1)
sn4 <- sumname(d4_1)
sn5 <- sumname(d5_1)
sn6 <- sumname(d6_1)
sn7 <- sumname(d7_1)


#把所有可能都合并成一个表
sn1 <- rbind(sn1,sn2,sn3,sn4,sn5,sn6,sn7)
colnames(sn0) <- colnames(sn1)
sn1 <- rbind(sn0,sn1)

x <-0
sn1 <- cbind(x,sn1)
x <- nrow(sn1)/2
sn1[1:x*2,1] <- 1:x
sn1[1:x*2-1,1] <- 1:x
write.csv(sn1,"H:\\佳佳研究生\\09课程作业\\新建文件夹\\1219\\22.csv")


#按照选择概率大小排序，再输出排序后的表
snn <- read.csv("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1219\\22.csv",stringsAsFactors = FALSE)
snn <- snn[,-1]

snn2 <- snn[1:x*2,]   
snn3 <- snn[1:x*2-1,]
snn3 <- cbind(snn3,snn2)

snn4 <- snn3[order(snn3[,26]),]
write.csv(snn4,"H:\\佳佳研究生\\09课程作业\\新建文件夹\\1219\\paixu2.csv")



#不同满意度下的阈值
nei <- ori$x2[29]
x1 <- nei-log((1-0.6)/0.6)
x2 <- nei-log((1-0.7)/0.7)
x3 <- nei-log((1-0.8)/0.8)
x4 <- nei-log((1-0.9)/0.9)
c <- c(x1,x2,x3)
cn <- c('满意度=60%','满意度=70%','满意度=80%')
cn <- cbind(cn,c)
write.csv(cn,"H:\\佳佳研究生\\09课程作业\\新建文件夹\\1219\\满意度大于60-2.csv")

#输出最开始每个要素水平的参数
write.csv(ori3,"H:\\佳佳研究生\\09课程作业\\新建文件夹\\1219\\2beta.csv")