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




#组合
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


#小函数：判断相加是否超过1.5
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



##小函数2：所有组合的名称
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
  
  
  # orrr3 <- data.frame(matrix(0,(nrow(orrr2)/2),(ncol(orrr2)*2)))
  # 
  # for (i in 1:((orrr2)/2))
  # {
  #   orrr3[i,1:ncol(orrr2)] <- sn1[1:i*2-1,]   
  #   orrr3[i,(ncol(orrr2)+1):(ncol(orrr2)*2)] <- orrr2[1:i*2,]
  # }
  # 
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

sn1 <- rbind(sn1,sn2,sn3,sn4,sn5,sn6,sn7)
colnames(sn0) <- colnames(sn1)
sn1 <- rbind(sn0,sn1)
# rownames(sn1) <- 1:nrow(sn1)

x <-0
sn1 <- cbind(x,sn1)
x <- nrow(sn1)/2
sn1[1:x*2,1] <- 1:x
sn1[1:x*2-1,1] <- 1:x

write.csv(sn1,"H:\\佳佳研究生\\09课程作业\\新建文件夹\\1219\\22.csv")
snn <- read.csv("H:\\佳佳研究生\\09课程作业\\新建文件夹\\1219\\22.csv",stringsAsFactors = FALSE)
snn <- snn[,-1]


snn2 <- snn[1:x*2,]   
snn3 <- snn[1:x*2-1,]
snn3 <- cbind(snn3,snn2)

snn4 <- snn3[order(snn3[,26]),]
write.csv(snn4,"H:\\佳佳研究生\\09课程作业\\新建文件夹\\1219\\paixu2.csv")

# snn <- sn1[sort(sn1[,1],sn1[,12]),]
# d8 <- t(combn(dd$ddd,8))
# d8_1 <- dsum(d8)

nei <- ori$x2[29]
x1 <- nei-log((1-0.6)/0.6)
x2 <- nei-log((1-0.7)/0.7)
x3 <- nei-log((1-0.8)/0.8)

x4 <- nei-log((1-0.9)/0.9)

c <- c(x1,x2,x3)
cn <- c('满意度=60%','满意度=70%','满意度=80%')
cn <- cbind(cn,c)



write.csv(cn,"H:\\佳佳研究生\\09课程作业\\新建文件夹\\1219\\满意度大于60-2.csv")

write.csv(ori3,"H:\\佳佳研究生\\09课程作业\\新建文件夹\\1219\\2beta.csv")






n <- 2
i <- 1
k <- 0

while ((sum1 >= ori$x1[29])|(i <=10))
{
  cc[i,2] <- ori3[i,7]-ori3[i,5]
  if (cc[i,1] != ori3[i,1]) { 
    cc[i,1] <- ori3[i,1]
    cc[i,2] <- ori3[i,2]}
  else {  cc[i,1] <- ori3[i,3]
  cc[i,2] <- ori3[i,4] }
  
  sum1 <- sum(as.numeric(cc[1:10,2]))
  if (sum1 >= ori$x1[29]) { 
    cc[nrow(cc),2] <- sum1
    ori_r[[n]] <- cc }
  
  cc[1:10,] <- ori3[,6:7]
  n <- n+1
  i <- i+1
  
}

i <- 1
j <- 1
while ((sum1 >= ori$x1[29])|(i <=10))
{
  cc[i,2] <- ori3[i,7]-ori3[i,5]
  if (cc[i,1] != ori3[i,1]) { 
    cc[i,1] <- ori3[i,1]
    cc[i,2] <- ori3[i,2]}
  else {  cc[i,1] <- ori3[i,3]
  cc[i,2] <- ori3[i,4] }
  
  for (j in (i+1):nrow(ori3))
  {
    
  }
  
  
  
  sum1 <- sum(as.numeric(cc[1:10,2]))
  if (sum1 >= ori$x1[29]) { 
    cc[nrow(cc),2] <- sum1
    ori_r[[n]] <- cc }
  
  cc[1:10,] <- ori3[,6:7]
  n <- n+1
  i <- i+1
  
}








c <- c('sum',0,0)
ori1 <- cbind(ori1,c)






rec[1:5*2,3]

ori[1:10*2,2]
