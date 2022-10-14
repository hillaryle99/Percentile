f1 = function(x,b,q1,q2){
  plot2 = function(x,b,q,q2){
    #dev.new(width=8, height=6)
    temp1 = b
    n <- length(x)
    temp2 = (1/n) * (n+1)
    for (i in range(1:n)){
      x[i] = b + (n+1-2*b)*q
    }
    hi = n+1-2*b
    y = b + (n+1-2*b)*q
    y2 = b + (n+1-2*b)*q2
    y = y/n
    y2 = y2/n
    x = sort(x)
    f <- rep(1/n,n);
    F <- cumsum(f)

    x1 <- min(x)
    x2 <- max(x)

    m <- mean(x)

    plot(x,F, type = "s", frame.plot = FALSE, xlab="data", ylab="ECDF", las=1, xaxs="i", yaxs="i",
         xlim=c(0,x2+1), ylim=c(0,temp2),xaxt="n",yaxt="n")
    arrows(min(x), 0, max(x)+2, 0, code=2, length=.1, xpd=TRUE)
    #arrows(1, F[n], 1,F[n]+2, code=2, length=.1, xpd=TRUE)
    axis(side=2, at=seq(0, 1.1, by=0.1))
    axis(side=1, at=seq(0,x2+2, by=1))
    i = 1
    while(i<n){
      segments(x[i], F[i], x[i+1],F[i+1],lty=1, col = "red")
      i= i+1
    }
    #segments(x[1],0,x[1],b[1],col="red")
    segments(x[1],0,x[1],F[1])
    segments(x[n],F[n],x[n],1)
    segments(-2,0,min(x),0)
    segments(0,1,x[n],1,lty=2)
    arrows(min(x), 0, max(x)+2, 0, code=2, length=.1, xpd=TRUE)
    arrows(0, 1.1, 0, 0.9,code=1, length =0.1, xpd = TRUE)
    a = c()
    b = c()
    i = 1
    j = 1
    while(i < n){
      if(x[i + 1] != x[i]){
        a[j] = x[i]
        b[j] = F[i]
        j = j + 1
      }
      i = i + 1
    }
    h = c()
    for(i in 1:length(a)){
      j = which(x == a[i])[1]
      h[i] = F[j]
    }
    #print(a)
    #print(h)
    nb = length(b) - 1
    for(i in 1:nb){
      if(y> b[i] && y<b[i+1]){
        res = a[i+1]
        temp = i + 1
      }
    }


    slope = (h[temp] - b[temp - 1])/(a[temp] - a[temp - 1])
    r = h[temp] - (a[temp]*slope)
    var = (y - r)/slope
    if(var > a[temp-1] && var < a[temp]) res2 = var
    else res2 = res
    segments(res2,0,res2,y,lty=2)
    segments(x2, 1, x2 + 1, 1, lwd = 0.75)
    segments(0,y,res2,y,lty=2)

    points(res2,y,pch=19,col="orange")
    text(res2,0.05, "x(q)")
    #return(y)
  }
  plot2 = function(x,b,q,q2){
    #dev.new(width=8, height=6)
    temp1 = b
    n <- length(x)
    temp3 = (1/n) * (n+1)
    for (i in range(1:n)){
      x[i] = b + (n+1-2*b)*q
    }
    hi = n+1-2*b
    y = b + (n+1-2*b)*q
    y = y/n
    y2 = b + (n+1-2*b)*q2
    y2 = y2/n
    x = sort(x)
    f <- rep(1/n,n);
    F <- cumsum(f)

    x1 <- min(x)
    x2 <- max(x)

    m <- mean(x)

    plot(x,F, type = "s", frame.plot = FALSE, xlab="data", ylab="ECDF", las=1, xaxs="i", yaxs="i",
         xlim=c(0,x2+1), ylim=c(0,temp3),xaxt="n",yaxt="n")
    arrows(min(x), 0, max(x)+2, 0, code=2, length=.1, xpd=TRUE)
    #arrows(1, F[n], 1,F[n]+2, code=2, length=.1, xpd=TRUE)
    axis(side=2, at=seq(0, 1.1, by=0.1))
    axis(side=1, at=seq(0,x2+2, by=1))
    i = 1
    while(i<n){
      segments(x[i], F[i], x[i+1],F[i+1],lty=1, col = "red")
      i= i+1
    }
    #segments(x[1],0,x[1],b[1],col="red")
    segments(x[1],0,x[1],F[1])
    segments(x[n],F[n],x[n],1)
    segments(-2,0,min(x),0)
    segments(0,1,x[n],1,lty=2)
    arrows(min(x), 0, max(x)+2, 0, code=2, length=.1, xpd=TRUE)
    arrows(0, 1.1, 0, 0.9,code=1, length =0.1, xpd = TRUE)
    a = c()
    b = c()
    i = 1
    j = 1
    while(i < n){
      if(x[i + 1] != x[i]){
        a[j] = x[i]
        b[j] = F[i]
        j = j + 1
      }
      i = i + 1
    }
    h = c()
    for(i in 1:length(a)){
      j = which(x == a[i])[1]
      h[i] = F[j]
    }
    #print(a)
    #print(h)
    nb = length(b) - 1
    for(i in 1:nb){
      if(y> b[i] && y<b[i+1]){
        res = a[i+1]
        temp = i + 1
      }
    }
    #temp2 = 0
    nb = length(b) - 1
    for(i in 1:nb){
      if(y2> b[i] && y2<b[i+1]){
        res2 = a[i+1]
        temp2 = i + 1
      }
    }


    slope = (h[temp] - b[temp - 1])/(a[temp] - a[temp - 1])
    r = h[temp] - (a[temp]*slope)

    slope2 = (h[temp2] - b[temp2 - 1])/(a[temp2] - a[temp2 - 1])
    r2 = h[temp2] - (a[temp2]*slope)
    #y = slope*x + r
    var = (y - r)/slope
    if(var > a[temp-1] && var < a[temp]) res_1 = var
    else res_1 = res

    var2 = (y2 - r2)/slope
    if(var2 > a[temp2-1] && var2 < a[temp2]) res_2 = var2
    else res_2 = res2
    segments(res_1,0,res_1,y,lty=2)
    segments(res_2,0,res_2,y2,lty=2)
    #print(slope)
    #print(res2)
    segments(x2, 1, x2 + 1, 1, lwd = 0.75)
    segments(0,y,res_1,y,lty=2)
    segments(0,y2,res_2,y2,lty=2)
    points(res_1,y,pch=19,col="orange")
    points(res_2,y2,pch=19,col="orange")
    text(res_1,0.05, "x(q2)")
    text(res_2,0.05, "x(q1)")
    segments(x[1],0,x[1],b[1],col="red")
    #return(y)
    print(b)
  }


  layout(
    matrix(c(1,2), nrow=1),
    widths=c(1,2), heights=c(2,2)
  )
  plot1(x,b,q1,q2)
  plot2(x,b,q1,q2)
}
#points(q,y,pch=19,col="green")
f1(x,0.5,0.645,0.2)
#push up by 1/(2n)
#Add the sample unit
# y axis (right) = ECDF
