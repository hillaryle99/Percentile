f1 = function(x,q){

  plot1 = function(x, q){
    b=1
    #segments(0,b,1,n+1-b)
    #(0,1) and (1,n)
    n = length(x)
    d=c(0,1)
    y = c(0,n)
    plot(d,y,frame.plot = FALSE,type="l",xlab="q", yaxt = "n", xaxt="n",ylab="n",las=1, xaxs="i", yaxs="i",
         xlim=c(0,1), ylim=c(0,n+1))
    arrows(0, 0, 1.1, 0, code=2, length=.1, xpd=TRUE)
    arrows(0, n+2, 0, n,code=1, length =0.1, xpd = TRUE)
    # 1 = b
    # n = a + 1 => a = n - 1
    # y = (n-1)*x + 1
    y = n*q
    points(q,y,pch=19,col="green")
    segments(q,0,q,y,lty=2)
    segments(q,y,1,y,lty=2)
    axis(side=2, at=seq(0, n+2, by=1))
    axis(side = 1, at=seq(0,1, by=0.1))
    segments(0,n, 1,n,lty =2)
    text(q,0.05*n, "q")
  }

  plot2 = function(x,q){
    #dev.new(width=8, height=6)
    n <- length(x)
    temp2 = (1/n) * (n+1)

    x = sort(x)
    f <- rep(1/n,n);
    F <- cumsum(f)
    x1 <- min(x)
    x2 <- max(x)

    m <- mean(x)
    y = n*q

    plot(x,F, type = "s", frame.plot = FALSE, xaxt = "n", yaxt = "n",xlab="data", ylab="ECDF", las=1, xaxs="i", yaxs="i",
         xlim=c(0,x2+1), ylim=c(0,temp2))
    segments(x2, 1, x2 + 1, 1, lwd = 0.75)
    arrows(min(x), 0, max(x)+2, 0, code=2, length=.1, xpd=TRUE)
    arrows(0, 1.1, 0, 0.9,code=1, length =0.1, xpd = TRUE)
    #segments(0,b,1,n+1-b)
    #y = b + (n+1-2*b)*q
    #points(q,y,pch=19,col="green")
    #arrows(min(x), 0, max(x)+2, 0, code=2, length=.1, xpd=TRUE)

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
    segments(min(x),0,a[1],b[1])
    i = 1
    while(i<=n+1){
      points(a[i],b[i],pch=19,col="blue")
      i = i +1
    }
    #print(b[length(b)])
    points(x[n],1,pch=19,col="blue")
    y = y/n
    nb = length(b) - 1
    for(i in 1:nb){
      if(y> b[i] && y<b[i+1]){
        res = a[i+1]
      }
    }
    #print(y)
    #return(res)
    segments(0,y,res,y,lty=2)
    segments(res,0,res,y,lty=2)
    points(res,y,pch=19,col="yellow")
    points(res,0,pch=19)
    text(res,0.05, "x(q)")
    axis(side=2, at=seq(0, 1.1, by=0.1))
    axis(side=1, at=seq(0,x2+2, by=1))
    #segments(0,y,res,y,lty=2)
    segments(0,1,x[n],1,lty=2)
  }


  layout(
    matrix(c(1,2), nrow=1),
    widths=c(1,2), heights=c(2,2)
  )
  plot1(x,q)
  plot2(x,q)
}

f2 = function(x,q){

  plot1 = function(x, q){
    b=1
    #segments(0,b,1,n+1-b)
    #(0,1) and (1,n)
    n = length(x)
    d=c(0,1)
    y = c(0,n)
    plot(d,y,frame.plot = FALSE,type="l",xlab="", yaxt = "n", xaxt="n",ylab="",las=1, xaxs="i", yaxs="i",
         xlim=c(0,1), ylim=c(0,n+1))
    arrows(min(x), 0, max(x)+2, 0, code=2, length=.1, xpd=TRUE)
    arrows(0,n, 0, n + 5)
    # 1 = b
    # n = a + 1 => a = n - 1
    # y = (n-1)*x + 1
    y = n*q
    points(q,y,pch=19,col="green")
    segments(q,0,q,y,lty=2)
    segments(q,y,1,y,lty=2)
    axis(side=2, at=seq(0, n+2, by=1))
    axis(side = 1, at=seq(0,1, by=0.1))
    segments(0,n, 1,n,lty =2)
    text(q,0.05*n, "q")

  }

  plot2 = function(x,q){
    #dev.new(width=8, height=6)
    n <- length(x)
    temp2 = (1/n) * (n+1)

    x = sort(x)
    f <- rep(1/n,n);
    F <- cumsum(f)
    x1 <- min(x)
    x2 <- max(x)

    m <- mean(x)
    y = n*q

    plot(x,F, type = "s", frame.plot = FALSE, xaxt = "n", yaxt = "n",xlab="", ylab="", las=1, xaxs="i", yaxs="i",
         xlim=c(0,x2+1), ylim=c(0,temp2))
    segments(x2, 1, x2 + 1, 1, lwd = 0.75)
    #segments(0,b,1,n+1-b)
    #y = b + (n+1-2*b)*q
    #points(q,y,pch=19,col="green")
    #arrows(min(x), 0, max(x)+2, 0, code=2, length=.1, xpd=TRUE)
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
    segments(min(x),0,a[1],b[1])
    i = 1
    while(i<=n){
      points((a[i]+a[i+1])/2,b[i],pch=19,col="red")
      i = i +1
    }
    points((a[length(a)]+x[n])/2,b[length(b)],pch=19,col="red")
    y = y/n
    nb = length(b) - 1
    for(i in 1:nb){
      if(y> b[i] && y<b[i+1]){
        res = a[i+1]
        temp = i + 1
      }
    }
    #print(a)
    #print(res)
    #print(b)
    res2 = (a[temp-1] + a[temp])/2
    segments(res,y,res,b[temp-1],lty=2, col ="blue")
    segments(res2,b[temp-1],res,b[temp-1],lty=2, col ="blue")
    segments(res2,0,res2,b[temp-1],lty=2)
    segments(0,y,res,y,lty=2)
    #segments(res2,0,res2,y,lty=2)
    points(res,y,pch=19,col="yellow")
    points(res2,0,pch=19)
    text(res2,0.05, "x(q)")
    axis(side=2, at=seq(0, 1.1, by=0.1))
    axis(side=1, at=seq(0,x2+2, by=1))
    segments(0,1,x[n],1,lty=2)
  }


  layout(
    matrix(c(1,2), nrow=1),
    widths=c(1,2), heights=c(2,2)
  )
  plot1(x,q)
  plot2(x,q)
}

percentile = function(data, q, adj){
  if(adj == "ecdf") f1(x,q)
  if(adj=="ecdf adjusted") f2(x,q)
}

percentile(x,0.5,"ecdf")
percentile(x,0.5,"ecdf adjusted")

# Give 2 examples q1 and q2
