f1 = function(x,q,q2){

  plot1 = function(x, q, q2){
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
    y2 = n*q2
    points(q,y,pch=19,col="green")
    points(q2,y2,pch=19,col="green")
    segments(q,0,q,y,lty=2)
    segments(q,y,1,y,lty=2)
    segments(q2,0,q2,y2,lty=2)
    segments(q2,y2,1,y2,lty=2)
    axis(side=2, at=seq(0, n+2, by=1))
    axis(side = 1, at=seq(0,1, by=0.1))
    segments(0,n, 1,n,lty =2)
    text(q,0.05*n, "q2")
    text(q2,0.05*n, "q1")
  }

  plot2 = function(x,q,q2){
    n <- length(x)
    temp2 = (1/n) * (n+1)

    x = sort(x)
    f <- rep(1/n,n);
    F <- cumsum(f)
    x1 <- min(x)
    x2 <- max(x)

    m <- mean(x)
    y = n*q
    y2 = n*q2

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
    y2 = y2/n
    nb = length(b) - 1
    for(i in 1:nb){
      if(y> b[i] && y<b[i+1]){
        res = a[i+1]
      }
      if(y == b[i]){
        res=a[i]
      }
    }
    for(i in 1:nb){
      if(y2> b[i] && y2<b[i+1]){
        res2 = a[i+1]
      }
      if(y2 == b[i]){
        res2=a[i]
      }
    }
    #print(y)
    #return(res)
    segments(0,y,res,y,lty=2)
    segments(res,0,res,y,lty=2)
    points(res,y,pch=19,col="yellow")
    points(res,0,pch=19)
    text(res,0.05, "x(q2)")
    segments(0,y2,res2,y2,lty=2)
    segments(res2,0,res2,y2,lty=2)
    points(res2,y2,pch=19,col="yellow")
    points(res2,0,pch=19)
    text(res2,0.05, "x(q1)")
    axis(side=2, at=seq(0, 1.1, by=0.1))
    axis(side=1, at=seq(0,x2+2, by=1))
    #segments(0,y,res,y,lty=2)
    segments(0,1,x[n],1,lty=2)
  }


  layout(
    matrix(c(1,2), nrow=1),
    widths=c(1,2), heights=c(2,2)
  )
  plot1(x,q,q2)
  plot2(x,q,q2)
}

f2 = function(x,q, q2){

  plot1 = function(x, q, q2){
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
    y2 = n*q2
    points(q,y,pch=19,col="green")
    points(q2,y2,pch=19,col="green")
    segments(q,0,q,y,lty=2)
    segments(q,y,1,y,lty=2)
    segments(q2,0,q2,y2,lty=2)
    segments(q2,y2,1,y2,lty=2)
    axis(side=2, at=seq(0, n+2, by=1))
    axis(side = 1, at=seq(0,1, by=0.1))
    segments(0,n, 1,n,lty =2)
    text(q,0.05*n, "q1")
    text(q2,0.05*n, "q2")
  }

  plot2 = function(x,q, q2){
    #dev.new(width=8, height=6)
    n <- length(x)
    t = (1/n) * (n+1)
    x = sort(x)
    f <- rep(1/n,n);
    F <- cumsum(f)
    x1 <- min(x)
    x2 <- max(x)

    m <- mean(x)
    y = n*q
    y2 = n*q2

    plot(x,F, type = "s", frame.plot = FALSE, xaxt = "n", yaxt = "n",xlab="", ylab="", las=1, xaxs="i", yaxs="i",
         xlim=c(0,x2+1), ylim=c(0,t))
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
    y2 = y2/n
    nb = length(b) - 1
    for(i in 1:nb){
      if(y> b[i] && y<b[i+1]){
        res_1 = a[i+1]
        temp = i + 1
      }
      if(y == b[i]){
        temp = i + 1
        res_1 = (a[temp-1] + a[temp])/2
      }
    }

    nb = length(b) - 1
    for(i in 1:nb){
      if(y2> b[i] && y2<b[i+1]){
        res_2 = a[i+1]
        temp2 = i + 1
      }
      if(y2 == b[i]){
        temp2 = i + 1
        res_2 = (a[temp2-1] + a[temp2])/2
      }
    }

    #print(a)
    #print(res)
    #print(b)

    #segments(res,y,res,b[temp-1],lty=2, col ="blue")
    segments(res_1,0,res_1,y,lty=2)
    segments(0,y,res_1,y,lty=2)
    segments(res_2,0,res_2,y2,lty=2)
    segments(0,y2,res_2,y2,lty=2)
    #segments(res2,0,res2,y,lty=2)
    points(res_1,y,pch=19,col="yellow")
    points(res_1,0,pch=19)
    points(res_2,y2,pch=19,col="yellow")
    points(res_2,0,pch=19)
    axis(side=2, at=seq(0, 1.1, by=0.1))
    axis(side=1, at=seq(0,x2+2, by=1))
    segments(0,1,x[n],1,lty=2)
    text(res_1,0.05, "x(q1)")
    text(res_2,0.05, "x(q2)")
  }



  layout(
    matrix(c(1,2), nrow=1),
    widths=c(1,2), heights=c(2,2)
  )
  plot1(x,q,q2)
  plot2(x,q,q2)
}

percentile = function(data, q, q2, adj){
  if(adj == "ecdf") f1(x,q, q2)
  if(adj=="ecdf adjusted") f2(x,q,q2)
}

percentile(x, 9/17,0.2,"ecdf")
percentile(x,0.2, 9/17,"ecdf adjusted")

# Give 2 examples q1 and q2
