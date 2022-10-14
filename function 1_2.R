f1 = function(x,b,q,q2){
  plot1 = function(x,b,q,q2){
    #segments(0,b,1,n+1-b)
    n = length(x)
    d = c(0,1)
    d1= c(0, (n+1-2*b))
    y = b + (n+1-2*b)*d

    plot(d,y,frame.plot = FALSE,type="l",xaxt="n",yaxt ="n",xlab="q", ylab="n",las=1, xaxs="i", yaxs="i",
         xlim=c(0,1), ylim=c(0,n+1))
    segments(0,0,1,(n+1-2*b),col="red")
    y = (n+1-2*b)*q
    y2 = (n+1-2*b)*q2
    points(q,y,pch=19,col="green")
    points(q2,y2,pch=19,col="green")
    segments(q,0,q,y,lty=2)
    segments(q,y,1,y,lty=2)
    segments(q2,0,q2,y2,lty=2)
    segments(q2,y2,1,y2,lty=2)
    axis(side=2, at=seq(0, n+2, by=1))
    axis(side = 1, at=seq(0,1, by=0.1))
    arrows(0, 0, 1.1, 0, code=2, length=.1, xpd=TRUE)
    arrows(0, n+2, 0, n,code=1, length =0.1, xpd = TRUE)
    segments(0,n, 1,n,lty =2)
    text(q,0.05*n, "q2")
    text(q2,0.05*n, "q1")
  }

  plot2 = function(x,b,q,q2){
    temp1 = b
    n <- length(x)
    temp2 = (1/n) * (n+1)
    for (i in range(1:n)){
      x[i] = b + (n+1-2*b)*q
    }
    hi = n+1-2*b
    y = (n+1-2*b)*q
    y = y/n
    y2 = (n+1-2*b)*q2
    y2 = y2/n
    x = sort(x)
    f <- rep(1/n,n);
    F <- cumsum(f)

    x1 <- min(x)
    x2 <- max(x)

    m <- mean(x)

    plot(x,F, type = "s", frame.plot = FALSE, xlab="data", ylab="ECDF", las=1, xaxs="i", yaxs="i",
         xlim=c(0,x2+1), ylim=c(0,temp2),xaxt="n",yaxt="n")
    #arrows(min(x), 0, max(x)+2, 0, code=2, length=.1, xpd=TRUE)
    #arrows(1, F[n], 1,F[n]+2, code=2, length=.1, xpd=TRUE)
    axis(side=2, at=seq(0, 1.1, by=0.1))
    axis(side=1, at=seq(0,x2+2, by=1))
    #arrows(0, 0, 1.1, 0, code=2, length=.1, xpd=TRUE)
    arrows(min(x), 0, max(x)+2, 0, code=2, length=.1, xpd=TRUE)
    arrows(0, 1.1, 0, 0.9,code=1, length =0.1, xpd = TRUE)
    segments(x[1],0,x[1],F[1])
    segments(x[n],F[n],x[n],1)
    segments(-2,0,min(x),0)
    segments(0,1,x[n],1,lty=2)
    segments(x2, 1, x2 + 1, 1, lwd = 0.75)
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
    i = 1
    t = c()
    while(i < length(b)){
      t[i] = b[i] + (1/(2*n))
      i = i + 1
    }

    nt = length(t) - 1
    for(i in 1:nt){
      if(y> t[i] && y<t[i+1]){
        res = a[i+1]
      }
    }

    nt = length(t) - 1
    for(i in 1:nt){
      if(y2> t[i] && y2<t[i+1]){
        res2 = a[i+1]
      }
    }
    segments(0,y,res,y,lty=2)
    segments(res,0,res,y,lty=2)
    points(res,y,pch=19,col="orange")
    text(res,0.05, "x(q2)")
    segments(0,y2,res2,y2,lty=2)
    segments(res2,0,res2,y2,lty=2)
    points(res2,y2,pch=19,col="orange")
    text(res2,0.05, "x(q1)")
  }


  layout(
    matrix(c(1,2), nrow=1),
    widths=c(1,2), heights=c(2,2)
  )
  plot1(x,b,q,q2)
  plot2(x,b,q,q2)
}
#points(q,y,pch=19,col="green")
f1(x,0.5,0.645,0.2)
#push up by 1/(2n)
#bring the slant line down by half. (0,0) & (1,n)
