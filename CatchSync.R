# /**
# * Given a directed graph with millions of nodes, how can we automatically spot anomalies?
# * Suspicious graph patterns show up in many applications,
# * from Twitter users who buy fake followers,
# * manipulating the social network,
# * to botnet members performing distributed denial of service attacks,
# * disturbing the network traffic graph.
# * 
# * Input: node pairs (edge list) of a directed graph;
# * Output:
# *   1. Feature space plots: in-degree vs. authority for target nodes
# *                           (out-degree vs. hubness for source nodes);
# *   2. Synchronicity (coherence) vs. normality: for source nodes.
# *
# * Edited and Copyright by Meng Jiang
# * Last update: Oct 29, 2014
# *
# * 1st: Feb 22, 2014
# * 2nd: Sep 27, 2014
# * 3rd: Oct  8, 2014
# * 4th: Oct 20, 2014
# *
# * CatchSync: Catch Synchronized Behavior in Large Directed Graphs (KDD 2014 Best Paper Final List)
# * Meng Jiang, Peng Cui, Alex Beutel, Christos Faloutsos and Shiqiang Yang
# */

setwd('.')
args <- commandArgs()
func <- args[3]
# parameters for image plotting
imgsz <- 1000
# parmar <- c(10,9,4,2)
# parmgp <- c(6,2,0.5)
parmar <- c(14,13,4,6)
parmgp <- c(9,3,0.5)
jet <- c('white','purple','blue','cyan',
         'green','yellow','orange','red')
gray <- c('white','gray70','gray60','gray50',
          'gray40','gray30')
xpercent <- 0.995
CONSTXA <- -0.075
CONSTXB <- 0.695
OVERPLOT <- 200
# parameters for axis
axissz <- 3
cexpoint <- 1
cexlegend <- 3.8
cexlab <- 5.5
cexaxis <- 4
cexsymbol <- 7
lwdline <- 4
lwdaxis <- 4
MINCONST <- -1e30
MAXCONST <- 1e30
DIGITNUMSMALL <- 1
TINYVALUE <- 1
REMOVEEMPTY <- 1

# expression to show the numbers in plot - axis, ...
expr <- function(x) {
  x <- as.numeric(x)
  if (x >= 1e4) return(signif(x,1))
  if (x >= 1) return(round(x))
  if (x >= 1e-3) return(signif(x,DIGITNUMSMALL))
  if (TINYVALUE == 1)
    return(signif(x,1))
  return(0)
}
exprlist <- function(xs,sz) {
  xs <- as.numeric(xs)	
  ret <- array('',dim=c(sz,1))
  for (i in 1:sz) {
    temp <- expr(xs[i])
    if (temp == 0) {
      ret[i] <- '0'
    } else if (temp >= 1e4) {
      logtemp <- floor(log(temp)/log(10))
      headtemp <- min(max(floor(temp/(10^logtemp)),1),9)
      ret[i] <- paste(headtemp,'E+',logtemp,sep='')
    } else if (temp <= 1e-3) {
      logtemp <- floor(log(temp)/log(10))
      headtemp <- min(max(floor(temp/(10^logtemp)),1),9)
      ret[i] <- paste(headtemp,'E-',-logtemp,sep='')
    } else {
      ret[i] <- temp
    }
  }
  return(ret)
}
# Given data (x, y) and number of marks on axis
# and type of plot ([log], [lin]),
# return the marks on x-axis and y-axis.
getxyonaxis <- function(data,sz,mathtype) {
  minx <- min(data[,1])
  miny <- min(data[,2])
  maxx <- max(data[,1])
  maxy <- max(data[,2])
  logstr <- ''
  if (mathtype == 'log') {
    if (maxx > 1) minx <- max(1,minx)
    if (maxy > 1) miny <- max(1,miny)
    x <- exp(log(minx)+(0:sz)*((log(maxx)-log(minx))/sz))
    y <- exp(log(miny)+(0:sz)*((log(maxy)-log(miny))/sz))
    logstr <- 'xy'
  }
  if (mathtype == 'lin') {
    x <- minx+(0:sz)*((maxx-minx)/sz)
    y <- miny+(0:sz)*((maxy-miny)/sz)
  }
  x[1] <- minx
  y[1] <- miny
  x[sz+1] <- maxx
  y[sz+1] <- maxy
  return(list('x'=x,'y'=y,'logstr'=logstr))
}
# Given data on X-axis and Y-axis and the scales [datax,datay,Nx,Ny]
# and the lower bound of x to plot [pxlim],
# return numbers on axes of plot.
matrix.axes <- function(datax,datay,Nx,Ny,pxlim) {
  szy <- axissz
  szx <- round(szy/(1-pxlim))
  x <- (0:szx)/szx
  y <- (0:szy)/szy
  xx <- 1+round(x*Nx)
  yy <- 1+round(y*Ny)
  xx[szx+1] = min(Nx,xx[szx+1])
  yy[szy+1] = min(Ny,yy[szy+1])
  axis(side=1,at=c(MINCONST,x,MAXCONST),labels=c(MINCONST,exprlist(datax[xx],szx+1),MAXCONST),las=0,cex.axis=cexaxis,lwd=lwdaxis);
  axis(side=2,at=c(MINCONST,y,MAXCONST),labels=c(MINCONST,exprlist(datay[yy],szy+1),MAXCONST),las=0,cex.axis=cexaxis,lwd=lwdaxis);
}
# Return numbers on axes of color stripe of plot.
matrix.colaxes <- function(mathtype,maxc) {
  sz <- axissz
  x <- (0:sz)*(maxc/sz)
  y <- x
  if (mathtype == 'log') y <- exp(x)
  y[1] = 0
  y <- exprlist(y,sz+1)
  axis(side=4,at=x,labels=y,cex.axis=cexaxis,mgp=c(0,1,0));
}
# Prepare data to plot into [Ns x Nx] cells, from [data] of ~cellfile,
# however, we remove blank stripes of cells in order to plot pretty.
compress <- function(data,Ns,mathtype) {
  if (REMOVEEMPTY == 1) {
    N <- dim(data)[2]
    gap = max(1,round(N/Ns))
    datax <- array(0,dim=c(Ns,1))
    datay <- array(0,dim=c(Ns,1))
    datac <- array(0,dim=c(Ns,Ns))
    for (i in 0:(Ns-1)) {
      datax[i+1,1] <- data[1,1+i*gap]
      datay[i+1,1] <- data[2,1+i*gap]
      for (j in 0:(Ns-1)) {
        starti <- 1+i*gap+2
        endi <- min(starti+gap-1,N+2)
        startj <- 1+j*gap
        endj <- min(startj+gap-1,N)
        datac[i+1,j+1] <- sum(as.numeric(data[starti:endi,startj:endj]))
      }
    }
    Ny <- 0
    for (j in 1:Ns) if (sum(datac[1:Ns,j]) > 0) Ny <- Ny+1
    newdatay <- array(0,dim=Ny)
    newdatac1 <- array(0,dim=c(Ns,Ny))
    jj <- 1
    for (j in 1:Ns) {
      if (sum(datac[1:Ns,j]) > 0) {
        newdatay[jj] <- datay[j]
        newdatac1[1:Ns,jj] <- datac[1:Ns,j]
        jj <- jj+1
      }
    }
    Nx <- 0
    for (i in 1:Ns) if (sum(newdatac1[i,1:Ny]) > 0) Nx <- Nx+1
    newdatax <- array(0,dim=Nx)
    newdatac2 <- array(0,dim=c(Nx,Ny))
    ii <- 1
    for (i in 1:Ns) {
      if (sum(newdatac1[i,1:Ny]) > 0) {
        newdatax[ii] <- datax[i]
        newdatac2[ii,1:Ny] <- newdatac1[i,1:Ny]
        ii <- ii+1
      }
    }
    newdatac <- newdatac2
    if (mathtype == 'log') {
      newdatac <- log(newdatac+(1e-10))
      for (i in 1:Nx) for (j in 1:Ny) newdatac[i,j] <- max(newdatac[i,j],0)
    }
    maxc <- max(newdatac)
  }
  if (REMOVEEMPTY == 0) {
    N <- dim(data)[2]
    gap = max(1,round(N/Ns))
    datax <- array(0,dim=c(Ns,1))
    datay <- array(0,dim=c(Ns,1))
    datac <- array(0,dim=c(Ns,Ns))
    for (i in 0:(Ns-1)) {
      datax[i+1,1] <- data[1,1+i*gap]
      datay[i+1,1] <- data[2,1+i*gap]
      for (j in 0:(Ns-1)) {
        starti <- 1+i*gap+2
        endi <- min(starti+gap-1,N+2)
        startj <- 1+j*gap
        endj <- min(startj+gap-1,N)
        datac[i+1,j+1] <- sum(as.numeric(data[starti:endi,startj:endj]))
      }
    }
    Ny <- 0
    for (j in 1:Ns) if (sum(datac[1:Ns,j]) >= 0) Ny <- Ny+1
    newdatay <- array(0,dim=Ny)
    newdatac1 <- array(0,dim=c(Ns,Ny))
    jj <- 1
    for (j in 1:Ns) {
      if (sum(datac[1:Ns,j]) >= 0) {
        newdatay[jj] <- datay[j]
        newdatac1[1:Ns,jj] <- datac[1:Ns,j]
        jj <- jj+1
      }
    }
    Nx <- 0
    for (i in 1:Ns) if (sum(newdatac1[i,1:Ny]) >= 0) Nx <- Nx+1
    newdatax <- array(0,dim=Nx)
    newdatac2 <- array(0,dim=c(Nx,Ny))
    ii <- 1
    for (i in 1:Ns) {
      if (sum(newdatac1[i,1:Ny]) >= 0) {
        newdatax[ii] <- datax[i]
        newdatac2[ii,1:Ny] <- newdatac1[i,1:Ny]
        ii <- ii+1
      }
    }
    newdatac <- newdatac2
    if (mathtype == 'log') {
      newdatac <- log(newdatac+(1e-10))
      for (i in 1:Nx) for (j in 1:Ny) newdatac[i,j] <- max(newdatac[i,j],0)
    }
    maxc <- max(newdatac)
  }  
  return(list('Nx'=Nx,'Ny'=Ny,
              'datasrcx'=datax,
              'datasrcy'=datay,
              'datax'=newdatax,
              'datay'=newdatay,
              'datac'=newdatac,
              'maxc'=maxc))
}
# Given cell distribution [datac],
# the number of cells on X-axis [Nx], Y-axis [Ny],
# and the percentage of x [percent]
# return the lower bound of x and pixel positions on plots.
getplotbound <- function(datac,Nx,Ny,percent) {
  sumdata <- sum(datac)
  tempdata <- 0
  for (x in 1:Nx) {
    tempdata <- tempdata+sum(datac[x,1:Ny])
    if (tempdata > (1-percent)*sumdata) break
  }
  pxlim <- (x-1)/Nx
  forexb <- CONSTXB+pxlim*(1-CONSTXB)
  forexa <- forexb-(1-pxlim)*(CONSTXB-CONSTXA)
  return(list('pxlim'=pxlim,
              'forexa'=forexa,
              'forexb'=forexb))
}

# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------
# -------------------------------------------------------------------------------

# Given file of data and png,
# and labels on x-axis and y-axis,
# and type of plot (log-log [log] or lin-lin [lin]),
# show plot of degree to frequency.
degplot <- function() {
  datafile <- args[4]
  pngfile <- args[5]
  xlabel <- args[6]
  ylabel <- args[7]
  mathtype <- args[8]
  data <- read.csv(datafile,header=F)
  xy <- getxyonaxis(data,axissz,mathtype)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  plot(data,log=xy$logstr,axes=F,
       cex=cexpoint,cex.lab=cexlab,pch=20,col='black',
       xlab=xlabel,ylab=ylabel)
  axis(side=1,at=c(MINCONST,xy$x,MAXCONST),
       labels=c(MINCONST,exprlist(round(xy$x),axissz+1),MAXCONST),cex.axis=cexaxis,lwd=lwdaxis);
  axis(side=2,at=c(MINCONST,xy$y,MAXCONST),
       labels=c(MINCONST,exprlist(round(xy$y),axissz+1),MAXCONST),cex.axis=cexaxis,lwd=lwdaxis);
  dev.off()
}

# Given file of data and png,
# and labels on x-axis and y-axis,
# and type of plot (log-log [log] or lin-lin [lin]),
# show plot of degree to CCDF
ccdfplot <- function() {
  datafile <- args[4]
  pngfile <- args[5]
  xlabel <- args[6]
  ylabel <- args[7]
  mathtype <- args[8]
  data <- read.csv(datafile,header=F)
  xy <- getxyonaxis(data,axissz,mathtype)
  png(file=pngfile,width=imgsz/2,heigh=imgsz/2)
  plot(data,log=xy$logstr,
       cex=cexpoint,pch=20,col='black',
       xlab=xlabel,ylab=ylabel)
  dev.off()
}

# Given file of data (before and after processed) and png,
# and labels on x-axis and y-axis,
# and type of plot (log-log [log] or lin-lin [lin]),
# show plot of degree to frequency.
degcompplot <- function() {
  cexpoint <- cexpoint*1.5
  lwdline <- lwdline*1.5
  datafile <- args[4]
  datanewfile <- args[5]
  datarmfile <- args[6]
  dataapproxfile <- args[7]
  pngfile <- args[8]
  xlabel <- args[9]
  ylabel <- args[10]
  mathtype <- args[11]
  degend <- as.numeric(args[12])
  legendstart <- as.numeric(args[13])
  legendend <- as.numeric(args[14])
  data <- read.csv(datafile,header=F)
  datanew <- read.csv(datanewfile,header=F)
  datarm <- read.csv(datarmfile,header=F)
  dataapprox <- read.csv(dataapproxfile,header=F)
  xy <- getxyonaxis(data,axissz,mathtype)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  cexpoint <- cexpoint*0.5
  plot(data,type='b',log=xy$logstr,axes=F,lwd=lwdline,
       cex=cexpoint,cex.lab=cexlab,pch=20,col='black',
       xlab=xlabel,ylab=ylabel,
       xlim=c(1,degend))
  points(datarm,type='b',lwd=lwdline*0.6,
         cex=cexpoint*0.6,cex.lab=cexlab,pch=20,col='blue')
  points(datanew,type='b',lwd=lwdline,
         cex=cexpoint,cex.lab=cexlab,pch=20,col='red')
  points(data,type='b',lwd=lwdline,
         cex=cexpoint,cex.lab=cexlab,pch=20,col='black')
  lines(dataapprox,type='l',lwd=lwdline,lty=2,col='magenta')
  axis(side=1,at=c(MINCONST,xy$x,MAXCONST),
       labels=c(MINCONST,exprlist(round(xy$x),axissz+1),MAXCONST),cex.axis=cexaxis,lwd=lwdaxis);
  axis(side=2,at=c(MINCONST,xy$y,MAXCONST),
       labels=c(MINCONST,exprlist(round(xy$y),axissz+1),MAXCONST),cex.axis=cexaxis,lwd=lwdaxis);
  legend(legendstart,legendend,bg='transparent',border='transparent',horiz=F,bty='n',
         legend=c('Entire graph','Linear approximation',
                  'Blamed nodes with','"synchronized behaviors"','Unblamed nodes'),
         lty=c(4,2,4,4,4),cex=cexlegend,lwd=lwdline*1.5,
         col=c('black','magenta','blue','white','red'))
  dev.off()
}

degcompsimpplot <- function() {
  cexpoint <- cexpoint*1.5
  lwdline <- lwdline*1.5
  datafile <- args[4]
  datanewfile <- args[5]
  datarmfile <- args[6]
  dataapproxfile <- args[7]
  pngfile <- args[8]
  xlabel <- args[9]
  ylabel <- args[10]
  mathtype <- args[11]
  degend <- as.numeric(args[12])
  legendstart <- as.numeric(args[13])
  legendend <- as.numeric(args[14])
  data <- read.csv(datafile,header=F)
  datanew <- read.csv(datanewfile,header=F)
  datarm <- read.csv(datarmfile,header=F)
  xy <- getxyonaxis(data,axissz,mathtype)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  cexpoint <- cexpoint*0.5
  plot(data,type='b',log=xy$logstr,axes=F,lwd=lwdline,
       cex=cexpoint,cex.lab=cexlab,pch=20,col='black',
       xlab=xlabel,ylab=ylabel,
       xlim=c(1,degend))
  points(datarm,type='b',lwd=lwdline*0.6,
         cex=cexpoint*0.6,cex.lab=cexlab,pch=20,col='blue')
  points(data,type='b',lwd=lwdline,
         cex=cexpoint,cex.lab=cexlab,pch=20,col='black')
  points(datanew,type='b',lwd=lwdline*1.2,
         cex=cexpoint,cex.lab=cexlab,pch=20,col='red')
  axis(side=1,at=c(MINCONST,xy$x,MAXCONST),
       labels=c(MINCONST,exprlist(round(xy$x),axissz+1),MAXCONST),cex.axis=cexaxis,lwd=lwdaxis);
  axis(side=2,at=c(MINCONST,xy$y,MAXCONST),
       labels=c(MINCONST,exprlist(round(xy$y),axissz+1),MAXCONST),cex.axis=cexaxis,lwd=lwdaxis);
  legend(legendstart,legendend,bg='transparent',border='transparent',horiz=F,bty='n',
         legend=c('Entire graph','Blamed nodes with','"synchronized behaviors"','Unblamed nodes'),
         lty=c(4,2,4,4),cex=cexlegend,lwd=lwdline*1.5,
         col=c('black','blue','white','red'))
  dev.off()
}

# Given [Ns x Ns] cell distributions [cellfile: top 2 lines for X-axis and Y-axis],
# plot in [pngfile] with jet color:
# label on X-axis [xlabel] and Y-axis [ylabel], frequency follows [mathtype:log].
jetplot <- function() {
  library(GA)
  library(gplots)
  cellfile <- args[4]
  pngfile <- args[5]
  xlabel <- args[6]
  ylabel <- args[7]
  mathtype <- args[8]
  Ns <- as.numeric(args[9])
  data <- compress(as.matrix(read.csv(cellfile,header=F)),Ns,mathtype)
  plotbound <- getplotbound(data$datac,data$Nx,data$Ny,xpercent)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  filled.contour(data$datac,color.palette=colorRampPalette(jet),axes=F,
                 plot.title=title(xlab=xlabel,ylab=ylabel,cex.lab=cexlab),
                 plot.axes=matrix.axes(data$datax,data$datay,data$Nx,data$Ny,plotbound$pxlim),
                 key.axes=matrix.colaxes(mathtype,data$maxc),
                 xlim=c(plotbound$pxlim,1))
  dev.off()
}
jetcompplot <- function() {
  library(GA)
  library(gplots)
  cellfile <- args[4]
  cellfilefore <- args[5]
  pngfile <- args[6]
  xlabel <- args[7]
  ylabel <- args[8]
  mathtype <- args[9]
  Ns <- as.numeric(args[10])
  data <- compress(as.matrix(read.csv(cellfile,header=F)),Ns,mathtype)
  datafore <- compress(as.matrix(read.csv(cellfilefore,header=F)),Ns,mathtype)
  newdatac <- array(0,dim=c(data$Nx,data$Ny))
  newdatax <- array(0,dim=c(datafore$Nx,1))
  newdatay <- array(0,dim=c(datafore$Ny,1))
  j <- 1
  for (i in 1:datafore$Nx) {
    while (datafore$datax[i] > data$datax[j]) {
      j <- j+1
    }
    newdatax[i] <- j
  }
  j <- 1
  for (i in 1:datafore$Ny) {
    while (datafore$datay[i] > data$datay[j]) {
      j <- j+1
    }
    newdatay[i] <- j
  }
  for (i in 1:datafore$Nx) {
    for (j in 1:datafore$Ny) {
      newdatac[newdatax[i],newdatay[j]] <- datafore$datac[i,j]
    }
  }
  plotbound <- getplotbound(data$datac,data$Nx,data$Ny,xpercent)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  filled.contour(newdatac,color.palette=colorRampPalette(jet),axes=F,
                 plot.title=title(xlab=xlabel,ylab=ylabel,cex.lab=cexlab),
                 plot.axes=matrix.axes(data$datax,data$datay,data$Nx,data$Ny,plotbound$pxlim),
                 key.axes=matrix.colaxes(mathtype,data$maxc),
                 xlim=c(plotbound$pxlim,1))
  dev.off()
}
grayplot <- function() {
  library(GA)
  library(gplots)
  cellfile <- args[4]
  pngfile <- args[5]
  xlabel <- args[6]
  ylabel <- args[7]
  mathtype <- args[8]
  Ns <- as.numeric(args[9])
  data <- compress(as.matrix(read.csv(cellfile,header=F)),Ns,mathtype)
  plotbound <- getplotbound(data$datac,data$Nx,data$Ny,xpercent)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  filled.contour(data$datac,color.palette=colorRampPalette(gray),axes=F,
                 plot.title=title(xlab=xlabel,ylab=ylabel,cex.lab=cexlab),
                 plot.axes=matrix.axes(data$datax,data$datay,data$Nx,data$Ny,plotbound$pxlim),
                 key.axes=matrix.colaxes(mathtype,data$maxc),
                 xlim=c(plotbound$pxlim,1))
  dev.off()
}
celltofore <- function(cell,datasrcx,datax,Nx,pxlim,forexa,forexb) {
  x = datasrcx[cell]
  for (i in 1:Nx) if (datax[i] > x) break
  fore <- forexa+((i-1)/Nx-pxlim)/(1-pxlim)*(forexb-forexa)
  return(fore)
}
celltopoint <- function(datacell,plotbound,data) {
  dimdatacell <- dim(datacell)
  for (i in 1:dimdatacell[1]) {
    datacell[i,1] <- celltofore(datacell[i,1]+1,data$datasrcx,data$datax,data$Nx,plotbound$pxlim,plotbound$forexa,plotbound$forexb)
    datacell[i,2] <- celltofore(datacell[i,2]+1,data$datasrcy,data$datay,data$Ny,0,0,1)
  }
  return(datacell)
}
grayabplot <- function() {
  library(GA)
  library(gplots)
  cellfile <- args[4]
  cellafile <- args[5]
  cellbfile <- args[6]
  pngfile <- args[7]
  xlabel <- args[8]
  ylabel <- args[9]
  mathtype <- args[10]
  Ns <- as.numeric(args[11])
  data <- compress(as.matrix(read.csv(cellfile,header=F)),Ns,mathtype)
  plotbound <- getplotbound(data$datac,data$Nx,data$Ny,xpercent)
  dataa <- as.matrix(read.csv(cellafile,header=F))
  datab <- as.matrix(read.csv(cellbfile,header=F))
  dataa <- celltopoint(dataa,plotbound,data)
  datab <- celltopoint(datab,plotbound,data)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  filled.contour(data$datac,color.palette=colorRampPalette(gray),axes=F,
                 plot.title=title(xlab=xlabel,ylab=ylabel,cex.lab=cexlab),
                 plot.axes=matrix.axes(data$datax,data$datay,data$Nx,data$Ny,plotbound$pxlim),
                 key.axes=matrix.colaxes(mathtype,data$maxc),
                 xlim=c(plotbound$pxlim,1))
  for (op in 1:OVERPLOT) {
    points(dataa[,1],dataa[,2],cex=cexsymbol,pch=0,col='red')
    points(datab[,1],datab[,2],cex=cexsymbol,pch=2,col='red')
  }
  legend(plotbound$forexa,1,bg='transparent',border='transparent',horiz=F,bty='n',
         c('',''),cex=cexsymbol,pch=c(0,2),col='red')
  dev.off()
}
grayabfillplot <- function() {
  library(GA)
  library(gplots)
  cellfile <- args[4]
  cellafile <- args[5]
  cellbfile <- args[6]
  pngfile <- args[7]
  xlabel <- args[8]
  ylabel <- args[9]
  mathtype <- args[10]
  Ns <- as.numeric(args[11])
  data <- compress(as.matrix(read.csv(cellfile,header=F)),Ns,mathtype)
  plotbound <- getplotbound(data$datac,data$Nx,data$Ny,xpercent)
  dataa <- as.matrix(read.csv(cellafile,header=F))
  datab <- as.matrix(read.csv(cellbfile,header=F))
  dataa <- celltopoint(dataa,plotbound,data)
  datab <- celltopoint(datab,plotbound,data)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  filled.contour(data$datac,color.palette=colorRampPalette(gray),axes=F,
                 plot.title=title(xlab=xlabel,ylab=ylabel,cex.lab=cexlab),
                 plot.axes=matrix.axes(data$datax,data$datay,data$Nx,data$Ny,plotbound$pxlim),
                 key.axes=matrix.colaxes(mathtype,data$maxc),
                 xlim=c(plotbound$pxlim,1))
  for (op in 1:OVERPLOT) {
    points(dataa[,1],dataa[,2],cex=cexsymbol,pch=15,col='red')
    points(datab[,1],datab[,2],cex=cexsymbol,pch=17,col='red')
  }
  legend(plotbound$forexa,1,bg='transparent',border='transparent',horiz=F,bty='n',
         c('',''),cex=cexsymbol,pch=c(15,17),col='red')
  dev.off()
}

matrix.lbaxes <- function(datax,datay,Nx,Ny,pxlim) {
  szy <- axissz
  szx <- round(szy/(1-pxlim))
  x <- (0:szx)/szx
  y <- (0:szy)/szy
  xx <- 1+round(x*Nx)
  yy <- 1+round(y*Ny)
  xx[szx+1] = min(Nx,xx[szx+1])
  yy[szy+1] = min(Ny,yy[szy+1])
  maxdatax = datax[xx[szx+1]]
  maxdatay = datay[yy[szy+1]]
  kddx = 1.0/axissz*(0:axissz)*maxdatax
  kddy = 1.0/axissz*(0:axissz)*maxdatay
  axis(side=1,at=c(MINCONST,x,MAXCONST),labels=c(MINCONST,exprlist(kddx,szx+1),MAXCONST),las=0,cex.axis=cexaxis,lwd=lwdaxis);
  axis(side=2,at=c(MINCONST,y,MAXCONST),labels=c(MINCONST,exprlist(kddy,szy+1),MAXCONST),las=0,cex.axis=cexaxis,lwd=lwdaxis);
}
jetlbplot <- function() {
  DIGITNUMSMALL <- 2
  library(GA)
  library(gplots)
  cellfile <- args[4]
  celllbfile <- args[5]
  pngfile <- args[6]
  xlabel <- args[7]
  ylabel <- args[8]
  mathtype <- args[9]
  Ns <- as.numeric(args[10])
  data <- compress(as.matrix(read.csv(cellfile,header=F)),Ns,mathtype)
  plotbound <- getplotbound(data$datac,data$Nx,data$Ny,xpercent)
  datalb <- as.matrix(read.csv(celllbfile,header=F))
  datalb <- celltopoint(datalb,plotbound,data)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  filled.contour(data$datac,color.palette=colorRampPalette(jet),axes=F,
                 plot.title=title(xlab=xlabel,ylab=ylabel,cex.lab=cexlab),
                 plot.axes=matrix.lbaxes(data$datax,data$datay,data$Nx,data$Ny,plotbound$pxlim),
                 key.axes=matrix.colaxes(mathtype,data$maxc),
                 xlim=c(plotbound$pxlim,1))
  lines(datalb[,1],datalb[,2],col='red',lty=1,lwd=lwdline+2)
  dev.off()
}

jethdplot <- function() {
  library(GA)
  library(gplots)
  cellfile <- args[4]
  pngfile <- args[5]
  xlabel <- args[6]
  ylabel <- args[7]
  mathtype <- args[8]
  Ns <- as.numeric(args[9])
  arr <- strsplit(args[10],'-')[[1]]
  if (arr[1] == '1m') strsz <- '[1M]'
  if (arr[1] == '2m') strsz <- '[2M]'
  if (arr[1] == '3m') strsz <- '[3M]'
  if (arr[2] == 'pop') camoutp <- 'TOP POPULAR'
  if (arr[2] == 'rand') camoutp <- 'RANDOM from ALL'  
  data <- compress(as.matrix(read.csv(cellfile,header=F)),Ns,mathtype)
  plotbound <- getplotbound(data$datac,data$Nx,data$Ny,xpercent)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  filled.contour(data$datac,color.palette=colorRampPalette(jet),axes=F,
                 plot.title=title(xlab=xlabel,ylab=ylabel,cex.lab=cexlab),
                 plot.axes=matrix.axes(data$datax,data$datay,data$Nx,data$Ny,plotbound$pxlim),
                 key.axes=matrix.colaxes(mathtype,data$maxc),
                 xlim=c(plotbound$pxlim,1))
  if (arr[3] == 'none') {
    legend(plotbound$forexa,1.05,bg='transparent',border='transparent',horiz=F,bty='n',
           c(paste(strsz,' nodes',sep=''),
             'No injected nodes'),
           cex=3.5,pch=20,col='white')
  } else{
    camousz <- as.numeric(arr[3])
    injectsz <- 100-camousz 
    legend(plotbound$forexa,1.05,bg='transparent',border='transparent',horiz=F,bty='n',
           c(paste(strsz,' + 31K nodes',sep=''),
             paste(injectsz,'% injected targets',sep=''),
             paste('[',camousz,'%] as camouflage',sep=''),
             paste('to ',camoutp,sep='')),
           cex=3.5,pch=20,col='white')
  }
  dev.off()
}
jetsnplot <- function() {
  library(GA)
  library(gplots)
  cellfile <- args[4]
  pngfile <- args[5]
  xlabel <- args[6]
  ylabel <- args[7]
  mathtype <- args[8]
  Ns <- as.numeric(args[9])
  arr <- strsplit(args[10],'-')[[1]]
  if (arr[1] == '1m') strsz <- '[1M]'
  if (arr[1] == '2m') strsz <- '[2M]'
  if (arr[1] == '3m') strsz <- '[3M]'
  if (arr[2] == 'pop') camoutp <- 'TOP POPULAR'
  if (arr[2] == 'rand') camoutp <- 'RANDOM from ALL'  
  data <- compress(as.matrix(read.csv(cellfile,header=F)),Ns,mathtype)
  plotbound <- getplotbound(data$datac,data$Nx,data$Ny,xpercent)
  png(file=pngfile,width=imgsz,heigh=imgsz)
  par(mar=parmar,mgp=parmgp)
  filled.contour(data$datac,color.palette=colorRampPalette(jet),axes=F,
                 plot.title=title(xlab=xlabel,ylab=ylabel,cex.lab=cexlab),
                 plot.axes=matrix.axes(data$datax,data$datay,data$Nx,data$Ny,plotbound$pxlim),
                 key.axes=matrix.colaxes(mathtype,data$maxc),
                 xlim=c(plotbound$pxlim,1))
  if (arr[3] == 'none') {
    legend(plotbound$forexa,1.05,bg='transparent',border='transparent',horiz=F,bty='n',
           c(paste(strsz,' nodes',sep=''),
             'No injected nodes'),
           cex=3.5,pch=20,col='white')
  } else{
    camousz <- as.numeric(arr[3])
    injectsz <- 100-camousz 
    legend(plotbound$forexa,1.05,bg='transparent',border='transparent',horiz=F,bty='n',
           c(paste(strsz,' + 31K nodes',sep=''),
             paste(injectsz,'% injected targets',sep=''),
             paste('[',camousz,'%] as camouflage',sep=''),
             paste('to ',camoutp,sep='')),
           cex=3.5,pch=20,col='white')
  }
  dev.off()
}

if (func == 'deg') degplot()
if (func == 'ccdf') ccdfplot()
if (func == 'degcomp') degcompplot()
if (func == 'degcompsimp') degcompsimpplot()
if (func == 'jet') jetplot()
if (func == 'jetcomp') jetcompplot()
if (func == 'gray') grayplot()
if (func == 'grayab') grayabplot()
if (func == 'grayabfill') grayabfillplot()
if (func == 'jetlb') {
  DIGITNUMSMALL <- 2
  jetlbplot()
}
if (func == 'jethd') {
  jethdplot()
}
if (func == 'jetsn') {
  xpercent < - 1.0
  REMOVEEMPTY <- 0
  DIGITNUMSMALL <- 2
  TINYVALUE <- 1
  jetsnplot()
}
