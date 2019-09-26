# from botom to top
rectPlot <- function(y, xStart,xEnd, col, height) 
{
  data <- cbind(xStart, xEnd)
  rect( data[,1], y, data[,2], y + height, 
        col = col, border = NA)
}
# from middle place to top
rectPlotMiddle <- function(y, xStart,xEnd, col, height) 
{
  data <- cbind(xStart, xEnd)
  rect( data[,1], y + (height), data[,2], y + 1, 
        col = col, border = NA)
}



verticleLinePlot <- function(y, x, lwd, col, height) 
{
  for (i in 1:length(x)) {lines(c(x[i],x[i]), c(y[i], y[i]+height), lwd = lwd, col = col)}
}


chrPlot <- function(chrNum, chrLength, genes, atac, modules)
{
  #chrNum <- 14
  chrLength <- chrLength[chrNum:1,]
  horizontalAxisPositionI  <- seq(from = 0,    to = (chrNum - 1) * 2.75, by = 2.75)
  horizontalAxisPositionII <- seq(from = 1.25, to = (chrNum ) * 2.75, by = 2.75)
  
  
  par(mar = c(1,0.5,1,1))
  plot(x=NA,y=NA,xlim = c(-200000,max(chrLength[,2])),
       ylim = c(-2, max(horizontalAxisPositionII)+ .5), 
       yaxt="n",
       xaxt = "n",
       xlab = '',
       ylab = '',
       bty = 'n',
       cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
  ###########plot rectangular for 
  # rect(649531, horizontalAxisPositionI[12]  - 0.125,
  #      712252, horizontalAxisPositionII[12] + 1.125,
  #      border = T, col = 'gold')
  
  ###plot axis for each chromosome
  for (i in chrNum:1)
  {
    axis(1, at = c(0-20000, chrLength[i,2])+10000, labels = c('', ''),
         las = 2,cex.axis=1, pos = c(horizontalAxisPositionI[i]),
         lwd = 0.5, lwd.ticks = 0,col = 'gray')
    axis(1, at = c(0-20000, chrLength[i,2])+10000, labels = c('', ''),
         las = 2,cex.axis=1, pos = c(horizontalAxisPositionII[i]),
         lwd = 0.5, lwd.ticks = 0,col = 'gray')
    axis(1, at = c(0-20000, chrLength[i,2])+10000, labels = c('', ''),
         las = 2,cex.axis=1, pos = c(horizontalAxisPositionI[i])+1,
         lwd = 0.5, lwd.ticks = 0,col = 'gray')
    axis(1, at = c(0-20000, chrLength[i,2])+10000, labels = c('', ''),
         las = 2,cex.axis=1, pos = c(horizontalAxisPositionII[i])+1,
         lwd = 0.5, lwd.ticks = 0,col = 'gray')
  }
  ####plot vertical axis for each chromosome
  for (i in chrNum:1) 
  {   
    axis(2, at = horizontalAxisPositionI[i] + c(0,0.5,1), labels = c('','', ''),
         las = 2,cex.axis=1, pos = -9000, outer = T,
         lwd = 0.5, lwd.ticks = 0, cex.axis= 0.4)
    axis(2, at = horizontalAxisPositionII[i] + c(0,0.5,1), labels = c('','', ''),
         las = 2,cex.axis=1, pos = -9000, outer = T,
         lwd = 0.5, lwd.ticks = 0, cex.axis= 0.4)
    
    axis(2, at = horizontalAxisPositionI[i] + c(0,1), labels = c('',''),
         las = 2,cex.axis=1, pos = chrLength[i,2]+10000, outer = T,
         lwd = 0.5, lwd.ticks = 0, cex.axis= 0.4, col = 'gray')
    axis(2, at = horizontalAxisPositionII[i] + c(0,1), labels = c('',''),
         las = 2,cex.axis=1, pos = chrLength[i,2]+10000, outer = T,
         lwd = 0.5, lwd.ticks = 0, cex.axis= 0.4, col = 'gray')
  } 
  
  
  # ###ttaaDensity
  # for (i in chrNum:1) 
  # { 
  #   geneTTAA_chri <- geneTTAA[which(geneTTAA$y == i), ]
  #   lines(y = rep(rev(horizontalAxisPositionII)[i], nrow(geneTTAA_chri)) + geneTTAA_chri$score,
  #         x = geneTTAA_chri$x,
  #         lwd = 0.7, col = 'gray30')
  #   # polygon(geneTTAA_chri$x,
  #   #         rep(rev(horizontalAxisPositionI)[i], nrow(geneTTAA_chri)) + geneTTAA_chri$score
  #   #         ,col='gray')
  # }
  # 
  rectPlotMiddle(rev(horizontalAxisPositionII)[atac[,1]],
           atac[,2], atac[,3],
           'forestgreen', 1/2)
  
  rectPlot(rev(horizontalAxisPositionII)[gene[,1]],
           gene[,2], gene[,3],
           rgb(217,116,43, max = 255), 1/2)
  
  rectPlot(rev(horizontalAxisPositionI)[modules[,1]],
           modules[,2], modules[,3],
           rgb(23,50,7, max=255), 1)

  # ####insertions Old  
  # verticleLinePlot(rev(horizontalAxisPositionI)[insertionsQIseqII[,1]],
  #                  insertionsQIseqII[,2], 0.075,
  #                  'navy', 1
  # )
  # 
  # 
  

  
  ####plot axis for each chromosome
  for (i in chrNum:1) 
  {   
    # axis(1, at = c(0, chrLength[i,2]), labels = c('', ''),
    #      las = 2,cex.axis=1, pos = horizontalAxisPositionI[i],
    #      lwd = 1, lwd.ticks = 0)
    text(x= -25000, y= (horizontalAxisPositionII)[i] - 0.225 , pos = 2,
         labels = paste('chr', 14-(i-1), sep = '_') )  
  }
  ############plot scale distance
  axis(1, at = c(100000, 200000), labels = c(' ', ''),
       las = 1,cex.axis=1, pos = -0.5,
       lwd = 1, lwd.ticks = 1)
  text(x = 150000, y = -1.9, '100KB', cex = 0.7)
  
  
  
}