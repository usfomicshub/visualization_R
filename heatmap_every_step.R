colorChangeOri <- function(x, nlevels = 10, 
                           colPlate = c('blue', 'red'),
                           breakk = NA)
{
  if (is.na(breakk)) 
  {
    breakk = seq(min(x, na.rm = T)-0.1,max(x, na.rm = T)+0.1,length.out = nlevels)
  }
  levels <- seq(min(x, na.rm = T),max(x, na.rm = T),length.out = nlevels)
  colCutoff <- colorRampPalette(colPlate)(nlevels)
  # colCutoff <- colorRampPalette(c(
  #   rgb(48, 58, 142, max = 255),
  #   rgb(63, 118, 191, max = 255),
  #   rgb(110, 177, 205, max = 255),
  #   rgb(172, 221, 237, max = 255),
  #   rgb(227, 241, 249, max = 255),
  #   'white',
  #   rgb(252, 247, 190, max = 255),
  #   rgb(235, 204, 141, max = 255),
  #   rgb(255, 170, 101, max = 255),
  #   rgb(245, 107, 77, max = 255),
  #   rgb(217, 52, 45, max = 255),
  #   rgb(134, 35, 88, max = 255)
  # ))(nlevels)
  
  # empty matrix for final color
  colMatrix    <- matrix(NA, nrow(x), ncol(x))
  
  colMatrix <- matrix(colCutoff[cut(x, breaks = breakk)], nrow(x), ncol(x))
  print(cut(x, breaks = breakk))
  # print(colMatrix)
  # colzMatrix  <- apply(colMatrix, 2, function(x) {  # sep or NA is replaced by 'white'
  #   x[which(is.na(x))] <- rep('white', length(which(is.na(x)))); x})
  
  col <- colCutoff
  return(list(colorMatrix = colMatrix, colorBar = col, levels = breakk))
}


###!!! matrixHeat is the color matrix, from colorChange
heatMap <- function(matrixHeat, Rname = '', Cname = '') 
{
  #par(mar = c(1,1,1,1))
  #layout(matrix(c(1,2,1,3), 2, 2, byrow  = TRUE), widths = c(4,1, 1), heights = c(4,2,2))
  plot(x=NA,y=NA,xlim = c(0, ncol(matrixHeat)),ylim = c(0.5,nrow(matrixHeat)-0.5), 
       yaxt="n",
       xaxt = "n",
       xlab = '', bty = 'n',
       ylab = '',
       cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
  # axis(1, at = 1:ncol(matrixHeat)-0.5, cex.axis = 1, las = 2,
  #      labels = rep('', ncol(matrixHeat)))
  text(par("usr")[1] + 0.2, 1:nrow(matrixHeat)-0.5, labels = rev(Rname), 
       srt = 0, pos = 2, xpd = TRUE, cex = 0.8)
  text(1:ncol(matrixHeat) -0.05, 
       par("usr")[3] - 0.07, labels = Cname, 
       srt = 0, pos = 2, xpd = T, cex = 0.1)
  
  for (i in 1:ncol(matrixHeat)) 
  {
    rect(rep(i, ncol(matrixHeat)) - 1,  # xleft
         1:(nrow(matrixHeat)) - 1,      # ybottom
         rep(i, ncol(matrixHeat)),      # xright
         1:(nrow(matrixHeat)),          # ytop
         col = rev(matrixHeat[,i]),
         border = 'gray40',
         lwd = 0.5
         #border = rev(matrixHeat[,i])
    )
  }
} 

colorBar <- function(col, levels, names = '') 
{
  par(mar = c(1,4,2,1))
  plot(x=NA,y=NA,xlim = c(0,0.95),
       ylim = c(min(levels) , max(levels) ), 
       yaxt="n",
       xaxt = "n",bty = 'n',
       xlab = '',
       ylab = '',
       cex.lab=1.5, cex.axis=1.5, cex.main=1, cex.sub=1)
  axis(4, at = seq(0,40,5), las = 3,
       cex.axis = 2, pos = 0.5)
  text(par("usr")[1] + 0.25, max(levels) + 0.05, labels = names, 
       srt = 1, pos = 3, xpd = TRUE, cex = 1.5)  # tittle of level bar
  rect(0, levels[-length(levels)], 0.5, levels[-1L],col=col) 
}
