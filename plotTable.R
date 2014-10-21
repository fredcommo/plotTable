# Visualize a contingency table


plotTable <- function(Table, r=max(Table)/100, t=1, showLines=TRUE, freq=TRUE, ...){
  rnames <- rownames(Table)
  Table <- lapply(nrow(Table):1, function(ii) Table[ii,])
  Table <- do.call(rbind, Table)
  rownames(Table) <- rev(rnames)
  
  nCol <- ifelse(is.na(ncol(Table)), 1, ncol(Table))
  nRow <- ifelse(is.na(nrow(Table)), 1, nrow(Table))
  argNames <-names(as.list(match.call()))
  axisNames <- names(attr(Table, "dimnames"))
  
  plot(c(0.5, nCol+.5), c(0.5, nRow+.5), type='n', axes=FALSE, ...)
  
  if(showLines){
    segments(x0 = seq(1, nCol), y0 = rep(0.5, nCol), y1 = rep(nRow+0.5, nCol), lty = 2)
    segments(x0 = rep(0.5, nRow), x1 = rep(nCol+0.5, nCol), y0 = seq(1, nRow), lty = 2)
  }
  axis(1, at = seq(1, nCol), labels = colnames(Table), lwd = 2, ...)
  axis(2, at = seq(nRow, 1), labels = rownames(Table), lwd = 2, ...)
  
  for(i in 1:nRow){
    y = Table[i,]
    if(freq){
      p = y
      Cex = ifelse(y != 0, log10(1+p*2)*t, 0)
    }
    else{
      p = round(y/sum(Table), 3)
      Cex = ifelse(y != 0, log10(1+y*2)*t, 0)
    }
    R <- ifelse(y != 0, log10(1+y)*r/(nRow*nCol), NA)
    symbols(seq(1, length(y)), rep(nRow - i + 1, length(y)),
            circles = R,
            inches = FALSE, add = TRUE, ...)
    text(seq(1, length(y)), rep(nRow - i + 1, length(y)),
         labels = ifelse(y != 0, round(p, 3), NA),
         cex = Cex)
  }
}

