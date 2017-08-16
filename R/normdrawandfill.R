#' Load a Matrix
#'
#' A function to draw Normal distribution density curve (bell curve) and highlight different areas of distribution
#' by filling the area with chosen color and displaying Probability percentages
#' Created by Sulekha Aloorravi
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export

normdrawandfill = function(mean=0,sd=1,points=200,linecolor="orange",fillcolor = "blue",fillafter = NULL,
                           fillbefore = NULL, fillwithin = NULL,thickness=40, xlabel = NULL, ylabel = NULL, axiscol = "red")
{
  startx = mean - 4*sd
  endx = mean + 4*sd
  xplot = seq(startx,endx, length = points)
  normcurve = dnorm(xplot,mean,sd)
  plot(xplot,normcurve, type = "l", col = linecolor, xlab = xlabel, ylab = ylabel)

  if(is.null(fillbefore)==FALSE)
  {
    before = xplot[xplot<fillbefore]
    areabefore = normcurve[xplot<fillbefore]
    polygon(c(before,rev(before)),c(rep(0,length(before)),rev(areabefore)),col=fillcolor, border = "pink", density = thickness)
  }

  if(is.null(fillafter)==FALSE)
  {
    after = xplot[xplot>fillafter]
    areaafter = normcurve[xplot>fillafter]
    polygon(c(after,rev(after)),c(rep(0,length(after)),rev(areaafter)),col=fillcolor, border = "pink", density = thickness)
  }

  if(is.null(fillwithin)==FALSE)
  {
    within = xplot[xplot>fillwithin[1]&xplot<fillwithin[2]]
    areawithin = normcurve[xplot>fillwithin[1]&xplot<fillwithin[2]]
    polygon(c(within,rev(within)),c(rep(0,length(within)),rev(areawithin)),col=fillcolor, border = "pink", density = thickness)
  }

  segments(mean,0,0,length(normcurve))
  viewMean = paste("Mean = ", mean,"  StdDev = ", sd)
  axis(1,at=c(mean,mean+sd), col = axiscol, lty = 2, lwd = 2, outer = FALSE, tick=TRUE)
  text(mean,0, labels = viewMean, font = 2, cex = 0.8)

}
