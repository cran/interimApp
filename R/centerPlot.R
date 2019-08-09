#' Creates center plot diplaying the cumulative number of centers that have been opened
#' as well as the cumulative number of centers that have been closed, if applicable, per trial week.
#'
#' Based on the function \code{trialCourse} and \code{eventCourse}.
#'
#' Requires
#' @param r recruitment scenario calculated with function \code{recruitment} of the interim package.

centerPlot <- function(r){
  e1Col="#81a5c9" # lightblue
  e2Col="#81a5c9" # lightblue
  coCol="#5ca754" # green
  ccCol="#cb4b41" # red
  scCol="#165b97" # darkblue
  enCol="#597dae" # midblue
  t1Col="#81a5c9" # lightblue
  t2Col="#81a5c9" # lightblue
  w=union(r$weeksOfTrial,r$weeksOfEnrollment)

  y=c(rep(0,length(w)-1),max(r$openCenters))
  on_ex <- par("pty","bg","col.axis","col.lab","col.main","col.sub","font.axis","font.lab","font.main","font.sub","cex.axis","cex.lab","cex.main")
  on.exit(par(on_ex))
  ## par argument ensures the correct display in the Shiny app
  par(pty="s",bg = "#6b6b6b", fg="#383838", col.axis="#6b6b6b", col.lab="#6b6b6b", col.main="#6b6b6b", col.sub="#6b6b6b",
      font.axis=2, font.lab=2, font.main=2, font.sub=2,
      cex.axis=1.2, cex.lab=1.5, cex.main=2)
  plot(w,y,type="n",main="Centers",xlab="Week",ylab="Openings and closings")
  ## h and rect argument ensures the background-color of the plot
  h <- par("usr")
  rect(h[1], h[3], h[2], h[4], col = "#ffffff")
  lines(r$weeksOfTrial,r$openCenters,ylim=c(0,max(r$openCenters,r$closedCenter)),type="l",lwd=3,col=coCol)
  lines(r$weeksOfTrial,r$closedCenters,type="l",lwd=3,col=ccCol)

  legend("topleft",lwd=3,col=c(coCol,ccCol),legend=c("Center openings","Center closings"),bty="n",text.font=2)
}
