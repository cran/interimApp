#' Creates event plot diplaying the number of patients that have been screened and enrolled per trial week.
#' If the parameter \code{e1} are not \code{NULL}, then the number of events and number of drop-out before the first event is also displayed.
#'
#' Based on the function \code{eventCourse}.
#'
#' Requires
#' @param r recruitment scenario calculated with function \code{recruitment} of the interim package and
#' @param e1 \emph{optional} for the simulation of the event simulation from function \code{event} of the interim package.

eventPlot <- function(r,e1=NULL){
  e1Col="#81a5c9" # lightblue
  e2Col="#81a5c9" # lightblue
  coCol="#5ca754" # green
  ccCol="#cb4b41" # red
  scCol="#165b97" # darkblue
  enCol="#597dae" # midblue
  t1Col="#81a5c9" # lightblue
  t2Col="#81a5c9" # lightblue
  w=union(r$weeksOfTrial,r$weeksOfEnrollment)
  yLabel="Screened and enrolled patients"

  if (!is.null(e1)) {
    w=union(w,e1$weeksOfEvent)
    yLabel="Screened, enrolled, and events"
  }

  y=c(rep(0,length(w)-1),max(r$screenings))
  on_ex <- par("pty","bg","col.axis","col.lab","col.main","col.sub","font.axis","font.lab","font.main","font.sub","cex.axis","cex.lab","cex.main")
  on.exit(par(on_ex))
  par(pty="s", bg = "#6b6b6b", fg="#383838", col.axis="#6b6b6b", col.lab="#6b6b6b", col.main="#6b6b6b", col.sub="#6b6b6b",
      font.axis=2, font.lab=2, font.main=2, font.sub=2,
      cex.axis=1.2, cex.lab=1.5, cex.main=2)
  plot(w,y,type="n",main="Patients",xlab="Week",ylab=yLabel)
  h <- par("usr")
  rect(h[1], h[3], h[2], h[4], col = "#ffffff")
  lines(r$weeksOfTrial,r$screenings,type="l",lwd=3,col=scCol)
  lines(r$weeksOfEnrollment,r$enrollments,type="l",lwd=3,col=enCol)
  if (!is.null(e1))
    lines(e1$weeksOfEvent,e1$events,type="l",lty=6,lwd=3,col=e1Col)
  if (!is.null(e1$drops))
    lines(e1$weeksOfEvent,e1$drops,type="l",lty=5,lwd=3,col=e1Col)
  if (is.null(e1))
    legend("topleft",lwd=3,col=c(scCol,enCol),legend=c("Screened patients","Enrolled patients"),bty="n",text.font=2)
  else
    if (!is.null(e1)&is.null(e1$drops))
      legend("topleft",lwd=3,lty=c(1,1,6),col=c(scCol,enCol,e1Col),legend=c("Screened patients","Enrolled patients","Events"),bty="n",text.font=2)
  else
    if (!is.null(e1)&!is.null(e1$drops))
      legend("topleft",lwd=3,lty=c(1,1,6,5),col=c(scCol,enCol,e1Col,e1Col),legend=c("Screened patients","Enrolled patients","Events","Drop outs before event"),bty="n",text.font=2)
}
