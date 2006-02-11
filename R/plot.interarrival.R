"plot.interarrival" <-
function(x, start, end, width = 11, logy=FALSE, maxy, id, ...){
	orig <- as.character(substitute(x))
	if(!inherits(x,"interarrival"))
		stop(gettextf("%s is not an %s object",
  		sQuote(orig),sQuote("interarrival")))
	old.par <- par(no.readonly=TRUE)
	on.exit(par(old.par))
	if(missing(id)) id <- x$id[1]
	if(missing(start)) start <- NULL
	if(missing(end)) end <- start
	dat <- mksub(x,start,end,id=id)
	if(nrow(dat) > 0)
		trange <- as.integer(format(range(dat$date),"%Y"))
	xlab <- .seasxlab(width)
	ylab1 <- gettext("Wet days"); ylab2 <- gettext("Dry days")
	main <- .seastitle(id=id,orig=orig,range=trange,...)
	ylim <- range(c(dat$wet,dat$dry),na.rm=TRUE)
	if(nrow(dat) <= 0) {
		frame(); title(main$title); text(.5,.5,gettext("no data"))
		return(NA)
	}
	if(logy) logy <- "y" else {
		logy <- ""
		ylim[1] <- 0
	}
	if(!missing(maxy)) ylim[2] <- maxy
	dat$fact <- mkfact(dat,width)
	num <- length(levels(dat$fact))
	par(mfrow=c(2,1)) # does plot.new()
	frame()
	par(mar=c(2.1,4.1,4.1,2.1),yaxs="i",xaxs="r",bty="l")
	plot.window(c(0.5,num+0.5),ylim=ylim,log=logy)
	if(is.numeric(width)) {
		l <- seq(0.5,num+0.5,length.out=13)
		abline(v=l,col = "lightgray")
		if(is.numeric(width)) {
			month.abb <- months(as.Date(paste(2000,1:12,1,sep="-")),TRUE)
			m <- l + diff(l)[1]/2
			m <- m[-length(m)]
			axis(3,m,month.abb,tick=FALSE,line=-1)
			#mtext(month.abb,at=m)
		}
	}
	boxplot(wet ~ fact,dat,add=TRUE,ylab=ylab1,col="blue",varwidth=TRUE)
	title(main=main$title,line=main$line)
	frame()
	par(mar=c(5.1,4.1,1.1,2.1),yaxs="i",xaxs="r",bty="c")
	plot.window(c(0.5,num+0.5),ylim=rev(ylim),log=logy)
	if(is.numeric(width)) {
		abline(v=l,col = "lightgray")
	}
	boxplot(dry ~ fact,dat,add=TRUE,ylab=ylab2,col="orange",varwidth=TRUE)
	axis(3,at=1:num,labels=FALSE)
}

