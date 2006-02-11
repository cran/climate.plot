"plot.seas.param" <-
function(x, start, end, width=11, param, col = "light grey", id,
		ylab, ylim, add.alt, ...) {
	orig <- as.character(substitute(x))
	if(!inherits(x,"data.frame"))
  		stop(gettextf("%s is not a %s",
  			sQuote(orig),sQuote("data.frame")))
	if(missing(id)) id <- x$id[1]
	if(missing(start)) start <- NULL
	if(missing(end)) end <- start
	dat <- mksub(x,start,end,id=id)
	if(nrow(dat) > 0)
		trange <- as.integer(format(range(dat$date),"%Y"))
	main <- .seastitle(id=id,orig=orig,range=trange,...)
	xlab <- .seasxlab(width)
	if(nrow(dat) <= 0) {
		frame(); title(main$title); text(.5,.5,gettext("no data"))
		warning("no data")
		return(NA)
	}
	if(missing(ylab)) ylab <- param
	dat$fact <- mkfact(dat,width)
	num <- length(levels(dat$fact))
	dat$val <- dat[,param]
	plot.new()
	par(bty="l",yaxs="i",xaxs="r")
	if(missing(add.alt))
		add.alt <- FALSE
	else {
		if(is.numeric(add.alt) && length(add.alt) == 2) {
			slope <- add.alt[1]
			inter <- add.alt[2]
			add.alt <- TRUE
		} else {
			warning(gettextf("%s must give the slope and intercept\nbetween the primary and secondary axis;\n use %s\n",
				sQuote("add.alt"),sQuote("add.imp=c(slope,inter)")))
			add.alt <- FALSE
		}
	}
	if(add.alt) par(mar=c(5.1,4.1,4.1,4.1),bty="u")
	else par(mar=c(5.1,4.1,4.1,2.1),bty="l")
	if(missing(ylim)) {
		ylim <- range(dat$val,na.rm=TRUE)
		ylim <- ylim+diff(ylim)*0.04*c(-1,1) # simulate yaxs="r"
	}
	plot.window(xlim=c(0.5,num+0.5),ylim)
	if(is.numeric(width)) {
		l <- seq(0.5,num+0.5,length.out=13)
		abline(v=l,col = "lightgray")
		if(is.numeric(width)) { # support locales
			month.abb <- months(as.Date(paste(2000,1:12,1,sep="-")),TRUE)
			m <- l + diff(l)[1]/2
			m <- m[-length(m)]
			axis(3,m,month.abb,tick=FALSE,line=-1)
		}
	}
	pl <- suppressWarnings(
		boxplot(by(dat,dat$fact,function(x)x$val),
			xlab=xlab,ylab=ylab[1],varwidth=TRUE,add=TRUE,col=col))
	title(main=main$title,line=main$line)
	if(add.alt) {
		alt.ax <- pretty(ylim*slope+inter)
		axis(side=4,at=(alt.ax-inter)/slope,lab=alt.ax,srt=90)
		if(!is.na(ylab[2]))
			mtext(ylab[2],side=4,line=2.8)
	}
	invisible(pl)
}
