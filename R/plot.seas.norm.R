"plot.seas.norm" <-
function(x, maxy, varwidth=FALSE, normwidth=FALSE,
		show.na=TRUE, leg, add.alt=FALSE, ...) {
	orig <- as.character(substitute(x))
	if(!inherits(x,"seas.norm"))
		stop(gettextf("%s is not a %s object",
			sQuote(orig), sQuote("seas.norm")))
	dat <- x
	fun <- dat$fun
	precip.norm <- dat$precip.norm
	width <- dat$width
	seas <- dat$seas
	ann <- dat$ann
	param <- dat$param
	main <- .seastitle(id=dat$id,orig=orig,fun=fun,range=dat$range,...)
	xlab <- .seasxlab(width)
	num <- length(dat$bins)
	active <- seas$active
	active[is.na(active)] <- 0
	active[active == 0] <- 0.2 # to show at least a sliver of data
	if(varwidth){ # normalize width of bars
		maxf <- max(seas$active,na.rm=TRUE)
		if(normwidth){
			if(is.logical(normwidth))
				active <- active/max(active,na.rm=TRUE)
			else {
				med.days <- median(table(mkfact(width=width,y=2001)))
				active <- active*med.days/normwidth # assumed numeric
				if (normwidth < maxf)
					warning(paste(gettextf("%s < maximum width value; not a good plot",sQuote("normwidth")),
						gettextf(" ... should be no less than %.1f",round(maxf,1)),sep="\n"))
			}
		}
	} else active <- 1
	unit <- dat$unit
	alt.unit <- ifelse(unit=="mm","in",ifelse(unit=="in","mm",""))
	if(fun %in% c("mean","sd","median","min","max","var")){
		ylab <- gettextf("%s/day",unit)
		alt.ylab <- gettextf("%s/day",alt.unit)
	} else alt.ylab <- ylab <- ""
	if(fun == "var") {
		squareSym <- iconv("\262","latin1","")
		ylab <- sprintf("(%s)%s",ylab,squareSym)
		alt.ylab <- sprintf("(%s)%s",alt.ylab,squareSym)
	}
	leg <- ifelse(missing(leg) && fun %in% c("mean","median"),TRUE,FALSE)
	if(!precip.norm) ylab <- paste(param,ylab)
	if (missing(maxy)) ylim <- range(pretty(range(0,seas$precip,seas$rain+seas$snow,na.rm=TRUE)))
	else ylim <- c(0,maxy)
	par(yaxs="i",xaxs="r")
	if(add.alt) par(mar=c(5.1,4.1,4.1,4.1),bty="u")
	else par(mar=c(5.1,4.1,4.1,2.1),bty="l")
	plot.new()
	plot.window(c(0.5,num+0.5),ylim=ylim)
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
	lx <- 1:num - active/2
	rx <- 1:num + active/2
	bot <- 1:num*0
	if(varwidth) border <- FALSE else border <- TRUE
	if(precip.norm){
		rect(lx,bot,rx,seas$snow,col="grey",border=border) # snow boxes
		rect(lx,seas$snow,rx,seas$snow+seas$rain,col="light blue",border=border) # rain boxes
	} else
		rect(lx,bot,rx,seas$precip,col="dark grey",border=border) # precipitation only
	if(!(is.logical(leg) && leg == FALSE)) {
		if(substitute(leg) == "locator") {
			xy <- locator(1)
			leg.x <- xy$x
			leg.y <- xy$y
		} else {
			leg.x <- 0.5+num*.02
			leg.y <- max(ylim)*0.98
		}
		annrate <- gettextf("%s/year",unit)
		if(!precip.norm)
			leg.text <- c(gettextf("%.1f days with %s",round(ann$active,1),param),
					gettextf("Total %s %.1f %s",param,ann[1,prime],annrate))
		else
			leg.text <- c(gettextf("%.1f days with precipitation",round(ann$active,1)),
					gettextf("Rain %.1f",ann$rain),
					gettextf("Snow %.1f",ann$snow),
					gettextf("Total %.1f %s",ann$precip,annrate))
		text(leg.x,leg.y,paste(leg.text,collapse="\n"),adj=c(0,1))
	}
	if(show.na) {
		na <- seas$na
		na.h = max(ylim)/150
		na[na < 0.05] <- NA # don't plot red box if there is only 5% data missing or less
		rect(1:num - na/2,0,1:num + na/2,na.h,col="red",border=FALSE)
	}
	abline(h=0)
	axis(1,1:num,dat$bins)
	axis(2)
	title(main=main$title,line=main$line)
	title(xlab=xlab,ylab=ylab)
	if(add.alt) {
		mm2in <- function(v)(v/25.4)
		in2mm <- function(v)(v*25.4)
		if(unit=="mm") {
			alt.ax <- pretty(mm2in(ylim))
			axis(side=4,at=in2mm(alt.ax),lab=alt.ax,srt=90)
			mtext(alt.ylab,side=4,line=2.8)
		} else if(unit=="in") {
			alt.ax <- pretty(in2mm(ylim))
			axis(side=4,at=mm2in(alt.ax),lab=alt.ax,srt=90)
			mtext(alt.ylab,side=4,line=2.8)
		}
	}
}
