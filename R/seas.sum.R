"seas.sum" <-
function(dat, start, end, width = 11, param, prime,
		a.cut = 0.3, na.cut = 0.2, unit = "mm", id, name) {
	cl <- match.call()
	orig <- as.character(substitute(dat))
	if(!inherits(dat,"data.frame"))
		stop(gettextf("%s is not a %s object",
			sQuote(orig),sQuote("data.frame")))
	if(missing(param))
		param <- c("precip","rain","snow","leak","evap","ezwat","et","runoff","air","soil")
	param <- names(dat)[names(dat) %in% param]
	if(length(param) == 0)
	stop(paste(gettextf("no sum parameters were found in %s",orig),
		gettextf("specify using %s parameter",sQuote("param")),collapse="\n"))
	if(missing(prime))
		prime <- ifelse(any("precip" %in% names(dat)),"precip",param[1])
	else if(!any(prime %in% param))
		warning(paste(gettextf("%s not found in %s",sQuote(prime),sQuote(param)),
			gettextf("using %s",prime <- param[1]),collapse="\n"))
	if(missing(id)) id <- unique(dat$id)[1]
	dat <- mksub(dat,id=id,rm.id=TRUE)
	dat <- dat[,c("date","year","jday",param)]
	if(missing(start)) start <- NULL
	if(missing(end)) end <- start
	dat <- mksub(dat,start,end)
	if(nrow(dat) > 0) {
		start <- min(dat$year)
		end   <- max(dat$year)
	} else {
		warning("no data")
		return(NA)
	}
	param.all <- c(param,"active","days","na")
	dat$fact <- mkfact(dat,width)
	bins <- levels(dat$fact)
	num <- as.integer(length(levels(dat$fact)))
	years <- as.integer(start:end)
	yearf <- factor(dat$year,levels=years)
	ann <- array(dim=c(length(years),length(param.all)),dimnames=list(years,param.all))
	seas <- array(dim=c(length(years),num,length(param.all)),dimnames=list(years,levels(dat$fact),param.all))
	if (length(na.cut) == 1) na.cut <- rep(na.cut,2)
	sum.is.num <- function(d) return(sum(!is.na(d),na.rm=TRUE)) # count missing days
	is.active <- function(test){ # test to count the number of active days
		tot <- numeric(length(test))
		tot[is.na(test)] <- NA # keep NAs
		tot[test > a.cut] <- 1 # find only days where the precipitation is greater than the cutoff
		if(sum(is.na(tot))/length(tot) < na.cut[2]) na.rm <- TRUE else na.rm <- FALSE
		return(sum(tot,na.rm=na.rm))
	}
	days <- function(y) (365 + ifelse(y%%4 == 0 & y%%100 != 0,1,0) + ifelse(y%%400 == 0,1,0))
	ann[,"days"] <- sapply(years,days)
	ann[,"active"] <- tapply(dat[,prime],yearf,is.active)
	ann[,"na"] <- tapply(dat[,prime],yearf,sum.is.num)
	for(p in param)
		ann[,p] <- tapply(dat[,p],yearf,sum,na.rm=TRUE)
	td <- function(y) table(mkfact(width=width,year=y))
	seas[,,"days"] <- t(sapply(years,td))
	for(y in as.character(years)) {
		s <- mksub(dat,as.integer(y))
		if(nrow(s) > 0) {
			seas[y,,"active"] <- tapply(s[,prime],s$fact,is.active)
			seas[y,,"na"] <- tapply(s[,prime],s$fact,sum.is.num)
			for(p in param)
				seas[y,,p] <- tapply(s[,p],s$fact,sum,na.rm=TRUE)
		}
	}
	ann[is.na(ann[,"na"]),"na"] <- 0
	ann[,"na"] <- ann[,"days"] - ann[,"na"]
	seas[,,"na"][is.na(seas[,,"na"])] <- 0
	seas[,,"na"] <- seas[,,"days"] - seas[,,"na"]
	ann[ann[,"na"]/ann[,"days"] > na.cut[1],c(param,"active")] <- NA
	seas[,,c(param,"active")][seas[,,"na"]/seas[,,"days"] > na.cut[2]] <- NA
	l <- list(ann=ann,seas=seas)
	l$call=cl
	l$years=years
	l$param=param
	l$prime=prime
	l$unit=unit
	l$width=width
	l$bins=bins
	l$na.cut=na.cut
	l$a.cut=a.cut
	attr(l,"class") <- "seas.sum"
	if(!is.null(id)) {
		l$id <- as.character(id)
		l$name <- getstnname(id)
	}
	if(!missing(name)) l$name <- name
	return(l)
}
