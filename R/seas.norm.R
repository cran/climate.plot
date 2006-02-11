"seas.norm" <-
function(dat, start, end, param, norm="days", fun="median",
		ann.only=FALSE, precip.norm=FALSE) {
	orig <- substitute(dat)
	if(!inherits(dat,"seas.sum"))
		stop(gettextf("%s is not a %s object",
			sQuote(orig), sQuote("seas.sum")))
	if(missing(param))
		param <- ifelse(precip.norm,"precip",dat$prime)
	if(!(param %in% dat$param))
		stop(gettextf("%s not found in %s",
			sQuote(param), sQuote(sprintf("%s$param",orig))))
	if(!(norm %in% dimnames(dat$seas)[[3]]))
		stop(gettextf("%s not found in %s",
			sQuote(norm),
			sQuote(sprintf("dimnames(%s$seas))[[3]])",orig))))
	if(precip.norm && (any(!c("rain","snow") %in% dat$param))) {
		warning(paste(
			gettextf("%s does not have either %s or %s parameters, or both",
			sQuote(orig),sQuote("rain"),sQuote("snow")),
			gettextf("using only %s",param),sep="\n"))
		precip.norm <- FALSE
	}
	if(missing(start) || is.null(start)) start <- NULL
	else {
		if(missing(end)) end <- start
		s <- dat$years %in% start:end
		dat$ann <- dat$ann[s,,drop=FALSE]
		dat$seas <- dat$seas[s,,,drop=FALSE]
		dat$years <- dat$years[s]
	}
	n.years <- length(dat$years)
	if(n.years == 0) {
		warning("no data")
		return(NA)
	}
	if(is.function(fun)) fun <- as.character(substitute(fun))
	width <- dat$width
	dat$range <- range(dat$years)
	num <- length(dat$bins)
	# determine annual normals
	ann <- data.frame(param=NA)
	param.a <- dat$ann[,param,drop=FALSE]
	if(any(!is.na(param.a))) { # make sure there are values to calculate annual normals
		ann$param <- eval(call(fun,param.a,na.rm=TRUE))
		if(precip.norm) {
			rain.a <- dat$ann[,"rain",drop=FALSE]
			snow.a <- dat$ann[,"snow",drop=FALSE]
			ann$rain <- eval(call(fun,rain.a,na.rm=TRUE))
			ann$snow <- eval(call(fun,snow.a,na.rm=TRUE))
		}
		ann$active <- NA
		ann$days <- eval(call(fun,dat$ann[,"days",drop=FALSE],na.rm=TRUE))
		ann$na <- eval(call(fun,dat$ann[,"na",drop=FALSE],na.rm=TRUE))
	} else { # an incomplete year
		ann$param <- NA
		if(precip.norm)
			 ann$snow <- ann$rain <- NA
		ann$na <- ann$active <- NA
		warning(gettext("not enough data to determine annual total precipitation"))
		if (fun == "median") {
			warning(gettextf("changing %s from %s to %s",sQuote("fun"),sQuote(fun),sQuote("mean")))
			fun <- "mean"
		}
	}
	active.a <- dat$ann[,"active",drop=FALSE]
	ann$active <- ifelse(any(!is.na(active.a)),eval(call(fun,active.a,na.rm=TRUE)),0)
	if(!ann.only) {
		# .b suffix is a matrix of sums in the width of the bin for each year
		param.b <- dat$seas[,,param,drop=FALSE]
		if(precip.norm) {
			rain.b <- dat$seas[,,"rain",drop=FALSE]
			snow.b <- dat$seas[,,"snow",drop=FALSE]
		}
		days.b <- dat$seas[,,"days",drop=FALSE]
		active.b <- dat$seas[,,"active",drop=FALSE]
		norm.b <- dat$seas[,,norm,drop=FALSE]
		na.b <- dat$seas[,,"na",drop=FALSE]
		seas <- data.frame(param=1:num*NA,row.names=dat$bins)
		if(fun == "median" && n.years > 2) {
			quan <- data.frame(param=NA)
			secant <- function (f) {
				# Secant method to find a root; f is a function which needs to be zero, specifically a quantile in [0,1]
				# Cheny, E. W. and Kincaid, D. 1999, "Numerical Mathematics and Computing", Pacific Grove: Brooks/Cole Pub., 671 p.
				a <- 0.45; b <- 0.55 # start around the 50% quantile or true median
				fa <- f(a); fb <- f(b)
				while (fa == fb && b < 1) # if there is no slope around initial guess
					fb <- f(b <- b + 0.05)
				for (i in 1:50) {
					if (a == b) stop("can not converge; outside quantile bounds [0,1]")
					if (abs(fa) > abs(fb)) { # swap
						z <- a;   a <- b;   b <- z
						fz <- fa; fa <- fb; fb <- fz
					}
					d <- (b - a)/(fb - fa)
					b <- a; fb <- fa
					d <- d*fa
					if (abs(fa) < 0.0001) return(a) # normal exit point for function
					a <- a - d
					if(a < 0) a <- 0 # fix the bounds between [0,1]
					if(a > 1) a <- 1
					fa <- f(a)
				}
				warning(gettextf("no convergence of quantile; %f > 0.0001",abs(fa)))
				return(a)
			}
			quan$param <- secant(function(qu) return(ann$param - sum(apply(param.b,2,quantile,qu,na.rm=TRUE,names=FALSE))))
			seas$param <- apply(param.b/norm.b,2,quantile,quan$param,na.rm=TRUE,names=FALSE)
			if(precip.norm) {
				quan$snow <- quan$rain <- NA
				# calculate the quantile of annual rain + snow, which equals the anual precipitation
				quan$rainsnow <- secant(function(qu) return(ann$param - quantile(rain.a,qu,na.rm=TRUE,names=FALSE) - quantile(snow.a,qu,na.rm=TRUE,names=FALSE)))
				rain.am <- quantile(ann$rain,quan$rainsnow,na.rm=TRUE,names=FALSE)
				# calculate the quantile of all rain data in the bins to equal the annual rain volume
				quan$rain <- secant(function(qu) return(rain.am - sum(apply(rain.b,2,quantile,qu,na.rm=TRUE,names=FALSE))))
				seas$rain <- apply(rain.b/norm.b,2,quantile,quan$rain,na.rm=TRUE,names=FALSE)
				snow.am <- quantile(ann$snow,quan$rainsnow,na.rm=TRUE,names=FALSE)
				if (snow.am > 0) { # median may be zero
					quan$snow <- secant(function(qu) return(snow.am - sum(apply(snow.b,2,quantile,qu,na.rm=TRUE))))
					seas$snow <- apply(snow.b/norm.b,2,quantile,quan$snow,na.rm=TRUE)
				} else seas$snow <- seas$rain*0
				rs.f <- seas$rain/(seas$rain+seas$snow) # calculate the fraction of rain
				seas$rain <- seas$param * rs.f
				seas$snow <- seas$param * (1 - rs.f) # these two operations make the bars equal for precip.norm=TRUE and FALSE
			}
			quan$active <- secant(function(qu) return(ann$active - sum(apply(active.b,2,quantile,qu,na.rm=TRUE,names=FALSE))))
			seas$active <- apply(active.b/norm.b,2,quantile,quan$active,na.rm=TRUE,names=FALSE)
			quan$days <- secant(function(qu) return(ann$days - sum(apply(days.b,2,quantile,qu,na.rm=TRUE,names=FALSE))))
			seas$days <- apply(days.b,2,quantile,quan$active,na.rm=TRUE,names=FALSE)
			if(ann$na > 0) {
				quan$na <- secant(function(qu) return(ann$na - sum(apply(na.b,2,quantile,qu,na.rm=TRUE,names=FALSE))))
				seas$na <- apply(na.b/norm.b,2,quantile,quan$active,na.rm=TRUE,names=FALSE)
			} else seas$na <- quan$days*0
		} else { # otherwise calculate stats conventionally (usually much faster)
			seas$param <- apply(param.b/norm.b,2,fun,na.rm=TRUE)
			if(precip.norm) {
				seas$rain <- apply(rain.b/norm.b,2,fun,na.rm=TRUE)
				seas$snow <- apply(snow.b/norm.b,2,fun,na.rm=TRUE)
			}
			seas$active <- apply(active.b/norm.b,2,fun,na.rm=TRUE)
			seas$days <- apply(days.b,2,fun,na.rm=TRUE)
			seas$na <- apply(na.b/norm.b,2,mean,na.rm=TRUE)
		}
	} # end if (!ann.only)
	fixnames <- function(n){
		n[n %in% "param"] <- param
		return(n)
	}
	names(ann) <- fixnames(names(ann))
	l <- list(ann=ann)
	if(!ann.only) {
		attr(l,"class") <- "seas.norm"
		names(seas) <- fixnames(names(seas))
		l$seas <- seas
		l$width <- dat$width
		l$bins <- dat$bins
	}
	l$param <- param
	l$unit <- dat$unit
	l$norm <- norm
	l$ann.only <- ann.only
	l$precip.norm <- precip.norm
	l$fun <- fun
	if(!ann.only && fun == "median") {
		names(quan) <- fixnames(names(quan))
		l$quantile <- quan
	}
	l$range <- dat$range
	l$id <- dat$id
	l$name <- dat$name
	l
}
