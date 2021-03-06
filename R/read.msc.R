"read.msc" <- function(file, flags = FALSE, add.elem, format, verbose = TRUE) {
  elem.all <- data.frame(alias=as.integer(c(1,2,3,10,11,12,13,77,123,133,61,64)),
                         name=I(c("max_t","min_t","mean_t","rain","snow","precip","snow_d","press","rain","sun","solar","net_solar")),
                         scale=c(.1,.1,.1,.1,.1,.1,1,.01,.1,.1,.001,.001))
  if(!missing(add.elem)) {
    if(class(add.elem) %in% c("list","data.frame"))
      elem.all <-	rbind(elem.all,add.elem)
    else
      stop(gettextf("%s must either be a data.frame or a list",sQuote("add.elem")))
  }
  orig <- as.character(substitute(file))
  if (is.character(file)) {
    file <- file(file, "r")
    on.exit(close(file))
  }
  if (!inherits(file, "connection")) 
    stop(gettextf("%s must be a character string or connection",orig))
  if (!isOpen(file)) {
    open(file, "r")
    on.exit(close(file))
  }
  read.time <- system.time(rd <- readLines(file))
  n <- nchar(rd)
  nn <- unique(n)
  if(length(nn) != 1)
    stop("records are not fixed-width; this probably is not a MSC archive file")
  if(missing(format)) { # auto-detect record type
    if(nn == 186)
      format <- "HLY"
    else if(nn == 233)
      format <- "DLY"
    else if(nn == 98) {
      format <- "MLY"
      stop("MLY archive format not supported at this time.\n(Note: you can easily obtain DLY data and re-create the equivalent.)\n")
    } else if(nn == 691) {
      format <- "FIF"
      stop("FIF archive format not supported at this time")
    } else
    stop("Format not recognized as either HLY, DLY, MLY, or FIF archive formats\nTry forcing using the 'format' option.\n")
  }
  d  <- data.frame(id=factor(substr(rd, 1, 7)))
  d$year       <- as.integer(substr(rd, 8,11))
  d$month      <- as.integer(substr(rd,12,13))
  if(format=="HLY") {
    rep <- 24
    off <- 16
    d$day <- as.integer(substr(rd,14,15))
  } else if(format=="DLY") {
    rep <- 31
    off <- 14
  }	
  d$elem <- factor(substr(rd,off,off+2))
  elem.name <- elem.all$name[elem.all$alias %in% as.integer(levels(d$elem))]
  levels(d$elem) <- elem.name.orig <- elem.name
  if(verbose) {
    message(gettextf("Format is %s with %i records from %i station%s,\nread in %.1f seconds",
                     format, nrow(d), length(levels(d$id)),
                     ifelse(length(levels(d$id))>1,"s",""),
                     read.time[3]))
    message(sprintf(ngettext(length(elem.name),
                             "%i element found: %s",
                             "%i elements found: %s"),
                    length(elem.name),
                    paste(elem.name,collapse=", ")))
  }
  if(flags) {
    elem.name <- paste(rep(elem.name,each=2),c("","_flag"),sep="")
    d[,paste(c("V","F"),rep(1:rep,each=2),sep="")] <- as.integer(NA)
  } else
  d[,paste("V",1:rep,sep="")] <- as.integer(NA)
  for(col in 1:rep) {
    voff <- off + 3 + (col-1)*7
    v <- as.integer(substr(rd,voff,voff+5))
    v[v==-99999] <- NA
    d[,paste("V",col,sep="")] <- v
    if(flags)
      d[,paste("F",col,sep="")] <- substr(rd,voff+6,voff+6)
  }
  FILENAME <- tempfile(paste("Rmsc",format,sep="."))
  on.exit(unlink(FILENAME), add = TRUE)
  FILE <- file(FILENAME,"w")
  if(format == "HLY") {
    d$date <- as.Date(paste(d$year,d$month,d$day,sep="-"))
    d$jday <- as.integer(format(d$date,"%j"))
    cat("id","year","jday","hour","date","datetime",elem.name,sep=",",file=FILE)
    if(flags) {
      col.v <- (0:23)*2+6
      col.f <- (0:23)*2+7
    } else
    col.v <- (0:23)+6
  } else if(format == "DLY") {
    cat("id","year","month","day",elem.name,sep=",",file=FILE)
  }
  writeLines("",con=FILE)
  for (id in levels(d$id)) {
    s <- d[d$id == id,]
    if(verbose) {
      msg1 <- gettextf("%8s: %7i records ",id,nrow(s))
      start <- min(s$year); end <- max(s$year)
      msg2 <- ifelse(start == end,
                     gettextf("in %4i",start),
                     gettextf("between %4i and %4i",start,end))
      message(paste(msg1,msg2))
    }
    for (year in unique(s$year)) {
      ss <- s[s$year == year,]
      if(format == "HLY")
        row <- unique(ss$jday)
      else if(format == "DLY") {
        d.mon <- unclass(table(mkfact(width="mon",year=year)))
        row <- unique(ss$month)
      }
      for (col in row) {
        if(format == "HLY") {
          m <- data.frame(id=id,year=year,jday=col,hour=0:23,date=NA,datetime=NA)
          qf <- ss$jday == col
        } else if(format == "DLY") {
          m <- data.frame(id=id,year=year,month=col,day=1:d.mon[col])
          qf <- ss$month == col
          if(flags) {
            col.v <- seq(1,d.mon[col]*2,2)+4
            col.f <- seq(1,d.mon[col]*2,2)+5
          } else
          col.v <- seq(1,d.mon[col],1)+4
        }
        m[,elem.name] <- NA
        e <- as.character(ss[qf,"elem"])
        if(flags) {
          p <- t(ss[qf,col.v])
          f <- t(ss[qf,col.f])
          p <- cbind(p,f)
          e <- c(e,paste(e,"flag",sep="_"))
        } else
        p <- t(ss[qf,col.v])
        colnames(p) <- e
        if (length(e) > 0) {
          m[,e] <- p
          m <- m[apply(!is.na(m[,elem.name.orig,drop=FALSE]),1,any),]
          write.table(m,FILE,append=TRUE,quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE)
					#write.table(m,quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE)
        }
			}
		}
	}
	close(FILE)
	FILE <- file(FILENAME, "r")
	on.exit(close(FILE), add = TRUE)
	if(format == "HLY")
		cc <- c("factor","integer","integer","integer","integer","integer")
	else if(format == "DLY")
		cc <- c("factor","integer","integer","integer")
	if(flags)
		cc <- c(cc,rep(c("integer","factor"),length(elem.name.orig)))
	else
		cc <- c(cc,rep("integer",length(elem.name.orig)))
	o <- read.table(FILE,sep=",",header=TRUE,colClasses=cc)
	if(format == "HLY") {
		o$date <- as.Date(paste(o$year,o$jday),"%Y %j")
		o$datetime <- as.POSIXct(strptime(paste(o$date,o$hour),"%F %H"))
	} else if(format == "DLY") {
		o$day <- as.Date(paste(o$year,o$month,o$day,sep="-"))
		o$month <- as.integer(format(o$day,"%j"))
		n <- names(o)
		n[n %in% c("month","day")] <- c("jday","date")
		names(o) <- n
	}
	for(i in elem.name.orig)
		o[,i] <- o[,i] * elem.all$scale[i == elem.all$name]
	return(o)
}
