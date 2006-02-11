"precip.norm" <-
function(dat, start, end, fun="median") {
	if(missing(start)) start <- NULL
	if(missing(end)) end <- NULL
	if(is.function(fun)) fun <- as.character(substitute(fun))
	seas.norm(dat=dat,start=start,end=end,fun=fun,precip.norm=TRUE)
}
