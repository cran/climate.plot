".seasxlab" <- 
function(width){
	if(is.numeric(width))
		return(gettextf("%s-day group",round(width,1)))
	if(any(grep("mon",width)))
		return(gettext("Monthly"))
	if(any(grep("zod",width)))
		return(gettext("Zodiac"))
	if(width == "DJF")
		return(gettext("Quarterly (seasonal)"))
	if(width == "JFM")
		return(gettext("Quarterly (annual)"))
	if(width == "JF")
		return(gettext("Two months"))
	return(as.character(width[1]))
}
".seastitle" <- 
function(main=NULL, id=NULL, name=NULL, orig=NULL, fun=NULL, range=NA, show.range=TRUE, show.id=TRUE, style=1) {
	if(is.null(main)) {
		if(is.null(name) && !is.null(id))
			name <- getstnname(id)
		if(!is.null(name) && !is.null(id) && show.id)
			main <- paste(name,id)
		else if(!is.null(name) && !show.id)
			main <- name
		else if(!is.null(id) && show.id)
			main <- id
		else if(!is.null(orig))
			main <- orig
		else
			main <- ""
	}
	if(!is.null(fun))
		main <- paste(main,fun,sep=ifelse(main == "",""," "))
	if(show.range) {
		if(style==1) {
			from <- sprintf(" %s ",gettext("from"))
			to <- sprintf(" %s ",gettext("to"))
			line <- 1
			cex <- 2
		} else if(style==2) {
			from <- "\n"
			to <- " - "
			line <- 1.5
			cex <- 1.5
		}
		if(main =="") {
			from <- ""
			line <- 1
			cex <- 2
		}
		range <- ifelse(range[1] == range[2],
					paste(range[1]),
					paste(range[1],range[2],sep=to))
		main <- paste(main,range,sep=from)
	}
	title <- list(title=main,line=line,cex=cex)
	title
}
