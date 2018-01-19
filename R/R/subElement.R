subElement <- function(inM = inM, index = index) 
{
	subElement <- NULL
	for (i in seq(dim(index)[1])) {
	    text_id <- ""
	    for (id in seq(dim(index)[2])) {
			if (id == 1) {
				text_id <- sprintf("inM[%d,", index[i, id])
			} else if (id == dim(index)[2]) {
				text_id <- sprintf("%s%d]", text_id, index[i, id])
			} else {
				text_id <- sprintf("%s%d,", text_id, index[i, id])
			}
		}
		
		subElement <- rbind(subElement, eval(parse(text=text_id)))	
	}
	return(subElement)

} 