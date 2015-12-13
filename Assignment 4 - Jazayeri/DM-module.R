## List with parameter values:

params <- list()
params$d <- .5

## ---------------------------------------------------------------------------

## DM functions

create.dm <- function(chunks,encounters) {
  if (chunks > 52) {
    stop("Only up to 52 chunks allowed.")
  }
  DM <- array(NA,c(chunks,encounters))
  row.names(DM) <- c(letters,LETTERS)[1:chunks]
  DM
}

add.encounter <- function(DM,chunk,time) {
  tmp <- DM[chunk,]
  DM[chunk,sum(!is.na(tmp))+1] <- time
  DM
}

get.encounters <- function(DM,chunk) {
  tmp <- DM[chunk,]
  tmp[!is.na(tmp)]
}

## ---------------------------------------------------------------------------

## Baselevel activation function:

actr.B <- function(encounters,curtime) {
	if (length(curtime)>1) {
		sapply(curtime,function(X) { actr.B(encounters,X)})
	} else {
		if (curtime < min(encounters)) {
			return(NA)
		} else {
			sum((curtime - encounters[encounters<curtime])^-params$d)
		}
	}
}

## Calculate the activation per chunk: 

#for (i in letters[1:params$num.chunks]) {
#  cat("Activation of chunk ",i," is ");
#  cat(actr.B(get.encounters(DM,i),params$duration),"\n");
#}
