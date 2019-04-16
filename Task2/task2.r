## "Empirical Evaluation in Informatics" - Task 2
## Lars Parmakerli
## Julius Brose


# Task 2-1

myread.cvsdata <- function(file_str) {
	df <- read.delim(file_str, as.is=c(T, T, T, F, F, F, F))
	df$developerf <- factor(df$developer)
	df$filef <- factor(df$file)
	df$description <- NULL
	return (df)
}


# Task 2-2

developer.count <- function(df) {
	return (length(levels(df$developerf)))
}

developer.busy <- function(df) {
	top5 <- head(sort(table(df$developerf), decreasing=T), 5)
	#return (nrow(df[df$developer %in% names(top5),]))
	return (top5)
}

developer.changedfiles <- function(df, developer) {
	files_per_developer <- tapply(df$filef, df$developer, function(x) length(unique(x)) )
	files_all           <- length(levels(df$filef))
	return (100 * files_per_developer[developer] / files_all)
}
