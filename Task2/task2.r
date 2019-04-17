# Task 2-1

myread.cvsdata <- function(file_str) {
	df <- read.delim(file_str, as.is=c(T, T, T, F, F, F, F))
	df$developerf <- factor(df$developer)
	df$filef <- factor(df$file)
	df$description <- NULL
	return (df)
}

# Task 2-2

developer.count <- function(data) {
	return (length(levels(data$developerf)))
}

developer.busy <- function(data) {
	top5 <- head(sort(table(data$developerf), decreasing=T), 5)
	return (top5)
}

developer.changedfiles <- function(data) {
	files_per_developer <- tapply(data$filef, data$developer, function(x) length(unique(x)) )
	files_all           <- length(levels(df$filef))
	return (100 * files_per_developer / files_all)
}
