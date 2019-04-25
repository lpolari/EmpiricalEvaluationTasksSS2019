# Task 2-1

library(lattice)

myread.cvsdata <- function(file_str) {
	df <- read.delim(file_str, as.is=c(T, T, T, F, F, F, F))
	df$developerf <- factor(df$developer)
	df$filef <- factor(df$file)
	df$description <- NULL

	df$tstamp2 <- as.POSIXct(df$tstamp)
	df$tstamp3 <- as.numeric(df$tstamp2)
	df$wday <- factor(as.POSIXlt(df$tstamp2)$wday, levels=c(0:6), labels=c("monday","tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"))
	df$hour <- factor(as.POSIXlt(df$tstamp2)$hour, levels=c(0:23))

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

myplot.hours.bars <- function(data) {
	plot(data$hours)
}

myplot.wdays.bars <- function(data) {
	plot(data$wdays)
}

myplot.lines_add.devs.boxplot <- function(data) {
	bwplot(developer~log(lines_add+1, 2), data)
}

myplot.lines_balance.devs.boxplot <- function(data) {
	bwplot(developer~log(lines_add - lines_del, 2), data)
}

myplot.lines_add.devs.densityplot <- function(data) {
	densityplot(~log(lines_add + 1, 2)|developer, data)
}

myplot.lines_add.devs.histogram <- function(data) {
	densityplot(~log(lines_add + 1, 2)|developer, data)
}

