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

	df$filetypef = sub(".*\\.", "", df$filef)
	df$filetypef[! grepl("\\.", df$filef)] = "None"

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
	files_all           <- length(levels(data$filef))
	return (100 * files_per_developer / files_all)
}

myplot.hours.bars <- function(data) {
	plot(data$hour)
}

myplot.wdays.bars <- function(data) {
	plot(data$wday)
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

# Task 4-1

# a) 1

files.count <- function(data) {
	return (length(levels(data$filef)))
}

# a) 4

files.changed_by_1_to_9 <- function(data) {
	return (head(table( tapply(data$developer, data$filef, function(x) length(levels(factor(x))))), 9))
}



# Task 4-2

# b) 1
filetypes.changes  <- function(data){
	return (sort(table(data$filetypef), decreasing=T))
}

# b) 2
filetypes.changes.mean <- function(data){
	return (tapply(data$filef, data$filetypef, function(x) round(length(x)/length(unique(x)))))
}

# b) 3 and 4
filetypes.files.count.unique <- function(data) {
	return (tapply(data$filef, data$filetypef, function(x) length(unique(x))))
}

filetypes.changes.max <- function(data) {
	return (tapply(data$filef, data$filetypef, function(x) max(table(x))))
}

filetypes.changes.min <- function(data) {
	return (tapply(data$filef, data$filetypef, function(x) min(table(x)[table(x) > 0])))
}

filetypes.table <- function(data) {
	mean = data.frame(t(filetypes.changes.mean(data)), row.names="mean")
	min = data.frame(t(filetypes.changes.min(data)), row.names="min")
	max = data.frame(t(filetypes.changes.max(data)), row.names="max")
	return (rbind(min, max, mean))
}

filetypes.table.display <- function(data) {
	View(filetypes.table(data))
}

# b) 5

filetypes.table.ordered_by_mean <- function(data) {
	filetypes.table(data)[order(mean, decreasing=T)]
}