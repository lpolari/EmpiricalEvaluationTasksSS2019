myread.csvdata = function (file_str) {
    data = read.delim(file_str, as.is=c(T,T,T,F,F,F,F))
    data$developerf = factor(data$developer)
    data$filef = factor(data$file)
    data$description = NULL
    data$tstamp2 = as.POSIXct( data$tstamp)
    data$tstamp3 = as.numeric( data$tstamp2)
    data$wday = factor(as.POSIXlt(data$tstamp2)$wday, c(0,1,2,3,4,5,6), c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
    data$hour = factor(as.POSIXlt(data$tstamp2)$hour, c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23))
    data$filetypef = sub("(.*)\\.", "", tolower(data$filef))
    return (data)
}

changedFiles.count = function(data) {
    return (length(levels(data$filef)))
}

changedFiles.top = function(data) {
    return (head(sort(table(data$filef), decreasing=T), 5))
}

changedFiles.ntimes = function(data) {
    return (table(factor(table(data$filef), levels=c(1,2,3,5,10))))
}
