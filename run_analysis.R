require(stringr)
require(plyr)

base.dir <- "UCI HAR Dataset"

# read column labels from $base.dir/features.txt
get.columns.info <- function(base.dir) {
  lab <- read.table(paste(base.dir, "features.txt", sep="/"), header=F, as.is=TRUE)$V2
  lab0 <- str_split(lab, "-")
  
  # normalize number of cols
  max.lab.parts <- max(sapply(lab0, length))
  lab <- lapply(lab0, function(x) if(length(x) < max.lab.parts) c(x, rep.int("", max.lab.parts-length(x))) else x)
  
  # convert list() to data.frame()
  lab <- ldply(lab)
  
  # get column index of mean() & std()
  idx <- which(lab[,2] %in% c("mean()", "std()"))
  
  # create column.names
  col.names <- lab[idx,]
  col.names[,2] <- gsub("\\(\\)", "", col.names[,2])
  col.names <- do.call(function(...) paste(..., sep="."), col.names)
  col.names <- gsub("\\.$", "", col.names)
  
  list(idx=idx, names=col.names)
}

process.dir <- function(base.dir, data.dir, col.info) {
  base.dir.check <- file.info(base.dir)$isdir
  if (is.na(base.dir.check) || !base.dir.check) {
    stop("No dataset dir found")
  }
  
  data.name <- data.dir
  data.dir <- paste(base.dir, data.name, sep="/")
  data.dir.check <- file.info(data.dir)$isdir
  if (is.na(data.dir.check) || !data.dir.check) {
    stop(paste("Directory [", data.dir, "] not found.", sep=""))
  }
  
  subject <- read.csv(paste(data.dir, paste("subject_", data.name, ".txt", sep=""), sep="/"), header=F)
  activity <- read.csv(paste(data.dir, paste("y_", data.name, ".txt", sep=""), sep="/"), header=F)
  measurements <- read.table(paste(data.dir, paste("X_", data.name, ".txt", sep=""), sep="/"), header=F)
  
  # retain only columns specified in col.info
  measurements <- measurements[,col.info$idx]
  names(measurements) <- col.info$names
  
  # check that we have the same # of rows
  if (nrow(subject) != nrow(activity)) {
    stop(paste("# of subjects <> # of activities: ", nrow(subject), "<>", nrow(activity)))
  }
  
  if (nrow(subject) != nrow(measurements)) {
    stop(paste("# of subjects <> # of measurements:", nrow(subject), "<>", nrow(measurements)))
  }
  
  data.frame(subject=subject[,1], activity=activity[,1], measurements)
}

get.activity.labels <- function(base.dir) {
  lab <- read.table(paste(base.dir, "activity_labels.txt", sep="/"), header=F, as.is=TRUE)
  names(lab) <- c("code", "activity")
  return(lab)
}


# #1: merge training & test datasets
# #2: extract only means & stdev
col.info <- get.columns.info(base.dir)
train <- process.dir(base.dir, "train", col.info)
test <- process.dir(base.dir, "test", col.info)
clean.data <- rbind(train, test)

# #3: use descriptive activity names
activity.labels <- get.activity.labels(base.dir)
names(clean.data) <- c("subject", "activity.code", names(clean.data)[-c(1:2)])
clean.data <- merge(clean.data, activity.labels, by.x="activity.code", by.y="code")
clean.data <- clean.data[,c(1,69,2:68)]

# #4: use descriptive variable names
# pass

# #5: calc average of each variable for each activity and subject
cd.averages <- ddply(clean.data, .(activity.code, activity, subject), 
                    function(x) {
                      data.frame(x[1,1:3],t(sapply(x[,4:ncol(x)], mean)))
                    })

#save(clean.data, cd.averages, file="clean-data.rda")
write.table(clean.data, file="clean-data.txt", sep=",", row.names=F)
write.table(cd.averages, file="clean-data-averages.txt", sep=",", row.names=F)
