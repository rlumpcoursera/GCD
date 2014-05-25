# Merges the training and the test sets to create one data set.

t1 <- read.table("train/X_train.txt")
t2 <- read.table("test/X_test.txt")
x <- rbind(t1, t2)

t1 <- read.table("train/subject_train.txt")
t2 <- read.table("test/subject_test.txt")
s <- rbind(t1, t2)

t1 <- read.table("train/y_train.txt")
t2 <- read.table("test/y_test.txt")
y <- rbind(t1, t2)

# Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
x <- x[, indices_of_good_features]
names(x) <- features[indices_of_good_features, 2]
names(x) <- gsub("\\(|\\)", "", names(x))
names(x) <- tolower(names(x))  # see last slide of the lecture Editing Text Variables (week 4)

# Uses descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
y[,1] = activities[y[,1], 2]
names(y) <- "activity"

# Appropriately labels the data set with descriptive activity names.

names(s) <- "subject"
clean <- cbind(s, y, x)
write.table(clean, "clean.txt")

# Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(s)[,1]
numSubjects = length(unique(s)[,1])
numActivities = length(activities[,1])
numCols = dim(clean)[2]
tidy = clean[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    tidy[row, 1] = uniqueSubjects[s]
    tidy[row, 2] = activities[a, 2]
    tmp <- clean[clean$subject==s & clean$activity==activities[a, 2], ]
    tidy[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(tidy, "tidy.txt")