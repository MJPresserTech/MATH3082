# a)
eq = read.csv("lab1/earthq.csv")

mean(eq$Magnitude) # mean or average
median(eq$Magnitude)
var(eq$Magnitude) # variance
sd(eq$Magnitude) # standard deviation = sqrt(variance)

par(mfrow = c(1, 2))
hist(eq$Magnitude, breaks = 20, main = "Histogram of Earthquake Magnitudes", xlab = "Magnitude", col = "lightblue")
boxplot(eq$Magnitude, main = "Boxplot of Earthquake Magnitudes", ylab = "Magnitude")

# b)
par(mfrow = c(1, 2))
plot(eq$Magnitude, eq$Depth, xlab = "Magnitude", ylab = "Depth", main = "Depth vs Magnitude")
plot(log(eq$Magnitude), log(eq$Depth), xlab = "Log(Magnitude)", ylab = "Log(Depth)", main = "Log(Depth) vs Log(Magnitude)")

# c)
par(mfrow = c(1, 1))
plot(eq$Lon, eq$Lat, xlab = "Longitude", ylab = "Latitude", main = "Earthquake Locations")

# 2)
# a)
tt = read.csv("lab1/ttucson.csv")
tt$x <- as.numeric(tt$x) # convert to numeric
tt <- tt[!is.na(tt$x), ] # remove NA values
tt$Ftemp <- tt$x * 1.8 + 32
head(tt)

# b)
print(mean(tt$Ftemp))
tt_outliers <- tt
tt_outliers$Z_score <- (tt_outliers$Ftemp - mean(tt_outliers$Ftemp)) / sd(tt_outliers$Ftemp)
tt_outliers <- tt_outliers[abs(tt_outliers$Z_score) > 3, ]
tt_outliers

# c)
print(mean(tt$x))
print(sd(tt$x))
print(mean(tt$Ftemp))
print(mean(tt$x) * 1.8 + 32) # check
print(sd(tt$Ftemp))
print(sd(tt$x) * 1.8) # check

# d)
teu = read.csv("lab1/teugene.csv")
teu$x <- as.numeric(teu$x) # convert to numeric
teu <- teu[!is.na(teu$x), ] # remove NA values
teu$Ftemp <- teu$x * 1.8 + 32

print(mean(tt$Ftemp))
print(sd(tt$Ftemp))
print(max(tt$Ftemp))
print(min(tt$Ftemp))

print(mean(teu$Ftemp))
print(sd(teu$Ftemp))
print(max(teu$Ftemp))
print(min(teu$Ftemp))

par(mfrow = c(2, 1))
hist(tt$Ftemp, main = "Histogram of Tucson Temperatures (F)", xlab = "Temperature (F)", col = "lightgreen")
hist(teu$Ftemp, main = "Histogram of Eugene Temperatures (F)", xlab = "Temperature (F)", col = "lightblue")