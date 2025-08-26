setwd("/home/mjpresserinspiron16/code/math3082/lab1")
# Problem 1. Use the earthquakes data set.
# a. Make graphical and numerical summaries for variable Magnitude. De-
# scribe in words what they tell you. Are strong (Mag. > 6) quakes
# frequent?
# b. Make a scatteplot of Magnitude vs Depth of earthquakes. Do these
# variables appear to be connected in some way?
# To make the graph more readable, make a scatterplot of log(Magnitude)
# vs log(Depth).
# c. Try and produce a “map” (of course, it has to be a flat representation
# of the Earth’s surface) of the quakes’ locations. Describe what you see.
# What are the two geographical regions with a lot of earthquake activity?

par(mfrow = c(1, 1))

# a)
eq = read.csv("earthq.csv")

mean(eq$Magnitude) # mean or average
median(eq$Magnitude)
var(eq$Magnitude) # variance
sd(eq$Magnitude) # standard deviation = sqrt(variance)

hist(eq$Magnitude)
boxplot(eq$Magnitude)

# b)

plot(eq$Magnitude, eq$Depth)
plot(log(eq$Magnitude), log(eq$Depth))

# c)

plot(eq$Lon, eq$Lat)

# 2)
# a)
tt = read.csv("ttucson.csv")
tt$Ftemp <- as.numeric(tt$x) * 1.8 + 32
head(tt$Ftemp)