library(readxl)
library(MVN)
library(heplots)

BonesData = read_xlsx("C:\\Applied Multivirate\\BonesData.xlsx")
TriathlonData = read_xlsx("C:\\Applied Multivirate\\TriathlonData.xlsx")

# variables in Bones Data 
x1  = data.frame(BonesData$DomRadius, BonesData$DomHumerus , BonesData$DomUlna )
x2 = data.frame(BonesData$NonDomRadius , BonesData$NonDomHumerus ,BonesData$NonDomUlna )

library(DescTools)

HotellingsT2Test(x1, x2)
HotellingsT2Test(x1, x2, test = 'chi')

# Creating Chi-square QQ plot for dominant bones
cqplot(x1, id.n = 3)
mvn(x1, mvnTest = "royston")
mvn(x1, mvnTest = "hz")

# Creating Chi-square QQ plot for non-dominant bones
cqplot(x2, id.n = 3)
mvn(x2, mvnTest = "royston")
mvn(x2, mvnTest = "hz")

HotellingsT2Test(x1, x2, conf.level = 0.95, alpha = 0.05)


df = subset(TriathlonData, CATEGORY != "CAT3")
combinations_colors <- c("CAT1" = "red", "CAT2" = "green")
combinations_shapes <- c("CAT1" = 19, "CAT2" = 17)

# Scatterplot Matrix with Colors and Shapes for Combinations
pairs(~ SWIM + BIKE + RUN, data = df,
      col = combinations_colors[df$CATEGORY],
      pch = combinations_shapes[df$CATEGORY],
      main = "Scatterplot Matrix with Colors and Shapes for Combinations",
      lower.panel = NULL)

# Add legend for combinations
legend("bottomleft", legend = unique(df$CATEGORY), title = "Category",
       col = unique(df$color), pch = unique(df$shape), inset = c(0.12, 0.32), cex = 0.8)

library(scatterplot3d)

new_data <- data.frame(
  Combination = df$CATEGORY,
  SWIM = df$SWIM,
  BIKE = df$BIKE,
  RUN = df$RUN
)

# Create a scatterplot3d object
scatterplot3d(new_data$SWIM, new_data$BIKE, new_data$RUN,
              color = combinations_colors[df$CATEGORY],
              pch = combinations_shapes[df$CATEGORY],
              main = "3D Scatterplot with Colors and Shapes for Combinations",
              xlab = "SWIM", ylab = "BIKE", zlab = "RUN")

# Add legend for combinations
legend("topright", legend = unique(df$CATEGORY), title = "Category",
       col = combinations_colors, pch = combinations_shapes, cex = 0.8)

# Loading necessary libraries
library(ggplot2)

# Subsetting the data into separate data frames for each category
x1 = subset(TriathlonData, CATEGORY == "CAT1", select = c(SWIM, BIKE, RUN))
x2 = subset(TriathlonData, CATEGORY == "CAT2", select = c(SWIM, BIKE, RUN))
x3 = subset(TriathlonData, CATEGORY == "CAT3", select = c(SWIM, BIKE, RUN))

library(DescTools)

# Hotelling’s test between x1 and x2
HotellingsT2Test(x1, x2)

# Chi-square test between x1 and x2
HotellingsT2Test(x1, x2, test = 'chi')

# Chi-square QQPlot for x1
cqplot(x1, id.n = 3)
mvn(x1, mvnTest = "royston")
mvn(x1, mvnTest = "hz")

# Chi-square QQPlot for x2
cqplot(x2, id.n = 3)
mvn(x2, mvnTest = "royston")
mvn(x2, mvnTest = "hz")

HotellingsT2Test(x1, x2, conf.level = 0.95, alpha = 0.05)

df <- TriathlonData
combinations_colors <- c("CAT1" = "red", "CAT2" = "green","CAT3" = "violet")
combinations_shapes <- c("CAT1" = 19, "CAT2" = 17, "CAT3" = 15)

# Scatterplot Matrix with Colors and Shapes for Combinations
pairs(~ SWIM + BIKE + RUN, data = df,
      col = combinations_colors[df$CATEGORY],
      pch = combinations_shapes[df$CATEGORY],
      main = "Scatterplot for Matrix with Colors and Shapes for Combinations",
      lower.panel = NULL)

# Add legend for combinations
legend("bottomleft", legend = unique(df$CATEGORY), title = "Category",
       col = combinations_colors, pch = combinations_shapes,
       cex = 1.0, inset = c(0.12, 0.17))


library(scatterplot3d)

new_data <- data.frame(
  Combination = df$CATEGORY,
  SWIM = df$SWIM,
  BIKE = df$BIKE,
  RUN = df$RUN
)

# Create a scatterplot3d object
scatterplot3d(new_data$SWIM, new_data$BIKE, new_data$RUN,
              color = combinations_colors[df$CATEGORY],
              pch = combinations_shapes[df$CATEGORY],
              main = "3D Scatterplot with Colors and Shapes for Combinations",
              xlab = "SWIM", ylab = "BIKE", zlab = "RUN")

# Add legend for combinations
legend("topright", legend = unique(df$CATEGORY), title = "Category",
       col = combinations_colors, pch = combinations_shapes, cex = 0.8)

library(car)

x1_loh = colMeans(x1)
x2_loh = colMeans(x2)
x3_loh = colMeans(x3)

L1 = var(x1)
L2 = var(x2)
L3 = var(x3)

Ns = 20
p = 3
g = 3
n = 60

W = (Ns-1)*L1 + (Ns-1)*L2 + (Ns-1)*L3

xbar = (Ns * x1_loh + Ns * x2_loh + Ns * x3_loh) / n

B = Ns * outer(x1_loh - xbar, x1_loh - xbar) +
  Ns * outer(x2_loh - xbar, x2_loh - xbar) +
  Ns * outer(x3_loh - xbar, x3_loh - xbar)

lambda = det(W) / det(B + W)

df1 = g - 1
df2 = n - g
Fapprox = ((n - p*g - 2) / (p * (g - 1))) * ((1 - sqrt(lambda)) / sqrt(lambda))
pval = pf(Fapprox, df1 * p, df2, lower.tail = FALSE)


cat("Wilks' Lambda:", lambda, "\n")
cat("Approximate F-statistic:", Fapprox, "\n")
cat("p-value:", pval, "\n")

library(mvtnorm)

alpha = 0.05
k = p * g * (g - 1) / 2
crit_val = qmvt(1 - alpha / k, df = n - g, delta = rep(0, p), sigma = W / (n - g))$quantile

for (i in 1:(g - 1)) {
  for (j in (i + 1):g) {
    diff = colMeans(list(x1, x2, x3)[[i]]) - colMeans(list(x1, x2, x3)[[j]])
    lower = diff - crit_val * sqrt(diag(W)[i] * (1 / Ns + 1 / Ns))
    upper = diff + crit_val * sqrt(diag(W)[i] * (1 / Ns + 1 / Ns))
    cat("Simultaneous 95% CI for difference between CAT", i, "and CAT", j, ":\n")
    print(cbind(lower, upper))
    cat("\n")
  }
}

new_data[,1] = as.factor(new_data[,1])
# MANOVA with car package
library(car)
# Computing Means and Var/Cov Mats per groups
VMeans = statList(new_data[,-1],new_data[,1],FUN = colMeans)
VMat = statList(new_data[,-1],new_data[,1],FUN = var)
Ns = table(new_data[,1]) # Sample sizes
p = 3
g = 3
n = nrow(new_data)
# Manually computing the W matrix
W = (Ns[1]-1)*VMat[[1]] + (Ns[2]-1)*VMat[[2]] + (Ns[3]-1)*VMat[[3]]
library(heplots)
library(MVN)
# Split data by group
new_data_gr = split(new_data,new_data[,1]) 
# Checking Multivariate normality for each group
par(mfrow = c(2,2))
for(i in 1:g){
  X = new_data_gr[[i]][,-1]
  cqplot(X, id.n=3)
  print(mvn(X, mvnTest = "royston"))
  print(mvn(X, mvnTest = "hz"))
}

# MANOVA with built in function
Y = as.matrix(new_data[,-1])
Gr = as.factor(new_data[,1])
# First fit a linear regression
LM.res = lm(Y~Gr)
# Call Manova, This command gives all Tests
# Wilk’s lambda, Pillai, Hotelling-Lawley and Roy
SUM = summary(Manova(LM.res),"Wilks")
SUM
# The W matrix using the build it functions
W = SUM$SSPE

# Checking which of the groups differ from each other
library(biotools)
mvpaircomp(LM.res,factor1 = "Gr",test = "Wilks", adjust = "bonferroni")
# Checking each variable for differences across the groups
summary.aov(LM.res)
# Simultaneous CIs, Critical value for Bonferroni
k = p*g*(g-1)/2
ta = qt(.05/(2*k),df=n-g,lower.tail = FALSE)
# Simultaneous CIs
i = 2 # Group A
j = 3 # Group B
v = 1 # Variable
LL = VMeans[[i]] - VMeans[[j]] - ta * sqrt((1/Ns[i]+1/Ns[j])*diag(W)/(n-g))
UL = VMeans[[i]] - VMeans[[j]] + ta * sqrt((1/Ns[i]+1/Ns[j])*diag(W)/(n-g))
cbind(LL,UL)









