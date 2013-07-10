## using mtcars dataset

# [, 1]	 mpg	 Miles/(US) gallon
# [, 2]	 cyl	 Number of cylinders
# [, 3]	 disp	 Displacement (cu.in.)
# [, 4]	 hp	 Gross horsepower
# [, 5]	 drat	 Rear axle ratio
# [, 6]	 wt	 Weight (lb/1000)
# [, 7]	 qsec	 1/4 mile time
# [, 8]	 vs	 V/S
# [, 9]	 am	 Transmission (0 = automatic, 1 = manual)
# [,10]	 gear	 Number of forward gears
# [,11]	 carb	 Number of carburetors


data(mtcars)
x <- mtcars
fit <- lm(mpg ~ . , data = x)
summary(fit)

# simplified model with one variable

fit2 <- lm(mpg ~ wt , data = x)
summary(fit2)

# polynomial fit

fit3 <- lm(mpg ~ poly(wt, 2, raw=TRUE))
summary(fit3)

fit4 <- lm(mpg ~ poly(wt, 3, raw=TRUE))
summary(fit4)

# R^2 seems to stay pretty constant in this particular case
