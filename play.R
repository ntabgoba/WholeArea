p <- qplot(mpg, cyl, data=mtcars, size=cyl)
p + scale_x_discrete(name ="Dose (mg)", 
                     limits=c(LETTERS[1:35]))
p + scale_size("number\nof\ncylinders")

p + scale_size(to = c(0, 10))
p + scale_size(to = c(1, 2))

# Map area, instead of width/radius
# Perceptually, this is a little better
p + scale_area()
p + scale_area(to = c(1, 25))

# Also works with factors, but not a terribly good
# idea, unless your factor is ordered, as in this example
qplot(mpg, cyl, data=mtcars, size=factor(cyl))

# To control the size mapping for discrete variable, use 
# scale_size_manual:
last_plot() + scale_size_manual(c(2,4,15))
