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


sl <- data.frame(a=c(10,20,30,40,50),c=c("me","you","her","she","us"),d=c(55402642, 55402661, 55402569, 55402662, 55402661))
slv <- sl %>%
        group_by(a,c,d) %>%
        mutate(newgrd = if(n( ) > 1) {grid_maker(d)} 
       else {paste0(d)})
sl$imane <- c(NA,NA,2,3,NA)
jio <- is.na(sl$imane)
jio
