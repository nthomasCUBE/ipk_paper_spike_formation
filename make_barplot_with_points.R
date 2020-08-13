function () 
{
	library(ggplot2)
	library(gridExtra)
	library(dplyr)
	library(backports)

	se <- function(x){sd(x)/sqrt(length(x))}
	my_dat <- summarise(group_by(mtcars, cyl),
                    my_mean = mean(disp),
                    my_se = se(disp))

	p1 <- ggplot() + 
	  geom_bar(data = my_dat,
	           aes(y = my_mean, x = cyl,
	               ymin = my_mean - my_se,
	               ymax = my_mean + my_se), stat="identity", width=0.75) + 
	  geom_errorbar(data = my_dat,
	                aes(y = my_mean, x = cyl,
	                    ymin = my_mean - my_se,
	                    ymax = my_mean + my_se), stat="identity", width=0.75) + 
	  geom_point(data = mtcars, aes(y = disp, x = cyl, fill = cyl)) +

  	theme_classic() + ylim(c(0,500))
	print(my_dat)
	print(mtcars)
	p1

}
