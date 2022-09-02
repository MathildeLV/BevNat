

p+ xlim(10, 9000)

p+ stat_function(fun = dnorm, geom = "line", size = 1.2, mapping = aes(BW, color = birthyear)) 


p <- ggplot(bevn_eco_in7, aes(x=BW, fill=birthyear, colour=birthyear)) + geom_density(
  stat = "density",
  position = "identity",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  outline.type = "upper"
)
p


# Kernel plots
#Birthweight 
BW_sex <- ggplot(bevn_eco_in7, aes(x=BW, colour=sex)) + geom_density()
BW_sex

BW_birthyear_in7 <- ggplot(bevn_eco_in7, aes(x=BW, colour=as.factor(birthyear))) + geom_density()
BW_birthyear_in7

BW_birthyear_in6 <- ggplot(bevn_eco_in6, aes(x=BW, colour=as.factor(birthyear))) + geom_density()
BW_birthyear_in6

BW_birthyear_in <- ggplot(bevn_eco, aes(x=BW, colour=as.factor(birthyear))) + geom_density()
BW_birthyear_in



GA_birthyear_in <- ggplot(bevn_eco_in7, aes(x=GA_days, colour=as.factor(birthyear))) + geom_density()
GA_birthyear_in

GA_birthyear_in6 <- ggplot(bevn_eco_in7, aes(x=GA_days, colour=as.factor(birthyear))) + geom_density()
GA_birthyear_in6

GA_birthyear_in7 <- ggplot(bevn_eco_in7, aes(x=GA_days, colour=as.factor(birthyear))) + geom_density()
GA_birthyear_in7



## Mother nationality through the years

gg_nation_birthy <- ggplot(bevn_eco_in7,
       aes(x=birthyear, fill=mother_nationality_cat2)) + 
  geom_bar(position = "fill") 

gg_nation_birthy
gg_nation_birthy +  ggtitle("Maternal nationality through the years") +
 scale_x_continuous(breaks = c(2007, 2010, 2013, 2016, 2020))