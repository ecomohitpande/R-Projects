library(dotwhisker)
library(broom)
library(dplyr)

# run a regression compatible with tidy
## use the cbind command to place variable in string likek m1 m2

m1 <- lm(mpg ~ wt + cyl + disp + gear, data = mtcars)

# draw a dot-and-whisker plot
dwplot(m1)
dwplot(m1, conf.level = .99)  # using 99% CI

dwplot(list(m1, m2, m3))
dwplot(list(m1, m2, m3), show_intercept = TRUE)
dwplot(list(m1, m2, m3),
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>% # plot line at zero _behind_ coefs
    relabel_predictors(c(wt = "Weight",                       
                         cyl = "Cylinders", 
                         disp = "Displacement", 
                         hp = "Horsepower", 
                         gear = "Gears", 
                         am = "Manual")) +
     theme_bw() + xlab("Coefficient Estimate") + ylab("") +
     geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
     ggtitle("Predicting Gas Mileage") +
     theme(plot.title = element_text(face="bold"),
           legend.position = c(0.007, 0.01),
           legend.justification = c(0, 0), 
           legend.background = element_rect(colour="grey80"),
           legend.title = element_blank())

### for the plot in black in white 
dwplot(by_trans, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) + # plot line at zero _behind_ coefs
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    ggtitle("Predicting Gas Mileage by Transmission Type") +
    theme(plot.title = element_text(face="bold"),
          legend.position = c(0.007, 0.01),
          legend.justification = c(0, 0),
          legend.background = element_rect(colour="grey80"),
          legend.title.align = .5) +
    scale_colour_grey(start = .3, end = .7,
                      name = "Transmission",
                      breaks = c(0, 1),
                      labels = c("Automatic", "Manual"))

### plot the normal distribution 

by_transmission_brackets <- list(c("Overall", "Weight", "Weight"), 
                       c("Engine", "Cylinders", "Horsepower"),
                       c("Transmission", "Gears", "Gears"))
        
{mtcars %>%
    split(.$am) %>%
    purrr::map(~ lm(mpg ~ wt + cyl + disp + hp + gear, data = .x)) %>%
    dwplot(style = "distribution") %>%
    relabel_predictors(wt = "Weight",
                         cyl = "Cylinders",
                         disp = "Displacement",
                         hp = "Horsepower",
                         gear = "Gears") +
    theme_bw() + xlab("Coefficient") + ylab("") +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    theme(legend.position = c(.995, .99),
          legend.justification = c(1, 1),
          legend.background = element_rect(colour="grey80"),
          legend.title.align = .5) +
    scale_colour_grey(start = .8, end = .4,
                      name = "Transmission",
                      breaks = c("Model 0", "Model 1"),
                      labels = c("Automatic", "Manual")) +
    scale_fill_grey(start = .8, end = .4,
                    name = "Transmission",
                    breaks = c("Model 0", "Model 1"),
                    labels = c("Automatic", "Manual"))} %>%
    add_brackets(by_transmission_brackets) +
    ggtitle("Predicting Gas Mileage by Transmission Type") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))


######################################################################## I used 
install.packages("devtools")
library("devtools")
install.packages("githubinstall")attach(as)
library(githubinstall)
library(rattle)
library("dotwhisker")
library("dplyr")
rattle()

install.packages("githubinstall")


reg <-cbind(concentrated_feed_before,concentrated_feed_present,	green_forage_before,	
            green_forage_present,	feed_supplement_before,	feed_supplement_present,
            minmix_before,	minmix_present)
            
m1 <-lm(em~reg)
m2 <-lm(emcb~reg)
m3 <-lm(emind~reg)

m2 <- update(m1, . ~ .)
m3 <- update(m2, . ~ .)

dwplot(list(m1,m2,m3))

print(summary(reg3))

dwplot(list(m1,m2,m3),
            vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)) %>%
             relabel_predictors(c(con_b = "concentrated feed before",                       
                      con_p = "concentrated feed present",
                       for_b = "green forage before",
                       for_p = "green forage present",
                       sup_b = "feed supplement before",
                       sup_p = "feed supplement present",
                       mix_b = "minmix",
                       mix_p = "minmix"))+
  theme_bw() + xlab("- < Coefficient Estimate  >  +
                    Model 1 = Buffalo, 
                    Model 2 = Cross Breed,
                    Model 3 = Indigenious Breed") + ylab("") +
  geom_vline(xintercept = 0, colour = "grey60", linetype = 2)+
  ggtitle("Predicting Methane Emission")+
theme(plot.title = element_text(face="bold"),
      legend.position = c(0.007, 0.01),
      legend.justification = c(0, 0), 
      legend.background = element_rect(colour="grey80"),
      legend.title = element_blank()) 

dwplot(reg3, conf.level = .95, xintercept = 0)


