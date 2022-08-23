mlb1 <- read_excel("C:/Users/Hayden/Downloads/mlb1.xls")
> View(mlb1)
> summary(a <- lm(log(mlb1$salary)~mlb1$years+mlb1$gamesyr+mlb1$bavg+mlb1$hrunsyr+
                    +                     mlb1$rbisyr+mlb1$runsyr+mlb1$fldperc+mlb1$allstar+mlb1$frstbase+
                    +                     mlb1$scndbase+mlb1$thrdbase+mlb1$shrtstop+mlb1$catcher))
> library(readxl)
> sleep75_1_ <- read_excel("C:/Users/Hayden/Downloads/sleep75 (1).xls")
> View(sleep75_1_)
> summary(b <- lm(sleep75$sleep~sleep75$totwrk+sleep75$educ+sleep75$age+I(sleep75$age^2)+sleep75$yngkid))