dat <- read.csv("C:/Users/Makrand/Documents/302-A2/Census.txt",  sep="")

names(dat)<-c("STATE","MALE", "BIRTH" ,"DIVO" ,"BEDS" ,"EDUC", "INCO" ,"LIFE")

x1 <- dat$MALE
x2 <- dat$BIRTH
x3 <- dat$DIVO
x4 <- dat$BEDS
x5 <- dat$EDUC
x6 <- dat$INCO
y <- dat$LIFE

# a, b, c, d
formula <- y ~ x1 + x2 + x3 + x4 + x5 + x6

model <- lm(formula)

summary(model)

# remove x1, x3, x5, x6

#e
reducdedFormula <- y ~ x2 + x3 + x4 + x5
reducdedModel <- lm(reducdedFormula)
anova(model, reducdedModel)

#f
formulaF <- y ~ x1
modelF <- lm(formulaF)
anova(model, modelF)

#g
formulaG1 <- y ~ 1
modelG1 <- lm(formulaG1)

formulaG2 <- y ~ x1 + x2
modelG2 <- lm(formulaG2)
anova(modelG1, modelG2)

