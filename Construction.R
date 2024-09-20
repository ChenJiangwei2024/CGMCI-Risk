#Data characteristics
library(tableone)
vars <- c("trueage","sex","education","nation","residence","marriage",
          "coresident","BMI","medicalfee","baselineMMSE","sleep","chronic",
          "BADL","IADL","ppt","chew","visual","aural","fruit","vegetable",
          "anipro","vegpro","tea","chore","field","garden","reading",
          "animals","cards","tvradio","social","smoking","drinking",
          "exercise","resilence", "lifequality","health","sleepquality",
          "economics","alimony","communityservices")
tableone <- CreateTableOne(vars=vars,
                           strata=c("group"),
                           addOverall=TRUE,
                           data=data)
table1 <- print(tableone,
                nonnormal=c("trueage","coresident","resilence","education","medicalfee",
                            "alimony","BMI","sleep","residence","sex","nation","marriage",
                            "lifequality","health","sleepquality","economics",
                            "baselineMMSE","fruit","vegetable","anipro","vegpro","tea",
                            "smoking","drinking","exercise","chore","field","garden",
                            "reading","animals","cards","tvradio","social","BADL","IADL",
                            "visual","aural","chew","ppt","chronic","communityservices"),
                noSpaces=TRUE,
                showAllLevels=TRUE,
                quote=FALSE,
                printToggle=FALSE)
write.csv(table1,file="Table_1.csv")

#Backward stepwise regression analysis
dataLog <- glm(formula = group~., data=data, family = binomial)
library(MASS)
stepAIC(dataLog, direction = "backward") 

#Logistic regression analysis
logit <- glm(formula = group ~ trueage + education + sex + exercise + 
               garden + tvradio + IADL + aural + chew,
             family = binomial, 
             data = data)
summary(logit)
coef <- coef(logit)
coef_CI <- confint(logit)
OR_Results <- exp(cbind("OR"=coef,
                        "LL"=coef_CI[,1],
                        "UL"=coef_CI[,2]))
round(OR_Results,3)

#Construct logistic model
model_dev <- glm(formula = group ~ trueage + education + sex + exercise + 
                   garden + tvradio + IADL + aural + chew,
                 family = binomial,  data = data)
#Equation of extraction
library(rms)
library(nomogramEx)
dd=datadist(data)
options(datadist="dd")
bestglm <- as.formula(group ~ sex + nation + age + edu + sleep + satis + 
resilience + basemmse + tvradio + BADL + IADL)
fit.glm <- lrm(formula=bestglm,data=data,x=TRUE,y=TRUE)
nom.glm <- nomogram(fit.glm,
                    fun=function(x)1/(1+exp(-x)),
                    lp=TRUE,
                    fun.at=c(0.1,0.3,0.5,0.7),
                    funlabel = "3-year risk")
nomogramEx(nom.glm,
           np=1)



