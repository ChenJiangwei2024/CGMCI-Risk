#Internal validation (Bootstrap)
library(caret)
data <- as.data.frame(data)
data$group <- factor(data$group,
                     levels = c(0,1),
                     labels = c("No","Yes")
)
train.control <- trainControl(method = "boot",
                              number = 1000,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)
set.seed(1)
bootmodel <- train(bestglm, 
                   data=data, 
                   trControl=train.control, 
                   method="glm")
#External validation (Temporal)
model_vad <- glm(formula = group ~ trueage + education + sex + exercise + 
                          garden + tvradio + IADL + aural + chew,
                 family = binomial, 
                 data = vad)
#Information on the AUROC curves
library(rms)
library(pROC)
dd=datadist(data)
dd_vad=datadist(vad)
options(datadist="dd")
options(datadist="dd_vad")
bestglm <- as.formula(group ~ trueage + education + sex + exercise + 
                        garden + tvradio + IADL + aural + chew)
bestglm_vad <- as.formula(group ~ trueage + education + sex + exercise + 
                            garden + tvradio + IADL + aural + chew)
fit.glm <- lrm(formula=bestglm, 
               data=data, 
               x=TRUE, 
               y=TRUE)
fit.glm_vad <- lrm(formula=bestglm_vad, 
                   data=vad, 
                   x=TRUE, 
                   y=TRUE)
predvalue <- predict(fit.glm)
predvalue_vad <- predict(fit.glm_vad)
ROC <- roc(data$group,
           predvalue)
ROC_vad <- roc(vad$group,
               predvalue_vad)
auc(ROC)
auc(ROC_vad)
ci(auc(ROC))
ci(auc(ROC_vad))
coords(ROC,
       x="best",
       ret="all",
       transpose=FALSE)
coords(ROC_vad,
       x="best",
       ret="all",
       transpose=FALSE)

#Drawing of the AUROC curves
par(mar=c(5,5,2,1))
plot(1-ROC$specificities, 
     ROC$sensitivities,
     type="l",
     col="#E64B35",
     lty=1,
     lwd=2,
     xlab="1-Specificities", 
     ylab="Sensitivities", 
     xlim=c(0,1), 
     ylim=c(0,1),
     xaxs="i", 
     yaxs="i", 
     cex=1.5, 
     cex.lab=1.5, 
     cex.axis=1.5,)
lines(1-ROC_vad$specificities, 
      ROC_vad$sensitivities, 
      col="#4DBBD5", 
      lty=1, 
      lwd=2)
abline(0,1,lty=2,lwd=2)
legend("bottomright", 
       legend(x=0.5,
              y=0.3,
              legend=c("Derivation = 0.781 (0.766 ~ 0.796)", 
                       "Validation = 0.782 (0.763 ~ 0.801)"),
              lty = c(1,1),
              lwd = c(1,2),
              col = c("#E64B35", 
                      "#4DBBD5"),
              cex=1))
#The difference between the two AUROC curves
result <- roc.test(ROC, ROC_vad, method="bootstrap")
print(result)

#p values for the H-L test
library(ResourceSelection)
model_glm <- glm(group ~ trueage + education + sex + exercise + 
                   garden + tvradio + IADL + aural + chew,
                 data = data, family = binomial)
p.hoslem <- hoslem.test(model_glm$y, fitted(model_glm), g=10)$p.value
p.hoslem

#Draw the calibration curve
library(rms)
cal.glm <- calibrate(fit.glm,method="boot",B=1000)
par(mar=c(5,5,2,1))
plot(cal.glm,
     xlim = c(0,0.7), ylim = c(0,0.7),
     xlab = "Predicted Probability", ylab="Observed  Probability",
     xaxs = "i", yaxs = "i",
     legend =FALSE, subtitles = F,
     cex=1.5, cex.axis=1.5, cex.lab=1.5)
abline(0,1,col="blue",lty=2,lwd=2)
lines(cal.glm[,c("predy","calibrated.orig")],type="l",lwd=2,col="red")
lines(cal.glm[,c("predy","calibrated.corrected")],type="l",lwd=2,col="green")
legend(x=0.45,y=0.25,
       legend=c("Ideal","Apparent","Bias-corrected"),
       lty = c(2,1,1),
       lwd = c(2,1,1),
       col = c("blue","red","green"),
       bty="n",
       cex=1.5)
cal.glm_vad <- calibrate(fit.glm_vad,method="boot",B=1000)
par(mar=c(5,5,2,1))#图形边界，下左上右
plot(cal.glm_vad,
     xlim = c(0,0.7), 
     ylim = c(0,0.7),
     xlab = "Predicted Probability", 
     ylab="Observed  Probability",
     xaxs = "i", 
     yaxs = "i",
     legend =F, 
     subtitles = F,
     cex=1.5, 
     cex.axis=1.5, 
     cex.lab=1.5)
abline(0,1,col="blue",lty=2,lwd=2)
lines(cal.glm_vad[,c("predy","calibrated.orig")],
      type="l",
      lwd=2,
      col="red")
lines(cal.glm_vad[,c("predy","calibrated.corrected")],
      type="l",
      lwd=2,
      col="green")
legend(x=0.45,y=0.25,
       legend=c("Ideal","Apparent","Bias-corrected"),
       lty = c(2,1,1),
       lwd = c(2,1,1),
       col = c("blue","red","green"),
       bty="n",
       cex=1.5)

#Decision curve analysis
library(rmda)
DCA <- decision_curve(bestglm,
                      data=data,
                      family = binomial(link ='logit'),
                      thresholds= seq(0,1, 
                                      by = 0.01),
                      confidence.intervals =0.95,
                      study.design = 'cohort')
DCA_vad <- decision_curve(bestglm_vad,
                          data=vad,
                          family = binomial(link ='logit'),
                          thresholds= seq(0,1, 
                                          by = 0.01),
                          confidence.intervals =0.95,
                          study.design = 'cohort')
summary(DCA)
DCA$derived.data
head(DCA$derived.data[,c("thresholds","NB","sNB","cost.benefit.ratio")])
plot_decision_curve(list(DCA,
                         DCA_vad),
                    curve.names=c("CGMCI-Risk (Derivation)", 
                                  "CGMCI-Risk (Validation)"),
                    xlim=c(0,0.8),
                    cost.benefit.axis=TRUE,
                    col=c("red",
                          "green"),
                    confidence.intervals=F,
                    standardize=F)
