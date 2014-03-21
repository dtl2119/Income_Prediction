### Applied Data Science - HW3
### Linear OLS Regression

# Data was loaded using the dummy script

full.train.lin = cbind(age, cap.gain, cap.loss, country[,2:dim(country)[2]], 
	ed.lvl[,2:dim(ed.lvl)[2]], ed.yrs, fnlwgt, marital[,2:dim(marital)[2]], 
	occ[,2:dim(occ)[2]], race[,2:dim(race)[2]], relation[,2:dim(relation)[2]], 
	work[,2:dim(work)[2]])

full.test.lin = cbind(age.t, cap.gain.t, cap.loss.t, country.t[,2:dim(country.t)[2]], 
	ed.lvl.t[,2:dim(ed.lvl.t)[2]], ed.yrs.t, fnlwgt.t, marital.t[,2:dim(marital.t)[2]], 
	occ.t[,2:dim(occ.t)[2]], race.t[,2:dim(race.t)[2]], relation.t[,2:dim(relation.t)[2]], 
	work.t[,2:dim(work.t)[2]])

income.lin = income
income.lin[income == 0] = -1
income.lin.t = income.t
income.lin.t[income.t == 0] = -1


############################################################
### Full Model with all data included
full.lin.model = lm(income.lin ~ full.train.lin)

betas = as.numeric(full.lin.model$coef)
### NOTE: we have to exclude some values because the data matrix is not
### full rank

test.set = cbind(rep(1,dim(full.test)[1]),full.test.lin[,!is.na(betas[2:length(betas)])])
test.pred = sign(test.set %*% betas[!is.na(betas)])

# Misclassification Rates
# Test
print(1 - sum(as.numeric(test.pred == income.lin.t)) / length(income.lin.t))
# Train
print(1 - sum(as.numeric(sign(full.lin.model$fit) == income.lin)) / length(income.lin))




#########################################################
# Step and Stagewise Regressions

min.lin.model = lm(income.lin ~ 1)
fwd.lin.model = step(min.lin.model, 
	scope = (~age+ cap.gain + cap.loss + country[,2:dim(country)[2]] + 
	ed.lvl[,2:dim(ed.lvl)[2]] + ed.yrs + fnlwgt + marital[,2:dim(marital)[2]] + 
	occ[,2:dim(occ)[2]] + race[,2:dim(race)[2]] + relation[,2:dim(relation)[2]] + 
	work[,2:dim(work)[2]]), direction = "forward")
summary(fwd.lin.model)
fwd.betas = as.numeric(fwd.lin.model$coef)
sum(as.numeric(sign(fwd.lin.model$fit) == income.lin)) / length(income.lin)

stage.lin.model = step(min.lin.model, 
	scope = (~age+ cap.gain + cap.loss + country[,2:dim(country)[2]] + 
	ed.lvl[,2:dim(ed.lvl)[2]] + ed.yrs + fnlwgt + marital[,2:dim(marital)[2]] + 
	occ[,2:dim(occ)[2]] + race[,2:dim(race)[2]] + relation[,2:dim(relation)[2]] + 
	work[,2:dim(work)[2]]), direction = "both")
summary(stage.lin.model)
stage.betas = as.numeric(stage.lin.model$coef)
sum(as.numeric(sign(stage.lin.model$fit) == income.lin)) / length(income.lin)


full.bwd.lin.model = lm(income.lin ~ age+ cap.gain + cap.loss + country[,2:dim(country)[2]] + 
	ed.lvl[,2:dim(ed.lvl)[2]] + ed.yrs + fnlwgt + marital[,2:dim(marital)[2]] + 
	occ[,2:dim(occ)[2]] + race[,2:dim(race)[2]] + relation[,2:dim(relation)[2]] + 
	work[,2:dim(work)[2]])
bwd.lin.model = step(full.bwd.lin.model, direction = "backward")
summary(bwd.lin.model)
bwd.betas = as.numeric(bwd.lin.model$coef)
sum(as.numeric(sign(bwd.lin.model$fit) == income.lin)) / length(income.lin)


#### We choose to use the forward stagewise selection model
stage.test.set = cbind(rep(1,dim(full.test)[1]), relation.t[,2:dim(relation.t)[2]],
	ed.lvl.t[,2:dim(ed.lvl.t)[2]], cap.gain.t, occ.t[,2:dim(occ.t)[2]], cap.loss.t,
	age.t, work.t[,2:dim(work.t)[2]], race.t[,2:dim(race.t)[2]], 
	marital.t[,2:dim(marital.t)[2]])
stage.test.set = stage.test.set[,!is.na(stage.betas)]
stage.pred = sign(stage.test.set %*% stage.betas[!is.na(stage.betas)])

### Forward Stepwise Missclassification Rate
# Test
print(1 - sum(as.numeric(stage.pred == income.lin.t)) / length(income.lin.t))
# Train
print(1 - sum(as.numeric(sign(stage.lin.model$fit) == income.lin)) / length(income.lin))





