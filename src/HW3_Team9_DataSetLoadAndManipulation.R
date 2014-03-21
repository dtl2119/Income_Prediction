#### LOADING DATA
dat = read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"), header = F)
#### if already loaded, the full data set should be relabeled "dat" 

n = dim(dat)

### SPLITTING DATA
n.train = ceiling(n[1] / 2)
train = dat[1:n.train,]
test = dat[(n.train + 1):n[1],]

train = apply(train, 2, function(x)gsub(" ","",x))
train = as.data.frame(train)

test = apply(test, 2, function(x)gsub(" ","",x))			
test = as.data.frame(test)

dat = apply(dat, 2, function(x)gsub(" ","",x))
dat = as.data.frame(dat)

################################################
### TRAINING SET CONSTRUCTION
n.train = dim(train)[1]

# INCOME
income = rep(0, n.train)
income[train$V15 == ">50K"] = 1	# 1 if >50K income

# GENDER
gender = rep(0, n.train)
gender[train$V10 == "Female"] = 1	

# MARIATAL STATUS
marital.uni = sort(unique(train$V6))
marital = matrix(0, nrow = n.train, ncol = length(marital.uni))
for(i in 1:length(marital.uni)){
	marital[train$V6 == marital.uni[i],i] = 1
}

# EDUCATION YEARS
ed.yrs = as.numeric(train$V5)

# AGE
age = as.numeric(train$V1)

# EDUCATION LEVEL
ed.lvl.uni = sort(unique(train$V4))
ed.lvl = matrix(0, nrow = n.train, ncol = length(ed.lvl.uni))
for(i in 1:length(ed.lvl.uni)){
	ed.lvl[train$V4 == ed.lvl.uni[i],i] = 1
}

# CAPITAL GAIN
cap.gain = as.numeric(train$V11)

# WORKCLASS
work.uni = sort(unique(train$V2))
work = matrix(0, nrow = n.train, ncol = length(work.uni))
for(i in 1:length(work.uni)){
	work[train$V2 == work.uni[i],i] = 1
}

# FNLWGT
fnlwgt = as.numeric(train$V3)

# OCCUPATION
occ.uni = sort(unique(train$V7))
occ = matrix(0, nrow = n.train, ncol = length(occ.uni))
for(i in 1:length(occ.uni)){
	occ[train$V7 == occ.uni[i], i] = 1
}

# RELATIONSHIP
relation.uni = sort(unique(train$V8))
relation = matrix(0, nrow = n.train, ncol = length(relation.uni))
for(i in 1:length(relation.uni)){
	relation[train$V8 == relation.uni[i],i] = 1
}

# RACE
race.uni = sort(unique(train$V9))
race = matrix(0, nrow = n.train, ncol = length(race.uni))
for(i in 1:length(race.uni)){
	race[train$V9 == race.uni[i],i] = 1
}

# CAPITAL LOSS
cap.loss = as.numeric(train$V12)

# COUNTRY
country.uni = sort(unique(as.character(dat$V14)))
country = matrix(0, nrow = n.train, ncol = length(country.uni))
for(i in 1:length(country.uni)){
	country[train$V14 == country.uni[i],i] = 1
}

full.train = cbind(age, cap.gain, cap.loss, country, ed.lvl, ed.yrs, fnlwgt, gender, marital, occ, race, relation, work)


################################################
### TEST SET CONSTRUCTION

n.test = dim(test)[1]

# INCOME
income.t = rep(0, n.test)
income.t[test$V15 == ">50K"] = 1	# 1 if >50K income

# GENDER
gender.t = rep(0, n.test)
gender.t[test$V10 == "Female"] = 1	

# MARIATAL STATUS
marital.uni = sort(unique(test$V6))
marital.t = matrix(0, nrow = n.test, ncol = length(marital.uni))
for(i in 1:length(marital.uni)){
	marital.t[test$V6 == marital.uni[i],i] = 1
}

# EDUCATION YEARS
ed.yrs.t = as.numeric(test$V5)

# AGE
age.t = as.numeric(test$V1)

# EDUCATION LEVEL
ed.lvl.uni = sort(unique(test$V4))
ed.lvl.t = matrix(0, nrow = n.test, ncol = length(ed.lvl.uni))
for(i in 1:length(ed.lvl.uni)){
	ed.lvl.t[test$V4 == ed.lvl.uni[i],i] = 1
}

# CAPITAL GAIN
cap.gain.t = as.numeric(test$V11)

# WORKCLASS
work.uni = sort(unique(test$V2))
work.t = matrix(0, nrow = n.test, ncol = length(work.uni))
for(i in 1:length(work.uni)){
	work.t[test$V2 == work.uni[i],i] = 1
}

# FNLWGT
fnlwgt.t = as.numeric(test$V3)

# OCCUPATION
occ.uni = sort(unique(test$V7))
occ.t = matrix(0, nrow = n.test, ncol = length(occ.uni))
for(i in 1:length(occ.uni)){
	occ.t[test$V7 == occ.uni[i], i] = 1
}

# RELATIONSHIP
relation.uni = sort(unique(test$V8))
relation.t = matrix(0, nrow = n.test, ncol = length(relation.uni))
for(i in 1:length(relation.uni)){
	relation.t[test$V8 == relation.uni[i],i] = 1
}

# RACE
race.uni = sort(unique(test$V9))
race.t = matrix(0, nrow = n.test, ncol = length(race.uni))
for(i in 1:length(race.uni)){
	race.t[test$V9 == race.uni[i],i] = 1
}

# CAPITAL LOSS
cap.loss.t = as.numeric(test$V12)

# COUNTRY
country.uni = sort(unique(as.character(dat$V14)))
country.t = matrix(0, nrow = n.test, ncol = length(country.uni))
for(i in 1:length(country.uni)){
	country.t[test$V14 == country.uni[i],i] = 1
}

full.test = cbind(age.t, cap.gain.t, cap.loss.t, country.t, ed.lvl.t, ed.yrs.t, fnlwgt.t, gender.t, marital.t, occ.t, race.t, relation.t, work.t)




