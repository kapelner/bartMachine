
bartMachineArr = function(bart_machine, R = 10){
	arr = list()
	arr[[1]] = bart_machine
	for (i in 2 : R){
		arr[[i]] = bart_machine_duplicate(bart_machine)
	}
	class(arr) = "bartMarchineArr"
	arr
}
#S3 predict method
predict.bartMarchineArr = function(object, new_data, ...){
	R = length(object)
	n_star = nrow(new_data)
	predicts = matrix(NA, nrow = n_star, ncol = R)
	for (r in 1 : R){
		predicts[, r] = predict(object[[r]], new_data, ...)
	}
	rowMeans(predicts)
}