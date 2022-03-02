stud_dist_increments = function(X){
  n = dim(X)[1]
  dists = dist(X[1:(n-1),]) # just to create matrix in the same format
  X_incr = X[2:n,] - X[1:(n-1),]
  X_max2 = pmax(X[1:(n-1),],X[2:n,])
  for(i in 1:(n-2)){
    beginning = (n-1)*(i-1) - i*(i-1)/2 + 1 # from dist documentation - where the smallest j resides
    dists[beginning:(beginning+n-i-2)] = one_vs_all_stud_dist_incr(X_max2[i,], X_max2[(i+1):(n-1),],X_incr[i,],X_incr[(i+1):(n-1),])
  }
  dists
}