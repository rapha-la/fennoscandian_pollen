stud_dist = function(X){
  dists = dist(X) # just to create matrix in the same format
  n = attr(dists,"Size")
  for(i in 1:(n-2)){
    print(i)
    beginning = n*(i-1) - i*(i-1)/2 + 1 # from dist documentation - where the smallest j resides
    dists[beginning:(beginning+n-i-1)] = one_vs_all_stud_dist(X[i,], X[(i+1):n,])
  }
  dists
}