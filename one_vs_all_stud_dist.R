one_vs_all_stud_dist = function(x, Y){
  x_sq = sqrt(x)
  Y_sq = sqrt(Y)
  if(is.null(dim(Y))){ # Y is just one row
    Yminx = Y-x
    max_sq = pmax(x_sq,Y_sq)
  }
  else{
    Yminx = sweep(Y, 2, x)
    max_sq = sweep(Y_sq, 2 , x_sq, FUN = pmax)
  }
  Yminx_abs = abs(Yminx)
  dim_dist = Yminx_abs/max_sq
  dim_dist[is.na(dim_dist)] = 0.
  #print(Yminx_abs)
  #print(max_sq)
  #print(dim_dist)
  if(is.null(dim(Y))){
    sum(dim_dist)
  }
  else{
    rowSums(dim_dist)
  }
}