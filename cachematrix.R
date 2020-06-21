makeCacheMatrix<-function(x=matrix()){
  m<-NULL # cached data NULL restart
  set<-function(y){ # se puede reasignar la matrix cambiando por otra matrix
    x<<-y
    m<-NULL
  }
  get<-function() x #guarda matrix x con funcion get
  setinverse<-function(solve) m<<-solve # guarda funcion que calcula inversa matriz
  getinverse<-function() m # devuelve la matriz inversa
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse) #lista de funciones utilizadas dentro de makeCacheMatrix
}

cacheSolve<-function(x,...){
  m<-x$getinverse() 
  if(!is.null(m)){ # si el dato m=NULL, devuelve mensaje cached
    message("getting cached data")
    return(m)
  }
  data<-x$get() # obtiene matrix x y asigna a data
  m<-solve(data,...) # calcula inversa de x, desde m asignado por setinverse
  x$setinverse(m)
  m # retorna resultado inversa
}
# Language : ESP
