### HW_4### 

##EXERCICI 1: CONSTRUCCIÓ D'UN ARBRE BINOMIAL

#a) Contrucció de l'arbre

# Creem la funció que retorna l'arbre binomial

build_stock_tree = function(S,u,v,N){
  tree <- matrix(data=0,nrow=N+1, ncol=N+1)
  for (i in 1:(N+1)){
    for (j in 1:i){
      tree[i,j]=S*u^(j-1) * v^{i-j}
    }
  }
  return (tree);
}
build_stock_tree(S=10, u=1.1, v=0.9, N=2)
#Obtenim justament l'arbre que voliem.

# b) Probabilitat de risc neutral

#Creem la funció seguint el prototip donat
q_prob = function(r,u,v,dt){
  q <- (1+r*dt-v)/(u-v)
  return (q)
}
q_prob(0.1, 1.1, 0.9, 1/256)


# c) Valoració recursiva
value_binomial_option= function(tree, u,v,r,dt,K,type){
  option_tree = matrix(0, nrow=nrow(tree), ncol=ncol(tree))
  q <- q_prob(r,u,v,dt)
  N <- nrow(tree)
  
  # Omplim els últims nodes de l'arbre amb la funció de pagament
  if (type=='call'){
    #La funció de pagament d'un K-Call es max(S_T-K,0)
    option_tree[N, 1:N] =  pmax(tree[N, 1:N]-K,0)
  }
  if (type=='put'){
    #La funció de pagament d'un K-Put es max(K-S_T,0)
    option_tree[N,1:N] = pmax(K-tree[N,1:N],0)
  }
  #Omplim amb la fórmula
  for (i in (N-1):1){
    option_tree[i, 1:i] = (q*option_tree[i+1,2:(i+1)]+(1-q)*option_tree[i+1,1:i])/(1+r*dt)
  }
  return (option_tree)
}

#Comprovem l'exemple
S=10
N=5
u=1.01
v=0.99
r=0.1
dt=1/256
K=10
tree=build_stock_tree(S,u,v,N)
value_binomial_option(tree, u,v,r,dt,K, type='call')


# d) Preu final
binomial_option = function(type, u, v, dt, r, K,S,N){
  tree       <- build_stock_tree(S,u,v,N)
  value_tree <- value_binomial_option(tree, u,v,r,dt,K, type)
  price      <- value_tree[1,1]
  return (price)
}

# Comrpovem l'exemple
binomial_option(type='call', u=1.01, v=0.99, dt=1/256, r=0.1, K=10, S=10, N=5)
# [1] 0.1036739


# EXERCICI 2
# Substituim els valors que indica l'enunciat
type="call"
u=1.01
v=0.99
r=0.1
dt=1/12
K=100
S=100
N=12
binomial_option(type, u, v, dt, r,K,S,N)
# 9.478797

