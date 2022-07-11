corrds:: [a]->[a]->[(a,a)]
corrds a b = [(i,j)| i<-a, j<-b]