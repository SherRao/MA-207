# Question 1
x.int = as.integer(factorial(10));
x.dub = exp(exp(-1));
x.log = x.int > 1;
x.char = as.character(x.log);

# Question 2
w1 = c(1, -1, 4);
w2 = c(1, -1, 4);
w3 = seq(from=10, to=4.75, by=-0.25);
w4 = seq(from=1, to=10, length.out=1000);
w5 = rep(4, times=432);
w6 = rep(c(1, 3), times=500);
w7 = rep(c(1, 0, 5), times=2000);
w8 = c(44, rep(c(1, 0), times=300), 4 -9);

# Question 3
i = 1:50;
s1 = sum(1/i);
s2 = sum(1/i^2);
s3 = sum(1/factorial(i));
s4 = sum( (-1)^(i+1)*1/i );

# Question 4
x = rep(1, times=10);
y = c(1, -1, 1, 0, 1, -1, 1, 0, 1, -1);
R_angle = acos( sum(x * y) / (sqrt(sum(x * x)) * sqrt(sum(y * y))) );

# Question 5
a1 = c(1, -7, 4, 8, -2, 5, 6, 2, 0, 1);
a2 = c(1, 5, -7, 6, 4, 2, 8, 0, -2, 1);
a3 = c(1, -7, 4, 8, -2);
a4 = c(5, 6, 2, 0, 1);
a5 = c(1, 5);
a6 = c(-7, 6);
a7 = c(4, 2);
a8 = c(8, 0);
a9 = c(-2, 1);
A = unname(rbind(a3, a4));

# Question 6
B.1 = as.matrix(c(1, 1, 2 ,4));
B.2 = t(as.matrix(c(1, 7, 9, 8)));
B.3 = t(as.matrix(100:1));
B.4 = as.matrix(seq(from=0.5, to=20.5, by=0.25));

# Question 7
options(max.print=100000000)
C = rbind(rep(c(1, 2), times=300), rep(c(1, 0, 3), times=200));
D = diag(seq(from=-5, to=23, by=1));
E_1 = cbind(diag(2, 5, 5), matrix(-3, 5, 5));
E_2 = cbind(matrix(7, 5, 2), matrix(0, 5, 8));
E = rbind(E_1, E_2);

# Question 8
Cn1 = matrix(0.5, 99, 1);
An = diag(0.5, 99, 99);
zn = matrix(0, 1, 99);
Q100_1 = cbind(Cn1, An);
Q100_2 = cbind(c(0.5), zn);
Q100 = rbind(Q100_1, Q100_2);

# Question 9
set.seed(103854);
P = matrix(sample(-9:9,432*587, replace=TRUE), nrow=432, ncol=587);
z1 = P[387, 517];
z2 = P[159,];
z3 = P[294, 1:10];
z4 = cbind(P[,ncol(P)]);
z5 = cbind(P[,ncol(P)-1], P[,ncol(P)]);
z6 = rbind(P[7,], P[143,]);

# Question 10
library(MASS);
M = matrix(c(16, 2, 3, 13, 5, 11, 10, 8, 9, 7, 6, 12, 4, 14, 15, 1), 4, 4, byrow=TRUE);
N = matrix(c(1, 1, 1, 1, 1, 2, 3, 4, 1, 3, 6, 10, 1, 4, 10, 20), 4, 4, byrow=TRUE);
S.1 = tcrossprod(M, N);
S.2 = M * as.vector(N);
S.3 = tcrossprod(M, tcrossprod(M, tcrossprod(M, M)));
S.4 = M * as.vector(M * as.vector(M * as.vector(M)));
S.5 = tcrossprod(t(M), ginv(N));
