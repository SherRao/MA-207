# Question 1
v = c("W", "L", "D");
p = c(1/2, 1/3, 1/6);
set.seed(398374621);

rand.chars = sample(v, 10000, replace=T, prob=p);
num.L = length(which(rand.chars == "L"));
freq.D = length(which(rand.chars == "D")) / length(rand.chars);
W.loc = which(rand.chars %in% "W")[547];

# Question 2
set.seed(398374621);
leaf.outcomes = sample(c("W", "L", "OTL"), 82, replace=T, prob=c(8/14, 5/14, 1/14));

indicate.win = leaf.outcomes == "W";
indicate.otl = leaf.outcomes == "OTL";
indicate.points = leaf.outcomes == "W" | leaf.outcomes == "OTL";

game.points = replace(leaf.outcomes, leaf.outcomes == "W", 2);
game.points = replace(game.points, game.points == "OTL", 1);
game.points = replace(game.points, game.points == "L", 0);
game.points = as.integer(game.points);
leaf.points = sum(game.points);

# Question 3
set.seed(246564839);
raptor.outcomes = sample(c("W", "L"), 82, replace=T, prob=c(7/13, 6/13));

raptor.indicate.win = raptor.outcomes == "W";
both.win = sum(indicate.win & raptor.indicate.win);
alo.win = sum(indicate.win | raptor.indicate.win);
same.outcome = sum(leaf.outcomes == raptor.outcomes);
same.outcome.alt = sum(replace(leaf.outcomes, leaf.outcomes == "OTL", "L") == raptor.outcomes);

# Question 4
x.big = c(10^10);
x.small = c(10^-10);
for(i in 1:49) {
  x.big[i+1] = (x.big[i] / 2) + (450 / x.big[i]);
  x.small[i+1] = (x.small[i] / 2) + (450 / x.small[i]);
}
x.guess = 30;

# Question 5
P.20 = matrix(1, 20, 20);
for(i in 2:20) {
  P.20[i,] = cumsum(P.20[i-1,]);
}

# Question 6
a = 1664525;
c = 1;
m = 2^32;
w = c(44);
u = c((w[1] + 0.5) / m);
for(i in 2:10000) {
  w[i] = (a * w[i-1] + c) %% m;
  u[i] = (w[i] + 0.5) / m;
}

f = rep(0, 10);
for (i in 1:10) {
  f[i] = sum(u > (i - 1) / 10 & u <= i / 10);
}

# Question 7
z = 109897655;
x = c(100, (100 / 2) + (z / 200));
eps = 10^-5;
iterations = 2;
while(abs(x[iterations] - x[iterations - 1]) >= eps) {
  x[iterations+1] = (x[iterations] / 2) + (z / (2 * x[iterations]));
  iterations = iterations + 1;
}
estimate = tail(x, n=1);
iterations = iterations - 1;

# Question 8
vec_norm = function(x) {
  result = norm(as.matrix(x), type="2");
  return(result);
}

two_out = function(x) {
  return(c(length=length(x), norm=norm(x, type="2")));
}

two_in = function(x, n) {
  return(matrix(x, ncol=length(x), nrow=n, byrow=TRUE));
}

# Question 9
savings_sequences = function(N, D, r) {
  B = c(D);
  I = c(0);
  C = c(D);
  
  for(i in 1:(N-1)) {
    B[i+1] = (1+r) * B[i] + D;
    I[i+1] = r*B[i] + I[i];
    C[i+1] = D + C[i]; 
  }
  
  result = rbind(rbind(B, I), C);
  rownames(result) = c("total", "earned", "saved");
  return(result);
}

# Question 10
investment_horizon = function(D, r, G) {
  B = D;
  I = c(0);
  C = c(D);
  N = 1;
  
  while(B < G) {
    B = (1+r)*B + D;
    I_n = r*B + I[N];
    C_n = D + C[N];

    I = c(I, I_n);
    C = c(C, C_n);
    N = N + 1;
  }
  
  return(N);
}