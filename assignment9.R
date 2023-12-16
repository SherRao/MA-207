# Question 1
matrix_power_temp = function(A, n) {
  X = A;
  for(i in 1:(n-1)) {
    X = X %*% A;
    print("A");
  }
  
  return(X);
}

matrix_power = function(A, n) {
  if(n == 1) {
    return(A);
  
  } else {
    return(matrix_power_temp(A, n));
  }
}

# Question 2
f = function(a) {
  if(a %% 4 == 2) {
    return(30);
    
  } else if(a %% 4 == 0) {
    return(20);
    
  } else if(a %% 4 == 3) {
    return(10);
    
  } else {
    return(0);
  }
}

n_terms = function(z, x1, n) {
  x = c(x1);
  for(i in 1:(n-1)) {
    x[i+1] = (x[i] / 2) + (z / (2 * x[i])) + (f(i) / i^2);
  }
  
  return(x);
}

estimate_root = function(z, x1, eps) {
  x = c(x1, (x1/2) + (z / (2 * x1)) + f(1) );
  i = 2;
  while(abs(x[i] - x[i-1]) >= eps) {
    x[i+1] = (x[i] / 2) + (z / (2 * x[i])) + (f(i) / i^2);
    i = i + 1;
  }
  
  estimate = tail(x, n=1);
  i = i - 1;
 
   return(c("estimate"=estimate, "iterations"=i));
}


# Graphs - Question 1.1
q1.1 = function(n) {
  a = c(1, 2, 2.25, 2 + (10/27));
  for(i in 5:n) {
    a[i] = (1 + (1/i))^i;
  }
  
  return(a);
}

plot(q1.1(11), type="b", pch=23, lty=2, col="blue4", main="Question 1", xlab="n", ylab="a_n", ylim=c(1,10));

# Graphs - Question 1.3
abline(v=0, h=exp(1), col="green4");
legend(x=5, y=6.5, legend=c("Euler Approx", "Euler's Number", "Q1.4 Sequence"), col=c("blue4", "green4", "red4"), cex=0.5, lty=c(2, 1));

# Graphs - Question 1.4
q1.4 = function(n) {
  a = c(1, 2, 2.5, 2 + (2/3));
  for(i in 5:n) {
    a[i] = 1 / factorial(i);
  }
  
  return(cumsum(a));
}

lines(q1.4(11), col="red4", type="b", pch=4, lty=3);


# Graphs - Question 2.1
q2.1 = function(x, n) {
  result = (1 + (x/n))^n;
  return (result);
}

x.vals = seq(-1, 1, length.out=100);
plot(q2.1(x.vals, 1), type="l", lty=2, col="darkred", main="Question 2", xlab="x", ylab="f_n(x)", ylim=c(0,2.5));
lines(q2.1(x.vals, 2), lty=3, col="darkgreen");
lines(q2.1(x.vals, 3), lty=4, col="blue");
lines(q2.1(x.vals, 4), lty=1, col="magenta");

# Graphs - Question 2.3
lines(exp(x.vals), lty=1, col="black");
legend(x=5, y=2.5, legend=c("f_1(x)", "f_2(x)", "f_3(x)", "f_4(x)", "e^x"), col=c("darkred", "darkgreen", "blue", "magenta", "black"), cex=0.5, lty=c(2, 3, 4, 1, 1));

# Graphs - Question 3
library(ggplot2);
q = function(p, k) {
  result = choose(k-1, 3) * ( (p^4 * (1 - p)^(k-4)) + p^(k-4)*(1 - p)^4 );
  return(result);
}

# Graphs - Question 3.1
par(mfrow=c(1,2));
heights.even = c(q(0.50, 4), q(0.50, 5), q(0.50, 6), q(0.50, 7));
barplot(heights.even, main="Even Probability", col="blue", names.arg=c("Four", "Five", "Six", "Seven"), ylim=c(0, 0.4), xlab="Games", ylab="Win Probability");

heights.uneven = c(q(0.75, 4), q(0.75, 5), q(0.75, 6), q(0.75, 7));
barplot(heights.uneven, main="Uneven Probability", col="green", names.arg=c("Four", "Five", "Six", "Seven"), ylim=c(0, 0.4), xlab="Games", ylab="Win Probability");
legend(x=0.5, y=0.4, legend=c("Even Probability", "Uneven Probability"), col=c("blue", "green"), cex=0.5, lty=1);

# Graphs - Question 3.2
condition = rep(c("Even" , "Uneven") , 2);
x.axis = c(rep("Four", 2) , rep("Five", 2) , rep("Six", 2) , rep("Seven", 2) );
y.axis = c(heights.even[1], heights.uneven[1], heights.even[2], heights.uneven[2], heights.even[3], heights.uneven[3], heights.even[4], heights.uneven[4]);
data = data.frame(x.axis, condition, y.axis);

ggplot(data, aes(fill=condition, y=y.axis, x=x.axis), col=c("blue", "green", "red", "magenta")) + 
  geom_bar(position="fill", stat="identity")