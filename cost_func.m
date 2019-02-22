function [result] = cost_func(fourrier_coeff, lambda, theta, peri)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
n = length(theta);
[X, Y] = get_points(fourrier_coeff, theta);
result = polyarea(X,Y);
new_peri = 0;
for i=1:n-1
    new_peri = new_peri + sqrt((X(i+1) - X(i))^2 + (Y(i+1) - Y(i))^2);
end

result = result - lambda * (new_peri - peri)^2;
end

