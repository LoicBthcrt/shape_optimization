function [X, Y] = get_points(fourrier_coeff,theta)
n = length(theta);
l = (length(fourrier_coeff)-1)/2;

X = ones(1,n);
Y = ones(1,n);
r = fourrier_coeff(1)*ones(1,n);

for i=2:l+1
    r = r + fourrier_coeff(i).*cos((i-1).*theta) + fourrier_coeff(i+l).*sin((i-1).*theta);
end

X = r.*cos(theta);
Y = r.*sin(theta);
end

