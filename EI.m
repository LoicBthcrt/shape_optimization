function [res] = EI(x, min_y, k)
% Expected improvement function
[m, s2] = k.predict(x);
temp = (min_y - m) / sqrt(s2);
res  = (min_y - m) * normcdf(temp) + sqrt(s2) * normpdf(temp);
end

