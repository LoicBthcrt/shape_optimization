clear all, close all

%rng default
% Defining domain
L = 2;
x1 = -L; x2 = L;
y1 = -L; y2 = L;
theta = linspace(0, 2*pi, 100);

% Plan d'experience
n_samples = 100;
dim = 3;
samples = zeros(n_samples, dim);
step = 2*L / n_samples;
values = zeros(n_samples, 1);

for i=1:n_samples
    samples(i, :) = -L + (i-1)*step + rand(1,dim) .* step;
    values(i) = -cost_func(samples(i, :), 10, theta, 2*pi);
end

k = oodacefit(samples, values);
[min_y, index] = min(values);
coef_min = samples(index, :);

% Options for particle swarm optimizer
lb = -L*ones(1,dim);
ub = L*ones(1,dim);
nvars = dim;
opts = optimoptions('particleswarm','MaxIterations',1000);

num_iter = 10;

for i=1:num_iter
    optim_func = @(x)-EI(x, min_y, k);
    x = particleswarm(optim_func,nvars,lb,ub, opts);
    samples = [samples ; x];
    y = -cost_func(x, 10, theta, 2*pi);
    values = [values; y];
    min_y = min(min_y, y);
    if min_y == y
        coef_min = x;
    end
    k = oodacefit(samples, values);
    %[X, Y] = get_points(coef_min, theta);
    %plot(X, Y)
    %pause(0.05)
end

%% 2D PLOT
% x1 = linspace(-L,L,100);
% y1 = linspace(-L,L,100);
% [X, Y] = meshgrid(x1, y1);
% Z = zeros(size(X));
% for i=1:100
%     for j=1:100
%         Z(i,j) = EI([1 X(i,j) Y(i,j)], min_y, k);
%     end
% end
% 
% surf(X, Y, Z);

%% 1-D PLOT
% x1 = linspace(-L,L,100);
% z = zeros(1,100);
% for i=1:100
%    z(i) =  EI([x1(i)], min_y, k);
% end
% 
% plot(x1, z)






