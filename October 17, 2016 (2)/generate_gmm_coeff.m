function coefficients = generate_gmm_coeff(frame,n)
Sigma = zeros(0,21);
coefficients = zeros(1,2*n*21);
rng(42)% for reporducibility
options = statset('Display','final');
obj = fitgmdist(frame,n,'CovarianceType','diagonal', 'RegularizationValue',0.001, ...
    'Options',options);
mu = obj.mu; %vector of mu
rho = obj.ComponentProportion; %this is the rho that gives the mixture weights
for j = 1:n
    Sigma =  vertcat(Sigma,obj.Sigma(:,:,j)); %take out the diagonals and stack them
end
coefficients = horzcat(reshape(vertcat(mu,Sigma),1,2*n*21),rho); %put it all together as a row

end


% Sigma = 
% options = statset('Display','final');
% X = super_frame;
% obj = fitgmdist(X,3,'Options',options);
% mu = obj.mu;
% sigma1 = obj.Sigma(:,:,1);mu
% sigma2 = obj.Sigma(:,:,2);
% sigma3 = obj.Sigma(:,:,3);
% 
% imagesc(sigma1)
% hold on
% imagesc(sigma2)
% hold on
% imagesc(sigma3)
% hold on
% imagesc(normc(X));
% 
% test = var(X);
% imagesc(X);
