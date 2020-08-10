function moments = f_moments(x)
%% Calculate the first four moments

x_mean = mean(x);
x_sigma = var(x);
x_skewness = skewness(x);
x_kurtosis = kurtosis(x);

moments = horzcat(x_mean,x_sigma,x_skewness,x_kurtosis); %put it all together as a row

end