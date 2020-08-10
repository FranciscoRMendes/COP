%function to downsample a matrix
%% x data matrix, column-wise
%% n downsample
%% average true/false (default=true)
function final_mat = downsample_mat(x,n, average)

if nargin < 3
    average = true;
end

diff = mod(size(x,1),n);
if(diff >= size(x,1) || n <= 0 || n==1)
    disp('Downsample n is either >= data length, or <=0, or =1. Skip downsampling');
    final_mat = x;
else
    x = x(1:(end-diff),:);
    n_dsamp = floor(size(x,1)/n);
    
    final_mat = zeros(n_dsamp,size(x,2));
    for i =1:size(x,2)
        x_reshaped = reshape(x(:,i),n,n_dsamp);
        if(average)
            final_mat(:,i) = mean(x_reshaped,1);
        else
            final_mat(:,i) = x_reshaped(1,:);
        end
        
    end
end
