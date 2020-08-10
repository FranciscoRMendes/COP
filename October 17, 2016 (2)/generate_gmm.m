function GMM= generate_gmm(ceps,window,no_of_mixtures)

% hard coded 4 moments
GMM = zeros(0,((21*(no_of_mixtures*2 + 4))+ no_of_mixtures ));
% window = 6000;

start_idx = 0;
end_idx = 0;
no_of_frames = length(1:window:length(ceps))%just to check the ceps number ofGMMS

for i = 1:window:length(ceps)
    
    start_idx = i;
    end_idx  = i+window;
   
    
    if(end_idx>length(ceps))
        end_idx = length(ceps)
    
    end
    
    super_frame = ceps(start_idx:end_idx,:);
    GMM = vertcat(GMM, [generate_gmm_coeff(super_frame,no_of_mixtures), f_moments(super_frame)]);
    
end


% imagesc(normc(GMM))
