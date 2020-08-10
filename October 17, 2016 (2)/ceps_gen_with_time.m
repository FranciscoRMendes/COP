 
function ceps = ceps_gen_with_time(data,time)
% y the input data
% k the length of the centre frame (100)
% Q is the number of output cepstral Coefficients (20)
% O is the amount of overlap (0)
% W sets window for smoothed periodogram or not (3)
% S is the number of frames either side of the centre frame (1)
% wl is the window length (33)

%generate the ceps code for the data
 Q=20;
 T_in = length(data);
 k=50;
 O = 0; 
 W = 0; 
 S = 1;
 wl = 33;
 NRG_THRES=0;
 min_spec = -1e-6;
 
 
 
 
 K = (2*S+1)*k;
 
 KFFT    = pow2(nextpow2(2*K));
 kFFT    = pow2(nextpow2(k));
 
 new     = floor(k-O);                   
 T_out   = floor( (T_in-K)/new +1 );  
 cep_coeff  = zeros (Q+1, T_out);
 time_stamp = zeros (1, T_out);
 ACFw       = zeros(kFFT,1);
 window = ones(2*wl-1,1)';
 t_index = 0;
 discard = 0;
 
% h = waitbar(0,'Creating Cepstral Coefficients');
% steps=T_out;
%  
 
  for t = 1 : T_out
           
  
            
            super_frame    = data ((t-1)*new+1 : (t-1)*new+K);
         
  
%  super_frame = mu2lin(super_frame);
 
         if std(super_frame).^2 >  NRG_THRES  

            t_index= t_index+1;

            ACF = real(ifft((abs(fft(super_frame, KFFT)).^2)/K, KFFT));

    
            ACFw(1:wl) = ACF(1:wl).*window(wl:2*wl-1)'; % Hanning window
            ACFw(kFFT-wl+2:kFFT) = flipud(ACFw(2:wl));
            spec = abs(fft(ACFw));
            low=(t-1)*new+1;
            high=(t-1)*new+K;
            index=floor((low+high)/2);
 

            cepstrum  = real(ifft(log(spec)));
            cep_coeff (:,t_index)=cepstrum(1:Q+1); 
            time_stamp(:,t_index)=time(index); 
         end                                      
  
%             msg2 = sprintf('%d,%s,%d',t,datestr(time(index),'mmmm dd, yyyy HH:MM:SS.FFF AM'),T_out);
% %            msg2 = sprintf('%d,%d,%d',t,((t-1)*new+1),T_out);
% %          
%          waitbar(t/steps,h,msg2);
%          
         
   
    end                                        
    status = 0;                                
    cep_coeff = cep_coeff(:,1:t_index);
    cep_coeff = cep_coeff';
    time_stamp = time_stamp';
    ceps=horzcat(time_stamp,cep_coeff);
%     close(h);
   
    
%     datestr(time((t-1)*new+1),'mmmm dd, yyyy HH:MM:SS.FFF AM')