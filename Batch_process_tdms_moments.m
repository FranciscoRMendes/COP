% Batch_process_tdms_moments.m
%set current directory to directory containing your files using cd
% cd('C://Users//frmendes//Downloads//Sandbox_Matlab//')

 
%get file list
% data_dir = 'C:\Users\mshapiro\Documents\Deloitte\Acoustics oil pipeline\Data\Kuparuk Acoustic Sensor Data September 7-9, 2016';
data_dir = '.\Data';
save_dir = '.\Output';
FileList = dir(fullfile(data_dir, '*.tdms'));
N = size(FileList,1);

window = 6000; %fix your window length here , 6000 is a minute. 
    steps=N;
    offset = 4/24;
    n_dsamp = 100;
    tcorr = 3/(24*60*60*10*n_dsamp);
        dt_cor = 2.345/(24*60*60*10000);
    %cycle through filelist
for k = 1:N
    k
    % get the file name:
    filename = fullfile(data_dir,FileList(k).name);
    disp(filename);
    tic()
    [m, filename] = simpleConvertTDMS_return(filename);
    toc()
    % insert your script code here:
    
%%%%%%%%%%%%%%%  SAVE converted data in .mat file
%%% dir_mat = ' PATH to .mat folder '
   %%% fname_mat = fullfile(dir_mat,filename);
   %%% save(fname_mat, 'm')
   
   
    % work with converted m struct
    time = m.UntitledTime;
    accl=m.UntitledAcceleration;
    time_data=time.Data;
    
    %OFFSET THE TIME HERE
    time_data=time_data-offset;
    % m.Time=time_data;
    accl_data=accl.Data;
    % m.Acceleration=accl_data;
    
    % Calculate cepstral coefficients
    tic()
    ceps = ceps_gen_with_time(accl_data,time_data);
    toc()
    % m.Ceps = ceps(:,2:end);
    % Acceleration envelope, downsampled
    % acc_envelope = abs(accl_data);
    % acc_filt = movingmean(acc_envelope, n_dsamp);
    % acc_env_dsamp = downsample_mat(acc_filt, n_dsamp);
    time_env_dsamp = downsample_mat(time_data, 100, false)+ tcorr;
    
    % m.Acc_env_dsamp = acc_env_dsamp;
    % m.Time_dsamp = time_env_dsamp;
    % % m.Time_str = datetime(time_data,'ConvertFrom', 'datenum', ...
    % %                          'Format', 'yyyy-MM-dd HH:mm:ss.SSSS' );
    % m.Time_dsamp_str = datetime(time_env_dsamp,'ConvertFrom', 'datenum', ...
    %                          'Format', 'yyyy-MM-dd HH:mm:ss.SS' );
    % % remove fileds
    % m = rmfield(m, 'convertVer');
    % m = rmfield(m, 'ci');
    % m = rmfield(m, 'fileFolder');
    % m = rmfield(m, 'Root');
    % m = rmfield(m, 'Untitled');
    % m = rmfield(m, 'UntitledTime');
    % m = rmfield(m, 'UntitledAcceleration');
    
    % save m structure with all fields
    % fsave_m = strrep(filename,'10k daq 1hr', 'm')
    % save(fsave_m, 'm')
    
    % save c structure with Acc_envelope and cepstral coeff only
    % c.Time = m.Time_dsamp(2:(end-1));
    % c.Time_str = m.Time_dsamp_str(2:(end-1));
    % c.Acc_env = m.Acc_env_dsamp(2:(end-1));
    ceps = ceps(:,2:end);
    % c.Ceps = ceps(:,2:end);
    
    % fsave_ceps = strrep(fsave_m,'m_', 'Ceps_')
    % save(fsave_ceps, 'c')
    % fsave_csv = strrep(fsave_ceps,'.mat', '.csv')
    % ceps_table = table(c.Time, c.Time_str,c.Acc_env,c.Ceps,'VariableNames',...
    %    {'Time', 'Time_str', 'Acc_env', 'Ceps'});
    %writetable(ceps_table,fsave_csv)
    
    % Calculate moments
  
    %     ceps = c.Ceps;
    %     time = c.Time;
    %     time_str=c.Time_str;
    
    % hard coded 4 moments
    
    start_idx = 0;
    end_idx = 0;
    no_of_frames = length(1:window:length(ceps(:,1)));%just to check the ceps number ofGMMS
    moments = zeros(no_of_frames, length(ceps(1,:))* 4);
    iwind = 1:window:length(ceps);
    for i = 1: length(iwind) %1:window:length(ceps)
        start_idx = iwind(i); %i;
        end_idx  = iwind(i) + window -1;
        
        if(end_idx>length(ceps))
            end_idx = length(ceps);
        end
        
        super_frame = ceps(start_idx:end_idx,:);
        moments(i,:) = f_moments(super_frame);
    end
    
    
    %    GMM_Moments = generate_gmm(ceps,window,no_of_mixtures); %the input paramters end up here
    %    g.GMM = GMM
    % correction for time roundup error
    %    t_cor = linspace(dt_cor, length(time)*dt_cor, length(time))';
    %    time_cor = time + t_cor;
    time_cor = time_env_dsamp + ...
        linspace(dt_cor, length(time_env_dsamp)*dt_cor, length(time_env_dsamp))';
    %    g.Time = time_cor(1:window:length(time));
    %    g.Time_str =  datetime(g.Time,'ConvertFrom', 'datenum', 'Format', 'yyyy-MM-dd HH:mm:ss');
    %    TIME =  datestr(time_cor(1:window:length(time)));
    TIME =  datestr(time_cor(1:window:length(time_env_dsamp)));
    %    foo=datetime(g.Time,'ConvertFrom', 'datenum', 'Format', 'yyyy-MM-dd HH:mm:ss.SSSSS')
    %    fsave_gmm = strrep(filename, 'Ceps', ['GMM_Moments_',num2str(no_of_mixtures),'mx.reg']);
    %    save(fsave_gmm, 'TIME', 'GMM_Moments')
    [pathstr,name,ext]=fileparts(filename{1});
    filesave = strrep(name,'10k daq 1hr', 'MOMENTS')
    save(fullfile(save_dir,filesave), 'TIME', 'moments')
    
end

toc
