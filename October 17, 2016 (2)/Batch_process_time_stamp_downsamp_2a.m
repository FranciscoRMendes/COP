%%set data directory
data_dir = './Data'

FileList = dir(fullfile(data_dir,'10k daq*.mat'));
N = size(FileList,1);
steps=N;
offset = 4/24;
n_dsamp = 100;
tcorr = 3/(24*60*60*10*n_dsamp);
%cycle through filelist
for k = 1 : N
    k
filename = fullfile(data_dir,FileList(k).name)


% waitbar(k/steps,h1,sprintf('%d Complete',k));% m = matfile(filename,'Writable',true);
m = load(filename);
time = m.UntitledTime;
accl=m.UntitledAcceleration;
time_data=time.Data;

%OFFSET THE TIME HERE
time_data=time_data-offset;
m.Time=time_data;
accl_data=accl.Data;
m.Acceleration=accl_data;

ceps = ceps_gen_with_time(accl_data,time_data);
m.Ceps = ceps(:,2:end);
%% Acceleratin envelope, downsampled
acc_envelope = abs(accl_data);
acc_filt = movingmean(acc_envelope, n_dsamp);
acc_env_dsamp = downsample_mat(acc_filt, n_dsamp);
time_env_dsamp = downsample_mat(time_data, 100, false)+ tcorr;

m.Acc_env_dsamp = acc_env_dsamp;
m.Time_dsamp = time_env_dsamp;
% m.Time_str = datetime(time_data,'ConvertFrom', 'datenum', ...
%                          'Format', 'yyyy-MM-dd HH:mm:ss.SSSS' );
m.Time_dsamp_str = datetime(time_env_dsamp,'ConvertFrom', 'datenum', ...
                         'Format', 'yyyy-MM-dd HH:mm:ss.SS' );  
% remove fileds
m = rmfield(m, 'convertVer');
m = rmfield(m, 'ci');
m = rmfield(m, 'fileFolder');
m = rmfield(m, 'Root');
m = rmfield(m, 'Untitled');
m = rmfield(m, 'UntitledTime');
m = rmfield(m, 'UntitledAcceleration');

% save m structure with all fields
fsave_m = strrep(filename,'10k daq 1hr', 'm')
% save(fsave_m, 'm')

% save c structure with Acc_envelope and cepstral coeff only
c.Time = m.Time_dsamp(2:(end-1));
c.Time_str = m.Time_dsamp_str(2:(end-1));
c.Acc_env = m.Acc_env_dsamp(2:(end-1));
c.Ceps = ceps(:,2:end);

fsave_ceps = strrep(fsave_m,'m_', 'Ceps_')
% save(fsave_ceps, 'c')
fsave_csv = strrep(fsave_ceps,'.mat', '.csv')
ceps_table = table(c.Time, c.Time_str,c.Acc_env,c.Ceps,'VariableNames',...
    {'Time', 'Time_str', 'Acc_env', 'Ceps'});
writetable(ceps_table,fsave_csv)                     
                     
end
