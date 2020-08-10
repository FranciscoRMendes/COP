%%set data directory
data_dir = './Data'

FileList = dir(fullfile(data_dir,'*.mat'));
N = size(FileList,1);
steps=N;
%cycle through filelist
for k = 1 % : N
    k
filename = fullfile(data_dir,FileList(k).name)

%m = matfile(filename,'Writable',true);
m = load(filename);
time = m.Time;
accl=m.Acceleration;
acc_envelope = abs(accl);
acc_filt = movingmean(acc_envelope, 100);
acc_env_dsamp = downsample_mat(acc_filt, 100);
time_env_dsamp = downsample_mat(time, 100, false);

m.Acceleration_dsamp = acc_env_dsamp;
m.Time_dsamp = time_env_dsamp;
m.Time_str = datetime(time,'ConvertFrom', 'datenum', ...
                         'Format', 'yyyy-MM-dd HH:mm:ss.SSSS' );
m.Time_dsamp_str = datetime(time_env_dsamp,'ConvertFrom', 'datenum', ...
                         'Format', 'yyyy-MM-dd HH:mm:ss.SSSS' );   
% m.UntitledAcceleration=[]; %delete the old Time struct
% m.UntitledTime=[]; %delete the old Acceleration struct

end

%% save downsampled Acceleration and time
for k = 1 : N
    k
    filename = fullfile(data_dir,FileList(k).name)
    m = load(filename);
    Time_num = m.Time_dsamp;
    Time_str = m.Time_dsamp_str;
    Acc_dsamp = m.Acceleration_dsamp;
    save(fullfile(data_dir,['Acc_dsamp',FileList(k).name(12:end)]), ...
        'Time_num', 'Time_str','Acc_dsamp')
end
