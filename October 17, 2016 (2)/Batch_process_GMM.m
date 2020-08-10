data_dir = '.\Data' ;

window = 6000; %fix your window length here , 6000 is a minute.
no_of_mixtures = 2; %no. of mixtures here
FileList = dir(fullfile(data_dir,'Ceps_*.mat'));
N = size(FileList,1);
steps=N;
% h = waitbar(0,'Creating GMM mat files.');
g = [];
for k = 1 : N
    k
    filename = fullfile(data_dir,FileList(k).name);
    disp(filename);
    % m = matfile(filename,'Writable',true);
    load(filename);
    ceps = c.Ceps;
    time = c.Time;
    time_str=c.Time_str;
    GMM_Moments = generate_gmm(ceps,window,no_of_mixtures); %the input paramters end up here
%    g.GMM = GMM
    % correction for time roundup error
    dt_cor = 2.345/(24*60*60*10000); 
    t_cor = linspace(dt_cor, length(time)*dt_cor, length(time))';
    time_cor = time + t_cor;
%    g.Time = time_cor(1:window:length(time));
%    g.Time_str =  datetime(g.Time,'ConvertFrom', 'datenum', 'Format', 'yyyy-MM-dd HH:mm:ss');
    TIME =  datestr(time_cor(1:window:length(time)));
%    foo=datetime(g.Time,'ConvertFrom', 'datenum', 'Format', 'yyyy-MM-dd HH:mm:ss.SSSSS')
    fsave_gmm = strrep(filename, 'Ceps', ['GMM_Moments_',num2str(no_of_mixtures),'mx.reg']);
    save(fsave_gmm, 'TIME', 'GMM_Moments')
    % waitbar(k/steps);
    
end