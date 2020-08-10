%set current directory to directory containing your files using cd
% cd('C://Users//frmendes//Downloads//Sandbox_Matlab//')

%get file list
data_dir = 'C:\Users\mshapiro\Documents\Deloitte\Acoustics oil pipeline\Data\Kuparuk Acoustic Sensor Data September 7-9, 2016';

FileList = dir(fullfile(data_dir, '*.tdms'));
N = size(FileList,1);

%cycle through filelist
for k = 1:N
    
    % get the file name:
    filename = fullfile(data_dir,FileList(k).name);
    disp(filename);
    simpleConvertTDMS(filename);
    % insert your script code here:
    
end
