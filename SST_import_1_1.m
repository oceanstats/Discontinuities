%Convert 'sst.mnmean.nc' to mat file with ascending latitude and longitude
%in the range -180 to 180

%'sst.mnmean.nc' can be downloaded from https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html

user_wd='...'; % user should change to their working directory containing 'sst.mnmean.nc'
SST=ncread(fullfile(user_wd,'sst.mnmean.nc'),'sst');% read SST data from netcdf
%1981/12 --> 2017/04 so remove from end the unnecessary
time=ncread('sst.mnmean.nc','time');
time=datetime(time*86400,'ConvertFrom','epochtime','Epoch','1800-01-01'); % convert netcdf discrete time to actual datetime 
SST=SST(:,:,190:421); %select time range required i.e. here sep 1997 to dec 2017

%reverse latitude so it is ascending
SST=flip(SST,2);
lat=ncread('sst.mnmean.nc','lat');
lat=flip(lat);
%convert longitude so it is in the range -180:180
lon=ncread('sst.mnmean.nc','lon');
lx=find(lon>179.5);
ux=find(lon<=179.5);
lon(lx)=lon(lx)-360;
lon=sort(lon);
temp=zeros(size(SST));
temp(1:length(lx),:,:)=SST(lx,:,:);
temp((length(lx)+1):end,:,:)=SST(ux,:,:);
SST=temp;

save("SST_for_v3.1.mat","SST")

%plot test figure
figure(1)
pcolor(lon,lat,SST(:,:,1)')


