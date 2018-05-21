clearvars
%import ESA data and convert grid down to 360*180 by averaging per 1 degree
%box for months sep 1997 - dec 2016
% 360*180*232 boxes to average
%also save lats and lons for use in 
%Data can be downloaded from ftp://oceancolour.org/occci-v3.1/geographic/netcdf/monthly/chlor_a/

user_wd='...'; % user should change to their working directory where ESA OC-CCI year directories are contained

%initalize output and temporary files
ind=0;
tick=0;

chl=zeros(360,180,232);
unc=zeros(360,180,232);
bias=zeros(360,180,232);
cells=zeros(360,180,232);

chltemp2=zeros(360,180);
unctemp2=zeros(360,180);
biastemp2=zeros(360,180);
cellstemp2=zeros(360,180);


lat=(linspace(-89.5,89.5,180))';
lon=(linspace(-179.5,179.5,360));

%load initial files to get lat/lon locations to match
lats=ncread(fullfile(user_wd,'1997\ESACCI-OC-L3S-CHLOR_A-MERGED-1M_MONTHLY_4km_GEO_PML_OCx-199711-fv3.1.nc'),'lat');
lons=ncread(fullfile(user_wd,'1997\ESACCI-OC-L3S-CHLOR_A-MERGED-1M_MONTHLY_4km_GEO_PML_OCx-199711-fv3.1.nc'),'lon');
lats=flipud(lats); %use ascending latitude


%import each month in to overall structure
for i=1997:2016
    for j=1:12
   ind=ind+1;
   if ind < 9 
   continue
   end
   tick=tick+1;
   disp(['Processing month: ', tick,' of: 232']) 
   year=num2str(i);
   month=num2str(j); 
   if j<10
    month=strcat('0',month);
   end
   
   
   part=strcat('ESACCI-OC-L3S-CHLOR_A-MERGED-1M_MONTHLY_4km_GEO_PML_OCx-', year, month ,'-fv3.1.nc');
   filename= fullfile('C:\Users\mh23g14\Downloads\ESA',year,part); 
   chltemp = fliplr(ncread(filename,'chlor_a'));%import chl as 8640*4320

 %average cells within 1 deg box

for l=1:length(lon)
for k=1:length(lat)

    latt=24*(k-1)+24;%max and mins indexes to average within (
    lont=24*(l-1)+24;
    latb=24*(k-1)+1;
    lonb=24*(l-1)+1;
    tempchl=chltemp(lonb:lont,latb:latt);
    cellsdata=sum(sum(~isnan(chltemp(lonb:lont,latb:latt)))); %count of number of cells making up each 1 degree box used for averaging

    if (sum(sum(isnan(tempchl)))/numel(tempchl))==1
      chltemp2(l,k)=NaN; %if all cells are NaN within box return NaN
    else
     
    chltemp2(l,k)=mean(tempchl(:),'omitnan'); %% as temp  
      cellstemp2(l,k)=cellsdata; %% as temp 
    end

end
end

%add to overall .mat file  
   chl(:,:,tick)=chltemp2;
         cells(:,:,tick)=cellstemp2;

    end
end







lats=lat;
lons=lon;
clearvars -except lats lons chl cells
save('ESA_v31_Data.mat')


