%% Section 1A - get the time indices for each squat in the squat trials and 
% for each stance phase in the run trials (one subject at a time)

%import variables the put 10 trials into a structure
clear all

%import ASCII file
%create structure for each subject with each condition as a substructure,
%each of which holds the variables, which at 101x10 matrices


%pick folder with data files
expfolder = uigetdir('Pick folder');
tmpFiles = dir(expfolder);
tmpFiles(ismember({tmpFiles.name},{'.','..'})) = [];
tmpFiles(ismember({tmpFiles.name},'.DS_Store')) = [];

%create structure or load previous structure
if isfile('LoftTunedData.mat') == 1
    load('LoftTunedData.mat')
else
    LoftTunedData = struct;
    DataTable = table();
    stepIdx = struct;
end

%grab right knee angle for squat trial only to find peaks - will allow for
%separation into 10 trials for each variable later on
   filelist = {tmpFiles(:).name};
   tmpfilename = tmpFiles(...
    contains(string(filelist), 'Squat') &...
    contains(string(filelist), 'KneeFlexion') == 1).name;
   KneeAngleSquat = table2array(...
    readtable(strcat(expfolder, '/', tmpfilename)));
   [pks, Pidx] = getTrials(KneeAngleSquat);

% for each shoe condition, load in GRFz to separate out each step and get
% indices to apply for each variable
shoelist = {'C1', 'C2', 'C3', 'C4', 'Sock'};
expfolder2 = uigetdir()
first_step_peak = input('What is the order of first peak for right leg?: ')

for cnd = 1:length(shoelist) 
    
   % if cnd < length(shoelist)
    Shoe = strcat( char(shoelist(cnd)), '_8min');
   % else
       %Shoe = char(shoelist(cnd));
  % end
    
    tmpfilename = tmpFiles(...
        contains(string(filelist), 'GRFz') &...
        contains(string(filelist), Shoe) == 1).name;
    idx = strfind(tmpfilename,'_');
    
    ID = tmpfilename((idx(end)+1):(end-4));
    
    
    tmpGRFz = table2array(readtable(strcat(expfolder, '/', tmpfilename)));
    tmpGRFz = tmpGRFz(1:10:length(tmpGRFz),:);
    figure(1),plot(tmpGRFz(1:1600,2))
    figure(1), hold on
    title(tmpfilename)
    
        
   
    tmpstart = round(ginput(1))
    if tmpstart(1) < 1
        tmpstart(1) = 1
    end
    %if strcmp(Shoe, 'Sock') == 1 && strcmp(ID, 'S01') ==1
        nonzeroGRFidx = tmpGRFz(tmpstart(1):(tmpstart(1)+1600),2) > 0.075;
   % else
    %nonzeroGRFidx = tmpGRFz(tmpstart(1):(tmpstart(1)+1600),2) > 0
   % end
    transitions = diff(nonzeroGRFidx) == 1;
    transitions2 = (diff(nonzeroGRFidx) == -1);
    FS = (find(transitions == 1)+tmpstart(1));
    TO = (find(transitions2 == 1)+tmpstart(1));
    % stance = FS(1) - (TO(1)-1)
    
    
    
    
    %pts = ginput(21);
    
    startpeak = first_step_peak(cnd);
    if startpeak == 1
        
        for stepnum = 1:10
            Step = strcat('Step', num2str(stepnum));
 
                tmpStepIdx = (FS(2*stepnum-1)-1):(TO(2*stepnum-1));
                
                figure(2), plot(tmpGRFz(tmpStepIdx,2))
                figure(2), hold on
                figure(1), area(tmpStepIdx, tmpGRFz(tmpStepIdx,2))
                %close(figure(2))
                stepIdx.(ID).(Shoe).(Step) = tmpStepIdx;
            %end
        end
        saveas(figure(2), strcat(expfolder2, '\', ID, '_', Shoe, '_GRFz_IndivdualSteps'))
        close(figure(2))
    elseif startpeak == 2
        
        for stepnum = 1:10
            Step = strcat('Step', num2str(stepnum));

                tmpStepIdx = (FS(2*stepnum)-1):(TO(2*stepnum));
                figure(2), plot(tmpGRFz(tmpStepIdx,2))
                hold on
               % savefig(strcat(expfolder2, '\', ID, '_', Shoe, '_GRFz_', Step))
               % close(figure(2))
                figure(1), area(tmpStepIdx, tmpGRFz(tmpStepIdx,2))
                stepIdx.(ID).(Shoe).(Step) = tmpStepIdx;
           % end
        end
        saveas(figure(2), strcat(expfolder2, '\', ID, '_', Shoe, '_GRFz_IndivdualSteps'), 'png')
        close(figure(2))
    else
        break
    end
    
    saveas(figure(1), strcat(expfolder2, '\', ID, '_', Shoe, '_GRFz_FullWaveform'), 'png')
    close(figure(1))
    
end
%% Section 1B - Use time indices to plot angles over 10 stance phases for 
% each parameter for each shoe, as well as squats - also time normalizes 
% data (one subject at time - continuation from above)

rawdataplotfolder = uigetdir();
normdataplotfolder = uigetdir();
    legendLabels = {'Step1','Step2','Step3','Step4','Step5','Step6','Step7','Step8','Step9','Step10'};
%load files and put data into structure
for i = 1:length(tmpFiles)
    
    %get file name
    tmpfilename = tmpFiles(i).name;
    
    %parse information from the filename
    idx = strfind(tmpfilename,'_');
    
    ID = tmpfilename((idx(end)+1):(end-4))
    Shoe = tmpfilename((idx(end-2)+1):(idx(end)-1))
    Var = tmpfilename(1:(idx(end-2)-1))
    
    %read ASCII file into a table
    tmpT = table2array(readtable(strcat(expfolder, '/', tmpfilename)));
    if strcmp(Var, 'GRFz') == 1 && strcmp(Shoe, 'Squat_Trial')~= 1 
        tmpT = tmpT(1:10:length(tmpT),:);
    end
    figure(50), plot(tmpT(:,2))
    title(strcat(ID, ' - ', Shoe, ' - ', Var, ' - ', 'All Steps'))
    savefig(strcat(rawdataplotfolder, '\', ID, '_', Shoe, '_', Var, '_FullWaveform')) 
    close(figure(50))
    
    if tmpFiles(i).bytes < 9999  %in case any didn't export right,
        continue                %this keeps it from failing
    else
        
        %special condition if squat trial because squats need to be parsed
        %out of the data to get an ensemble curve
        if strcmp(Shoe, 'Squat_Trial') == 1
            
            num_trials = (length(Pidx)-1);
            normTrials = zeros(100,10);
            
            for sq = 1:num_trials
                Squat = strcat(Shoe, num2str(sq));
                
                tmpTrial  = (...
                    tmpT((Pidx(sq)):(Pidx(sq+1)),:));
                LoftTunedData.(ID).(Shoe).(Var).RawData.(Squat) = tmpTrial(:,2);
                
                figure(500+i), plot(tmpTrial(:,2))
                title(strcat(ID, ' - ', Shoe, ' - ', Var, ' - Raw'))
                hold on
                
                normTrials(:,sq) = spline(...
                    tmpTrial(:,1), tmpTrial(:,2), linspace(tmpTrial(1,1), tmpTrial(end,1)));
                figure(600+i), plot(normTrials(:,sq))
                title(strcat(ID, ' - ', Shoe, ' - ', Var, ' - Norm'))
                hold on
                
            end
            
            figure(500+i), legend(legendLabels)
            saveas(figure(500+i), strcat(rawdataplotfolder, '\', ID, '_', Shoe, '_', Var, ' _Raw'), 'png');
            close(figure(500+i))
            
            figure(600+i), legend(legendLabels)
            saveas(figure(600+i), strcat(normdataplotfolder, '\', ID, '_', Shoe, '_', Var, ' _Norm'), 'png');
            close(figure(600+i))
            
            LoftTunedData.(ID).(Shoe).(Var).NormData = normTrials;
       % elseif strcmp(ID, 'S12') == 1 && strcmp(Var, 'GRFz') == 1 
        %    continue
        else
            normTrials = zeros(100,10);
            for stepnum = 1:length(fieldnames(stepIdx.(ID).(Shoe)))
                
                Step = strcat('Step', num2str(stepnum));
                s_idx = (stepIdx.(ID).(Shoe).(Step));
                LoftTunedData.(ID).(Shoe).(Var).RawData.(Step) = tmpT(s_idx,2);
                figure(100+i), plot(tmpT(s_idx,2))
                hold on
                
                title(strcat(ID, ' - ', Shoe, ' - ', Var, ' - Raw'))
                %savefig(strcat(rawdataplotfolder, '\', ID, '_', Shoe, '_', Var, '_', Step))
                
                
                
                tmpTrial = tmpT(s_idx,2);
                
                normTrials(:,stepnum) = spline(...
                    1:length(tmpTrial), tmpTrial(:,1), ...
                    linspace(1,length(tmpTrial)));
                figure(200+i), plot(normTrials(:,stepnum))
                hold on
                
                title(strcat(ID, ' - ', Shoe, ' - ', Var, ' - Norm'))
                %savefig(strcat(normdataplotfolder, '\', ID, '_', Shoe, '_', Var, '_', Step))
                
                
                
            end
            figure(100+i), legend(legendLabels)
            saveas(figure(100+i), strcat(rawdataplotfolder, '\', ID, '_', Shoe, '_', Var, ' _Raw'), 'png');
            close(figure(100+i))
            
           figure(200+i), legend(legendLabels)
            saveas(figure(200+i), strcat(normdataplotfolder, '\', ID, '_', Shoe, '_', Var, ' _Norm'), 'png');
            close(figure(200+i))
            
            LoftTunedData.(ID).(Shoe).(Var).NormData = normTrials;
            
        end
    end
end

 save('LoftTunedData.mat', 'LoftTunedData', 'DataTable', 'stepIdx')
 
 %%%old code
%  elseif strcmp(Var, 'GRFz') == 1
% 
%             if strcmp(Shoe, 'G8min') & strcmp(ID, 'S05') == 1
%                 %%1st column is index, 2nd has data, 11 has NaNs to skip
%                 LoftTunedData.(ID).(Shoe).(Var).Data = tmpT(:,[2:10,12]);
%             else
%                 %1st column is index, 2nd has data
%                 LoftTunedData.(ID).(Shoe).(Var).Data = tmpT(:,2:11);
%             end
%         else
%             %1st column is index, 2 and 3 are mean & SD
%             LoftTunedData.(ID).(Shoe).(Var).Data = tmpT(:,4:13);
%         end
 
%% Section 2: Caclulate ensemble curves for each parameter for each shoe for each 
%subject (can do multiple subjects at one time)
ShoeMeans = struct;
 ids = fieldnames(LoftTunedData);

 count = 0
 for i = 1:length(ids)
     
     ID = char(ids(i));
     shoes = fieldnames(LoftTunedData.(ID));
     
     Group = 'All';
     count = count + 1;

     for s = 1:length(shoes)
         
         Shoe = char(shoes(s));
         vars = fieldnames(LoftTunedData.(ID).(Shoe));

         for v = 1:length(vars)
             
             Var = char(vars(v));

             for d = 1:length(LoftTunedData.(ID).(Shoe).(Var).NormData)
                 
                      LoftTunedData.(ID).(Shoe).(Var).Mean(d,1) = mean(...
                     LoftTunedData.(ID).(Shoe).(Var).NormData(d,:));
                
             end
             
             if strcmp(Var, 'GRFz') == 1
                 
              ShoeMeans.(Group).(Var).(Shoe)(:,count) = LoftTunedData.(ID).(Shoe).(Var).Mean;

                 
             elseif strcmp(Var, 'EversionVel') == 1
                 
                 ShoeMeans.(Group).(Var).(Shoe)(:,count) = LoftTunedData.(ID).(Shoe).(Var).Mean;
                 
             elseif strcmp(Var, 'AnkleEversion') == 1
                 
                 ShoeMeans.(Group).(Var).(Shoe)(:,count) = LoftTunedData.(ID).(Shoe).(Var).Mean;
                 
             end
             
             
         end
     end
 end

 T = cell2table({'Subj_ID', 'ShoeCnd', 'PeakKneeFlex', 'KneeFlexROM',...
     'PeakKneeAdd', 'KneeAddROM', 'PeakTibRot', 'TibRotROM',...
     'PeakAnkFlex', 'AnkFlexROM', 'PeakAnkEv', 'AnkEvROM', 'PeakAnkAdd', 'AnkAddROM',...
     'InclinationAngle', 'MaxEvVel'}) 

T = plot4lvlstruct(LoftTunedData);
if isfile('LoftTunedData.mat') == 1
    DataTable = vertcat(DataTable, T);
else
    DataTable = T;
end

 save('LoftTunedData.mat', 'LoftTunedData', 'DataTable', 'stepIdx', 'ShoeMeans')
 

 
 %% Section 3 - calculate impact peaks and deviation values - plots knee 
 % flexion, knee abd/adduction, tibial rotation, and ankle eversion for 
 % each shoe (+ squat) and marks the angles used for deviation calculation      

 %Calculate deviation
 savefolder = uigetdir('Pick folder');
 
 ID_list = unique(T.Subj_ID)
 Shoe_list = unique(T.ShoeCnd)
 shoenum = length(Shoe_list)
 color_list = {'ro', 'bo', 'rx', 'bx', 'rd', 'bd'};
 
 for i = 1:length(ID_list)
     
     ID = char(ID_list(i));
     
     for j = 1:(length(Shoe_list))
         
         Shoe = char(Shoe_list(j));
         if j < (length(color_list)+1)
         mk_prop = char(color_list(j));
         else
         end
        % x1 = ismember(T.Subj_ID, cellstr(ID)) == 1 & ismember(T.ShoeCnd, cellstr(Shoe));
        % x2 = ismember(T.ShoeCnd, cellstr(Shoe));
         t_idx = ismember(T.Subj_ID, cellstr(ID)) == 1 & ismember(T.ShoeCnd, cellstr(Shoe));
         
         if strcmp(Shoe, 'Squat_Trial') == 1
             T.ImpactPeak(t_idx) = 0;
             T.VALR(t_idx) = 0;
             T.EvDev(t_idx) = 0;
             T.TibRotDev(t_idx) = 0;
             T.KneeAddDev(t_idx) = 0;
             T.TotalKneeDev(t_idx) = 0;    
             
         else
             
             figure(100-i), plot(LoftTunedData.(ID).(Shoe).GRFz.Mean), ...
                 title(strcat(ID, ' - ', Shoe))
             pk_idx = ginput(1);
             if isempty(pk_idx) == 1
                 pk_idx = .2*length(LoftTunedData.(ID).(Shoe).GRFz.Mean);
                 T.ImpactPeak(t_idx) = max(LoftTunedData.(ID).(Shoe).GRFz.Mean(round(pk_idx)));
             else
                 T.ImpactPeak(t_idx) = max(LoftTunedData.(ID).(Shoe).GRFz.Mean(1:round(pk_idx(1))));
                 
             end
             
             T.VALR(t_idx) = (LoftTunedData.(ID).(Shoe).GRFz.Mean(round(pk_idx(1)*.8)) - ...
                 LoftTunedData.(ID).(Shoe).GRFz.Mean(round(pk_idx(1)*.2))) / ...
                 (round(pk_idx(1)*.8) - round(pk_idx(1)*.2));
             
             %find max knee angle in run (critical knee angle)
             [crit_knee_run, run_idx] = min(LoftTunedData.(ID).(Shoe).KneeFlexion.Mean);
             
             figure(i), subplot(4,shoenum,(j+1)), plot(LoftTunedData.(ID).(Shoe).KneeFlexion.Mean, 'k')
             title(Shoe)
             hold on
             figure(i), subplot(4,shoenum,(j+1)), scatter(run_idx, crit_knee_run, mk_prop)
             
             %find where critical knee angle occurs in eccentric portion of squat -
             %isolate eccentric portion by finding time between initiation and peak
             %angle
             [peak_squat_ang, pksq] = min(LoftTunedData.(ID).(Shoe).KneeFlexion.Mean); %indexing peak squat angle to find eccentric portion
             
             %since data has been resampled to 100 points, the exact
             %critical knee angle may not appear in the squat data. 
             %Find closest angle by finding the minumum absolute difference
             %between critical knee angle and (eccentric portion) squat data 
             [diff, squat_idx] = min(abs(...
                 LoftTunedData.(ID).Squat_Trial.KneeFlexion.Mean(1:pksq)-crit_knee_run));
             
             crit_knee_squat = LoftTunedData.(ID).Squat_Trial.KneeFlexion.Mean(squat_idx);
             figure(i), subplot(4,shoenum,1), plot(LoftTunedData.(ID).Squat_Trial.KneeFlexion.Mean, 'k')
             title('Squat')
             ylabel('Knee Flex.')
             hold on
             figure(i), subplot(4,shoenum,1), scatter(squat_idx, crit_knee_squat, mk_prop)
             
             %find angles (tib rot, eversion, knee abd/add) at time point of crit
             %knee angle in run
             
             crit_ev_run = LoftTunedData.(ID).(Shoe).AnkleEversion.Mean(run_idx);
             figure(i), subplot(4,shoenum,shoenum+(j+1)), plot(LoftTunedData.(ID).(Shoe).AnkleEversion.Mean, 'k')
             hold on
             figure(i), subplot(4,shoenum,shoenum+(j+1)), scatter(run_idx, crit_ev_run, mk_prop)
             
             crit_TR_run = LoftTunedData.(ID).(Shoe).TibialRotation.Mean(run_idx);
             figure(i), subplot(4,shoenum,(2*shoenum+(j+1))), plot(LoftTunedData.(ID).(Shoe).TibialRotation.Mean, 'k')
             hold on
             figure(i), subplot(4,shoenum,(2*shoenum+(j+1))), scatter(run_idx, crit_TR_run, mk_prop)
             
             crit_KA_run = LoftTunedData.(ID).(Shoe).KneeAdduction.Mean(run_idx);
             figure(i), subplot(4,shoenum,(3*shoenum+(j+1))), plot(LoftTunedData.(ID).(Shoe).KneeAdduction.Mean, 'k')
             hold on
             figure(i), subplot(4,shoenum,(3*shoenum+(j+1))), scatter(run_idx, crit_KA_run, mk_prop)
             
             %find angles at time point of critical knee angle in squat
             crit_ev_squat = LoftTunedData.(ID).Squat_Trial.AnkleEversion.Mean(squat_idx);
             figure(i), subplot(4,shoenum,shoenum+1), plot(LoftTunedData.(ID).Squat_Trial.AnkleEversion.Mean, 'k')
             ylabel('Eversion')
             hold on
             figure(i), subplot(4,shoenum,shoenum+1), scatter(squat_idx, crit_ev_squat, mk_prop)
             
             crit_TR_squat = LoftTunedData.(ID).Squat_Trial.TibialRotation.Mean(squat_idx);
             figure(i), subplot(4,shoenum,2*shoenum+1), plot(LoftTunedData.(ID).Squat_Trial.TibialRotation.Mean, 'k')
             ylabel('Tibial Rotation')
             hold on
             figure(i), subplot(4,shoenum,2*shoenum+1), scatter(squat_idx, crit_TR_squat, mk_prop)
             
             crit_KA_squat = LoftTunedData.(ID).Squat_Trial.KneeAdduction.Mean(squat_idx);
             figure(i), subplot(4,shoenum,3*shoenum+1), plot(LoftTunedData.(ID).Squat_Trial.KneeAdduction.Mean, 'k')
             ylabel('Knee Add/Abd')
             hold on
             figure(i), subplot(4,shoenum,3*shoenum+1), scatter(squat_idx, crit_KA_squat, mk_prop)
             
             figfilename = strcat(savefolder, '\', ID, '_HMP');
             savefig(figfilename)
             saveas(figure(i), figfilename, 'png')
             
             %subtract squat values from run values to get deviation
             Dev_Ev =  crit_ev_run - crit_ev_squat;
             Dev_TR =  crit_TR_run - crit_TR_squat;
             Dev_KA =  crit_KA_run - crit_KA_squat;
             
             %total knee deviation
             Dev_Total_Knee = abs(Dev_TR) + abs(Dev_KA);
             
             %add values back to table
             
             T.EvDev(t_idx) = Dev_Ev;
             T.TibRotDev(t_idx) = Dev_TR;
             T.KneeAddDev(t_idx) = Dev_KA;
             T.TotalKneeDev(t_idx) = Dev_Total_Knee;
         end

     end
     s_idx = ismember(T.Subj_ID, cellstr(ID)) == 1;
     sock_idx = ismember(T.ShoeCnd, cellstr('Sock_8min')) == 1 & ismember(T.Subj_ID, cellstr(ID)) == 1;
    if T.TotalKneeDev(sock_idx) >= 7
         T.DevGroup(s_idx) = cellstr('High');
    else
         T.DevGroup(s_idx) = cellstr('Low');
    end
     close(figure(i))
 end
 
  save('LoftTunedData.mat', 'LoftTunedData', 'DataTable', 'stepIdx', 'T',...
     'ShoeMeans') %, 'GRFmeansH', 'EvVELmeansH', 'AnkEVmeansH', ...
     %'GRFmeansL', 'EvVELmeansL', 'AnkEVmeansL')
    %%%add code to get deviator group (7 as threshold in sock) - do in R?
    %%%add code to get 'LoftTuned Dev' = ShoeCnd - Control?
    %%%add code to plot shoe means across stance
  %% 
    
%add code to get mass from GRFz

  writetable(T, 'LoftTunedData_080821.xlsx')


