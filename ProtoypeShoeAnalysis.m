%% Section 1A - get the time indices for each squat in the squat trials and
% for each stance phase in the run trials (one subject at a time)

%import variables the put 10 trials into a structure
clear all

%import ASCII file - time series of entire 15-30 second trial
%create structure for each subject with each condition as a substructure,
%each of which holds the variables, which at 101x10 matrices


%pick folder with data files
expfolder = uigetdir('Pick folder');
tmpFiles = dir(expfolder);
tmpFiles(ismember({tmpFiles.name},{'.','..'})) = [];
tmpFiles(ismember({tmpFiles.name},'.DS_Store')) = [];

%create structure or load previous structure
if isfile('StudyData.mat') == 1
    load('StudyData.mat')
else
    StudyData = struct;
    DataTable = table();
    stepIdx = struct;
end

%grab right knee angle for squat trial only to find peaks - needed for deviation calculation later
filelist = {tmpFiles(:).name};
tmpfilename = tmpFiles(...
    contains(string(filelist), 'Squat_Trial') &...
    contains(string(filelist), 'KneeFlexion') == 1).name;
KneeAngleSquat = table2array(...
    readtable(strcat(expfolder, '\', tmpfilename)));
[pks, Pidx] = getTrials(KneeAngleSquat);

% Next portion of code essentially replicates V3D's step detection (heel strike to toe off)
% for each shoe condition, load in GRFz to separate out each step and grab time indices for each step
% time indices will be used in order to separate out steps for every other variable
shoelist = {'C1', 'C2', 'C3', 'C4', 'Sock'};
expfolder2 = uigetdir()

% used V3D to note which full GRFz peak in the full time series
% was the first step with the right leg for each condition
% input for the next line is an array (e.g. [1 2 2 1 2] ) 
first_step_peak = input('What is the order of first peak for right leg?: ')

for cnd = 1:length(shoelist)
    
   % appending to '_8min' to the shoe condition in order to find correct file name
   % need to do this based on our own naming convention
    % if cnd < length(shoelist)
    Shoe = strcat( char(shoelist(cnd)), '_8min');
    % else
    %Shoe = char(shoelist(cnd));  %used this if '_8min' wasn't part of Sock file name
    % end
    
    tmpfilename = tmpFiles(...
        contains(string(filelist), 'GRFz') &...
        contains(string(filelist), Shoe) == 1).name;
    idx = strfind(tmpfilename,'_');
    
    ID = tmpfilename((idx(end)+1):(end-4)); %grabbing Subject ID
    
    
    tmpGRFz = table2array(readtable(strcat(expfolder, '/', tmpfilename)));
    tmpGRFz = tmpGRFz(1:10:length(tmpGRFz),:); %reducing number of data points to match motion capture data
    figure(1),plot(tmpGRFz(1:1600,2)) % plotting enough time to capture at least 10 steps
    figure(1), hold on
    title(tmpfilename)
    
    
    %click portion of time series before the first full GRFz peak in waveform
	% NOT first step by right leg - that is handled by the input from earlier
    tmpstart = round(ginput(1))
    if tmpstart(1) < 1
        tmpstart(1) = 1 %if click is accidentally to left of y-axis, auto-sets value to x = 1
    end

    nonzeroGRFidx = tmpGRFz(tmpstart(1):(tmpstart(1)+1600),2) > 0.075; % using a GRFz detection threshold to find when a GRFz peak occurs

    transitions = diff(nonzeroGRFidx) == 1;
    transitions2 = (diff(nonzeroGRFidx) == -1);
    FS = (find(transitions == 1)+tmpstart(1)); %finding heel strike indices of all GRFz peaks
    TO = (find(transitions2 == 1)+tmpstart(1)); %finding toe off indices of all GRFz peaks
   
    
    startpeak = first_step_peak(cnd);

% the peak of the first step determines how the FS and TO indices are accessed
    if startpeak == 1
        
        for stepnum = 1:10
            Step = strcat('Step', num2str(stepnum));
            
            tmpStepIdx = (FS(2*stepnum-1)-1):(TO(2*stepnum-1));
            
            figure(2), plot(tmpGRFz(tmpStepIdx,2))
            figure(2), hold on
            figure(1), area(tmpStepIdx, tmpGRFz(tmpStepIdx,2))
          
            stepIdx.(ID).(Shoe).(Step) = tmpStepIdx;
            
        end
        saveas(figure(2), strcat(expfolder2, '\', ID, '_', Shoe, '_GRFz_IndivdualSteps'))
        close(figure(2))
    elseif startpeak == 2
        
        for stepnum = 1:10
            Step = strcat('Step', num2str(stepnum));
            
            tmpStepIdx = (FS(2*stepnum)-1):(TO(2*stepnum));
            figure(2), plot(tmpGRFz(tmpStepIdx,2))
            hold on

            figure(1), area(tmpStepIdx, tmpGRFz(tmpStepIdx,2))
            stepIdx.(ID).(Shoe).(Step) = tmpStepIdx;
            
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

StaticMeans = findStaticAngles(expfolder);
for i = 1:length(tmpFiles)
    
    %get file name
    tmpfilename = tmpFiles(i).name;
    
    if contains(tmpfilename, 'Static') == 1  %ignores static trials at the moment
        continue
    else

        %parse information from the filename
        idx = strfind(tmpfilename,'_');
        ID = tmpfilename((idx(end)+1):(end-4))
        Shoe = tmpfilename((idx(end-2)+1):(idx(end)-1))
        Var = tmpfilename(1:(idx(end-2)-1))
        if strfind(Shoe, 'Squat') == 1
            ShoeStatic = strrep(Shoe, 'Trial', 'Static')
        else
            ShoeStatic = strrep(Shoe, '8min', 'Static')
        end
        
        %read ASCII file into a table
        tmpT_raw = table2array(readtable(strcat(expfolder, '/', tmpfilename)))
        if strcmp(Var, 'GRFz') == 1 %&& strcmp(Shoe, 'Squat_Trial')~= 1
            tmpT_raw = tmpT_raw(1:10:length(tmpT_raw),:);
        end
        
        if strcmp(Var, 'GRFz') == 1 || strcmp(Var, 'EversionVel') == 1
            tmpT = tmpT_raw
        else
            tmpT = tmpT_raw - StaticMeans.(ID).(ShoeStatic).(Var)
        end
        
        figure(50), plot(tmpT(:,2)), hold on, plot(tmpT_raw(:,2))
        title(strcat(ID, ' - ', Shoe, ' - ', Var, ' - ', 'All Steps'))
        savefig(strcat(rawdataplotfolder, '\', ID, '_', Shoe, '_', Var, '_FullWaveform'))
        close(figure(50))
        
        if tmpFiles(i).bytes < 9999  %in case any didn't export right,
            continue                %this keeps it from failing
        else
            
        %special condition for squat trial because individual squats are parsed using
        %external function from above
            if strcmp(Shoe, 'Squat_Trial') == 1  %treating the squat trial as a shoe condition for naming convention
                
                num_trials = (length(Pidx)-1);
                normTrials = zeros(100,10);
                
                for sq = 1:num_trials
                    Squat = strcat(Shoe, num2str(sq));

			%plotting raw data
                    tmpTrial  = (...
                        tmpT((Pidx(sq)):(Pidx(sq+1)),:));
                    StudyData.(ID).(Shoe).(Var).RawData.(Squat) = tmpTrial(:,2);
                    figure(500+i), plot(tmpTrial(:,2))
                    title(strcat(ID, ' - ', Shoe, ' - ', Var, ' - Raw'))
                    hold on
                    
			%plotting time-normalized data
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
                StudyData.(ID).(Shoe).(Var).NormData = normTrials;

            else
 %using step indices from above to parse out joint angles for each step
        %then plotting raw data and time-normalized data for each

                normTrials = zeros(100,10);
                for stepnum = 1:length(fieldnames(stepIdx.(ID).(Shoe)))
                    
                    Step = strcat('Step', num2str(stepnum));
                    s_idx = (stepIdx.(ID).(Shoe).(Step));
                    StudyData.(ID).(Shoe).(Var).RawData.(Step) = tmpT(s_idx,2);
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

%adding legend labels to joint angle plots then saving them
                figure(100+i), legend(legendLabels)
                saveas(figure(100+i), strcat(rawdataplotfolder, '\', ID, '_', Shoe, '_', Var, ' _Raw'), 'png');
                close(figure(100+i))
                
                figure(200+i), legend(legendLabels)
                saveas(figure(200+i), strcat(normdataplotfolder, '\', ID, '_', Shoe, '_', Var, ' _Norm'), 'png');
                close(figure(200+i))
                
                StudyData.(ID).(Shoe).(Var).NormData = normTrials;
                
            end
        end
        %end
    end
    
end
save('StudyData.mat', 'StudyData', 'DataTable', 'stepIdx', 'StaticMeans')

%% Section 2: Caclulate ensemble curves for each parameter for each shoe for each
%subject (can do multiple subjects at one time)
ShoeMeans = struct;
ids = fieldnames(StudyData);
count = 0
for i = 1:length(ids)
    
    ID = char(ids(i));
    shoes = fieldnames(StudyData.(ID));
    
    Group = 'All';  %if separating subjects based on group, add code here to do so
    count = count + 1;
    
    for s = 1:length(shoes)
        
        Shoe = char(shoes(s));
        vars = fieldnames(StudyData.(ID).(Shoe));
        
        for v = 1:length(vars)
            
            Var = char(vars(v));
            
            for d = 1:length(StudyData.(ID).(Shoe).(Var).NormData)
                
                StudyData.(ID).(Shoe).(Var).Mean(d,1) = mean(...
                    StudyData.(ID).(Shoe).(Var).NormData(d,:));
                
            end
            
             % if separating subjects into groups, the following 'if-else' 
             % creates an ensemble curve for each group for the specified variables
            if strcmp(Var, 'GRFz') == 1
                
                ShoeMeans.(Group).(Var).(Shoe)(:,count) = StudyData.(ID).(Shoe).(Var).Mean;
                
            elseif strcmp(Var, 'EversionVel') == 1
                
                ShoeMeans.(Group).(Var).(Shoe)(:,count) = StudyData.(ID).(Shoe).(Var).Mean;
                
            elseif strcmp(Var, 'AnkleEversion') == 1
                
                ShoeMeans.(Group).(Var).(Shoe)(:,count) = StudyData.(ID).(Shoe).(Var).Mean;
                
            elseif strcmp(Var, 'Resupination') == 1
                    
                ShoeMeans.(Group).(Var).(Shoe)(:,count) = StudyData.(ID).(Shoe).(Var).Mean;
                
            end
            
            
        end
    end
end

T = cell2table({'Subj_ID', 'ShoeCnd', 'PeakKneeFlex', 'KneeFlexROM',...
    'PeakKneeAdd', 'KneeAddROM', 'PeakTibRot', 'TibRotROM',...
    'PeakAnkFlex', 'AnkFlexROM', 'PeakAnkEv', 'AnkEvROM', 'PeakAnkAdd', 'AnkAddROM',...
    'InclinationAngle', 'MaxEvVel'})

T = plot4lvlstruct(StudyData);  %external function to plot & save each ensemble curve for each shoe for each subject
if isfile('StudyData.mat') == 1
    DataTable = vertcat(DataTable, T);
else
    DataTable = T;
end

save('StudyData.mat', 'StudyData', 'DataTable', 'stepIdx', 'ShoeMeans', 'StaticMeans')



%% Section 3 - calculate impact peaks and deviation values - plots knee
% flexion, knee abd/adduction, tibial rotation, and ankle eversion for
% each shoe (+ squat) and marks the angles used for deviation calculation

savefolder = uigetdir('Pick folder');

ID_list = unique(T.Subj_ID)
Shoe_list = unique(T.ShoeCnd)
shoenum = length(Shoe_list)
color_list = {'ro', 'bo', 'rx', 'bx', 'rd', 'bd'};
Fs= 200
for i = 1:length(ID_list)
    
    ID = char(ID_list(i));
    
    marker_folder = uigetdir();
    markerFiles = dir(marker_folder)
    markerFiles(ismember({markerFiles.name},{'.','..'})) = [];
    markerFiles(ismember({markerFiles.name},'.DS_Store')) = [];
    
    for j = 1:(length(Shoe_list))
        
        Shoe = char(Shoe_list(j));
        if j < (length(color_list)+1)
            mk_prop = char(color_list(j));
        else
        end

        t_idx = ismember(T.Subj_ID, cellstr(ID)) == 1 & ismember(T.ShoeCnd, cellstr(Shoe));
        
%Squat trial won't have values for the following variables, so setting to -
        if strcmp(Shoe, 'Squat_Trial') == 1
            T.ImpactPeak(t_idx) = 0;
            T.VALR(t_idx) = 0;
            T.EvDev(t_idx) = 0;
            T.TibRotDev(t_idx) = 0;
            T.KneeAddDev(t_idx) = 0;
            T.TotalKneeDev(t_idx) = 0;
            T.Cadence(t_idx) = 0;
            T.ContactTime(t_idx) = 0;
            T.JerkRF(t_idx) = 0;
            T.JerkFF(t_idx) = 0;
        else
            
		%plotting ensemble GRFz for each shoe condition
             %if impact peak exists, then click in the 'valley' after impact peak
             %which allows code to find value of impact peak by finding the max between FS and click
             %if no discernible impact peak exists, just hit 'Enter' and impact peak will be designated
             %as 20% of GRFz peak
            figure(100-i), plot(StudyData.(ID).(Shoe).GRFz.Mean), ...
                title(strcat(ID, ' - ', Shoe))
            pk_idx = ginput(1);
            if isempty(pk_idx) == 1
                pk_idx = .2*length(StudyData.(ID).(Shoe).GRFz.Mean);
                T.ImpactPeak(t_idx) = max(StudyData.(ID).(Shoe).GRFz.Mean(round(pk_idx)));
            else
                T.ImpactPeak(t_idx) = max(StudyData.(ID).(Shoe).GRFz.Mean(1:round(pk_idx(1))));
                
            end
            
            T.VALR(t_idx) = (StudyData.(ID).(Shoe).GRFz.Mean(round(pk_idx(1)*.8)) - ...
                StudyData.(ID).(Shoe).GRFz.Mean(round(pk_idx(1)*.2))) / ...
                (round(pk_idx(1)*.8) - round(pk_idx(1)*.2));
            
            %find max knee angle in run (critical knee angle)
            [crit_knee_run, run_idx] = min(StudyData.(ID).(Shoe).KneeFlexion.Mean);
            
 		%creating a figure with subplots where each row is a variable 
             % (knee flexion, tibial rotation, knee add/abd, and ankle eversion)
             % and each column is a condition (shoe condition + squat)
             % each subplot is an ensemble curve with the time index highlighted on the curve
            figure(i), subplot(4,shoenum,(j+1)), plot(StudyData.(ID).(Shoe).KneeFlexion.Mean, 'k')
            title(Shoe)
            hold on
            figure(i), subplot(4,shoenum,(j+1)), scatter(run_idx, crit_knee_run, mk_prop)
            
            %find where critical knee angle occurs in eccentric portion of squat -
            %isolate eccentric portion by finding time between initiation and peak
            %angle
            [peak_squat_ang, pksq] = min(StudyData.(ID).(Shoe).KneeFlexion.Mean); %indexing peak squat angle to find eccentric portion
            
            %since data has been resampled to 100 points, the exact
            %critical knee angle may not appear in the squat data.
            %Find closest angle by finding the minumum absolute difference
            %between critical knee angle and (eccentric portion) squat data
            [diff, squat_idx] = min(abs(...
                StudyData.(ID).Squat_Trial.KneeFlexion.Mean(1:pksq)-crit_knee_run));
            
            crit_knee_squat = StudyData.(ID).Squat_Trial.KneeFlexion.Mean(squat_idx);
            figure(i), subplot(4,shoenum,1), plot(StudyData.(ID).Squat_Trial.KneeFlexion.Mean, 'k')
            title('Squat')
            ylabel('Knee Flex.')
            hold on
            figure(i), subplot(4,shoenum,1), scatter(squat_idx, crit_knee_squat, mk_prop)
            
            %find angles (tibial rotation, eversion, knee abd/add) at time point 
            %of critical knee angle in run
            
            crit_ev_run = StudyData.(ID).(Shoe).AnkleEversion.Mean(run_idx);
            figure(i), subplot(4,shoenum,shoenum+(j+1)), plot(StudyData.(ID).(Shoe).AnkleEversion.Mean, 'k')
            hold on
            figure(i), subplot(4,shoenum,shoenum+(j+1)), scatter(run_idx, crit_ev_run, mk_prop)
            
            crit_TR_run = StudyData.(ID).(Shoe).TibialRotation.Mean(run_idx);
            figure(i), subplot(4,shoenum,(2*shoenum+(j+1))), plot(StudyData.(ID).(Shoe).TibialRotation.Mean, 'k')
            hold on
            figure(i), subplot(4,shoenum,(2*shoenum+(j+1))), scatter(run_idx, crit_TR_run, mk_prop)
            
            crit_KA_run = StudyData.(ID).(Shoe).KneeAdduction.Mean(run_idx);
            figure(i), subplot(4,shoenum,(3*shoenum+(j+1))), plot(StudyData.(ID).(Shoe).KneeAdduction.Mean, 'k')
            hold on
            figure(i), subplot(4,shoenum,(3*shoenum+(j+1))), scatter(run_idx, crit_KA_run, mk_prop)
            
            %find angles at time point of critical knee angle in squat
            crit_ev_squat = StudyData.(ID).Squat_Trial.AnkleEversion.Mean(squat_idx);
            figure(i), subplot(4,shoenum,shoenum+1), plot(StudyData.(ID).Squat_Trial.AnkleEversion.Mean, 'k')
            ylabel('Eversion')
            hold on
            figure(i), subplot(4,shoenum,shoenum+1), scatter(squat_idx, crit_ev_squat, mk_prop)
            
            crit_TR_squat = StudyData.(ID).Squat_Trial.TibialRotation.Mean(squat_idx);
            figure(i), subplot(4,shoenum,2*shoenum+1), plot(StudyData.(ID).Squat_Trial.TibialRotation.Mean, 'k')
            ylabel('Tibial Rotation')
            hold on
            figure(i), subplot(4,shoenum,2*shoenum+1), scatter(squat_idx, crit_TR_squat, mk_prop)
            
            crit_KA_squat = StudyData.(ID).Squat_Trial.KneeAdduction.Mean(squat_idx);
            figure(i), subplot(4,shoenum,3*shoenum+1), plot(StudyData.(ID).Squat_Trial.KneeAdduction.Mean, 'k')
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
            
            %Calculating cadence
            frames = stepIdx.(ID).(Shoe).Step10(1) - stepIdx.(ID).(Shoe).Step1(1);
            steps = 18;
            sampRate = 200;
            tmpCadence = (steps/frames)*sampRate*60;
            T.Cadence(t_idx) = round(tmpCadence);
            
		%Calculating contact time
            tmpCT = zeros(10,1);
            for heel_touch = 1:10
                Step = strcat('Step', num2str(heel_touch));
                step_frames = stepIdx.(ID).(Shoe).(Step)(end) - stepIdx.(ID).(Shoe).(Step)(1);
                tmpCT(heel_touch) = step_frames/sampRate;
            end
            T.ContactTime(t_idx) = mean(tmpCT);
            
		%load in files with rearfoot and forefoot information
            RF_filename = strcat('RF_', Shoe, '_', ID, '.txt');
            FF_filename = strcat('FF_', Shoe, '_', ID, '.txt');
            RF_pos = table2array(readtable(strcat(marker_folder, '/', RF_filename)));
            FF_pos = table2array(readtable(strcat(marker_folder, '/', FF_filename)));
            HS = zeros(10,1); TO = zeros(10,1);
            for step = 1:10
                Step = strcat('Step', num2str(step));
                HS(step) = stepIdx.(ID).(Shoe).(Step)(1);
                TO(step) = stepIdx.(ID).(Shoe).(Step)(end);
            end
            
            shoe_pos = [RF_pos(:,2:end)];
            shoe_pos(:,:,2) = [FF_pos(:,2:end)];
            
		%calculates jerk
            [jerk_cost, norm_jerk, kinem] = jerkAnalysis(shoe_pos, Fs, HS, TO)
            
            tmpJerkRF = jerk_cost.notnorm.stance(4,:,1);
            T.JerkRF(t_idx) = mean(tmpJerkRF);
            tmpJerkFF = jerk_cost.notnorm.stance(4,:,2);
            T.JerkFF(t_idx) = mean(tmpJerkFF);
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

save('StudyData.mat', 'StudyData', 'DataTable', 'stepIdx', 'T',...
    'ShoeMeans', 'StaticMeans') %, 'GRFmeansH', 'EvVELmeansH', 'AnkEVmeansH', ...

writetable(T, 'StudyData.xlsx')
