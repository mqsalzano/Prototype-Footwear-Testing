function [T] = plot4lvlstruct(StudyData)
%Function to loop through data structure, plot ensemble curves for each joint angle for each shoe, and save plots
%   Detailed explanation goes here
T=table();
count = 0;

ids = fieldnames(StudyData);
expfolder = uigetdir('Pick folder to save curves');
for i = 1:length(ids)
    
    ID = char(ids(i));
    shoes = fieldnames(StudyData.(ID));
    
    for j = 1:length(shoes)
        
        Shoe = char(shoes(j));
        vars = fieldnames(StudyData.(ID).(Shoe));
        count = count + 1
        
        for k = 1:length(vars)
            
            
            Var = char(vars(k));
            
            plot(StudyData.(ID).(Shoe).(Var).Mean(:,1))
            hold on
            title(strcat(ID, ' - ', Shoe, ' - ', strrep(Var, '_', ' '), ' Mean'))
            
            tmpfilename = strcat(expfolder, '/', ID, '_', Shoe, '_', Var);

            
            T.Subj_ID(count) = cellstr(ID);
            T.ShoeCnd(count) = cellstr(Shoe);
            
            newVar1 = strcat('peak_', Var);
            newVar2 = strcat(Var, '_ROM');
            newVar3 = strcat(Var, '_@FS');
            
            if  strcmp(Var, 'InclinationAngle') == 1          
            T.InclinationAngleFS(count) = StudyData.(ID).(Shoe).InclinationAngle.Mean(1);
            scatter(1, StudyData.(ID).(Shoe).InclinationAngle.Mean(1), 'bo')
            
            elseif strcmp(Var, 'EversionVel') == 1
                newVar1 = strcat('POS_', newVar1);
                newVar2 = strcat('NEG_', newVar1);
            [T.(newVar1)(count), idx] = max(StudyData.(ID).(Shoe).EversionVel.Mean);
           [T.(newVar2)(count), idx2] = min(StudyData.(ID).(Shoe).EversionVel.Mean);
            scatter(idx, StudyData.(ID).(Shoe).EversionVel.Mean(idx), 'rd')
            scatter(idx2, StudyData.(ID).(Shoe).EversionVel.Mean(idx2), 'ro')
            
            elseif strcmp(Var, 'KneeFlexion')  == 1 | strcmp(Var, 'KneeAdduction') == 1 
                [T.(newVar1)(count), idx2] = min(StudyData.(ID).(Shoe).(Var).Mean);
                T.(newVar2)(count) = max(StudyData.(ID).(Shoe).(Var).Mean)-min(StudyData.(ID).(Shoe).(Var).Mean);
                T.(newVar3)(count) = StudyData.(ID).(Shoe).(Var).Mean(1);
                scatter(idx2, StudyData.(ID).(Shoe).(Var).Mean(idx2, 1), 'ro')
                scatter(1, StudyData.(ID).(Shoe).(Var).Mean(1, 1), 'bo')
            elseif strcmp(Var, 'AnkleEversion') == 1
                [T.(newVar1)(count), idx2] = min(StudyData.(ID).(Shoe).(Var).Mean(20:80,1));
                T.(newVar2)(count) = max(StudyData.(ID).(Shoe).(Var).Mean)-min(StudyData.(ID).(Shoe).(Var).Mean);
                T.(newVar3)(count) = StudyData.(ID).(Shoe).(Var).Mean(1);
                scatter((idx2+20), StudyData.(ID).(Shoe).(Var).Mean((idx2+20), 1), 'ro')
                scatter(1, StudyData.(ID).(Shoe).(Var).Mean(1, 1), 'bo')
            else
            [T.(newVar1)(count), idx]= max(StudyData.(ID).(Shoe).(Var).Mean);
            T.(newVar2)(count) = max(StudyData.(ID).(Shoe).(Var).Mean)-min(StudyData.(ID).(Shoe).(Var).Mean); 
            T.(newVar3)(count) = StudyData.(ID).(Shoe).(Var).Mean(1);
            scatter(idx, StudyData.(ID).(Shoe).(Var).Mean(idx, 1), 'ro')
            scatter(1, StudyData.(ID).(Shoe).(Var).Mean(1, 1), 'bo')
            end
            savefig(tmpfilename)
            saveas(gcf, tmpfilename, 'png')
            close(gcf)
        end
    end
end

