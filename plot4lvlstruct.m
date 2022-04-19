function [T] = plot4lvlstruct(LoftV4)
%Function to loop through data structure, plot ensemble curves for each joint angle for each shoe, and save plots
%   Detailed explanation goes here
T=table();
count = 0;

ids = fieldnames(LoftV4);
expfolder = uigetdir('Pick folder to save curves');
for i = 1:length(ids)
    
    ID = char(ids(i));
    shoes = fieldnames(LoftV4.(ID));
    
    for j = 1:length(shoes)
        
        Shoe = char(shoes(j));
        vars = fieldnames(LoftV4.(ID).(Shoe));
        count = count + 1
        
        for k = 1:length(vars)
            
            
            Var = char(vars(k));
            
            plot(LoftV4.(ID).(Shoe).(Var).Mean(:,1))
            hold on
            title(strcat(ID, ' - ', Shoe, ' - ', strrep(Var, '_', ' '), ' Mean'))
            
            tmpfilename = strcat(expfolder, '/', ID, '_', Shoe, '_', Var);

            
            T.Subj_ID(count) = cellstr(ID);
            T.ShoeCnd(count) = cellstr(Shoe);
            
            newVar1 = strcat('peak_', Var);
            newVar2 = strcat(Var, '_ROM');
            newVar3 = strcat(Var, '_@FS');
            
            if  strcmp(Var, 'InclinationAngle') == 1          
            T.InclinationAngleFS(count) = LoftV4.(ID).(Shoe).InclinationAngle.Mean(1);
            scatter(1, LoftV4.(ID).(Shoe).InclinationAngle.Mean(1), 'bo')
            
            elseif strcmp(Var, 'EversionVel') == 1
                newVar1 = strcat('POS_', newVar1);
                newVar2 = strcat('NEG_', newVar1);
            [T.(newVar1)(count), idx] = max(LoftV4.(ID).(Shoe).EversionVel.Mean);
           [T.(newVar2)(count), idx2] = min(LoftV4.(ID).(Shoe).EversionVel.Mean);
            scatter(idx, LoftV4.(ID).(Shoe).EversionVel.Mean(idx), 'rd')
            scatter(idx2, LoftV4.(ID).(Shoe).EversionVel.Mean(idx2), 'ro')
            
            elseif strcmp(Var, 'KneeFlexion')  == 1 | strcmp(Var, 'KneeAdduction') == 1 
                [T.(newVar1)(count), idx2] = min(LoftV4.(ID).(Shoe).(Var).Mean);
                T.(newVar2)(count) = max(LoftV4.(ID).(Shoe).(Var).Mean)-min(LoftV4.(ID).(Shoe).(Var).Mean);
                T.(newVar3)(count) = LoftV4.(ID).(Shoe).(Var).Mean(1);
                scatter(idx2, LoftV4.(ID).(Shoe).(Var).Mean(idx2, 1), 'ro')
                scatter(1, LoftV4.(ID).(Shoe).(Var).Mean(1, 1), 'bo')
            elseif strcmp(Var, 'AnkleEversion') == 1
                [T.(newVar1)(count), idx2] = min(LoftV4.(ID).(Shoe).(Var).Mean(20:80,1));
                T.(newVar2)(count) = max(LoftV4.(ID).(Shoe).(Var).Mean)-min(LoftV4.(ID).(Shoe).(Var).Mean);
                T.(newVar3)(count) = LoftV4.(ID).(Shoe).(Var).Mean(1);
                scatter((idx2+20), LoftV4.(ID).(Shoe).(Var).Mean((idx2+20), 1), 'ro')
                scatter(1, LoftV4.(ID).(Shoe).(Var).Mean(1, 1), 'bo')
            else
            [T.(newVar1)(count), idx]= max(LoftV4.(ID).(Shoe).(Var).Mean);
            T.(newVar2)(count) = max(LoftV4.(ID).(Shoe).(Var).Mean)-min(LoftV4.(ID).(Shoe).(Var).Mean); 
            T.(newVar3)(count) = LoftV4.(ID).(Shoe).(Var).Mean(1);
            scatter(idx, LoftV4.(ID).(Shoe).(Var).Mean(idx, 1), 'ro')
            scatter(1, LoftV4.(ID).(Shoe).(Var).Mean(1, 1), 'bo')
            end
            savefig(tmpfilename)
            saveas(gcf, tmpfilename, 'png')
            close(gcf)
        end
    end
end

