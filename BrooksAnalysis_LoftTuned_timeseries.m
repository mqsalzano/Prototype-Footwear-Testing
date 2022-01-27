HighDev = unique(T.Subj_ID(ismember(T.DevGroup, "High")), 'stable');
LowDev = unique(T.Subj_ID(ismember(T.DevGroup, "Low")), 'stable');

HighDevIdx = ismember(unique(T.Subj_ID, 'stable'), HighDev);
LowDevIdx = ismember(unique(T.Subj_ID, 'stable'), LowDev);

StanceData = table(0,0,0,0,0,0,0,0);
StanceData.Properties.VariableNames = {'Subj_ID', 'DevGroup', 'PctStance', 'C1', 'C2','C3','C4','C5'};
SubjList = unique(T.Subj_ID, 'stable');
ShoeList = fieldnames(ShoeMeans.All.GRFz);
NormTime = [1:100]';

for i = 1:length(SubjList)
   
    tmpDev = {};
    tmpID = {};
    if ismember(SubjList(i), HighDev) == 1
        DevStatus = "High"
    else
        DevStatus = "Low"
    end
        
    ID = char(SubjList(i))
    
    for k = 1:100
        tmpDev(k) = cellstr(DevStatus);
        tmpID(k) = cellstr(ID);
    end
    
     tmpDev = tmpDev'
     tmpID = tmpID'
     tmpTrial = zeros(100,1)
     
     for shoe = 1:5
         ShoeID = char(ShoeList(shoe))
         tmpTrial(:,shoe) = ShoeMeans.All.GRFz.(ShoeID)(:,i)
     end
         
     tmpT = horzcat(table(tmpID, tmpDev, NormTime), array2table(tmpTrial));  
    tmpT.Properties.VariableNames = {'Subj_ID', 'DevGroup', 'PctStance', 'C1', 'C2','C3','C4','C5'}
     
     StanceData = vertcat(StanceData, tmpT)
end

StanceData(1,:) = [];

writetable(StanceData, 'GRFzStance.xlsx')