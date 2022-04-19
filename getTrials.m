function [pks, Pidx] = getTrials(tmpT)
%Function to separate out squats from squat trial
%   Detailed explanation goes here
plot((tmpT(:,2))) %plot the squat data
title('Knee Angle X - Squat')
pts = round(ginput(12));    %gather point indices, roughly at peaks (AKA closest to 0)
pks = zeros(length(pts)-1,1);
Pidx = zeros(length(pts)-1,1);
%run a for-loop to get the max and its index between each
%"minimum" as these will be the indications to separate trials
for k = 1:length(pks)
    %if k == 1
        [pks(k), Pidx(k)] = max(...
            tmpT(pts(k,1):(pts(k+1,1)),2));
        Pidx(k) = Pidx(k) + pts((k),1);
   % elseif k == (length(pts)+1)
   %     [pks(k), Pidx(k)] = max((...
   %         tmpT((pts((k-1),1)):end,2)));
   %     Pidx(k) = Pidx(k) + pts((k-1),1);
   % else
   %     [pks(k), Pidx(k)] = max(...
   %         tmpT(   (pts((k-1),1)):(pts(k,1))   ,2));
   %    Pidx(k) = Pidx(k) + pts((k-1),1);
    %end
end

end

