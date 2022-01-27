function [normTrials] = norm2spline(tmpT, num_trials, Pidx)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here


%parse out the squat trials by gathering the data between peaks
normTrials = zeros(100,10);
for sq = 1:num_trials
    tmpTrial  = (...
        tmpT((Pidx(sq)):(Pidx(sq+1)),:));
    normTrials(:,sq) = spline(...
        tmpTrial(:,1), tmpTrial(:,2), linspace(Pidx(sq), Pidx(sq+1)));
end
end

