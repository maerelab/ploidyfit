
% Steven Maere, 2025
%
% function calculating sqrt(chi-squared error between fitted model and
% data), used as minimization criterion in whole_dataset_model.m

function f = linfit(data, prm)

y=[];
y2=[];
stdev=[];
x = (1:1:22)' ;

for(i=0:1:7)

    cell=[];
    cell(:,:) = data(i+1,:,:);
    av_cell=[];
    stdev_cell=[];
    
    % delete last 2 cell files 9 and 10 for ccs52a1 H and N because no data
    if((i==4)||(i==5)) 
        cell(:,10) = [];
        cell(:,9) = [];
    end
    
    for j=1:1:size(cell,1)
        av_cell(j) = mean(cell(j,~isnan(cell(j,:))));
        stdev_cell(j) = std(cell(j,~isnan(cell(j,:))));
    end
    
    % first and last cell files often contain fewer data points restricting
    % stdev, e.g. first stdev for control atrichoblasts is 0 and last (22nd)
    % stdev for ccs52a1 atrichoblasts is 0 because only 1 data point, causing trouble in optimization. 
    % Put all stdevs for cell numbers with <4 data points to stdevs of following cell (for first cell) or preceding cell (for last cell) 
    
    if(sum(~isnan(cell(1,:)))<4)
        stdev_cell(1) = stdev_cell(2);
    end
    
    if(sum(~isnan(cell(size(av_cell,2),:)))<4)
        stdev_cell(size(av_cell,2)) = stdev_cell(size(av_cell,2)-1);
    end

    LL = find(~isnan(av_cell));
    y3 = zeros(size(x(LL)));
    x3 = x(LL);
    
    for k = 1:1:length(x3)
        if x3(k) < prm(3*i+3)
            y3(k) = prm(1) ;
        elseif x3(k) < prm(3*i+4)
            y3(k) = prm(2)*prm(1) ; 
        elseif x3(k) < prm(3*i+5)
            y3(k) = (prm(2)^2)*prm(1) ; 
        else
            y3(k) = (prm(2)^3)*prm(1) ;
        end
    end
    
    y = [y av_cell(LL)];
    stdev = [stdev stdev_cell(LL)];
    y2 = [y2 y3'];

end

f = sqrt(((y-y2)./stdev)*((y-y2)./stdev)');

end
